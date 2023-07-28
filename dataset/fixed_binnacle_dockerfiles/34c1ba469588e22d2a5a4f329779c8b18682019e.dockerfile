FROM ubuntu:14.04
ENV LC_ALL="C"
ENV PG_VERSION="9.6"
ENV KEYCLOAK_VERSION="3.4.3.Final"
ENV KONG_VERSION="0.10.1"
ENV MKT_VERSION_TAG="acc"
ENV PUB_VERSION_TAG="acc"
USER root
WORKDIR /tmp
COPY db-creation.sql /tmp
COPY t1g-inserts.sql /tmp
COPY t1g-schema.sql /tmp
COPY postgres.zip /tmp
COPY standalone.xml /tmp
COPY kong.conf /tmp
COPY start.sh /tmp
COPY application.conf /tmp
COPY wildfly_install.sh /tmp
COPY t1g-ear.ear /tmp
COPY kc-db-export.json /tmp
COPY wildfly-kc-import-service.sh /tmp
COPY wildfly-kc-no-import-service.sh /tmp
COPY key-auth-handler.lua /tmp
COPY fixtures.lua /tmp
COPY bower-mkt.json /tmp
COPY bower-pub.json /tmp
COPY config-mkt.yml /tmp
COPY config-pub.yml /tmp
COPY keycloak.json /tmp
RUN :
RUN apt-get -y upgrade
RUN ln -fs /usr/share/zoneinfo/Europe/Brussels /etc/localtime \
 && dpkg-reconfigure -f noninteractive tzdata
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 software-properties-common=0.92.37.8 nano=2.2.6-1ubuntu1 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 unzip=6.0-9ubuntu1.5 wget=1.15-1ubuntu1.14.04.5 sudo=1.8.9p5-1ubuntu1.4 netcat=1.10-40 openssl=1.0.1f-1ubuntu2.27 libpcre3=1:8.31-2ubuntu2.3 dnsmasq=2.68-1ubuntu0.2 procps=1:3.3.9-1ubuntu2.3 perl=5.18.2-2ubuntu1.7 -y )
RUN add-apt-repository -y ppa:webupd8team/java
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main" | sudo tee -a /etc/apt/sources.list.d/pgdg.list
RUN wget -q https://www.postgresql.org/media/keys/ACCC4CF8.asc -O - | sudo apt-key add -
RUN curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
RUN echo debconf shared/accepted-oracle-license-v1-1 select true | sudo debconf-set-selections
RUN echo debconf shared/accepted-oracle-license-v1-1 seen true | sudo debconf-set-selections
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 oracle-java8-installer postgresql-$PG_VERSION postgresql-client-$PG_VERSION postgresql-contrib-$PG_VERSION -y -q )
RUN npm install pm2@5.3.0 -g
#   Configure PostgreSQL
RUN echo "host all all 0.0.0.0/0 md5" >> /etc/postgresql/$PG_VERSION/main/pg_hba.conf
RUN echo "host all all ::0/0 md5" >> /etc/postgresql/$PG_VERSION/main/pg_hba.conf
RUN echo "listen_addresses='*'" >> /etc/postgresql/$PG_VERSION/main/postgresql.conf
USER postgres
RUN /etc/init.d/postgresql start \
 && psql -f db-creation.sql \
 && psql t1gengine -f t1g-schema.sql \
 && psql t1gengine -f t1g-inserts.sql
USER root
#   Install Wildfly & Keycloak overlay
RUN chmod u+x /tmp/wildfly_install.sh \
 && /tmp/wildfly_install.sh \
 && service wildfly stop \
 && rm -rf /var/run/wildfly
RUN wget https://downloads.jboss.org/keycloak/$KEYCLOAK_VERSION/keycloak-overlay-$KEYCLOAK_VERSION.zip \
 && mv /tmp/keycloak-overlay-$KEYCLOAK_VERSION.zip /opt \
 && unzip -o /opt/keycloak-overlay-$KEYCLOAK_VERSION.zip -d /opt/wildfly
RUN /opt/wildfly/bin/jboss-cli.sh --file=/opt/wildfly/bin/keycloak-install.cli
RUN mv /tmp/postgres.zip /opt/wildfly/modules/system/layers/base/org \
 && unzip /opt/wildfly/modules/system/layers/base/org/postgres.zip -d /opt/wildfly/modules/system/layers/base/org \
 && rm /opt/wildfly/modules/system/layers/base/org/postgres.zip \
 && mv /tmp/standalone.xml /opt/wildfly/standalone/configuration/standalone.xml \
 && /etc/init.d/postgresql start \
 && mv /tmp/wildfly-kc-import-service.sh /etc/init.d/wildfly \
 && service wildfly start \
 && service wildfly stop \
 && rm -rf /var/run/wildfly \
 && mv /tmp/wildfly-kc-no-import-service.sh /etc/init.d/wildfly
RUN mv /tmp/application.conf /opt/wildfly/standalone/configuration \
 && mv /tmp/t1g-ear.ear /opt/wildfly/standalone/deployments
RUN /etc/init.d/postgresql start \
 && wget https://github.com/Kong/kong/releases/download/$KONG_VERSION/kong-$KONG_VERSION.trusty_all.deb \
 && sudo dpkg -i kong-$KONG_VERSION.trusty_all.deb \
 && mv /tmp/kong.conf /etc/kong/kong.conf \
 && luarocks install kong-plugin-jwt-up \
 && mv /tmp/fixtures.lua /usr/local/share/lua/5.1/kong/plugins/jwt-up \
 && mv /tmp/key-auth-handler.lua /usr/local/share/lua/5.1/kong/plugins/key-auth/handler.lua \
 && kong start --vv
#   Install Marketplace + overwrite bower.json to correct Keycloak version
RUN wget https://github.com/Trust1Team/api-market/archive/$MKT_VERSION_TAG.zip \
 && unzip /tmp/$MKT_VERSION_TAG.zip -d /opt/ \
 && mv /opt/api-market-$MKT_VERSION_TAG /opt/api-market \
 && mv /tmp/bower-mkt.json /opt/api-market/bower.json \
 && cp /tmp/keycloak.json /opt/api-market/config \
 && mv /tmp/config-mkt.yml /opt/api-market/config/config.yml \
 && rm -rf /tmp/$MKT_VERSION_TAG.zip \
 && cd /opt/api-market \
 && npm run deploy
#   Install Publisher  + overwrite bower.json to correct Keycloak version
RUN wget https://github.com/Trust1Team/api-publisher/archive/$PUB_VERSION_TAG.zip \
 && unzip /tmp/$PUB_VERSION_TAG.zip -d /opt/ \
 && mv /opt/api-publisher-$PUB_VERSION_TAG /opt/api-publisher \
 && mv /tmp/bower-pub.json /opt/api-publisher/bower.json \
 && cp /tmp/keycloak.json /opt/api-publisher/config \
 && mv /tmp/config-pub.yml /opt/api-publisher/config/config.yml \
 && rm -rf /tmp/$PUB_VERSION_TAG.zip \
 && cd /opt/api-publisher \
 && npm run deploy
#  Cleanup
RUN rm -rf /opt/keycloak-overlay-$KEYCLOAK_VERSION.zip \
 && rm -rf /tmp/hsperf* \
 && rm -rf /tmp/kong-$KONG_VERSION.trusty_all.deb \
 && rm -rf /tmp/npm* \
 && rm -rf /tmp/phantomjs \
 && rm -rf /tmp/*.sql \
 && rm -rf /tmp/wildfly* \
 && rm -rf /tmp/v8-* \
 && rm -rf /opt/wildfly/bin/kc-db-export.json
RUN ["chmod", "+x", "/tmp/start.sh"]
EXPOSE 5432/tcp 28080/tcp 28443/tcp 29990/tcp 29993/tcp 3000/tcp 3003/tcp 8000/tcp 8001/tcp
CMD /tmp/start.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
