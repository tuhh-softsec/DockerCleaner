#
#   GCPlot all-in-one dockerfile
#
FROM openjdk:8-jdk
ARG GCPLOT_VERSION=2.0.1
ARG CASSANDRA_VERSION=311x
ARG ORIENTDB_VERSION=2.2.13
ENV GCPLOT_MEMORY="512m"
ENV ORIENTDB_MEMORY="256m"
ENV CASSANDRA_MEMORY="1g"
#   Install packages and create appropriate user
RUN apt-get update -y \
 && apt-get install --no-install-recommends wget=1.21-1+deb11u1 curl=7.74.0-1.3+deb11u7 python unzip=6.0-26+deb11u1 nginx=1.18.0-6.1+deb11u3 net-tools=1.60+git20181103.0eebece-1 netcat=1.10-46 -y \
 && groupadd --system gcserver \
 && useradd -d /home/gcserver -u 1040 -g gcserver -s /bin/bash gcserver \
 && mkdir -p /home/gcserver \
 && mkdir -m 755 -p /home/gcserver/logs \
 && mkdir -m 755 -p /home/gcserver/config \
 && mkdir -m 755 -p /home/gcserver/lib \
 && mkdir -m 777 -p /tmp/gcserver-file-uploads \
 && chown gcserver:gcserver /tmp/gcserver-file-uploads \
 && chown -R gcserver:gcserver /home/gcserver
COPY gcserver/logback.xml /home/gcserver/config
COPY gcserver/gcplot.properties /home/gcserver/config
COPY gcserver/gcserver /etc/init.d
RUN chmod 755 /etc/init.d/gcserver \
 && chmod -R 755 /home/gcserver/config/*
RUN wget "https://downloads.gcplot.com/artifacts/gcserver/${GCPLOT_VERSION}/com.gcplot.web-${GCPLOT_VERSION}-all.jar" -O /home/gcserver/lib/bootstrap.jar \
 && chown -R gcserver:gcserver /home/gcserver
#   Install OrientDB
RUN groupadd --system orientdb \
 && useradd -d /var/orientdb -u 1041 -g orientdb -s /bin/bash orientdb \
 && mkdir -p /var/orientdb \
 && mkdir -p /var/lib/orientdb \
 && mkdir -p /var/log/orientdb \
 && chown -R orientdb:orientdb /var/orientdb \
 && chown -R orientdb:orientdb /var/lib/orientdb \
 && chown -R orientdb:orientdb /var/log/orientdb \
 && wget "http://orientdb.com/download.php?file=orientdb-community-$ORIENTDB_VERSION.tar.gz&os=multi" -O /tmp/orientdb.tar.gz \
 && tar -zvxf /tmp/orientdb.tar.gz --directory /opt \
 && mv /opt/orientdb-community-$ORIENTDB_VERSION /opt/orientdb \
 && chown -R orientdb:orientdb /opt/orientdb
COPY orientdb/orientdb-server-config.xml /opt/orientdb/config/orientdb-server-config.xml
COPY orientdb/orientdb-server-log.properties /opt/orientdb/config/orientdb-server-log.properties
COPY orientdb/orientdb.sh /opt/orientdb/bin/orientdb.sh
RUN cp /opt/orientdb/bin/orientdb.sh /etc/init.d/orientdb \
 && chmod 755 /etc/init.d/orientdb
#   Install Cassandra
RUN echo "deb http://www.apache.org/dist/cassandra/debian $CASSANDRA_VERSION main" | tee -a /etc/apt/sources.list.d/cassandra.sources.list \
 && curl https://www.apache.org/dist/cassandra/KEYS | apt-key add - \
 && apt-get update -y \
 && apt-get install --no-install-recommends cassandra -y \
 && sed -i '/ulimit/d' /etc/init.d/cassandra
COPY cassandra/cassandra-rackdc.properties /etc/cassandra/cassandra-rackdc.properties
COPY cassandra/cassandra-topology.properties /etc/cassandra/cassandra-topology.properties
COPY cassandra/cassandra.yaml /etc/cassandra/cassandra.yaml
COPY cassandra/jvm.properties /etc/cassandra/jvm.options
COPY cassandra/cdb.cql /etc/cassandra/cdb.cql
COPY start.sh /start.sh
RUN chmod 755 /start.sh
#   Install UI
RUN wget "https://downloads.gcplot.com/artifacts/ui/$GCPLOT_VERSION/gcplot-ui-$GCPLOT_VERSION.tar.gz" -O /tmp/gcplot-ui.tar.gz \
 && tar -xvzf /tmp/gcplot-ui.tar.gz -C /tmp \
 && sed -i "s/${CACHE_BUSTER}/$( date +%s ;)/g" /tmp/dist/index.html \
 && mkdir /var/www/app \
 && mkdir /var/www/landing \
 && mv /tmp/dist/* /var/www/app \
 && mv /tmp/landing/* /var/www/landing \
 && chmod -R 755 /var/www/*
#   Configuring nginx
COPY nginx/gcplot.conf /etc/nginx/sites-available/gcplot.conf
COPY nginx/nginx /etc/nginx/nginx.conf
RUN ln -s /etc/nginx/sites-available/gcplot.conf /etc/nginx/sites-enabled/gcplot.conf \
 && rm -rf /etc/nginx/sites-enabled/default \
 && rm -rf /etc/nginx/sites-available/default
VOLUME ["/var/lib/cassandra", "/var/lib/orientdb"]
EXPOSE 9042/tcp 2424/tcp 2480/tcp 80/tcp
CMD ["/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
