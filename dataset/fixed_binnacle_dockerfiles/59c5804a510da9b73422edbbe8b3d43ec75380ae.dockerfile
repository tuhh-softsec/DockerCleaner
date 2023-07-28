#
#   Cartodb container
#
FROM ubuntu:18.04
LABEL maintainer="Stefan Verhoeven <s.verhoeven@esciencecenter.nl>"
#   Configuring locales
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.6.14 software-properties-common=0.96.24.32.20 locales=2.27-3ubuntu1.6 -y -q \
 && dpkg-reconfigure locales \
 && locale-gen en_US.UTF-8 \
 && update-locale LANG=en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#  ENV CARTODB_VERSION=v4.11.152
#  ENV CARTODB_VERSION=v4.12.9
#  ENV CARTODB_VERSION=v4.12.26
#  ENV CARTODB_VERSION=v4.12.30
ENV CARTODB_VERSION="master"
#  ENV SQLAPI_VERSION=1.47.2
ENV SQLAPI_VERSION="master"
#  ENV CRANKSHAFT_VERSION=0.8.1
ENV CRANKSHAFT_VERSION="master"
#  ENV WINDSHAFT_VERSION=5.4.0
ENV WINDSHAFT_VERSION="master"
#  ENV DATASERVICES_VERSION=0.0.2
ENV DATASERVICES_VERSION="master"
#  ENV DATAERVICESAPI_VERSION=0.30.5-server
ENV DATAERVICESAPI_VERSION="master"
#  ENV OBSERVATORY_VERSION=1.9.0
ENV OBSERVATORY_VERSION="master"
RUN useradd -m -d /home/cartodb -s /bin/bash cartodb \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 autoconf=2.69-11 automake=1:1.15.1-3ubuntu2 libtool=2.4.6-2 checkinstall=1.6.2-4ubuntu2 unp=2.0~pre7+nmu1 zip=3.0-11build1 unzip=6.0-21ubuntu1.2 git-core git=1:2.17.1-1ubuntu0.17 subversion=1.9.7-4ubuntu1.1 curl=7.58.0-2ubuntu3.24 libgeos-c1v5=3.6.2-1build2 libgeos-dev=3.6.2-1build2 libjson-c-dev=0.12.1-1.3ubuntu0.3 python-simplejson=3.13.2-1 proj-bin=4.9.3-2 proj-data=4.9.3-2 libproj-dev=4.9.3-2 gdal-bin=2.2.3+dfsg-2 libgdal-dev=2.2.3+dfsg-2 postgresql-10=10.23-0ubuntu0.18.04.1 postgresql-client-10=10.23-0ubuntu0.18.04.1 postgresql-contrib-10 postgresql-server-dev-10=10.23-0ubuntu0.18.04.1 postgresql-plpython-10=10.23-0ubuntu0.18.04.1 postgresql-10-plproxy=2.8-1 postgresql-10-postgis-2.4=2.4.3+dfsg-4 postgresql-10-postgis-scripts=2.4.3+dfsg-4 postgis=2.4.3+dfsg-4 liblwgeom-2.4-0=2.4.3+dfsg-4 ca-certificates=20211016ubuntu0.18.04.1 redis-server=5:4.0.9-1ubuntu0.2 python2.7-dev=2.7.17-1~18.04ubuntu1.11 python-setuptools=39.0.1-2ubuntu0.1 imagemagick=8:6.9.7.4+dfsg-16ubuntu6.15 libmapnik-dev=3.0.19+ds-1 mapnik-utils=3.0.19+ds-1 python-mapnik=1:0.0~20180130-804a7947d-1 python-argparse python-gdal=2.2.3+dfsg-2 python-chardet=3.0.4-1 python-all-dev=2.7.15~rc1-1 python-docutils=0.14+dfsg-3 openssl=1.1.1-1ubuntu2.1~18.04.21 libreadline7=7.0-3 zlib1g=1:1.2.11.dfsg-0ubuntu2.2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libyaml-dev=0.1.7-2ubuntu3 libsqlite3-dev=3.22.0-1ubuntu0.7 sqlite3=3.22.0-1ubuntu0.7 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libxslt-dev libc6-dev=2.27-3ubuntu1.6 ncurses-dev bison=2:3.0.4.dfsg-1build1 pkg-config=0.29.1-0ubuntu2 libpq5=10.23-0ubuntu0.18.04.1 libpq-dev=10.23-0ubuntu0.18.04.1 libcurl4-gnutls-dev=7.58.0-2ubuntu3.24 libffi-dev=3.2.1-8 libgdbm-dev=1.14.1-6 gnupg=2.2.4-1ubuntu1.6 libreadline6-dev libcairo2-dev=1.15.10-2ubuntu0.1 libjpeg8-dev=8c-2ubuntu8 libpango1.0-dev=1.40.14-1ubuntu0.1 libgif-dev=5.1.4-2ubuntu0.1 libgmp-dev=2:6.1.2+dfsg-2ubuntu0.1 libicu-dev=60.2-3ubuntu3.2 wget=1.19.4-1ubuntu2.2 nginx-light=1.14.0-0ubuntu1.11 net-tools=1.60+git20161116.90da8a0-1ubuntu1 ruby2.5-dev=2.5.1-1ubuntu1.13 xz-utils=5.2.2-1.3ubuntu0.1 -y -q \
 && rm -rf /var/lib/apt/lists/*
RUN git config --global user.email you@example.com
RUN git config --global user.name "Your Name"
#   Varnish 3, Ubuntu:18.04 comes with Varnish 5.1 which can't be run with anonymous admin telnet
RUN cd /opt \
 && wget http://varnish-cache.org/_downloads/varnish-3.0.7.tgz \
 && tar -zxf varnish-3.0.7.tgz \
 && cd varnish-3.0.7 \
 && ./configure --prefix=/opt/varnish \
 && make \
 && make install \
 && cd /opt \
 && rm -rf varnish-3.0.7 varnish-3.0.7.tgz
#   Install NodeJS
RUN curl https://nodejs.org/dist/v10.15.3/node-v10.15.3-linux-x64.tar.xz | tar -Jxf - --strip-components=1 -C /usr \
 && npm install grunt-cli@1.4.3 -g \
 && npm install npm@6 -g \
 && rm -r /tmp/npm-* /root/.npm
#   Setting PostgreSQL
RUN sed -i 's/\(peer\|md5\)/trust/' /etc/postgresql/10/main/pg_hba.conf \
 && service postgresql start \
 && createuser publicuser --no-createrole --no-createdb --no-superuser -U postgres \
 && createuser tileuser --no-createrole --no-createdb --no-superuser -U postgres \
 && service postgresql stop
#   Crankshaft: CARTO Spatial Analysis extension for PostgreSQL
RUN cd / \
 && curl https://bootstrap.pypa.io/get-pip.py | python \
 && git clone https://github.com/CartoDB/crankshaft.git \
 && cd /crankshaft \
 && git checkout $CRANKSHAFT_VERSION \
 && make install \
 && pip install scikit-learn==0.17.0 --force-reinstall --no-cache-dir \
 && cd ..
#   Initialize template postgis db
COPY ./template_postgis.sh /tmp/template_postgis.sh
RUN service postgresql start \
 && /bin/su postgres -c /tmp/template_postgis.sh \
 && service postgresql stop
COPY ./cartodb_pgsql.sh /tmp/cartodb_pgsql.sh
#   Install CartoDB API
RUN git clone git://github.com/CartoDB/CartoDB-SQL-API.git \
 && cd CartoDB-SQL-API \
 && git checkout $SQLAPI_VERSION \
 && npm install
#   Install Windshaft
RUN git clone git://github.com/CartoDB/Windshaft-cartodb.git \
 && cd Windshaft-cartodb \
 && git checkout $WINDSHAFT_VERSION \
 && npm install \
 && mkdir logs
#   Install CartoDB
RUN git clone --recursive git://github.com/CartoDB/cartodb.git \
 && cd cartodb \
 && git checkout $CARTODB_VERSION \
 && cd lib/sql \
 && PGUSER=postgres make install \
 && service postgresql start \
 && /bin/su postgres -c /tmp/cartodb_pgsql.sh \
 && service postgresql stop \
 && cd - \
 && npm install \
 && rm -r /tmp/npm-* /root/.npm \
 && perl -pi -e 's/gdal==1\.10\.0/gdal==2.2.2/' python_requirements.txt \
 && pip install :all:==null --no-binary -r python_requirements.txt \
 && gem install bundler --version 2.4.12 --version=1.17.3 \
 && gem install compass --version 1.0.3 \
 && bundle update thin \
 && /bin/bash -l -c 'bundle install' \
 && cp config/grunt_development.json ./config/grunt_true.json \
 && /bin/bash -l -c 'bundle exec grunt'
#   && \
#  rm -rf .git /root/.cache/pip node_modules
#   Geocoder SQL client + server
RUN git clone https://github.com/CartoDB/data-services.git \
 && cd /data-services/geocoder/extension \
 && git checkout $DATASERVICES_VERSION \
 && PGUSER=postgres make all install \
 && cd / \
 && git clone https://github.com/CartoDB/dataservices-api.git \
 && cd /dataservices-api/server/extension \
 && git checkout $DATAERVICESAPI_VERSION \
 && PGUSER=postgres make install \
 && cd ../lib/python/cartodb_services \
 && pip install -r requirements.txt \
 && pip install . \
 && cd ../../../../client \
 && PGUSER=postgres make install
#   Observatory extension
RUN cd / \
 && git clone --recursive https://github.com/CartoDB/observatory-extension.git \
 && cd observatory-extension \
 && git checkout $OBSERVATORY_VERSION \
 && PGUSER=postgres make deploy
#   Copy confs
COPY ./config/CartoDB-dev.js /CartoDB-SQL-API/config/environments/development.js
COPY ./config/WS-dev.js /Windshaft-cartodb/config/environments/development.js
COPY ./config/app_config.yml /cartodb/config/app_config.yml
COPY ./config/database.yml /cartodb/config/database.yml
COPY ./create_dev_user /cartodb/script/create_dev_user
COPY ./setup_organization.sh /cartodb/script/setup_organization.sh
COPY ./config/cartodb.nginx.proxy.conf /etc/nginx/sites-enabled/default
COPY ./config/varnish.vcl /etc/varnish.vcl
COPY ./geocoder.sh /cartodb/script/geocoder.sh
COPY ./geocoder_server.sql /cartodb/script/geocoder_server.sql
COPY ./fill_geocoder.sh /cartodb/script/fill_geocoder.sh
COPY ./sync_tables_trigger.sh /cartodb/script/sync_tables_trigger.sh
ENV PATH="/usr/local/rvm/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
RUN mkdir -p /cartodb/log \
 && touch /cartodb/log/users_modifications \
 && /opt/varnish/sbin/varnishd -a :6081 -T localhost:6082 -s malloc,256m -f /etc/varnish.vcl \
 && perl -pi.bak -e 's/^bind 127.0.0.1 ::1$/bind 0.0.0.0/' /etc/redis/redis.conf \
 && service postgresql start \
 && service redis-server start \
 && perl -pi -e 's/0\.22\.0/0.22.2/' /cartodb/app/models/user/db_service.rb \
 && bash -l -c "cd /cartodb \
 && bash script/create_dev_user \
 && bash script/setup_organization.sh \
 && bash script/geocoder.sh" \
 && service postgresql stop \
 && service redis-server stop \
 && chmod +x /cartodb/script/fill_geocoder.sh \
 && chmod +x /cartodb/script/sync_tables_trigger.sh
EXPOSE 80/tcp
ENV GDAL_DATA="/usr/share/gdal/2.2"
#   Number of seconds between a sync tables task is run
#   Default interval is an hour, use `docker run -e SYNC_TABLES_INTERVAL=60 ...` to change it
ENV SYNC_TABLES_INTERVAL="3600"
COPY ./startup.sh /opt/startup.sh
CMD ["/bin/bash", "/opt/startup.sh"]
HEALTHCHECK CMD curl -f http://localhost || exit 1
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
