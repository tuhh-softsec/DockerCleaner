#   -*-dockerfile-*-
FROM phusion/baseimage:latest@sha256:29479c37fcb28089eddd6619deed43bcdbcccf2185369e0199cc51a5ec78991b
LABEL maintainer="Natan Sągol <m@merlinnot.com>"
#   Use bash
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
#   Update image
RUN : \
 && apt-get -qq upgrade -y -o Dpkg::Options::="--force-confold"
#   Update locales
USER root
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.37-0ubuntu2 -y )
ENV DEBIAN_FRONTEND="noninteractive"
ENV LANG="C.UTF-8"
RUN locale-gen en_US.UTF-8
RUN update-locale LANG=en_US.UTF-8
#   Add postgresql sources
USER root
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 -y )
RUN echo "deb http://apt.postgresql.org/pub/repos/apt xenial-pgdg main" >> /etc/apt/sources.list \
 && wget --quiet -O - http://apt.postgresql.org/pub/repos/apt/ACCC4CF8.asc | apt-key add -
RUN :
#   Set build variables
ARG PGSQL_VERSION=9.6
ARG POSTGIS_VERSION=2.4
#   Install build dependencies
USER root
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.55-1ubuntu2 build-essential=12.9ubuntu3 ca-certificates=20230311 cmake=3.25.1-1 curl=7.88.1-7ubuntu1 g++=4:12.2.0-3ubuntu1 git=1:2.39.2-1ubuntu1 libapache2-mod-php=2:8.1+92ubuntu1 libboost-dev=1.74.0.3ubuntu7 libboost-filesystem-dev=1.74.0.3ubuntu7 libboost-python-dev=1.74.0.3ubuntu7 libboost-system-dev=1.74.0.3ubuntu7 libbz2-dev=1.0.8-5build1 libexpat1-dev=2.5.0-1 libgeos-dev=3.11.1-1 libgeos++-dev=3.11.1-1 libpq-dev=15.2-1 libproj-dev=9.1.1-1build1 libxml2-dev=2.9.14+dfsg-1.1build2 openssl=3.0.8-1ubuntu1 osmosis=0.48.3-2 php=2:8.1+92ubuntu1 php-db=1.11.0-0.2 php-intl=2:8.1+92ubuntu1 php-pear=1:1.10.13+submodules+notgz+2022032202-2 php-pgsql=2:8.1+92ubuntu1 python python-pip python-setuptools sudo=1.9.13p1-1ubuntu2 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 postgresql-${PGSQL_VERSION}-postgis-${POSTGIS_VERSION} postgresql-${PGSQL_VERSION}-postgis-scripts postgresql-contrib-${PGSQL_VERSION} postgresql-server-dev-${PGSQL_VERSION} -y )
RUN pip install pip==23.1 --upgrade
RUN pip install osmium==3.6.0
#   Create nominatim user account
USER root
RUN useradd -d /srv/nominatim -s /bin/bash -m nominatim
ENV USERNAME="nominatim"
ENV USERHOME="/srv/nominatim"
RUN chmod a+x ${USERHOME}
#   Install Nominatim
USER nominatim
ARG REPLICATION_URL=https://planet.osm.org/replication/hour/
WORKDIR /srv/nominatim
RUN git clone --recursive git://github.com/openstreetmap/Nominatim.git
RUN echo $'<?php\n @define('CONST_Postgresql_Version', '${PGSQL_VERSION}'); \n @define('CONST_Postgis_Version', '${POSTGIS_VERSION}'); \n @define('CONST_Osm2pgsql_Flatnode_File', '/srv/nominatim/flatnode'); \n @define('CONST_Pyosmium_Binary', '/usr/local/bin/pyosmium-get-changes'); \n @define('CONST_Website_BaseURL', '/nominatim/'); \n @define('CONST_Replication_Url', '${REPLICATION_URL}'); \n @define('CONST_Replication_MaxInterval', '86400'); \n @define('CONST_Replication_Update_Interval', '86400'); \n @define('CONST_Replication_Recheck_Interval', '900'); \n' > ./Nominatim/settings/local.php
RUN wget -O Nominatim/data/country_osm_grid.sql.gz http://www.nominatim.org/data/country_grid.sql.gz
RUN mkdir ${USERHOME}/Nominatim/build \
 && cd ${USERHOME}/Nominatim/build \
 && cmake ${USERHOME}/Nominatim \
 && make
#   Download data for initial import
USER nominatim
ARG PBF_URL=https://planet.osm.org/pbf/planet-latest.osm.pbf
RUN curl -L ${PBF_URL} --create-dirs -o /srv/nominatim/src/data.osm.pbf
#   Filter administrative boundaries
USER nominatim
ARG BUILD_THREADS=16
ARG IMPORT_ADMINISTRATIVE=false
COPY scripts/filter_administrative.sh /srv/nominatim/scripts/filter_administrative.sh
RUN /srv/nominatim/scripts/filter_administrative.sh
#   Add postgresql users
USER root
RUN service postgresql start \
 && sudo -u postgres createuser -s nominatim \
 && sudo -u postgres createuser www-data \
 && service postgresql stop
#   Tune postgresql configuration for import
USER root
ARG BUILD_MEMORY=32GB
ENV PGCONFIG_URL="https://api.pgconfig.org/v1/tuning/get-config"
RUN IMPORT_CONFIG_URL="${PGCONFIG_URL}? format=alter_system& pg_version=${PGSQL_VERSION}& total_ram=${BUILD_MEMORY}& max_connections=$((8 * ${BUILD_THREADS} + 32))& environment_name=DW& include_pgbadger=false" \
 && IMPORT_CONFIG_URL=${IMPORT_CONFIG_URL// /} \
 && service postgresql start \
 && (curl -sSL "${IMPORT_CONFIG_URL}" ;echo $'ALTER SYSTEM SET fsync TO \'off\';\n' ;echo $'ALTER SYSTEM SET full_page_writes TO \'off\';\n' ;echo $'ALTER SYSTEM SET logging_collector TO \'off\';\n' ) | sudo -u postgres psql -e \
 && service postgresql stop
#   Initial import
USER root
ARG OSM2PGSQL_CACHE=24000
RUN service postgresql start \
 && sudo -u nominatim ${USERHOME}/Nominatim/build/utils/setup.php --osm-file /srv/nominatim/src/data.osm.pbf --all --threads ${BUILD_THREADS} --osm2pgsql-cache ${OSM2PGSQL_CACHE} \
 && service postgresql stop
#   Use safe postgresql configuration
USER root
ARG RUNTIME_THREADS=2
ARG RUNTIME_MEMORY=8GB
RUN IMPORT_CONFIG_URL="${PGCONFIG_URL}? format=alter_system& pg_version=${PGSQL_VERSION}& total_ram=${RUNTIME_MEMORY}& max_connections=$((8 * ${RUNTIME_THREADS} + 32))& environment_name=WEB& include_pgbadger=true" \
 && IMPORT_CONFIG_URL=${IMPORT_CONFIG_URL// /} \
 && service postgresql start \
 && (curl -sSL "${IMPORT_CONFIG_URL}" ;echo $'ALTER SYSTEM SET fsync TO \'on\';\n' ;echo $'ALTER SYSTEM SET full_page_writes TO \'on\';\n' ;echo $'ALTER SYSTEM SET logging_collector TO \'on\';\n' ) | sudo -u postgres psql -e \
 && service postgresql stop
#   Configure Apache
USER root
COPY nominatim.conf /etc/apache2/conf-available/nominatim.conf
RUN a2enconf nominatim
#   Clean up
USER root
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Expose ports
EXPOSE 8080/tcp
#   Init scripts
USER root
ENV KILL_PROCESS_TIMEOUT="300"
ENV KILL_ALL_PROCESSES_TIMEOUT="300"
RUN mkdir -p /etc/my_init.d
COPY scripts/start_postgresql.sh /etc/my_init.d/00-postgresql.sh
RUN chmod +x /etc/my_init.d/00-postgresql.sh
COPY scripts/start_apache2.sh /etc/my_init.d/00-apache2.sh
RUN chmod +x /etc/my_init.d/00-apache2.sh
CMD ["/sbin/my_init"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
