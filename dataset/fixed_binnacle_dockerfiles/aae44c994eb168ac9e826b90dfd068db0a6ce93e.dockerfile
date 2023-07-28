FROM ubuntu:14.04
MAINTAINER Jan Nonnen <helvalius@gmail.com>
#   Define the OSM argument, use monaco as default
ARG OSM=http://download.geofabrik.de/europe/monaco-latest.osm.pbf
RUN :
#   Install basic software
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 -y )
#   Note: libgeos++-dev is included here too (the nominatim install page suggests installing it if there is a problem with the 'pear install DB' below - it seems safe to install it anyway)
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 gcc=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 osmosis=0.40.1+ds1-7 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libgeos-dev=3.4.2-4ubuntu1 libpq-dev=9.3.24-0ubuntu0.14.04 libbz2-dev=1.0.6-5 libtool=2.4.2-1.7ubuntu1 cmake=2.8.12.2-0ubuntu3 libproj-dev=4.8.0-2ubuntu2 proj-bin=4.8.0-2ubuntu2 libgeos-c1=3.4.2-4ubuntu1 libgeos++-dev=3.4.2-4ubuntu1 libexpat1-dev=2.1.0-4ubuntu1.4 -y )
#   Install Boost (required by osm2pqsql)
RUN (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-6 make=3.81-8.2ubuntu3 g++=4:4.8.2-1ubuntu6 libboost-dev=1.54.0.1ubuntu1 libboost-system-dev=1.54.0.1ubuntu1 libboost-filesystem-dev=1.54.0.1ubuntu1 libboost-thread-dev=1.54.0.1ubuntu1 lua5.2=5.2.3-1 liblua5.2-dev=5.2.3-1 -y )
#   Install PHP5
RUN (apt-get update ;apt-get install --no-install-recommends php5=5.5.9+dfsg-1ubuntu4.29 php-pear=5.5.9+dfsg-1ubuntu4.29 php5-pgsql=5.5.9+dfsg-1ubuntu4.29 php5-json=1.3.2-2build1 php-db=1.7.14-2 -y )
#   From the website "If you plan to install the source from github, the following additional packages are needed:"
#   RUN apt-get -y install git autoconf-archive
#   Install Postgres, PostGIS and dependencies
RUN (apt-get update ;apt-get install --no-install-recommends postgresql=9.3+154ubuntu1.1 postgis=2.1.2+dfsg-2ubuntu0.2 postgresql-contrib=9.3+154ubuntu1.1 postgresql-9.3-postgis-2.1=2.1.2+dfsg-2ubuntu0.2 postgresql-server-dev-9.3=9.3.24-0ubuntu0.14.04 -y )
#   Work around for AUFS bug as per https://github.com/docker/docker/issues/783#issuecomment-56013588
RUN mkdir /etc/ssl/private-copy ; mv /etc/ssl/private/* /etc/ssl/private-copy/ ; rm -r /etc/ssl/private ; mv /etc/ssl/private-copy /etc/ssl/private ; chmod -R 0700 /etc/ssl/private ; chown -R postgres /etc/ssl/private
#   Some additional packages that may not already be installed
#   bc is needed in configPostgresql.sh
RUN (apt-get update ;apt-get install --no-install-recommends bc=1.06.95-8ubuntu1 -y )
#   Install Apache
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.7-1ubuntu4.22 -y )
#   Add Protobuf support
RUN (apt-get update ;apt-get install --no-install-recommends libprotobuf-c0-dev=0.15-1build1 protobuf-c-compiler=0.15-1build1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends sudo=1.8.9p5-1ubuntu1.4 -y )
#
RUN pear install DB
RUN useradd -m -p password1234 nominatim
RUN mkdir -p /app/git/
RUN git clone --recursive https://github.com/twain47/Nominatim.git /app/git/
RUN mkdir -p /app/nominatim
WORKDIR /app/nominatim
RUN cmake /app/git/
RUN make
#   Configure postgresql
RUN service postgresql start \
 && pg_dropcluster --stop 9.3 main
RUN service postgresql start \
 && pg_createcluster --start -e UTF-8 9.3 main
RUN sudo -u postgres /usr/lib/postgresql/9.3/bin/pg_ctl start -w -D /etc/postgresql/9.3/main/ \
 && cat /var/log/postgresql/postgresql-9.3-main.log \
 && sudo -u postgres psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='nominatim'" | grep -q 1 || sudo -u postgres createuser -s nominatim \
 && sudo -u postgres psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='www-data'" | grep -q 1 || sudo -u postgres createuser -SDR www-data \
 && sudo -u postgres psql postgres -c "DROP DATABASE IF EXISTS nominatim"
RUN wget --output-document=/app/data.pbf $OSM
#   RUN wget --output-document=/app/data.pbf http://download.geofabrik.de/europe/luxembourg-latest.osm.pbf
#   RUN wget --output-document=/app/data.pbf http://download.geofabrik.de/north-america-latest.osm.pbf
#   RUN wget --output-document=/app/data.pbf http://download.geofabrik.de/north-america/us/delaware-latest.osm.pbf
WORKDIR /app/nominatim
COPY local.php /app/nominatim/settings/local.php
RUN ./utils/setup.php --help
RUN chown -R nominatim:nominatim /app/nominatim
RUN sudo -u postgres /usr/lib/postgresql/9.3/bin/pg_ctl start -w -D /etc/postgresql/9.3/main/ \
 && sudo -u nominatim ./utils/setup.php --osm-file /app/data.pbf --all --threads 2
RUN mkdir -p /var/www/nominatim
RUN cp -R /app/nominatim/website /var/www/nominatim/
RUN cp -R /app/nominatim/settings /var/www/nominatim/
RUN chown -R nominatim:www-data /var/www/nominatim
#   Adjust PostgreSQL configuration so that remote connections to the
#   database are possible.
RUN echo "host all all 0.0.0.0/0 trust" >> /etc/postgresql/9.3/main/pg_hba.conf
#   And add ``listen_addresses`` to ``/etc/postgresql/9.3/main/postgresql.conf``
RUN echo "listen_addresses='*'" >> /etc/postgresql/9.3/main/postgresql.conf
#   Expose the PostgreSQL port
EXPOSE 5432/tcp
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 -y )
COPY 400-nominatim.conf /etc/apache2/sites-available/400-nominatim.conf
RUN service apache2 start \
 && a2ensite 400-nominatim.conf \
 && /etc/init.d/apache2 reload
#   Expose the HTTP port
EXPOSE 8080/tcp
COPY configPostgresql.sh /app/nominatim/configPostgresql.sh
WORKDIR /app/nominatim
RUN chmod +x ./configPostgresql.sh
COPY start.sh /app/nominatim/start.sh
RUN chmod +x /app/nominatim/start.sh
RUN echo "Using OSM URL: "$OSM
CMD /app/nominatim/start.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
