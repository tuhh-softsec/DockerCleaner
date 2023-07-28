FROM ubuntu:16.04
MAINTAINER Claus Stadler <cstadler@informatik.uni-leipzig.de>
ENV DEBIAN_FRONTEND="noninteractive"
RUN echo "force rebuild 7"
RUN apt-get update -y --fix-missing \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 wget=1.17.1-1ubuntu1.5 git=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 sudo=1.8.16-0ubuntu1.10 -y \
 && apt-get update
ENV POSTGRES_CLIENT_VERSION="9.5"
ENV POSTGIS_CLIENT_VERSION="2.2"
#  ENV POSTGRES_CLIENT_VERSION ${POSTGRES_VERSION}
#  ENV POSTGIS_CLIENT_VERSION ${POSTGIS_VERSION}
RUN echo "pgclientver: ${POSTGRES_CLIENT_VERSION}"
#   for 16.04: libgeos-c1v5
RUN apt-get install --no-install-recommends build-essential=12.1ubuntu2 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libpq-dev=9.5.25-0ubuntu0.16.04.1 libbz2-dev=1.0.6-8ubuntu0.2 libtool=2.4.6-0.1 automake=1:1.15-4ubuntu1 libproj-dev=4.9.2-2 libboost-dev=1.58.0.1ubuntu1 libboost-system-dev=1.58.0.1ubuntu1 libboost-filesystem-dev=1.58.0.1ubuntu1 libboost-thread-dev=1.58.0.1ubuntu1 libexpat-dev gcc=4:5.3.1-1ubuntu1 proj-bin=4.9.2-2 libgeos-c1v5=3.5.0-1ubuntu2 libgeos++-dev=3.5.0-1ubuntu2 libexpat-dev php-pgsql=1:7.0+35ubuntu6.1 php-json=1:7.0+35ubuntu6.1 php-db=1.7.14-3build1 postgresql=9.5+173ubuntu0.3 postgis=2.2.1+dfsg-2ubuntu0.1 postgresql-contrib=9.5+173ubuntu0.3 wget=1.17.1-1ubuntu1.5 osm2pgsql=0.88.1-1 osmosis=0.44.1-4 gettext-base=0.19.7-2ubuntu3.1 osmctools=0.6-1 postgresql-${POSTGRES_CLIENT_VERSION}-postgis-${POSTGIS_CLIENT_VERSION} postgresql-server-dev-${POSTGRES_CLIENT_VERSION} -y
#   Install the linked geo data package for the lgd-osm-replication-sequences tool
RUN wget -qO - http://cstadler.aksw.org/repos/apt/conf/packages.precise.gpg.key | sudo apt-key add - \
 && echo 'deb http://cstadler.aksw.org/repos/apt precise main contrib non-free' | sudo tee -a /etc/apt/sources.list.d/cstadler.aksw.org.list \
 && apt-get update \
 && apt-get install --no-install-recommends linkedgeodata -y
WORKDIR /app/nominatim
#   Nominatim install
ENV NOMINATIM_VERSION="v2.5.1"
RUN git clone https://github.com/openstreetmap/Nominatim ./src
RUN cd ./src \
 && git checkout "$NOMINATIM_VERSION"
#  RUN git clone --recursive https://github.com/openstreetmap/Nominatim ./src
#  RUN cd ./src && git checkout "$NOMINATIM_VERSION" && git submodule update --recursive --init && \
#    ./autogen.sh && ./configure && make
RUN echo "force rebuild b6"
#   Nominatim create site
#   FIXME Eventually get rid of the need for the .pgpass file, but the latest osm2pgsql tool fails to read password from the PGPASS environment variable
#   Alternatively, create the file dynamically within the container
COPY target/.pgpass /root/
RUN chmod 600 /root/.pgpass
COPY local.php.dist ./src/settings/
COPY setup-patched.php ./src/utils/
RUN chmod +x ./src/utils/setup-patched.php
COPY update-patched.php ./src/utils/
RUN chmod +x ./src/utils/update-patched.php
COPY target/nominatim ./src/nominatim/
RUN chmod +x ./src/nominatim/nominatim
COPY target/osm2pgsql ./src/osm2pgsql/
RUN chmod +x ./src/osm2pgsql/osm2pgsql
COPY target/nominatim.so ./src/module/
COPY configuration.txt.dist ./src/settings/
#  RUN rm -rf /var/www/html/* && ./src/utils/setup.php --create-website /var/www/html
#   Apache configure
#  COPY nominatim.conf /etc/apache2/sites-enabled/000-default.conf
#  ENV DB_HOST test
#  RUN echo "xxxyyyDB: ${DB_HOST} $DB_URL"
#   Load initial data
#  ENV PBF_DATA http://download.geofabrik.de/europe/monaco-latest.osm.pbf
#  RUN curl -L $PBF_DATA --create-dirs -o /app/src/data.osm.pbf
#  RUN service postgresql start && \
#      psql "$DB_URL" -c "CREATE ROLE nominatim;" || true && \
#      psql "$DB_URL" -c "CREATE ROLE www-data NOSUPERUSER NOCREATEDB NOCREATEROL;" || true && \
#      psql "$DB_URL" -c "CREATE DATABASE IF NOT EXISTS nominatim" && \
#      sudo -u postgres psql postgres -c "DROP DATABASE IF EXISTS nominatim" && \
#      useradd -m -p password1234 nominatim && \
#      chown -R nominatim:nominatim ./src && \
#      sudo -u nominatim ./src/utils/setup.php --osm-file /app/src/data.osm.pbf --all --threads 2 && \
#      service postgresql stop
#  EXPOSE 5432
#  EXPOSE 8080
COPY start.sh .
CMD ./start.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
