FROM ubuntu:18.04
#   Based on
#   https://switch2osm.org/manually-building-a-tile-server-18-04-lts/
#   Set up environment
ENV TZ="UTC"
ENV AUTOVACUUM="on"
ENV UPDATES="disabled"
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone
#   Install dependencies
RUN echo "deb [ allow-insecure=yes ] http://apt.postgresql.org/pub/repos/apt/ bionic-pgdg main" >> /etc/apt/sources.list.d/pgdg.list \
 && apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.6.14 ca-certificates=20211016ubuntu0.18.04.1 -y \
 && apt-get install --no-install-recommends apache2=2.4.29-1ubuntu4.27 apache2-dev=2.4.29-1ubuntu4.27 autoconf=2.69-11 build-essential=12.4ubuntu1 bzip2=1.0.6-8.1ubuntu0.2 cmake=3.10.2-1ubuntu2.18.04.2 fonts-noto-cjk=1:20190409+repack1-0ubuntu0.18.04 fonts-noto-hinted=20171026-2 fonts-noto-unhinted=20171026-2 clang=1:6.0-41~exp5~ubuntu1 gdal-bin=2.2.3+dfsg-2 git-core libagg-dev=1:2.4-r127+dfsg1-1 libboost-all-dev=1.65.1.0ubuntu1 libbz2-dev=1.0.6-8.1ubuntu0.2 libcairo-dev libcairomm-1.0-dev=1.12.2-3 libexpat1-dev=2.2.5-3ubuntu0.9 libfreetype6-dev=2.8.1-2ubuntu2.2 libgdal-dev=2.2.3+dfsg-2 libgeos++-dev=3.6.2-1build2 libgeos-dev=3.6.2-1build2 libgeotiff-epsg=1.4.2-1 libicu-dev=60.2-3ubuntu3.2 liblua5.3-dev=5.3.3-1ubuntu0.18.04.1 libmapnik-dev=3.0.19+ds-1 libpq-dev=10.23-0ubuntu0.18.04.1 libproj-dev=4.9.3-2 libprotobuf-c0-dev libtiff5-dev=4.0.9-5ubuntu0.10 libtool=2.4.6-2 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 lua5.3=5.3.3-1ubuntu0.18.04.1 make=4.1-9.1ubuntu1 mapnik-utils=3.0.19+ds-1 nodejs=8.10.0~dfsg-2ubuntu0.4 npm=3.5.2-0ubuntu4 postgis=2.4.3+dfsg-4 postgresql-10=10.23-0ubuntu0.18.04.1 postgresql-10-postgis-2.5 postgresql-10-postgis-2.5-scripts postgresql-contrib-10 protobuf-c-compiler=1.2.1-2 python-mapnik=1:0.0~20180130-804a7947d-1 sudo=1.8.21p2-3ubuntu1.5 tar=1.29b-2ubuntu0.4 ttf-unifont=1:10.0.07-1 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 osmosis=0.46-2 osmium-tool=1.7.1-1 cron=3.0pl1-128.1ubuntu1.2 python-psycopg2=2.7.4-1 python-shapely=1.6.4-1 python-lxml=4.2.1-1ubuntu0.6 -y --allow-unauthenticated \
 && apt-get clean autoclean \
 && apt-get autoremove --yes \
 && rm -rf /var/lib/{apt,dpkg,cache,log}/
#   Set up renderer user
RUN adduser --disabled-password --gecos "" renderer
USER renderer
#   Install latest osm2pgsql
RUN mkdir /home/renderer/src
WORKDIR /home/renderer/src
RUN git clone https://github.com/openstreetmap/osm2pgsql.git
WORKDIR /home/renderer/src/osm2pgsql
RUN mkdir build
WORKDIR /home/renderer/src/osm2pgsql/build
RUN cmake .. \
 && make -j $( nproc ;)
USER root
RUN make install
USER renderer
#   Install and test Mapnik
RUN python -c 'import mapnik'
#   Install mod_tile and renderd
WORKDIR /home/renderer/src
RUN git clone -b switch2osm https://github.com/SomeoneElseOSM/mod_tile.git
WORKDIR /home/renderer/src/mod_tile
RUN ./autogen.sh \
 && ./configure \
 && make -j $( nproc ;)
USER root
RUN make -j $( nproc ;) install \
 && make -j $( nproc ;) install-mod_tile \
 && ldconfig
USER renderer
#   Configure stylesheet
WORKDIR /home/renderer/src
RUN git clone https://github.com/gravitystorm/openstreetmap-carto.git
WORKDIR /home/renderer/src/openstreetmap-carto
USER root
RUN npm install carto@1.2.0 -g
USER renderer
RUN carto project.mml > mapnik.xml
#   Load shapefiles
WORKDIR /home/renderer/src/openstreetmap-carto
RUN scripts/get-shapefiles.py
#   Configure renderd
USER root
RUN sed -i 's/renderaccount/renderer/g' /usr/local/etc/renderd.conf \
 && sed -i 's/hot/tile/g' /usr/local/etc/renderd.conf
USER renderer
#   Configure Apache
USER root
RUN mkdir /var/lib/mod_tile \
 && chown renderer /var/lib/mod_tile \
 && mkdir /var/run/renderd \
 && chown renderer /var/run/renderd
RUN echo "LoadModule tile_module /usr/lib/apache2/modules/mod_tile.so" >> /etc/apache2/conf-available/mod_tile.conf \
 && a2enconf mod_tile
COPY apache.conf /etc/apache2/sites-available/000-default.conf
COPY leaflet-demo.html /var/www/html/index.html
RUN ln -sf /proc/1/fd/1 /var/log/apache2/access.log \
 && ln -sf /proc/1/fd/2 /var/log/apache2/error.log
#   Configure PosgtreSQL
COPY postgresql.custom.conf.tmpl /etc/postgresql/10/main/
RUN chown -R postgres:postgres /var/lib/postgresql \
 && chown postgres:postgres /etc/postgresql/10/main/postgresql.custom.conf.tmpl \
 && echo "\ninclude 'postgresql.custom.conf'" >> /etc/postgresql/10/main/postgresql.conf
#   copy update scripts
COPY openstreetmap-tiles-update-expire /usr/bin/
RUN chmod +x /usr/bin/openstreetmap-tiles-update-expire \
 && mkdir /var/log/tiles \
 && chmod a+rw /var/log/tiles \
 && ln -s /home/renderer/src/mod_tile/osmosis-db_replag /usr/bin/osmosis-db_replag \
 && echo "* * * * * renderer openstreetmap-tiles-update-expire\n" >> /etc/crontab
#   install trim_osc.py helper script
USER renderer
RUN cd ~/src \
 && git clone https://github.com/zverik/regional \
 && chmod u+x ~/src/regional/trim_osc.py
#   Start running
USER root
COPY run.sh /
COPY indexes.sql /
ENTRYPOINT ["/run.sh"]
CMD []
EXPOSE 80/tcp 5432/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
