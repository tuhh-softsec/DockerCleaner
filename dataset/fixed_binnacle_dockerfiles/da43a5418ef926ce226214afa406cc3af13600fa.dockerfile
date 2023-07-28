FROM ubuntu:18.04
ENV TZ="Europe/Prague"
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone
RUN apt-get update \
 && apt-get install --no-install-recommends wget=1.19.4-1ubuntu2.2 git=1:2.17.1-1ubuntu0.17 python=2.7.15~rc1-1 python-dev=2.7.15~rc1-1 python-nose=1.3.7-3 python-pip=9.0.1-2.3~ubuntu1.18.04.8 libboost-filesystem-dev=1.65.1.0ubuntu1 libboost-program-options-dev=1.65.1.0ubuntu1 libboost-python-dev=1.65.1.0ubuntu1 libboost-regex-dev=1.65.1.0ubuntu1 libboost-system-dev=1.65.1.0ubuntu1 libboost-thread-dev=1.65.1.0ubuntu1 libicu-dev=60.2-3ubuntu3.2 libxml2=2.9.4+dfsg1-6.1ubuntu1.8 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libfreetype6=2.8.1-2ubuntu2.2 libfreetype6-dev=2.8.1-2ubuntu2.2 libjpeg-dev=8c-2ubuntu8 libpng-dev=1.6.34-1ubuntu0.18.04.2 libproj-dev=4.9.3-2 libtiff-dev=4.0.9-5ubuntu0.10 libcairo2=1.15.10-2ubuntu0.1 libcairo2-dev=1.15.10-2ubuntu0.1 python-cairo=1.16.2-1 python-cairo-dev=1.16.2-1 libcairomm-1.0-1v5=1.12.2-3 libcairomm-1.0-dev=1.12.2-3 ttf-unifont=1:10.0.07-1 ttf-dejavu=2.37-1 ttf-dejavu-core=2.37-1 ttf-dejavu-extra=2.37-1 git=1:2.17.1-1ubuntu0.17 build-essential=12.4ubuntu1 libgdal-dev=2.2.3+dfsg-2 python-gdal=2.2.3+dfsg-2 postgresql-10=10.23-0ubuntu0.18.04.1 postgresql-server-dev-10=10.23-0ubuntu0.18.04.1 postgresql-contrib-10 postgis=2.4.3+dfsg-4 libsqlite3-dev=3.22.0-1ubuntu0.7 osm2pgsql=0.94.0+ds-1 npm=3.5.2-0ubuntu4 uthash-dev=2.0.2-1 sqlite3=3.22.0-1ubuntu0.7 spatialite-bin=4.3.0-2build1 gdal-bin=2.2.3+dfsg-2 fonts-liberation=1:1.07.4-7~18.04.1 -y
RUN pip2 install --upgrade pip
RUN pip2 install psycopg2 pysqlite
RUN cd /tmp \
 && git clone https://github.com/mapnik/mapnik mapnik-2.3.x -b 2.3.x --depth 10 \
 && cd mapnik-2.3.x \
 && ./configure \
 && make \
 && make install \
 && ldconfig \
 && rm -rf /tmp/mapnik-2.3.x
RUN npm install millstone@0.6.17 carto@1.2.0 --global
RUN wget -O - http://m.m.i24.cc/osmconvert.c | cc -x c - -lz -O3 -o /usr/local/bin/osmconvert
RUN cd /tmp \
 && git clone https://github.com/bigr/o5mreader \
 && cd o5mreader \
 && ./configure \
 && make \
 && make install \
 && ldconfig \
 && rm -rf /tmp/o5mreader
RUN cd /tmp \
 && git clone https://github.com/bigr/o5m2sqlite \
 && cd o5m2sqlite \
 && ./configure \
 && make \
 && make install \
 && ldconfig \
 && rm -rf /tmp/o5msqlite
RUN cp /usr/share/fonts/truetype/liberation/* /usr/local/lib/mapnik/fonts
RUN cd /root \
 && git clone https://github.com/bigr/map1.git \
 && chmod -R 777 /root/map1
RUN /etc/init.d/postgresql start \
 && cd /root/map1 \
 && git pull \
 && chmod -R 777 /root/map1 \
 && su postgres -c "createdb -E UTF8 -O postgres gis_eu" \
 && su postgres -c "psql gis_eu -c \"CREATE EXTENSION postgis;\"" \
 && su postgres -c "psql gis_eu -c \"CREATE EXTENSION hstore;\"" \
 && su postgres -c "createdb -E UTF8 -O postgres gis_eu_1000" \
 && su postgres -c "psql gis_eu_1000 -c \"CREATE EXTENSION postgis;\"" \
 && su postgres -c "psql gis_eu_1000 -c \"CREATE EXTENSION hstore;\"" \
 && cd /root/map1 \
 && su postgres -c "./get_data_pgis"
RUN /etc/init.d/postgresql start \
 && cd /root/map1 \
 && git pull \
 && cd /root/map1/vodak \
 && su postgres -c "./vodak.py gis_eu > /tmp/vodak.sql" \
 && su postgres -c "psql -d gis_eu < vodak.sql" \
 && unlink /tmp/vodak.sql
RUN cd /root/map1/stylesheets/general/prepare-db \
 && cd /root/map1 \
 && git pull \
 && su postgres -c "php ./get-sql.php | psql -d gis_eu"
COPY srtm /root/map1/data/dem/
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
