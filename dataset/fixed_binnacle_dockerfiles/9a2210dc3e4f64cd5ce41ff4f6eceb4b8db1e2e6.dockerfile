#  # -*- docker-image-name: "ncareol/osm-tiles" -*-
#  #
#   The OpenStreetMap Tile Server
#
#   This creates an image with containing the OpenStreetMap tile server stack as
#   described at
#   <http://switch2osm.org/serving-tiles/manually-building-a-tile-server-12-04/>.
#
FROM ncareol/baseimage:0.9.18
MAINTAINER Erik Johnson <ej@ucar.edu>
#   Set the locale. This affects the encoding of the Postgresql template
#   databases.
ENV LANG="C.UTF-8"
RUN update-locale LANG=C.UTF-8
#   Ensure `add-apt-repository` is present
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common python-software-properties -y )
RUN (apt-get update ;apt-get install --no-install-recommends libboost-dev libboost-filesystem-dev libboost-program-options-dev libboost-python-dev libboost-regex-dev libboost-system-dev libboost-thread-dev -y )
#   Install remaining dependencies
RUN (apt-get update ;apt-get install --no-install-recommends subversion git-core tar unzip wget bzip2 build-essential autoconf libtool libxml2-dev libgeos-dev libpq-dev libbz2-dev munin-node munin libprotobuf-c0-dev protobuf-c-compiler libfreetype6-dev libpng12-dev libtiff4-dev libicu-dev libgdal-dev libcairo-dev libcairomm-1.0-dev apache2 apache2-dev libagg-dev liblua5.2-dev ttf-unifont -y )
RUN (apt-get update ;apt-get install --no-install-recommends autoconf apache2-dev libtool libxml2-dev libbz2-dev libgeos-dev libgeos++-dev libproj-dev gdal-bin libgdal1-dev mapnik-utils python-mapnik libmapnik-dev -y )
#   Install postgresql and postgis
RUN (apt-get update ;apt-get install --no-install-recommends postgresql-9.3-postgis-2.1 postgresql-contrib postgresql-server-dev-9.3 -y )
#   Install osm2pgsql
RUN cd /tmp \
 && git clone git://github.com/openstreetmap/osm2pgsql.git \
 && cd /tmp/osm2pgsql \
 && git checkout 0.88.1 \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install \
 && cd /tmp \
 && rm -rf /tmp/osm2pgsql
#   TODO: mapnik 3.0.5
#   Install the Mapnik library
RUN cd /tmp \
 && git clone git://github.com/mapnik/mapnik \
 && cd /tmp/mapnik \
 && git checkout 2.2.x \
 && python scons/scons.py configure INPUT_PLUGINS=all OPTIMIZATION=3 SYSTEM_FONTS=/usr/share/fonts/truetype/ \
 && python scons/scons.py \
 && python scons/scons.py install \
 && ldconfig \
 && cd /tmp \
 && rm -rf /tmp/mapnik
#   Verify that Mapnik has been installed correctly
RUN python -c 'import mapnik'
#   Install mod_tile and renderd
RUN cd /tmp \
 && git clone git://github.com/openstreetmap/mod_tile.git \
 && cd /tmp/mod_tile \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install \
 && make install-mod_tile \
 && ldconfig \
 && cd /tmp \
 && rm -rf /tmp/mod_tile
#   Install the Mapnik stylesheet
RUN cd /usr/local/src \
 && svn co http://svn.openstreetmap.org/applications/rendering/mapnik mapnik-style
#   Install the coastline data
RUN cd /usr/local/src/mapnik-style \
 && ./get-coastlines.sh /usr/local/share
#   Configure mapnik style-sheets
RUN cd /usr/local/src/mapnik-style/inc \
 && cp fontset-settings.xml.inc.template fontset-settings.xml.inc
COPY datasource-settings.sed /tmp/
RUN cd /usr/local/src/mapnik-style/inc \
 && sed --file /tmp/datasource-settings.sed datasource-settings.xml.inc.template > datasource-settings.xml.inc
COPY settings.sed /tmp/
RUN cd /usr/local/src/mapnik-style/inc \
 && sed --file /tmp/settings.sed settings.xml.inc.template > settings.xml.inc
#   Configure renderd
COPY renderd.conf.sed /tmp/
RUN cd /usr/local/etc \
 && sed --file /tmp/renderd.conf.sed --in-place renderd.conf
#   Create the files required for the mod_tile system to run
RUN mkdir /var/run/renderd \
 && chown www-data: /var/run/renderd
RUN mkdir /var/lib/mod_tile \
 && chown www-data /var/lib/mod_tile
#   Replace default apache index page with OpenLayers demo
COPY index.html /var/www/html/index.html
#   Configure mod_tile
COPY mod_tile.load /etc/apache2/mods-available/
COPY mod_tile.conf /etc/apache2/mods-available/
RUN a2enmod mod_tile
#   Ensure the webserver user can connect to the gis database
RUN sed -i -e 's/local all all peer/local gis www-data peer/' /etc/postgresql/9.3/main/pg_hba.conf
#   Tune postgresql
COPY postgresql.conf.sed /tmp/
RUN sed --file /tmp/postgresql.conf.sed --in-place /etc/postgresql/9.3/main/postgresql.conf
#   Define the application logging logic
COPY syslog-ng.conf /etc/syslog-ng/conf.d/local.conf
RUN rm -rf /var/log/postgresql
#   Create a `postgresql` `runit` service
COPY postgresql /etc/sv/postgresql
RUN update-service --add /etc/sv/postgresql
#   Create an `apache2` `runit` service
COPY apache2 /etc/sv/apache2
RUN update-service --add /etc/sv/apache2
#   Create a `renderd` `runit` service
COPY renderd /etc/sv/renderd
RUN update-service --add /etc/sv/renderd
#   Clean up APT when done
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Expose the webserver and database ports
EXPOSE 80/tcp 5432/tcp
#   We need the volume for importing data from
VOLUME ["/data"]
#   Set the osm2pgsql import cache size in MB. Used in `run import`.
ENV OSM_IMPORT_CACHE="40"
#   Add the README
COPY README.md /usr/local/share/doc/
#   Add the help file
RUN mkdir -p /usr/local/share/doc/run
COPY help.txt /usr/local/share/doc/run/help.txt
#   Add the entrypoint
COPY run.sh /usr/local/sbin/run
ENTRYPOINT ["/sbin/my_init", "--", "/usr/local/sbin/run"]
#   Default to showing the usage text
CMD ["help"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
