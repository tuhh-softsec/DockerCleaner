FROM ubuntu:12.04
RUN locale-gen en_US.UTF-8 \
 && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
RUN apt-get update \
 && apt-get install autoconf binutils-doc bison build-essential flex -q -y
# apt-utils
RUN apt-get install apt-utils -q -y
# ruby-rvm
RUN apt-get install ruby-rvm -q -y
# varnish
RUN apt-get install varnish -q -y
# git
RUN apt-get install git -q -y
# apt tools
RUN apt-get install python-software-properties -q -y
RUN add-apt-repository ppa:cartodb/gcc \
 && apt-get update
RUN apt-get install gcc-4.9 g++-4.9 -q -y
ENV CC="/usr/bin/gcc-4.9"
ENV CXX="/usr/bin/g++-4.9"
# postgresql
RUN add-apt-repository ppa:cartodb/postgresql-9.5 \
 && apt-get update
# client packages
RUN apt-get install libpq5 libpq-dev postgresql-client-9.5 postgresql-client-common -q -y
# server packages
RUN apt-get install postgresql-9.5 postgresql-contrib-9.5 postgresql-server-dev-9.5 postgresql-plpython-9.5 -q -y
# PostgreSQL Config
RUN sed -i 's/\(peer\|md5\)/trust/' /etc/postgresql/9.5/main/pg_hba.conf
# create users
RUN service postgresql start \
 && createuser publicuser --no-createrole --no-createdb --no-superuser -U postgres \
 && createuser tileuser --no-createrole --no-createdb --no-superuser -U postgres \
 && service postgresql stop
# carto postgres extension
RUN git clone https://github.com/CartoDB/cartodb-postgresql.git \
 && cd cartodb-postgresql \
 && PGUSER=postgres make install
# GIS dependencies
RUN apt-get upgrade -q -y \
 && add-apt-repository ppa:cartodb/gis \
 && apt-get update \
 && apt-get install proj proj-bin proj-data libproj-dev -q -y \
 && apt-get install libjson0 libjson0-dev python-simplejson -q -y \
 && apt-get install libgeos-c1v5 libgeos-dev -q -y \
 && apt-get install gdal-bin libgdal1-dev libgdal-dev -q -y \
 && apt-get install gdal2.1-static-bin -q -y
# postgis
RUN apt-get install libxml2-dev -q -y \
 && apt-get install liblwgeom-2.2.2 postgis postgresql-9.5-postgis-2.2 postgresql-9.5-postgis-scripts -q -y
# postgis setup
RUN service postgresql start \
 && createdb -T template0 -O postgres -U postgres -E UTF8 template_postgis \
 && psql -U postgres template_postgis -c 'CREATE EXTENSION postgis;CREATE EXTENSION postgis_topology;' \
 && ldconfig \
 && service postgresql stop
# redis
RUN add-apt-repository ppa:cartodb/redis \
 && apt-get update
RUN apt-get install redis-server -q -y
# nodejs
RUN add-apt-repository ppa:cartodb/nodejs \
 && apt-get update \
 && apt-get install nodejs -q -y
RUN apt-get install libpixman-1-0 libpixman-1-dev -q -y
RUN apt-get install libcairo2-dev libjpeg-dev libgif-dev libpango1.0-dev -q -y
# SQL API
RUN git clone git://github.com/CartoDB/CartoDB-SQL-API.git \
 && cd CartoDB-SQL-API \
 && git checkout master \
 && npm install
# MAPS API:
RUN git clone git://github.com/CartoDB/Windshaft-cartodb.git \
 && cd Windshaft-cartodb \
 && git checkout master \
 && npm install yarn@0.27.5 -g \
 && yarn install
# Ruby
RUN apt-get install wget -q -y \
 && wget -O ruby-install-0.5.0.tar.gz https://github.com/postmodern/ruby-install/archive/v0.5.0.tar.gz \
 && tar -xzvf ruby-install-0.5.0.tar.gz \
 && cd ruby-install-0.5.0/ \
 && make install
RUN apt-get install libreadline6-dev openssl -q -y \
 && ruby-install ruby 2.2.3
ENV PATH="$PATH:/opt/rubies/ruby-2.2.3/bin"
RUN gem install bundler --version=1.17.3 \
 && gem install compass
ENV RAILS_ENV="production"
# Carto Editor
RUN git clone --recursive https://github.com/CartoDB/cartodb.git \
 && cd cartodb \
 && wget -O /tmp/get-pip.py https://bootstrap.pypa.io/get-pip.py \
 && python /tmp/get-pip.py
RUN apt-get install python-all-dev -q -y \
 && apt-get install imagemagick unp zip -q -y
RUN cd cartodb \
 && bundle install \
 && npm install
ENV CPLUS_INCLUDE_PATH="/usr/include/gdal"
ENV C_INCLUDE_PATH="/usr/include/gdal"
ENV PATH="$PATH:/usr/include/gdal"
RUN cd cartodb \
 && pip install --no-use-wheel -r python_requirements.txt
# Config Files
ADD ./config/SQLAPI-prod.js /CartoDB-SQL-API/config/environments/production.js
ADD ./config/WS-prod.js /Windshaft-cartodb/config/environments/production.js
ADD ./config/app_config.yml /cartodb/config/app_config.yml
ADD ./config/database.yml /cartodb/config/database.yml
ADD ./config/grunt_production.json /cartodb/config/grunt_production.json
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
RUN cd cartodb \
 && export PATH=$PATH:$PWD/node_modules/grunt-cli/bin \
 && bundle install \
 && bundle exec grunt --environment production
RUN service postgresql start \
 && service redis-server start \
 && cd cartodb \
 && bundle exec rake db:create \
 && bundle exec rake db:migrate \
 && service postgresql stop \
 && service redis-server stop
ADD ./create_user /cartodb/script/create_user
RUN service postgresql start \
 && service redis-server start \
 && bash -l -c "cd /cartodb \
 && bash script/create_user" \
 && service postgresql stop \
 && service redis-server stop
EXPOSE 3000/tcp 8080/tcp 8181/tcp
ENV GDAL_DATA="/usr/share/gdal/2.1"
ADD ./startup.sh /opt/startup.sh
ADD ./config/varnish /etc/default/varnish
VOLUME ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql"]
CMD ["/bin/bash", "/opt/startup.sh"]
