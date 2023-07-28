#
#   Cartodb container
#
FROM ubuntu:14.04
MAINTAINER Adrien Fleury <fleu42@gmail.com>
#   Configuring locales
RUN dpkg-reconfigure locales \
 && locale-gen en_US.UTF-8 \
 && update-locale LANG=en_US.UTF-8
ENV LANG="en_US.UTF-8  "
ENV LANGUAGE="en_US:en  "
ENV LC_ALL="en_US.UTF-8"
#   Preparing apt
RUN apt-get update \
 && useradd -m -d /home/cartodb -s /bin/bash cartodb \
 && apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y -q \
 && add-apt-repository -y ppa:chris-lea/node.js \
 && apt-get update
#   Installing stuff 
RUN apt-get install --no-install-recommends build-essential=11.6ubuntu6 checkinstall=1.6.2-4ubuntu1 unp=2.0~pre7+nmu1 zip=3.0-8 libgeos-c1=3.4.2-4ubuntu1 libgeos-dev=3.4.2-4ubuntu1 libjson0=0.11-3ubuntu1.2 python-simplejson=3.3.1-1ubuntu6 libjson0-dev=0.11-3ubuntu1.2 proj-bin=4.8.0-2ubuntu2 proj-data=4.8.0-2ubuntu2 libproj-dev=4.8.0-2ubuntu2 postgresql-9.3=9.3.24-0ubuntu0.14.04 postgresql-client-9.3=9.3.24-0ubuntu0.14.04 postgresql-contrib-9.3=9.3.24-0ubuntu0.14.04 postgresql-server-dev-9.3=9.3.24-0ubuntu0.14.04 postgresql-plpython-9.3=9.3.24-0ubuntu0.14.04 gdal-bin=1.10.1+dfsg-5ubuntu1 libgdal1-dev=1.10.1+dfsg-5ubuntu1 nodejs=0.10.25~dfsg2-2ubuntu1.2 redis-server=2:2.8.4-2ubuntu0.2 python2.7-dev=2.7.6-8ubuntu0.5 build-essential=11.6ubuntu6 python-setuptools=3.3-1ubuntu2 varnish=3.0.5-2ubuntu0.1 imagemagick=8:6.7.7.10-6ubuntu3.13 git=1:1.9.1-1ubuntu0.10 postgresql-9.3-postgis-2.1=2.1.2+dfsg-2ubuntu0.2 libmapnik-dev=2.2.0+ds1-6build2 python-mapnik=2.2.0+ds1-6build2 mapnik-utils=2.2.0+ds1-6build2 postgresql-9.3-postgis-2.1-scripts=2.1.2+dfsg-2ubuntu0.2 postgis=2.1.2+dfsg-2ubuntu0.2 python-argparse python-gdal=1.10.1+dfsg-5ubuntu1 python-chardet=2.0.1-2build2 openssl=1.0.1f-1ubuntu2.27 libreadline6=6.3-4ubuntu2 curl=7.35.0-1ubuntu2.20 git-core=1:1.9.1-1ubuntu0.10 zlib1g=1:1.2.8.dfsg-1ubuntu1.1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libssl-dev=1.0.1f-1ubuntu2.27 libyaml-dev=0.1.4-3ubuntu3.1 libsqlite3-dev=3.8.2-1ubuntu2.2 sqlite3=3.8.2-1ubuntu2.2 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt-dev autoconf=2.69-6 libc6-dev=2.19-0ubuntu6.15 ncurses-dev automake=1:1.14.1-2ubuntu1 libtool=2.4.2-1.7ubuntu1 bison=2:3.0.2.dfsg-2 subversion=1.8.8-1ubuntu3.3 pkg-config=0.26-1ubuntu4 libpq5=9.3.24-0ubuntu0.14.04 libpq-dev=9.3.24-0ubuntu0.14.04 libcurl4-gnutls-dev=7.35.0-1ubuntu2.20 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libgdbm-dev=1.8.3-12build1 gnupg=1.4.16-1ubuntu2.6 libreadline6-dev=6.3-4ubuntu2 -y -q
#   Setting PostgreSQL
RUN sed -i 's/\(peer\|md5\)/trust/' /etc/postgresql/9.3/main/pg_hba.conf
#   Install schema_triggers
RUN git clone https://github.com/CartoDB/pg_schema_triggers.git \
 && cd pg_schema_triggers \
 && make all install \
 && sed -i "/#shared_preload/a shared_preload_libraries = 'schema_triggers.so'" /etc/postgresql/9.3/main/postgresql.conf
COPY ./template_postgis.sh /tmp/template_postgis.sh
RUN service postgresql start \
 && /bin/su postgres -c /tmp/template_postgis.sh \
 && service postgresql stop
#   Install cartodb extension
RUN git clone --branch 0.5.1 https://github.com/CartoDB/cartodb-postgresql \
 && cd cartodb-postgresql \
 && PGUSER=postgres make install
COPY ./cartodb_pgsql.sh /tmp/cartodb_pgsql.sh
RUN service postgresql start \
 && /bin/su postgres -c /tmp/cartodb_pgsql.sh \
 && service postgresql stop
#   Install CartoDB API
RUN git clone git://github.com/CartoDB/CartoDB-SQL-API.git \
 && cd CartoDB-SQL-API \
 && ./configure \
 && npm install
COPY ./config/CartoDB-dev.js /CartoDB-SQL-API/config/environments/development.js
#   Install Windshaft
RUN git clone git://github.com/CartoDB/Windshaft-cartodb.git \
 && cd Windshaft-cartodb \
 && ./configure \
 && npm install \
 && mkdir logs
COPY ./config/WS-dev.js /Windshaft-cartodb/config/environments/development.js
#   Install rvm
RUN gpg --keyserver hkp://keys.gnupg.net --recv-keys D39DC0E3
RUN curl -L https://get.rvm.io | bash -s stable --ruby
RUN echo 'source /usr/local/rvm/scripts/rvm' >> /etc/bash.bashrc
RUN /bin/bash -l -c rvm requirements
ENV PATH="/usr/local/rvm/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
RUN /bin/bash -l -c 'rvm install 1.9.3-p547 --patch railsexpress'
RUN /bin/bash -l -c 'rvm use 1.9.3-p547 --default'
RUN /bin/bash -l -c 'gem install bundle archive-tar-minitar'
#   Install bundler
RUN /bin/bash -l -c 'gem install bundler --no-doc --no-ri'
#   Install CartoDB (with the bug correction on bundle install)
RUN git clone git://github.com/CartoDB/cartodb.git \
 && cd cartodb \
 && /bin/bash -l -c 'bundle install' || /bin/bash -l -c "cd $( /bin/bash -l -c 'gem contents debugger-ruby_core_source' | grep CHANGELOG | sed -e 's,CHANGELOG.md,,' ;) \
 && /bin/bash -l -c 'rake add_source VERSION=$( /bin/bash -l -c 'ruby --version' | awk '{print $2}' | sed -e 's,p55,-p55,' ;)' \
 && cd /cartodb \
 && /bin/bash -l -c 'bundle install'"
#   Copy confs
COPY ./config/app_config.yml /cartodb/config/app_config.yml
COPY ./config/database.yml /cartodb/config/database.yml
COPY ./create_dev_user /cartodb/script/create_dev_user
ENV PATH="/usr/local/rvm/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
RUN service postgresql start \
 && service redis-server start \
 && bash -l -c "cd /cartodb \
 && bash script/create_dev_user" \
 && service postgresql stop \
 && service redis-server stop
EXPOSE 3000/tcp
COPY ./startup.sh /opt/startup.sh
CMD ["/bin/bash", "/opt/startup.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
