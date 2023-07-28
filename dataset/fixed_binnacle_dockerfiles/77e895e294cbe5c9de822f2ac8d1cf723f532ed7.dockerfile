#   runnable base
FROM boxcar/raring
#   REPOS
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.99.35 -y -q )
RUN add-apt-repository -y "deb http://archive.ubuntu.com/ubuntu $( lsb_release -sc ;) universe"
RUN add-apt-repository -y ppa:gophers/go/ubuntu
RUN add-apt-repository -y ppa:chris-lea/node.js
RUN add-apt-repository -y ppa:webupd8team/java
RUN add-apt-repository -y ppa:nginx/stable
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv 7F0CEB10
RUN echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | sudo tee /etc/apt/sources.list.d/10gen.list
RUN :
#  SHIMS
RUN dpkg-divert --local --rename --add /sbin/initctl
RUN ln -s /bin/true /sbin/initctl
ENV DEBIAN_FRONTEND="noninteractive"
#   EDITORS
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:9.0.1000-4ubuntu2 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends nano=7.2-1 -y -q )
#   TOOLS
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends make=4.3-4.1build1 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 -y -q )
#   RUN apt-get install -y -q supervisor
#   BUILD
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.9ubuntu3 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends g++=4:12.2.0-3ubuntu1 -y -q )
#   SERVICES
#  # MEMCACHED
RUN (apt-get update ;apt-get install --no-install-recommends memcached=1.6.18-1 -y -q )
#  RUN pecl install -y memcache
#  # COUCHDB
RUN (apt-get update ;apt-get install --no-install-recommends couchdb -y -q )
#  # REDIS
RUN (apt-get update ;apt-get install --no-install-recommends redis-server=5:7.0.8-4 -y -q )
#  # MONGO
RUN (apt-get update ;apt-get install --no-install-recommends mongodb-10gen -y -q )
#  # POSTGRES
RUN echo "#!/bin/sh\nexit 101" > /usr/sbin/policy-rc.d; chmod +x /usr/sbin/policy-rc.d
RUN (apt-get update ;apt-get install --no-install-recommends postgresql-9.1 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends postgresql-contrib-9.1 -y -q )
RUN rm /usr/sbin/policy-rc.d
RUN (apt-get update ;apt-get install --no-install-recommends pgadmin3 -y -q )
#  # MAGICK
RUN (apt-get update ;apt-get install --no-install-recommends imagemagick=8:6.9.11.60+dfsg-1.6 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends graphicsmagick=1.4+really1.3.40-2build1 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends graphicsmagick-libmagick-dev-compat=1.4+really1.3.40-2build1 -y -q )
#   #RUN pecl install -y imagick
#  # APACHE
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.55-1ubuntu2 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends libapache2-mod-php5 -y -q )
#  # MYSQL
RUN (apt-get update ;apt-get install --no-install-recommends mysql-client=8.0.32-0ubuntu4 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends mysql-server=8.0.32-0ubuntu4 -y -q )
#  # NGINX
RUN (apt-get update ;apt-get install --no-install-recommends nginx=1.22.0-1ubuntu3 -y -q )
#   LANGS
#  # GO
RUN (apt-get update ;apt-get install --no-install-recommends golang=2:1.20~0ubuntu1 -y -q )
#  # RUBY
RUN (apt-get update ;apt-get install --no-install-recommends ruby=1:3.1 -y -q )
#  # NODE
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=18.13.0+dfsg1-1ubuntu2 -y -q )
#  # PHP
RUN (apt-get update ;apt-get install --no-install-recommends php5 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends php-pear=1:1.10.13+submodules+notgz+2022032202-2 -y -q )
#  # ERLANG
RUN (apt-get update ;apt-get install --no-install-recommends erlang=1:25.2.3+dfsg-1 -y -q )
#   # # JAVA
#   # # Broken
#   # RUN debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections
#   # RUN debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
#   # RUN apt-get install -y -q oracle-java7-installer
#  # PYTHON
RUN (apt-get update ;apt-get install --no-install-recommends python-software-properties -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends python -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends python-dev -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends python-distribute -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends python-pip -y -q )
RUN pip install pip==23.1 --no-input --exists-action=w --upgrade
#   LIBS
RUN (apt-get update ;apt-get install --no-install-recommends libjpeg8-dev=8c-2ubuntu11 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends libfreetype6-dev=2.12.1+dfsg-4 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends liblcms1-dev -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends libwebp-dev=1.2.4-0.1build1 -y -q )
RUN (apt-get update ;apt-get install --no-install-recommends libtiff-dev=4.5.0-4ubuntu1 -y -q )
ENV DEBIAN_FRONTEND="dialog"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
