#  runnable base
FROM boxcar/raring
#  REPOS
RUN apt-get update -y
RUN apt-get install software-properties-common -y -q
RUN add-apt-repository -y "deb http://archive.ubuntu.com/ubuntu $( lsb_release -sc ;) universe"
RUN add-apt-repository -y ppa:gophers/go/ubuntu
RUN add-apt-repository -y ppa:chris-lea/node.js
RUN add-apt-repository -y ppa:webupd8team/java
RUN add-apt-repository -y ppa:nginx/stable
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv 7F0CEB10
RUN echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | sudo tee /etc/apt/sources.list.d/10gen.list
RUN apt-get update -y
# SHIMS
RUN dpkg-divert --local --rename --add /sbin/initctl
RUN ln -s /bin/true /sbin/initctl
ENV DEBIAN_FRONTEND="noninteractive"
#  EDITORS
RUN apt-get install vim -y -q
RUN apt-get install nano -y -q
#  TOOLS
RUN apt-get install curl -y -q
RUN apt-get install git -y -q
RUN apt-get install make -y -q
RUN apt-get install wget -y -q
#  RUN apt-get install -y -q supervisor
#  BUILD
RUN apt-get install build-essential -y -q
RUN apt-get install g++ -y -q
#  SERVICES
# # MEMCACHED
RUN apt-get install memcached -y -q
# RUN pecl install -y memcache
# # COUCHDB
RUN apt-get install couchdb -y -q
# # REDIS
RUN apt-get install redis-server -y -q
# # MONGO
RUN apt-get install mongodb-10gen -y -q
# # POSTGRES
RUN echo "#!/bin/sh\nexit 101" > /usr/sbin/policy-rc.d; chmod +x /usr/sbin/policy-rc.d
RUN apt-get install postgresql-9.1 -y -q
RUN apt-get install postgresql-contrib-9.1 -y -q
RUN rm /usr/sbin/policy-rc.d
RUN apt-get install pgadmin3 -y -q
# # MAGICK
RUN apt-get install imagemagick -y -q
RUN apt-get install graphicsmagick -y -q
RUN apt-get install graphicsmagick-libmagick-dev-compat -y -q
#  #RUN pecl install -y imagick
# # APACHE
RUN apt-get install apache2 -y -q
RUN apt-get install libapache2-mod-php5 -y -q
# # MYSQL
RUN apt-get install mysql-client -y -q
RUN apt-get install mysql-server -y -q
# # NGINX
RUN apt-get install nginx -y -q
#  LANGS
# # GO
RUN apt-get install golang -y -q
# # RUBY
RUN apt-get install ruby -y -q
# # NODE
RUN apt-get install nodejs -y -q
# # PHP
RUN apt-get install php5 -y -q
RUN apt-get install php-pear -y -q
# # ERLANG
RUN apt-get install erlang -y -q
#  # # JAVA
#  # # Broken
#  # RUN debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections
#  # RUN debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
#  # RUN apt-get install -y -q oracle-java7-installer
# # PYTHON
RUN apt-get install python-software-properties -y -q
RUN apt-get install python -y -q
RUN apt-get install python-dev -y -q
RUN apt-get install python-distribute -y -q
RUN apt-get install python-pip -y -q
RUN pip install pip --no-input --exists-action=w --upgrade
#  LIBS
RUN apt-get install libjpeg8-dev -y -q
RUN apt-get install zlib1g-dev -y -q
RUN apt-get install libfreetype6-dev -y -q
RUN apt-get install liblcms1-dev -y -q
RUN apt-get install libwebp-dev -y -q
RUN apt-get install libtiff-dev -y -q
ENV DEBIAN_FRONTEND="dialog"
