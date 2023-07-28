#  runnable base
FROM boxcar/raring
#  REPOS
RUN apt-get update -y
RUN apt-get install --no-install-recommends software-properties-common -y -q
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
RUN apt-get install --no-install-recommends vim -y -q
RUN apt-get install --no-install-recommends nano -y -q
#  TOOLS
RUN apt-get install --no-install-recommends curl -y -q
RUN apt-get install --no-install-recommends git -y -q
RUN apt-get install --no-install-recommends make -y -q
RUN apt-get install --no-install-recommends wget -y -q
#  RUN apt-get install -y -q supervisor
#  BUILD
RUN apt-get install --no-install-recommends build-essential -y -q
RUN apt-get install --no-install-recommends g++ -y -q
#  SERVICES
# # MEMCACHED
RUN apt-get install --no-install-recommends memcached -y -q
# RUN pecl install -y memcache
# # COUCHDB
RUN apt-get install --no-install-recommends couchdb -y -q
# # REDIS
RUN apt-get install --no-install-recommends redis-server -y -q
# # MONGO
RUN apt-get install --no-install-recommends mongodb-10gen -y -q
# # POSTGRES
RUN echo "#!/bin/sh\nexit 101" > /usr/sbin/policy-rc.d; chmod +x /usr/sbin/policy-rc.d
RUN apt-get install --no-install-recommends postgresql-9.1 -y -q
RUN apt-get install --no-install-recommends postgresql-contrib-9.1 -y -q
RUN rm /usr/sbin/policy-rc.d
RUN apt-get install --no-install-recommends pgadmin3 -y -q
# # MAGICK
RUN apt-get install --no-install-recommends imagemagick -y -q
RUN apt-get install --no-install-recommends graphicsmagick -y -q
RUN apt-get install --no-install-recommends graphicsmagick-libmagick-dev-compat -y -q
#  #RUN pecl install -y imagick
# # APACHE
RUN apt-get install --no-install-recommends apache2 -y -q
RUN apt-get install --no-install-recommends libapache2-mod-php5 -y -q
# # MYSQL
RUN apt-get install --no-install-recommends mysql-client -y -q
RUN apt-get install --no-install-recommends mysql-server -y -q
# # NGINX
RUN apt-get install --no-install-recommends nginx -y -q
#  LANGS
# # GO
RUN apt-get install --no-install-recommends golang -y -q
# # RUBY
RUN apt-get install --no-install-recommends ruby -y -q
# # NODE
RUN apt-get install --no-install-recommends nodejs -y -q
# # PHP
RUN apt-get install --no-install-recommends php5 -y -q
RUN apt-get install --no-install-recommends php-pear -y -q
# # ERLANG
RUN apt-get install --no-install-recommends erlang -y -q
#  # # JAVA
#  # # Broken
#  # RUN debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections
#  # RUN debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
#  # RUN apt-get install -y -q oracle-java7-installer
# # PYTHON
RUN apt-get install --no-install-recommends python-software-properties -y -q
RUN apt-get install --no-install-recommends python -y -q
RUN apt-get install --no-install-recommends python-dev -y -q
RUN apt-get install --no-install-recommends python-distribute -y -q
RUN apt-get install --no-install-recommends python-pip -y -q
RUN pip install pip --no-input --exists-action=w --upgrade
#  LIBS
RUN apt-get install --no-install-recommends libjpeg8-dev -y -q
RUN apt-get install --no-install-recommends zlib1g-dev -y -q
RUN apt-get install --no-install-recommends libfreetype6-dev -y -q
RUN apt-get install --no-install-recommends liblcms1-dev -y -q
RUN apt-get install --no-install-recommends libwebp-dev -y -q
RUN apt-get install --no-install-recommends libtiff-dev -y -q
ENV DEBIAN_FRONTEND="dialog"
