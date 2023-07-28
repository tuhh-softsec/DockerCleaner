FROM ubuntu:16.04
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends language-pack-en-base=1:16.04+20160627 -qy ) \
 && locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.18-2ubuntu3.17 -y )
RUN a2enmod headers
RUN a2enmod rewrite
#   add PPA for PHP 7
RUN (apt-get update ;apt-get install --no-install-recommends apt-utils=1.2.35 -y )
RUN :
RUN apt-get -y dist-upgrade
RUN apt-get remove --purge ca-certificates
RUN apt-get autoremove
RUN (apt-get update ;apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 )
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 -y )
RUN add-apt-repository -y ppa:ondrej/php
#    Adding php 7
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends php7.1 php7.1-fpm php7.1-cli php7.1-common php7.1-mbstring php7.1-gd php7.1-intl php7.1-xml php7.1-mysql php7.1-mcrypt php7.1-zip -y )
RUN (apt-get update ;apt-get install --no-install-recommends libapache2-mod-php7.1 php7.1 php7.1-cli php-xdebug=2.4.0-1 php7.1-mbstring sqlite3=3.11.0-1ubuntu1.5 php7.1-mysql php-imagick=3.4.0~rc6-1ubuntu3 php-memcached=2.2.0-51-ge573a6e+2.2.0-2build2 php-pear=1:1.10.1+submodules+notgz-6ubuntu0.3 curl=7.47.0-1ubuntu2.19 imagemagick=8:6.8.9.9-7ubuntu5.16 php7.1-dev php7.1-phpdbg php7.1-gd npm=3.5.2-0ubuntu4 nodejs-legacy=4.2.6~dfsg-1ubuntu4.2 php7.1-json php7.1-curl php7.1-sqlite3 php7.1-intl apache2=2.4.18-2ubuntu3.17 vim=2:7.4.1689-3ubuntu1.5 git-core=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 libsasl2-dev=2.1.26.dfsg1-14ubuntu0.2 libssl-dev=1.0.2g-1ubuntu4.20 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libsslcommon2-dev=0.16-9ubuntu2 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 autoconf=2.69-9 g++=4:5.3.1-1ubuntu1 make=4.1-6 openssl=1.0.2g-1ubuntu4.20 libssl-dev=1.0.2g-1ubuntu4.20 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 pkg-config=0.29.1-0ubuntu1 libsasl2-dev=2.1.26.dfsg1-14ubuntu0.2 libpcre3-dev=2:8.38-3.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends imagemagick=8:6.8.9.9-7ubuntu5.16 graphicsmagick=1.3.23-1ubuntu0.6 -y )
RUN a2enmod headers
RUN a2enmod rewrite
ENV APACHE_RUN_USER="www-data"
ENV APACHE_RUN_GROUP="www-data"
ENV APACHE_LOG_DIR="/var/log/apache2"
ENV APACHE_PID_FILE="/var/run/apache2.pid"
ENV APACHE_RUN_DIR="/var/run/apache2"
ENV APACHE_LOCK_DIR="/var/lock/apache2"
RUN ln -sf /dev/stdout /var/log/apache2/access.log \
 && ln -sf /dev/stderr /var/log/apache2/error.log
RUN mkdir -p $APACHE_RUN_DIR $APACHE_LOCK_DIR $APACHE_LOG_DIR
#   Update application repository list and install the Redis server. 
#  RUN apt-get update && apt-get install -y redis-server
#   Allow Composer to be run as root
ENV COMPOSER_ALLOW_SUPERUSER="1"
#   Setup the Composer installer
RUN curl -o /tmp/composer-setup.php https://getcomposer.org/installer \
 && curl -o /tmp/composer-setup.sig https://composer.github.io/installer.sig \
 && php -r "if (hash('SHA384', file_get_contents('/tmp/composer-setup.php')) !== trim(file_get_contents('/tmp/composer-setup.sig'))) { unlink('/tmp/composer-setup.php'); echo 'Invalid installer' . PHP_EOL; exit(1); }" \
 && php /tmp/composer-setup.php \
 && chmod a+x composer.phar \
 && mv composer.phar /usr/local/bin/composer
#   Install composer dependencies
RUN echo pwd: `pwd ` \
 && echo ls: `ls `
#   RUN composer install
EXPOSE 80/tcp
#   Expose default port
#  EXPOSE 6379
VOLUME [ "/var/www/html" ,"/etc/mysql"]
WORKDIR /var/www/html
ENTRYPOINT ["/usr/sbin/apache2"]
CMD ["-D", "FOREGROUND"]
COPY . /var/www/html
COPY ./deploy/vhost.conf /etc/apache2/sites-available/000-default.conf
RUN chown -R www-data:www-data /var/www/html \
 && a2enmod rewrite
RUN chown -R 777 /var/www/html/
RUN composer install
#  RUN php artisan vendor:publish --provider="Tymon\JWTAuth\Providers\LaravelServiceProvider"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
