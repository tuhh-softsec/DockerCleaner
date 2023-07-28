FROM php:7.1-fpm
MAINTAINER Stepanov Nikolai <nstepanovdev@gmail.com>
RUN :
#   Install locale
ARG TZ=UTC
ENV TZ="${TZ}"
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone \
 && (apt-get update ;apt-get install --no-install-recommends locales=2.28-10+deb10u2 -f -y --reinstall ) \
 && locale-gen en_US.UTF-8
#   INSTALL EXTENSIONS
#   apcu
RUN pecl install apcu
RUN echo "extension=apcu.so" > /usr/local/etc/php/conf.d/apcu.ini
#   bz2
RUN (apt-get update ;apt-get install --no-install-recommends libbz2-dev=1.0.6-9.2~deb10u2 -y )
RUN docker-php-ext-install bz2
#   gd
RUN (apt-get update ;apt-get install --no-install-recommends libfreetype6-dev=2.9.1-3+deb10u3 libjpeg62-turbo-dev=1:1.5.2-2+deb10u1 libpng-dev=1.6.36-6 -y )
RUN docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/
RUN docker-php-ext-install gd
#   mcrypt
RUN (apt-get update ;apt-get install --no-install-recommends libmcrypt-dev=2.5.8-3.4 -y )
RUN docker-php-ext-install mcrypt
#   pdo
RUN docker-php-ext-install pdo_mysql \
 && (apt-get update ;apt-get install --no-install-recommends libpq-dev=11.19-0+deb10u1 -y ) \
 && docker-php-ext-install pdo_pgsql \
 && (apt-get update ;apt-get install --no-install-recommends libsqlite3-dev=3.27.2-3+deb10u2 -y ) \
 && docker-php-ext-install pdo_sqlite
#   phpredis
RUN pecl install -o -f redis \
 && rm -rf /tmp/pear \
 && echo "extension=redis.so" > /usr/local/etc/php/conf.d/redis.ini
#   xsl
RUN (apt-get update ;apt-get install --no-install-recommends libxslt-dev -y )
RUN docker-php-ext-install xsl
#   intl
RUN (apt-get update ;apt-get install --no-install-recommends libicu-dev=63.1-6+deb10u3 -y )
RUN docker-php-ext-configure intl
RUN docker-php-ext-install intl
#   zip
RUN (apt-get update ;apt-get install --no-install-recommends zlib1g-dev=1:1.2.11.dfsg-1+deb10u2 -y ) \
 && docker-php-ext-install zip
#   xdebug
RUN pecl install xdebug \
 && echo "zend_extension=$( find /usr/local/lib/php/extensions/ -name xdebug.so ;)\n" >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo "xdebug.remote_enable=1" >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo "xdebug.remote_autostart=0" >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo "xdebug.remote_connect_back=0" >> /usr/local/etc/php/conf.d/xdebug.ini
#   common
RUN (apt-get update ;apt-get install --no-install-recommends libssl-dev=1.1.1n-0+deb10u4 -y )
RUN docker-php-ext-install opcache calendar dba pcntl bcmath mbstring xmlrpc ftp shmop mysqli
#   preconf enviroment
ENV PHP_EXTRA_CONFIGURE_ARGS="--enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data"
RUN usermod -u 1000 -d /data -s /bin/bash www-data
RUN mkdir /data \
 && chmod -R 644 /data \
 && find /data -type d -exec chmod 755 {}
RUN (apt-get update ;apt-get install --no-install-recommends libfreetype6-dev=2.9.1-3+deb10u3 libjpeg62-turbo-dev=1:1.5.2-2+deb10u1 libmcrypt-dev=2.5.8-3.4 libpng-dev=1.6.36-6 msmtp=1.8.3-1 imagemagick=8:6.9.10.23+dfsg-2.1+deb10u4 libssl-dev=1.1.1n-0+deb10u4 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libicu-dev=63.1-6+deb10u3 libxslt-dev wget=1.20.1-1.1 git=1:2.20.1-2+deb10u8 vim=2:8.1.0875-5+deb10u4 ruby=1:2.5.1 ruby-dev=1:2.5.1 libcurl4-openssl-dev=7.64.0-4+deb10u5 -y )
#   Clean apt
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
COPY etc/php-fpm.conf /usr/local/etc/
COPY etc/php.ini /usr/local/etc/php/
#   Put host's IP to php.ini
ARG HOSTIP=127.0.0.1
ENV HOSTIP="${HOSTIP}"
RUN echo xdebug.remote_host=$HOSTIP >> /usr/local/etc/php/conf.d/xdebug.ini
RUN chmod ugo+rX -R /usr/local/etc/php
#   SSH
RUN echo "" >> /etc/ssh/ssh_config \
 && echo "IdentityFile /data/.ssh/id_rsa" >> /etc/ssh/ssh_config
WORKDIR /data
CMD ["php-fpm"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
