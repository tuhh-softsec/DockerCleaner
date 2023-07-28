FROM php:7.1-fpm
RUN DEBIAN_FRONTEND=noninteractive
#   Install dotdeb repo
RUN echo "deb http://packages.dotdeb.org jessie all" > /etc/apt/sources.list.d/dotdeb.list \
 && curl -sS https://www.dotdeb.org/dotdeb.gpg | apt-key add - \
 && :
#   Install required libs
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends sudo=1.8.27-1+deb10u5 git=1:2.20.1-2+deb10u8 libcurl4-openssl-dev=7.64.0-4+deb10u5 libedit-dev=3.1-20181209-1 libssl-dev=1.1.1n-0+deb10u4 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libsqlite3-dev=3.27.2-3+deb10u2 sqlite3=3.27.2-3+deb10u2 libz-dev libpq-dev=11.19-0+deb10u1 libjpeg-dev=1:1.5.2-2+deb10u1 libpng12-dev libfreetype6-dev=2.9.1-3+deb10u3 libssl-dev=1.1.1n-0+deb10u4 libmcrypt-dev=2.5.8-3.4 libjudydebian1=1.0.5-5 libjudy-dev=1.0.5-5 cron=3.0pl1-134+deb10u1 -y ) \
 && apt-get clean
#
#   Configure
#
RUN docker-php-ext-configure gd --enable-gd-native-ttf --with-jpeg-dir=/usr/lib --with-freetype-dir=/usr/include/freetype2
#
#   Install extensions
#
RUN docker-php-ext-install mcrypt pdo_pgsql pdo_sqlite pcntl sockets bcmath opcache gd
#
#   Install non standard extensions
#
RUN pecl install ev
#
#   Export configs
#
COPY ./opcache.ini /usr/local/etc/php/conf.d/opcache.ini
#
#   Enable non standard extensions
#
RUN docker-php-ext-enable opcache ev
#
#  --------------------------------------------------------------------------
#   Final Touch
#  --------------------------------------------------------------------------
#
COPY ./laravel.ini /usr/local/etc/php/conf.d
COPY ./laravel.pool.conf /usr/local/etc/php-fpm.d/
RUN rm -r /var/lib/apt/lists/*
RUN rm -rf /tmp/pear
#  ####################################
#   Crontab
#  ####################################
COPY ./crontab /var/spool/cron/crontabs
#  ####################################
#   Composer:
#  ####################################
#   Install composer and add its bin to the PATH.
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN echo "" >> ~/.bashrc \
 && echo 'export PATH="/var/www/laravel.su/vendor/bin:$PATH"' >> ~/.bashrc
COPY ./start.sh /start.sh
RUN chmod 0755 /start.sh
WORKDIR /var/www/laravel.su
RUN usermod -u 1000 www-data
USER www-data
EXPOSE 9000/tcp
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
