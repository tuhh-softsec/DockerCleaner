FROM php:7.2-fpm
LABEL maintainer="hareku <hareku908@gmail.com>"
USER root
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.64.0-4+deb10u5 busybox-static=1:1.30.1-4 libmemcached-dev=1.0.18-4.2 libz-dev libpq-dev=11.19-0+deb10u1 libjpeg-dev=1:1.5.2-2+deb10u1 libpng-dev=1.6.36-6 libfreetype6-dev=2.9.1-3+deb10u3 libssl-dev=1.1.1n-0+deb10u4 libmcrypt-dev=2.5.8-3.4 -y \
 && rm -rf /var/lib/apt/lists/*
RUN docker-php-ext-install pdo_mysql \
 && docker-php-ext-install pdo_pgsql \
 && docker-php-ext-configure gd --enable-gd-native-ttf --with-jpeg-dir=/usr/lib --with-freetype-dir=/usr/include/freetype2 \
 && docker-php-ext-install gd
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -yqq \
 && apt-get install --no-install-recommends apt-utils=1.8.2.3 -y \
 && pecl channel-update pecl.php.net
#  ##########################################################################
#   ZipArchive:
#  ##########################################################################
RUN apt-get install --no-install-recommends libzip-dev=1.5.1-4 -y \
 && docker-php-ext-configure zip --with-libzip \
 && docker-php-ext-install zip
#  ##########################################################################
#   Mysql Client:
#  ##########################################################################
RUN apt-get install --no-install-recommends mysql-client -y
#  ##########################################################################
#   PHP Memcached:
#  ##########################################################################
RUN curl -L -o /tmp/memcached.tar.gz "https://github.com/php-memcached-dev/php-memcached/archive/php7.tar.gz"
RUN mkdir -p memcached \
 && tar -C memcached -zxvf /tmp/memcached.tar.gz --strip 1 \
 && (cd memcached \
 && phpize \
 && ./configure \
 && make -j$( nproc ;) \
 && make install ) \
 && rm -r memcached \
 && rm /tmp/memcached.tar.gz \
 && docker-php-ext-enable memcached
#  ##########################################################################
#   PHP REDIS EXTENSION:
#  ##########################################################################
RUN printf "\n" | pecl install -o -f redis \
 && rm -rf /tmp/pear \
 && docker-php-ext-enable redis
#  ##########################################################################
#   Image optimizers:
#  ##########################################################################
RUN apt-get install --no-install-recommends jpegoptim=1.4.6-1 optipng=0.7.7-1 pngquant=2.12.2-1 gifsicle=1.91-5 -y
#  ##########################################################################
#   ImageMagick:
#  ##########################################################################
RUN apt-get install --no-install-recommends libmagickwand-dev=8:6.9.10.23+dfsg-2.1+deb10u4 imagemagick=8:6.9.10.23+dfsg-2.1+deb10u4 -y \
 && pecl install imagick \
 && docker-php-ext-enable imagick
#  ##########################################################################
#   Crontab
#  ##########################################################################
COPY ./crontab /var/spool/cron/crontabs/root
#  ##########################################################################
#   Opcache:
#  ##########################################################################
RUN docker-php-ext-install opcache
COPY ./opcache.ini /usr/local/etc/php/conf.d/opcache.ini
#  ##########################################################################
#   Supervisor:
#  ##########################################################################
RUN apt-get install --no-install-recommends supervisor=3.3.5-1 -y \
 && rm -rf /var/lib/apt/lists/*
COPY ./supervisord.conf /etc/
COPY ./supervisord.d/crond.conf /etc/supervisord.d/
COPY ./supervisord.d/laravel-worker.conf /etc/supervisord.d/
COPY ./supervisord.d/php-fpm.conf /etc/supervisord.d/
#
#  --------------------------------------------------------------------------
#   Final Touch
#  --------------------------------------------------------------------------
#
COPY ./laravel.ini /usr/local/etc/php/conf.d
COPY ./xlaravel.pool.conf /usr/local/etc/php-fpm.d/
COPY ./php7.2.ini /usr/local/etc/php/php.ini
#   Clean up
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && rm /var/log/lastlog /var/log/faillog
RUN usermod -u 1000 www-data
ENTRYPOINT ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
WORKDIR /etc/supervisor/conf.d/
EXPOSE 9000/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
