ARG PHP_VERSION=7.2.19
FROM --platform=$TARGETPLATFORM php:${PHP_VERSION}-fpm-alpine AS php
LABEL maintainer="khs1994-docker/lnmp <khs1994@khs1994.com>"
ARG PHP_EXTENSION_EXTRA
ARG PECL_EXTENSION_EXTRA
ARG APK_EXTRA
ARG APK_DEV_EXTRA
ENV TZ="Asia/Shanghai" \
    APP_ENV="development"
ENV PHP_EXTENSION="bcmath  bz2  calendar  enchant  exif  gd  gettext  gmp  imap  intl  mysqli  pcntl  pdo_pgsql  pdo_mysql  pgsql  sockets  sysvmsg  sysvsem  sysvshm  xmlrpc  xsl  zip  ${PHP_EXTENSION_EXTRA:-}"
ENV PECL_EXTENSION="mongodb  igbinary  redis  memcached  xdebug  yaml  ${PECL_EXTENSION_EXTRA:-}"
ARG ALPINE_URL=dl-cdn.alpinelinux.org
RUN sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && set -xe \
 && PHP_FPM_RUN_DEPS=" bash tzdata libmemcached-libs libpq libzip libpng freetype libjpeg-turbo libxpm libwebp yaml libbz2 libexif libxslt gmp xmlrpc-c enchant c-client icu-libs ${APK_EXTRA:-} zlib " \
 && PHP_FPM_BUILD_DEPS=" openssl-dev libmemcached-dev zlib-dev cyrus-sasl-dev postgresql-dev libzip-dev libpng-dev freetype-dev libjpeg-turbo-dev libxpm-dev libwebp-dev yaml-dev libexif-dev libxslt-dev gmp-dev xmlrpc-c-dev bzip2-dev enchant-dev imap-dev gettext-dev libwebp-dev icu-dev ${APK_DEV_EXTRA:-} " \
 && apk add --no-cache --virtual .php-fpm-run-deps $PHP_FPM_RUN_DEPS \
 && apk add --no-cache --virtual .php-fpm-build-deps $PHP_FPM_BUILD_DEPS \
 && docker-php-ext-configure zip --with-libzip \
 && docker-php-ext-configure gd --disable-gd-jis-conv --with-freetype-dir=/usr --with-jpeg-dir=/usr --with-png-dir=/usr --with-webp-dir=/usr --with-xpm-dir=/usr \
 && docker-php-ext-install $PHP_EXTENSION \
 && apk add --no-cache --virtual .build-deps $PHPIZE_DEPS \
 && for extension in ${PHP_EXTENSION}; do strip --strip-all $( php-config --extension-dir ;)/$( echo ${extension} | cut -d '-' -f 1 ;).so ; done \
 && for extension in $PECL_EXTENSION; do pecl install $extension \
 && docker-php-ext-enable $( echo ${extension} | cut -d '-' -f 1 ;) || echo "pecl ${extension} install error" \
 && rm -rf /usr/local/lib/php/doc/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && rm -rf /usr/local/lib/php/test/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && rm -rf /usr/local/include/php/ext/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${extension} | cut -d '-' -f 1 ;).so ; done \
 && test `uname -m ` = 'x86_64' \
 && (cd /tmp \
 && wget https://github.com/tideways/php-xhprof-extension/archive/master.zip \
 && unzip master.zip \
 && cd php-xhprof-extension-master \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && docker-php-ext-enable opcache tideways_xhprof \
 && mv /usr/local/etc/php/conf.d/docker-php-ext-tideways_xhprof.ini /usr/local/etc/php/conf.d/docker-php-ext-tideways_xhprof.ini.default ) || docker-php-ext-enable opcache \
 && apk del --no-network --no-cache .build-deps .php-fpm-build-deps \
 && rm -rf /tmp/* \
 && rm -rf /usr/local/lib/php/.registry/.channel.pecl.php.net/* \
 && mv /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini.default \
 && mkdir -p /var/log/php-fpm \
 && ln -sf /dev/stdout /var/log/php-fpm/access.log \
 && ln -sf /dev/stderr /var/log/php-fpm/error.log \
 && ln -sf /dev/stderr /var/log/php-fpm/xdebug-remote.log \
 && chmod -R 777 /var/log/php-fpm
WORKDIR /app
