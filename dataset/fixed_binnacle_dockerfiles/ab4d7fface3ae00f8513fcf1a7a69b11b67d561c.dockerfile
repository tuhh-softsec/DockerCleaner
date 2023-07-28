ARG PHP_VERSION=7.0.33
FROM --platform=$TARGETPLATFORM php:${PHP_VERSION}-fpm-alpine AS php
LABEL maintainer="khs1994-docker/lnmp <khs1994@khs1994.com>"
ARG PHP_EXTENSION_EXTRA
ARG PECL_EXTENSION_EXTRA
ARG APK_EXTRA
ARG APK_DEV_EXTRA
ENV TZ="Asia/Shanghai" \
    APP_ENV="development"
ENV PHP_EXTENSION="bcmath  bz2  calendar  enchant  exif  gd  gettext  gmp  imap  intl  mysqli  pcntl  pdo_pgsql  pdo_mysql  pgsql  sockets  sysvmsg  sysvsem  sysvshm  xmlrpc  xsl  zip  ${PHP_EXTENSION_EXTRA:-}"
ENV PECL_EXTENSION="mongodb  igbinary  redis  memcached  xdebug  yaml  swoole  ${PECL_EXTENSION_EXTRA:-}"
ARG ALPINE_URL=dl-cdn.alpinelinux.org
RUN sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && set -xe \
 && PHP_FPM_RUN_DEPS=" bash tzdata libmemcached-libs libpq zlib libpng freetype libjpeg-turbo libxpm libwebp yaml libbz2 libexif libxslt gmp xmlrpc-c enchant c-client icu-libs ${APK_EXTRA:-} " \
 && PHP_FPM_BUILD_DEPS=" libressl-dev libmemcached-dev cyrus-sasl-dev postgresql-dev zlib-dev libpng-dev freetype-dev libjpeg-turbo-dev libxpm-dev libwebp-dev yaml-dev libexif-dev libxslt-dev gmp-dev xmlrpc-c-dev bzip2-dev enchant-dev imap-dev gettext-dev libwebp-dev icu-dev nghttp2-dev ${APK_DEV_EXTRA:-} " \
 && apk add $PHP_FPM_RUN_DEPS --no-cache --virtual .php-fpm-run-deps \
 && apk add $PHP_FPM_BUILD_DEPS --no-cache --virtual .php-fpm-build-deps \
 && docker-php-ext-configure gd --with-freetype-dir=/usr --with-jpeg-dir=/usr --with-png-dir=/usr --with-webp-dir=/usr --with-xpm-dir=/usr \
 && docker-php-ext-install $PHP_EXTENSION \
 && apk add $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && for extension in ${PHP_EXTENSION}; do strip --strip-all $( php-config --extension-dir ;)/$( echo ${extension} | cut -d '-' -f 1 ;).so ; done \
 && for extension in $PECL_EXTENSION; do pecl install $extension \
 && docker-php-ext-enable $( echo ${extension} | cut -d '-' -f 1 ;) || echo "pecl ${extension} install error" \
 && rm -rf /usr/local/lib/php/doc/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && rm -rf /usr/local/lib/php/test/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && rm -rf /usr/local/include/php/ext/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${extension} | cut -d '-' -f 1 ;).so ; done \
 && docker-php-ext-enable opcache \
 && docker-php-ext-enable $PECL_EXTENSION opcache \
 && apk del --no-network --no-cache .build-deps .php-fpm-build-deps \
 && mv /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini.default \
 && rm -rf /tmp/* \
 && rm -rf /usr/local/lib/php/.registry/.channel.pecl.php.net/* \
 && mkdir -p /var/log/php-fpm \
 && ln -sf /dev/stdout /var/log/php-fpm/access.log \
 && ln -sf /dev/stderr /var/log/php-fpm/error.log \
 && ln -sf /dev/stderr /var/log/php-fpm/xdebug-remote.log \
 && chmod -R 777 /var/log/php-fpm
STOPSIGNAL SIGQUIT
WORKDIR /app
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
