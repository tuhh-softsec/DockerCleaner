# syntax = docker/dockerfile:experimental
#  https://sources.debian.org/src/php7.2/7.2.4-1/debian/control/
ARG PHP_VERSION=7.2.19
FROM --platform=$TARGETPLATFORM php:${PHP_VERSION}-fpm-stretch
LABEL maintainer="khs1994-docker/lnmp <khs1994@khs1994.com>"
ENV TZ="Asia/Shanghai"
ENV PHP_EXTENSION="bcmath  bz2  calendar  enchant  exif  gd  gettext  gmp  imap  intl  mysqli  pcntl  pdo_pgsql  pdo_mysql  pgsql  sockets  sysvmsg  sysvsem  sysvshm  xmlrpc  xsl  zip"
ENV PECL_EXTENSION="mongodb  igbinary  redis  memcached  xdebug  yaml  swoole"
ARG DEB_URL=deb.debian.org
ARG DEB_SECURITY_URL=security.debian.org/debian-security
RUN sed -i "s!deb.debian.org!${DEB_URL}!g" /etc/apt/sources.list \
 && sed -i "s!security.debian.org/debian-security!${DEB_SECURITY_URL}!g" /etc/apt/sources.list \
 && set -xe \
 && buildDeps=" wget unzip libfreetype6-dev libjpeg62-turbo-dev libpng-dev libsasl2-dev libssl-dev libmemcached-dev libpq-dev libzip-dev zlib1g-dev libyaml-dev libxmlrpc-epi-dev libbz2-dev libexif-dev libgmp-dev libicu-dev libwebp-dev libxpm-dev libenchant-dev libxslt1-dev libc-client2007e-dev libkrb5-dev " \
 && runDeps=" libfreetype6 libjpeg62-turbo libpng16-16 libssl1.1 libmemcached11 libmemcachedutil2 libpq5 libzip4 zlib1g libyaml-0-2 libxslt1.1 libxmlrpc-epi0 libbz2-1.0 libexif12 libgmp10 libicu57 libxpm4 libwebp6 libenchant1c2a libc-client2007e libkrb5-3 " \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps $runDeps -y \
 && rm -r /var/lib/apt/lists/* \
 && docker-php-ext-configure zip --with-libzip \
 && docker-php-ext-configure gd --disable-gd-jis-conv --with-freetype-dir=/usr --with-jpeg-dir=/usr --with-png-dir=/usr --with-webp-dir=/usr --with-xpm-dir=/usr \
 && docker-php-ext-configure imap --with-kerberos --with-imap-ssl \
 && docker-php-ext-install $PHP_EXTENSION \
 && for extension in ${PHP_EXTENSION}; do strip --strip-all $( php-config --extension-dir ;)/$( echo ${extension} | cut -d '-' -f 1 ;).so ; done \
 && for extension in $PECL_EXTENSION; do pecl install $extension \
 && docker-php-ext-enable $( echo ${extension} | cut -d '-' -f 1 ;) || echo "pecl ${extension} install error" \
 && rm -rf /usr/local/lib/php/doc/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && rm -rf /usr/local/lib/php/test/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && rm -rf /usr/local/include/php/ext/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${extension} | cut -d '-' -f 1 ;).so ; done \
 && docker-php-ext-enable opcache \
 && apt-get purge -y --auto-remove $buildDeps \
 && rm -rf /tmp/* \
 && rm -rf /usr/local/lib/php/.registry/.channel.pecl.php.net/* \
 && mv /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini.default \
 && mkdir -p /var/log/php-fpm \
 && ln -sf /dev/stdout /var/log/php-fpm/access.log \
 && ln -sf /dev/stderr /var/log/php-fpm/error.log \
 && ln -sf /dev/stderr /var/log/php-fpm/xdebug-remote.log \
 && chmod -R 777 /var/log/php-fpm
#  install composer
ENV COMPOSER_ALLOW_SUPERUSER="1" \
    COMPOSER_HOME="/tmp" \
    COMPOSER_VERSION="1.8.6" \
    PS1="[\u@\h \w]# "
#  https://github.com/composer/docker
RUN --mount=type=bind,target=/opt/bin/composer,source=/usr/bin/composer,from=arm32v7/composer:1.8.6 echo "memory_limit=-1" > "$PHP_INI_DIR/conf.d/memory-limit.ini" \
 && echo "date.timezone=${PHP_TIMEZONE:-PRC}" > "$PHP_INI_DIR/conf.d/date_timezone.ini" \
 && curl -fsSL https://raw.githubusercontent.com/composer/docker/master/1.8/docker-entrypoint.sh > /docker-entrypoint.composer.sh \
 && chmod +x /docker-entrypoint.composer.sh \
 && cp -a /opt/bin/composer /usr/bin/composer \
 && composer --ansi --version --no-interaction \
 && composer config -g repo.packagist composer https://packagist.laravel-china.org \
 && composer global require --prefer-dist "laravel/installer" \
 && composer global require --prefer-dist "friendsofphp/php-cs-fixer" \
 && curl -fsSL http://get.sensiolabs.org/sami.phar > /usr/local/bin/sami \
 && chmod +x /usr/local/bin/sami \
 && ln -sf /tmp/vendor/bin/* /usr/local/bin \
 && rm -rf /tmp/cache /tmp/installer.php /tmp/*.pub /tmp/composer.lock
WORKDIR /app
