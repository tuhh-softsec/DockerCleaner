#   Dockerfile written by 10up <sales@10up.com>
#
#   Work derived from official PHP Docker Library:
#   Copyright (c) 2014-2015 Docker, Inc.
FROM debian:jessie
#   persistent / runtime deps
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates curl libssl-dev libxml2-dev libxml2 libpng12-dev libmcrypt-dev php5-memcached -y \
 && rm -r /var/lib/apt/lists/*
#   phpize deps
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf file g++ gcc libc-dev make pkg-config re2c -y \
 && rm -r /var/lib/apt/lists/*
ENV PHP_INI_DIR="/usr/local/etc/php"
RUN mkdir -p $PHP_INI_DIR/conf.d
ENV PHP_EXTRA_CONFIGURE_ARGS="--enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data"
ENV GPG_KEYS="6E4F6AB321FDC07F2C332E3AC2BF0BC433CFC8B3 1A4E8B7277C42E53DBA9C7B9BCAA30EA9C0D5763"
RUN set -xe \
 && for key in $GPG_KEYS; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done
ENV PHP_VERSION="7.0.5"
ENV PHP_FILENAME="php-7.0.5.tar.xz"
#   --enable-mysqlnd is included below because it's harder to compile after the fact the extensions are (since it's a plugin for several extensions, not an extension in itself)
RUN buildDeps=" $PHP_EXTRA_BUILD_DEPS libcurl4-openssl-dev libreadline6-dev xz-utils " \
 && set -x \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && curl -fSL "http://php.net/get/$PHP_FILENAME/from/this/mirror" -o "$PHP_FILENAME" \
 && curl -fSL "http://php.net/get/$PHP_FILENAME.asc/from/this/mirror" -o "$PHP_FILENAME.asc" \
 && gpg --verify "$PHP_FILENAME.asc" \
 && mkdir -p /usr/src/php \
 && tar -xf "$PHP_FILENAME" -C /usr/src/php --strip-components=1 \
 && rm "$PHP_FILENAME"* \
 && cd /usr/src/php \
 && ./configure --with-config-file-path="$PHP_INI_DIR" --with-config-file-scan-dir="$PHP_INI_DIR/conf.d" $PHP_EXTRA_CONFIGURE_ARGS --disable-cgi --enable-mysqlnd --with-curl --with-openssl --with-readline --with-zlib \
 && make -j"$( nproc ;)" \
 && make install \
 && { find /usr/local/bin /usr/local/sbin -type f -executable -exec strip --strip-all '{}' + || true ; } \
 && make clean \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false -o APT::AutoRemove::SuggestsImportant=false $buildDeps \
 && apt-get autoremove
COPY docker-php-ext-* /usr/local/bin/
ENV extensionDeps="  rsync  "
RUN extensions=" gd mysqli soap mcrypt mbstring " ; apt-get update \
 && apt-get install --no-install-recommends $extensionDeps -y \
 && docker-php-ext-install $extensions \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false -o APT::AutoRemove::SuggestsImportant=false $extensionDeps \
 && apt-get autoremove
ENV peclDeps="  libmemcached-dev  "
RUN apt-get update \
 && apt-get install --no-install-recommends $peclDeps -y \
 && pecl install apcu \
 && echo extension=apcu.so > $PHP_INI_DIR/conf.d/ext-apcu.ini \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false -o APT::AutoRemove::SuggestsImportant=false $peclDeps \
 && apt-get autoremove
ENV memcachedDeps="  git  libmemcached-dev  "
RUN apt-get update \
 && apt-get install --no-install-recommends $memcachedDeps -y \
 && git clone https://github.com/php-memcached-dev/php-memcached.git \
 && cd php-memcached \
 && git checkout php7 \
 && phpize \
 && ./configure --disable-memcached-sasl \
 && make \
 && make install \
 && echo extension=memcached.so > $PHP_INI_DIR/conf.d/ext-memcached.ini \
 && curl -fSL "https://github.com/websupport-sk/pecl-memcache/archive/fdbd46bbc6f53ed6e024521895e142cbfc9b3340/memcache-3.0.9-fdbd46b.tar.gz" -o "memcache.tar.gz" \
 && mkdir -p /usr/src/php-memcache \
 && tar -xf "memcache.tar.gz" -C /usr/src/php-memcache --strip-components=1 \
 && cd /usr/src/php-memcache \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && echo extension=memcache.so > $PHP_INI_DIR/conf.d/ext-memcache.ini \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false -o APT::AutoRemove::SuggestsImportant=false $memcachedDeps \
 && apt-get autoremove
MAINTAINER 10up
WORKDIR /var/www/html
COPY php-fpm.conf /usr/local/etc/
EXPOSE 9070/tcp
CMD ["php-fpm"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
