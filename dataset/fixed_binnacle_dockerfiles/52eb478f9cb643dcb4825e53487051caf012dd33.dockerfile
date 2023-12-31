FROM alexmasterov/alpine-libv8:6.7 AS libv8
FROM alpine:3.9
LABEL repository.hub="alexmasterov/alpine-php:7.2" \
      repository.url="https://github.com/AlexMasterov/dockerfiles" \
      maintainer="Alex Masterov <alex.masterow@gmail.com>"
ARG PHP_VERSION=7.2.19
ARG PHP_CONFIG=/etc/php
ARG REALPATH_TURBO_TAG=v2.0.0
ARG XDEBUG_TAG=2.6.1
ARG MSGPACK_TAG=msgpack-2.0.2
ARG REDIS_TAG=4.2.0
ARG TARANTOOL_VERSION=0.3.2
ARG MONGODB_VERSION=1.5.3
ARG TARANTOOL_TAG=0.3.2
ARG AMQP_TAG=v1.9.4
ARG PHPV8_VERSION=0.2.2
COPY --from=libv8 /usr/local/v8 /usr/local/v8
RUN set -x \
 && apk add ssmtp tini --update \
 && addgroup -g 82 -S www-data \
 && adduser -u 82 -S -D -h /var/cache/www-data -s /sbin/nologin -G www-data www-data
RUN set -x \
 && apk add argon2-dev autoconf binutils bison bzip2-dev curl curl-dev file freetype-dev g++ gcc git icu-dev jpeg-dev libmcrypt-dev libpng-dev libsodium-dev libtool libwebp-dev libxml2-dev libxslt-dev libzip-dev make pcre2-dev postgresql-dev re2c readline-dev sqlite-dev --virtual .php-build-dependencies \
 && : "---------- Proper iconv ----------" \
 && apk add gnu-libiconv-dev --no-cache --virtual .ext-runtime-dependencies --repository https://nl.alpinelinux.org/alpine/edge/testing/ \
 && : "---------- Replace binary and headers ----------" \
 && (mv /usr/bin/gnu-iconv /usr/bin/iconv ;mv /usr/include/gnu-libiconv/*.h /usr/include ;rm -rf /usr/include/gnu-libiconv ) \
 && : "---------- Proper libpcre2 ----------" \
 && (cd /usr/lib ;ln -sf libpcre2-posix.a libpcre2.a ;ln -sf libpcre2-posix.so libpcre2.so ) \
 && : "---------- Build flags ----------" \
 && export LDFLAGS="-Wl,-O2 -Wl,--hash-style=both -pie" \
 && export CFLAGS="-O2 -march=native -fstack-protector-strong -fpic -fpie" \
 && export CPPFLAGS=${CFLAGS} \
 && export MAKEFLAGS="-j $( expr $( getconf _NPROCESSORS_ONLN ;) + 1 ;)" \
 && : "---------- PHP ----------" \
 && PHP_SOURCE="https://secure.php.net/get/php-${PHP_VERSION}.tar.xz/from/this/mirror" \
 && curl -fSL --connect-timeout 30 ${PHP_SOURCE} | tar xJ -C /tmp \
 && cd /tmp/php-${PHP_VERSION} \
 && : "---------- Build ----------" \
 && ./configure --prefix=/usr --sysconfdir=${PHP_CONFIG} --with-config-file-path=${PHP_CONFIG} --with-config-file-scan-dir=${PHP_CONFIG}/conf.d --without-pear --disable-cgi --disable-debug --disable-ipv6 --disable-phpdbg --disable-rpath --disable-static --enable-bcmath --enable-calendar --enable-dom --enable-exif --enable-fd-setsize=$( ulimit -n ;) --enable-fpm --with-fpm-group=www-data --with-fpm-user=www-data --enable-ftp --enable-inline-optimization --enable-intl --enable-json --enable-libxml --with-libxml-dir=/usr --enable-mbregex --enable-mbstring --enable-opcache --enable-huge-code-pages --enable-opcache-file --enable-option-checking=fatal --enable-pcntl --enable-phar --enable-session --enable-shmop --enable-soap --enable-sockets --enable-xml --enable-xmlreader --enable-xmlwriter --with-bz2=/usr --with-curl=/usr --with-sodium=/usr --with-gd --with-freetype-dir=/usr --with-jpeg-dir=/usr --with-png-dir=/usr --with-webp-dir=/usr --with-xpm-dir=no --with-zlib-dir=/usr --with-iconv=/usr --with-libzip=/usr --with-mhash --with-openssl=/usr --with-system-ciphers --with-password-argon2 --with-pcre-regex --with-pcre-jit --with-pdo-mysql=mysqlnd --with-pdo-pgsql --with-pdo-sqlite --with-pgsql --with-readline=/usr --with-xmlrpc --with-xsl=/usr \
 && make \
 && make install \
 && runtimeDeps="$( scanelf --needed --nobanner --recursive /usr/sbin/php-fpm | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add ${runtimeDeps} --virtual .php-runtime-dependencies \
 && : "---------- https://bugs.php.net/bug.php?id=52312 ----------" \
 && git clone -o ${REALPATH_TURBO_TAG} --depth 1 https://github.com/Whissi/realpath_turbo.git /tmp/realpath_turbo \
 && cd /tmp/realpath_turbo \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && : "---------- xDebug ----------" \
 && git clone -o ${XDEBUG_TAG} --depth 1 https://github.com/xdebug/xdebug.git /tmp/xdebug \
 && cd /tmp/xdebug \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && : "---------- Msgpack ----------" \
 && git clone -o ${MSGPACK_TAG} --depth 1 https://github.com/msgpack/msgpack-php.git /tmp/msgpack-php \
 && cd /tmp/msgpack-php \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && : "---------- Redis ----------" \
 && git clone -o ${REDIS_TAG} --depth 1 https://github.com/phpredis/phpredis.git /tmp/redis \
 && cd /tmp/redis \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && : "---------- Tarantool ----------" \
 && apk add libltdl --virtual .tarantool-runtime-dependencies \
 && TARANTOOL_FILENAME="tarantool-php-${TARANTOOL_VERSION}" \
 && TARANTOOL_SOURCE="https://github.com/tarantool/tarantool-php/archive/${TARANTOOL_VERSION}.tar.gz" \
 && curl -fSL --connect-timeout 30 ${TARANTOOL_SOURCE} | tar xz -C /tmp \
 && cd /tmp/${TARANTOOL_FILENAME} \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && : "---------- MongoDB ----------" \
 && apk add cmake pkgconfig --virtual .mongodb-build-dependencies \
 && MONGODB_FILENAME="mongodb-${MONGODB_VERSION}" \
 && MONGODB_SOURCE="https://github.com/mongodb/mongo-php-driver/releases/download/${MONGODB_VERSION}/${MONGODB_FILENAME}.tgz" \
 && curl -fSL --connect-timeout 30 ${MONGODB_SOURCE} | tar xz -C /tmp \
 && cd /tmp/${MONGODB_FILENAME} \
 && phpize \
 && ./configure --with-mongodb-ssl=openssl \
 && make \
 && make install \
 && apk del .mongodb-build-dependencies \
 && : "---------- php-v8 ----------" \
 && PHPV8_FILENAME="php-v8-${PHPV8_VERSION}" \
 && PHPV8_SOURCE="https://github.com/pinepain/php-v8/archive/v${PHPV8_VERSION}.tar.gz" \
 && curl -fSL --connect-timeout 30 ${PHPV8_SOURCE} | tar xz -C /tmp \
 && cd /tmp/${PHPV8_FILENAME} \
 && phpize \
 && ./configure --with-v8=/usr/local/v8 \
 && make \
 && make install \
 && : "---------- Removing build dependencies, clean temporary files ----------" \
 && apk del .php-build-dependencies \
 && rm -rf /var/cache/apk/* /var/tmp/* /tmp/*
ENTRYPOINT ["tini", "--"]
CMD ["php-fpm"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
