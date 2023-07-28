FROM alpine:3.9
LABEL repository.hub="alexmasterov/alpine-php:7.3" \
      repository.url="https://github.com/AlexMasterov/dockerfiles" \
      maintainer="Alex Masterov <alex.masterow@gmail.com>"
ARG PHP_VERSION=7.3.6
ARG PHP_CONFIG=/etc/php
RUN set -x \
 && apk add ssmtp=2.64-r14 tini=0.18.0-r0 --update \
 && addgroup -g 82 -S www-data \
 && adduser -u 82 -S -D -h /var/cache/www-data -s /sbin/nologin -G www-data www-data
RUN set -x \
 && apk add argon2-dev=20171227-r2 autoconf=2.69-r2 binutils=2.31.1-r2 bison=3.0.5-r0 bzip2-dev=1.0.6-r7 curl=7.64.0-r5 curl-dev=7.64.0-r5 file=5.36-r1 freetype-dev=2.9.1-r3 g++=8.3.0-r0 gcc=8.3.0-r0 git=2.20.4-r0 icu-dev=62.1-r1 jpeg-dev=8-r6 libmcrypt-dev=2.5.8-r7 libpng-dev=1.6.37-r0 libsodium-dev=1.0.16-r0 libtool=2.4.6-r5 libwebp-dev=1.0.1-r0 libxml2-dev=2.9.9-r3 libxslt-dev=1.1.33-r3 libzip-dev=1.5.1-r2 make=4.2.1-r2 pcre2-dev=10.32-r1 postgresql-dev=11.11-r0 re2c=1.1.1-r0 readline-dev=7.0.003-r1 sqlite-dev=3.28.0-r3 --virtual .php-build-dependencies \
 && : "---------- Proper iconv ----------" \
 && apk add gnu-libiconv-dev --no-cache --virtual .ext-runtime-dependencies --repository https://nl.alpinelinux.org/alpine/edge/community/ \
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
 && : "---------- Removing build dependencies, clean temporary files ----------" \
 && apk del .php-build-dependencies \
 && rm -rf /var/cache/apk/* /var/tmp/* /tmp/*
ENTRYPOINT ["tini", "--"]
CMD ["php-fpm"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
