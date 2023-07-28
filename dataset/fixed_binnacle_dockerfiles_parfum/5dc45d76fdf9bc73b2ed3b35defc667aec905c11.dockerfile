#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM alpine:3.9
#  dependencies required for running "phpize"
#  these get automatically installed and removed by "docker-php-ext-*" (unless they're already installed)
ENV PHPIZE_DEPS="autoconf  dpkg-dev dpkg  file  g++  gcc  libc-dev  make  pkgconf  re2c"
#  persistent / runtime deps
RUN apk add --no-cache ca-certificates curl tar xz openssl
#  ensure www-data user exists
RUN set -x \
 && addgroup -g 82 -S www-data \
 && adduser -u 82 -D -S -G www-data www-data
#  82 is the standard uid/gid for "www-data" in Alpine
#  https://git.alpinelinux.org/aports/tree/main/apache2/apache2.pre-install?h=3.9-stable
#  https://git.alpinelinux.org/aports/tree/main/lighttpd/lighttpd.pre-install?h=3.9-stable
#  https://git.alpinelinux.org/aports/tree/main/nginx/nginx.pre-install?h=3.9-stable
ENV PHP_INI_DIR="/usr/local/etc/php"
RUN set -eux ; mkdir -p "$PHP_INI_DIR/conf.d" ; [ ! -d /var/www/html ] ; mkdir -p /var/www/html ; chown www-data:www-data /var/www/html ; chmod 777 /var/www/html
# #<autogenerated>##
ENV PHP_EXTRA_CONFIGURE_ARGS="--enable-maintainer-zts --disable-cgi"
# #</autogenerated>##
#  Apply stack smash protection to functions using local buffers and alloca()
#  Make PHP's main executable position-independent (improves ASLR security mechanism, and has no performance impact on x86_64)
#  Enable optimization (-O2)
#  Enable linker optimization (this sorts the hash buckets to improve cache locality, and is non-default)
#  Adds GNU HASH segments to generated executables (this is used if present, and is much faster than sysv hash; in this configuration, sysv hash is also generated)
#  https://github.com/docker-library/php/issues/272
ENV PHP_CFLAGS="-fstack-protector-strong -fpic -fpie -O2"
ENV PHP_CPPFLAGS="$PHP_CFLAGS"
ENV PHP_LDFLAGS="-Wl,-O1 -Wl,--hash-style=both -pie"
ENV GPG_KEYS="A917B1ECDA84AEC2B568FED6F50ABC807BD5DCD0 528995BFEDFBA7191D46839EF9BA0ADA31CBD89E 1729F83938DA44E27BA0F4D3DBDB397470D12172"
ENV PHP_VERSION="7.1.30"
ENV PHP_URL="https://www.php.net/get/php-7.1.30.tar.xz/from/this/mirror" \
    PHP_ASC_URL="https://www.php.net/get/php-7.1.30.tar.xz.asc/from/this/mirror"
ENV PHP_SHA256="6310599811536dbe87e4bcf212bf93196bdfaff519d0c821e4c0068efd096a7c" \
    PHP_MD5=""
RUN set -xe ; apk add --no-cache --virtual .fetch-deps gnupg wget ; mkdir -p /usr/src ; cd /usr/src ; wget -O php.tar.xz "$PHP_URL" ; if [ -n "$PHP_SHA256" ] ; then echo "$PHP_SHA256 *php.tar.xz" | sha256sum -c - ; fi ; if [ -n "$PHP_MD5" ] ; then echo "$PHP_MD5 *php.tar.xz" | md5sum -c - ; fi ; if [ -n "$PHP_ASC_URL" ] ; then wget -O php.tar.xz.asc "$PHP_ASC_URL" ;export GNUPGHOME="$( mktemp -d ;)" ;for key in $GPG_KEYS; do gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ;gpg --batch --verify php.tar.xz.asc php.tar.xz ;command -v gpgconf > /dev/null \
 && gpgconf --kill all ;rm -rf "$GNUPGHOME" ; fi ; apk del --no-network .fetch-deps
COPY docker-php-source /usr/local/bin/
RUN set -xe \
 && apk add --no-cache --virtual .build-deps $PHPIZE_DEPS coreutils curl-dev libedit-dev libxml2-dev openssl-dev sqlite-dev \
 && export CFLAGS="$PHP_CFLAGS" CPPFLAGS="$PHP_CPPFLAGS" LDFLAGS="$PHP_LDFLAGS" \
 && docker-php-source extract \
 && cd /usr/src/php \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --with-config-file-path="$PHP_INI_DIR" --with-config-file-scan-dir="$PHP_INI_DIR/conf.d" --enable-option-checking=fatal --with-mhash --enable-ftp --enable-mbstring --enable-mysqlnd --with-curl --with-libedit --with-openssl --with-zlib $( test "$gnuArch" = 's390x-linux-gnu' \
 && echo '--without-pcre-jit' ;) $PHP_EXTRA_CONFIGURE_ARGS \
 && make -j "$( nproc ;)" \
 && find -type f -name '*.a' -delete \
 && make install \
 && { find /usr/local/bin /usr/local/sbin -type f -perm +0111 -exec strip --strip-all '{}' + || true ; } \
 && make clean \
 && cp -v php.ini-* "$PHP_INI_DIR/" \
 && cd / \
 && docker-php-source delete \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add --no-cache $runDeps \
 && apk del --no-network .build-deps \
 && pecl update-channels \
 && rm -rf /tmp/pear ~/.pearrc \
 && php --version
COPY docker-php-ext-* docker-php-entrypoint /usr/local/bin/
ENTRYPOINT ["docker-php-entrypoint"]
# #<autogenerated>##
CMD ["php", "-a"]
# #</autogenerated>##
