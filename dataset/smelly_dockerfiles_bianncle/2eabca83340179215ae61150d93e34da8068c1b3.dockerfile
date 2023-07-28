#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM debian:stretch-slim
#  prevent Debian's PHP packages from being installed
#  https://github.com/docker-library/php/pull/542
RUN set -eux ; { echo 'Package: php*' ;echo 'Pin: release *' ;echo 'Pin-Priority: -1' ; } > /etc/apt/preferences.d/no-debian-php
#  dependencies required for running "phpize"
#  (see persistent deps below)
ENV PHPIZE_DEPS="autoconf  dpkg-dev  file  g++  gcc  libc-dev  make  pkg-config  re2c"
#  persistent / runtime deps
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates curl xz-utils $PHPIZE_DEPS -y \
 && rm -r /var/lib/apt/lists/*
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
ENV GPG_KEYS="1729F83938DA44E27BA0F4D3DBDB397470D12172 B1B44D8F021E4E2D6021E995DC9FF8D3EE5AF27F"
ENV PHP_VERSION="7.2.19"
ENV PHP_URL="https://www.php.net/get/php-7.2.19.tar.xz/from/this/mirror" \
    PHP_ASC_URL="https://www.php.net/get/php-7.2.19.tar.xz.asc/from/this/mirror"
ENV PHP_SHA256="4ffa2404a88d60e993a9fe69f829ebec3eb1e006de41b6048ce5e91bbeaa9282" \
    PHP_MD5=""
RUN set -xe ; fetchDeps=' wget ' ; if ! command -v gpg > /dev/null; then fetchDeps="$fetchDeps dirmngr gnupg " ; fi ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; rm -rf /var/lib/apt/lists/* ; mkdir -p /usr/src ; cd /usr/src ; wget -O php.tar.xz "$PHP_URL" ; if [ -n "$PHP_SHA256" ] ; then echo "$PHP_SHA256 *php.tar.xz" | sha256sum -c - ; fi ; if [ -n "$PHP_MD5" ] ; then echo "$PHP_MD5 *php.tar.xz" | md5sum -c - ; fi ; if [ -n "$PHP_ASC_URL" ] ; then wget -O php.tar.xz.asc "$PHP_ASC_URL" ;export GNUPGHOME="$( mktemp -d ;)" ;for key in $GPG_KEYS; do gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ;gpg --batch --verify php.tar.xz.asc php.tar.xz ;command -v gpgconf > /dev/null \
 && gpgconf --kill all ;rm -rf "$GNUPGHOME" ; fi ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false $fetchDeps
COPY docker-php-source /usr/local/bin/
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends libcurl4-openssl-dev libedit-dev libsodium-dev libsqlite3-dev libssl-dev libxml2-dev zlib1g-dev ${PHP_EXTRA_BUILD_DEPS:-} -y ; sed -e 's/stretch/buster/g' /etc/apt/sources.list > /etc/apt/sources.list.d/buster.list; { echo 'Package: *' ;echo 'Pin: release n=buster' ;echo 'Pin-Priority: -10' ;echo ;echo 'Package: libargon2*' ;echo 'Pin: release n=buster' ;echo 'Pin-Priority: 990' ; } > /etc/apt/preferences.d/argon2-buster; apt-get update ; apt-get install --no-install-recommends libargon2-dev -y ; rm -rf /var/lib/apt/lists/* ; export CFLAGS="$PHP_CFLAGS" CPPFLAGS="$PHP_CPPFLAGS" LDFLAGS="$PHP_LDFLAGS" ; docker-php-source extract ; cd /usr/src/php ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; debMultiarch="$( dpkg-architecture --query DEB_BUILD_MULTIARCH ;)" ; if [ ! -d /usr/include/curl ] ; then ln -sT "/usr/include/$debMultiarch/curl" /usr/local/include/curl ; fi ; ./configure --build="$gnuArch" --with-config-file-path="$PHP_INI_DIR" --with-config-file-scan-dir="$PHP_INI_DIR/conf.d" --enable-option-checking=fatal --with-mhash --enable-ftp --enable-mbstring --enable-mysqlnd --with-password-argon2 --with-sodium=shared --with-curl --with-libedit --with-openssl --with-zlib $( test "$gnuArch" = 's390x-linux-gnu' \
 && echo '--without-pcre-jit' ;) --with-libdir="lib/$debMultiarch" ${PHP_EXTRA_CONFIGURE_ARGS:-} ; make -j "$( nproc ;)" ; find -type f -name '*.a' -delete ; make install ; find /usr/local/bin /usr/local/sbin -type f -executable -exec strip --strip-all '{}' + || true ; make clean ; cp -v php.ini-* "$PHP_INI_DIR/" ; cd / ; docker-php-source delete ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; find /usr/local -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; pecl update-channels ; rm -rf /tmp/pear ~/.pearrc ; php --version
COPY docker-php-ext-* docker-php-entrypoint /usr/local/bin/
#  sodium was built as a shared module (so that it can be replaced later if so desired), so let's enable it too (https://github.com/docker-library/php/issues/598)
RUN docker-php-ext-enable sodium
ENTRYPOINT ["docker-php-entrypoint"]
# #<autogenerated>##
CMD ["php", "-a"]
# #</autogenerated>##
