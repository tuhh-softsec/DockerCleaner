#
#   NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh" AND UPDATED VIA openssl-gost
#
#   PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM rnix/openssl-gost AS openssl-gost
FROM debian:stretch-slim
COPY --from=openssl-gost /usr/local/ssl /usr/local/ssl
COPY --from=openssl-gost /usr/local/ssl/bin/openssl /usr/bin/openssl
COPY --from=openssl-gost /usr/local/curl /usr/local/curl
COPY --from=openssl-gost /usr/local/curl/bin/curl /usr/bin/curl
COPY --from=openssl-gost /usr/local/bin/gostsum /usr/local/bin/gostsum
COPY --from=openssl-gost /usr/local/bin/gost12sum /usr/local/bin/gost12sum
#   pkgconfig is used to compile php
COPY --from=openssl-gost /usr/local/ssl/lib/pkgconfig/* /usr/lib/x86_64-linux-gnu/pkgconfig/
COPY --from=openssl-gost /usr/local/curl/lib/pkgconfig/* /usr/lib/x86_64-linux-gnu/pkgconfig/
#   prevent Debian's PHP packages from being installed
#   https://github.com/docker-library/php/pull/542
RUN set -eux ; { echo 'Package: php*' ;echo 'Pin: release *' ;echo 'Pin-Priority: -1' ; } > /etc/apt/preferences.d/no-debian-php
#   dependencies required for running "phpize"
#   (see persistent deps below)
ENV PHPIZE_DEPS="autoconf  dpkg-dev  file  g++  gcc  libc-dev  make  pkg-config  re2c"
#   persistent / runtime deps
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20230311 curl=7.88.1-7ubuntu1 xz-utils=5.4.1-0.2 $PHPIZE_DEPS -y \
 && rm -r /var/lib/apt/lists/*
ENV PHP_INI_DIR="/usr/local/etc/php"
RUN mkdir -p $PHP_INI_DIR/conf.d
#  #<autogenerated>##
ENV PHP_EXTRA_CONFIGURE_ARGS="--enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data"
#  #</autogenerated>##
#   Apply stack smash protection to functions using local buffers and alloca()
#   Make PHP's main executable position-independent (improves ASLR security mechanism, and has no performance impact on x86_64)
#   Enable optimization (-O2)
#   Enable linker optimization (this sorts the hash buckets to improve cache locality, and is non-default)
#   Adds GNU HASH segments to generated executables (this is used if present, and is much faster than sysv hash; in this configuration, sysv hash is also generated)
#   https://github.com/docker-library/php/issues/272
ENV PHP_CFLAGS="-fstack-protector-strong -fpic -fpie -O2"
ENV PHP_CPPFLAGS="$PHP_CFLAGS"
ENV PHP_LDFLAGS="-Wl,-O1 -Wl,--hash-style=both -pie"
ENV GPG_KEYS="1729F83938DA44E27BA0F4D3DBDB397470D12172 B1B44D8F021E4E2D6021E995DC9FF8D3EE5AF27F"
ENV PHP_VERSION="7.2.4"
ENV PHP_URL="https://secure.php.net/get/php-7.2.4.tar.xz/from/this/mirror" \
    PHP_ASC_URL="https://secure.php.net/get/php-7.2.4.tar.xz.asc/from/this/mirror"
ENV PHP_SHA256="7916b1bd148ddfd46d7f8f9a517d4b09cd8a8ad9248734e7c8dd91ef17057a88" \
    PHP_MD5=""
RUN set -xe ; fetchDeps=' wget ' ; if ! command -v gpg > /dev/null; then fetchDeps="$fetchDeps dirmngr gnupg " ; fi ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; rm -rf /var/lib/apt/lists/* ; mkdir -p /usr/src ; cd /usr/src ; wget -O php.tar.xz "$PHP_URL" ; if [ -n "$PHP_SHA256" ] ; then echo "$PHP_SHA256 *php.tar.xz" | sha256sum -c - ; fi ; if [ -n "$PHP_MD5" ] ; then echo "$PHP_MD5 *php.tar.xz" | md5sum -c - ; fi ; if [ -n "$PHP_ASC_URL" ] ; then wget -O php.tar.xz.asc "$PHP_ASC_URL" ;export GNUPGHOME="$( mktemp -d ;)" ;for key in $GPG_KEYS; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ;gpg --batch --verify php.tar.xz.asc php.tar.xz ;rm -rf "$GNUPGHOME" ; fi ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false $fetchDeps
COPY docker-php-source /usr/local/bin/
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends libargon2-0-dev libedit-dev=3.1-20221030-2 libsodium-dev=1.0.18-1build2 libsqlite3-dev=3.40.1-1 libxml2-dev=2.9.14+dfsg-1.1build2 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 ${PHP_EXTRA_BUILD_DEPS:-} -y ; rm -rf /var/lib/apt/lists/* ; export CFLAGS="$PHP_CFLAGS" CPPFLAGS="$PHP_CPPFLAGS" LDFLAGS="$PHP_LDFLAGS" ; docker-php-source extract ; cd /usr/src/php ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; debMultiarch="$( dpkg-architecture --query DEB_BUILD_MULTIARCH ;)" ; if [ ! -d /usr/include/curl ] ; then ln -sT "/usr/include/$debMultiarch/curl" /usr/local/include/curl ; fi ; ./configure --build="$gnuArch" --with-config-file-path="$PHP_INI_DIR" --with-config-file-scan-dir="$PHP_INI_DIR/conf.d" --disable-cgi --enable-ftp --enable-mbstring --enable-mysqlnd --with-password-argon2 --with-sodium=shared --with-curl=/usr/local/curl --with-libedit --with-openssl --with-openssl-dir=/usr/local/ssl --with-zlib $( test "$gnuArch" = 's390x-linux-gnu' \
 && echo '--without-pcre-jit' ;) --with-libdir="lib/$debMultiarch" ${PHP_EXTRA_CONFIGURE_ARGS:-} ; make -j "$( nproc ;)" ; make install ; find /usr/local/bin /usr/local/sbin -type f -executable -exec strip --strip-all '{}' + || true ; make clean ; cd / ; docker-php-source delete ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; find /usr/local -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; php --version ; pecl update-channels ; rm -rf /tmp/pear ~/.pearrc
COPY docker-php-ext-* docker-php-entrypoint /usr/local/bin/
#   sodium was built as a shared module (so that it can be replaced later if so desired), so let's enable it too (https://github.com/docker-library/php/issues/598)
RUN docker-php-ext-enable sodium
ENTRYPOINT ["docker-php-entrypoint"]
#  #<autogenerated>##
WORKDIR /var/www/html
RUN set -ex \
 && cd /usr/local/etc \
 && if [ -d php-fpm.d ] ; then sed 's!=NONE/!=!g' php-fpm.conf.default | tee php-fpm.conf > /dev/null;cp php-fpm.d/www.conf.default php-fpm.d/www.conf ; else mkdir php-fpm.d ;cp php-fpm.conf.default php-fpm.d/www.conf ;{ echo '[global]' ;echo 'include=etc/php-fpm.d/*.conf' ; } | tee php-fpm.conf ; fi \
 && { echo '[global]' ;echo 'error_log = /proc/self/fd/2' ;echo ;echo '[www]' ;echo '; if we send this to /proc/self/fd/1, it never appears' ;echo 'access.log = /proc/self/fd/2' ;echo ;echo 'clear_env = no' ;echo ;echo '; Ensure worker stdout and stderr are sent to the main error log.' ;echo 'catch_workers_output = yes' ; } | tee php-fpm.d/docker.conf \
 && { echo '[global]' ;echo 'daemonize = no' ;echo ;echo '[www]' ;echo 'listen = 9000' ; } | tee php-fpm.d/zz-docker.conf
EXPOSE 9000/tcp
CMD ["php-fpm"]
#  #</autogenerated>##
COPY --from=openssl-gost /usr/local/ssl/bin/openssl /usr/bin/openssl
COPY --from=openssl-gost /usr/local/curl/bin/curl /usr/bin/curl
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
