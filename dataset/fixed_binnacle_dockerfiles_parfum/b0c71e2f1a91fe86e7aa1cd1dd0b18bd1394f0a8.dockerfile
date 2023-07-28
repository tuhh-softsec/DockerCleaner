#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM armhf/debian:jessie
#  persistent / runtime deps
ENV PHPIZE_DEPS="autoconf  file  g++  gcc  libc-dev  make  pkg-config  re2c"
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates curl libedit2 libsqlite3-0 libxml2 xz-utils $PHPIZE_DEPS -y \
 && rm -r /var/lib/apt/lists/*
ENV PHP_INI_DIR="/usr/local/etc/php"
RUN mkdir -p $PHP_INI_DIR/conf.d
# #<autogenerated>##
ENV PHP_EXTRA_CONFIGURE_ARGS="--enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data"
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
ENV GPG_KEYS="A917B1ECDA84AEC2B568FED6F50ABC807BD5DCD0 528995BFEDFBA7191D46839EF9BA0ADA31CBD89E"
ENV PHP_VERSION="7.1.3"
ENV PHP_URL="https://secure.php.net/get/php-7.1.3.tar.xz/from/this/mirror" \
    PHP_ASC_URL="https://secure.php.net/get/php-7.1.3.tar.xz.asc/from/this/mirror"
ENV PHP_SHA256="e4887c2634778e37fd962fbdf5c4a7d32cd708482fe07b448804625570cb0bb0" \
    PHP_MD5="d604d688be17f4a05b99dbb7fb9581f4"
RUN set -xe ; fetchDeps=' wget ' ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; rm -rf /var/lib/apt/lists/* ; mkdir -p /usr/src ; cd /usr/src ; wget -O php.tar.xz "$PHP_URL" ; if [ -n "$PHP_SHA256" ] ; then echo "$PHP_SHA256 *php.tar.xz" | sha256sum -c - ; fi ; if [ -n "$PHP_MD5" ] ; then echo "$PHP_MD5 *php.tar.xz" | md5sum -c - ; fi ; if [ -n "$PHP_ASC_URL" ] ; then wget -O php.tar.xz.asc "$PHP_ASC_URL" ;export GNUPGHOME="$( mktemp -d ;)" ;for key in $GPG_KEYS; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ;gpg --batch --verify php.tar.xz.asc php.tar.xz ;rm -r "$GNUPGHOME" ; fi ; apt-get purge -y --auto-remove $fetchDeps
COPY docker-php-source /usr/local/bin/
RUN set -xe \
 && buildDeps=" $PHP_EXTRA_BUILD_DEPS libcurl4-openssl-dev libedit-dev libsqlite3-dev libssl-dev libxml2-dev " \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && export CFLAGS="$PHP_CFLAGS" CPPFLAGS="$PHP_CPPFLAGS" LDFLAGS="$PHP_LDFLAGS" \
 && docker-php-source extract \
 && cd /usr/src/php \
 && ./configure --with-config-file-path="$PHP_INI_DIR" --with-config-file-scan-dir="$PHP_INI_DIR/conf.d" --disable-cgi --enable-ftp --enable-mbstring --enable-mysqlnd --with-curl --with-libedit --with-openssl --with-zlib $PHP_EXTRA_CONFIGURE_ARGS \
 && make -j "$( nproc ;)" \
 && make install \
 && { find /usr/local/bin /usr/local/sbin -type f -executable -exec strip --strip-all '{}' + || true ; } \
 && make clean \
 && docker-php-source delete \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false $buildDeps
COPY docker-php-ext-* docker-php-entrypoint /usr/local/bin/
ENTRYPOINT ["docker-php-entrypoint"]
# #<autogenerated>##
WORKDIR /var/www/html
RUN set -ex \
 && cd /usr/local/etc \
 && if [ -d php-fpm.d ] ; then sed 's!=NONE/!=!g' php-fpm.conf.default | tee php-fpm.conf > /dev/null;cp php-fpm.d/www.conf.default php-fpm.d/www.conf ; else mkdir php-fpm.d ;cp php-fpm.conf.default php-fpm.d/www.conf ;{ echo '[global]' ;echo 'include=etc/php-fpm.d/*.conf' ; } | tee php-fpm.conf ; fi \
 && { echo '[global]' ;echo 'error_log = /proc/self/fd/2' ;echo ;echo '[www]' ;echo '; if we send this to /proc/self/fd/1, it never appears' ;echo 'access.log = /proc/self/fd/2' ;echo ;echo 'clear_env = no' ;echo ;echo '; Ensure worker stdout and stderr are sent to the main error log.' ;echo 'catch_workers_output = yes' ; } | tee php-fpm.d/docker.conf \
 && { echo '[global]' ;echo 'daemonize = no' ;echo ;echo '[www]' ;echo 'listen = [::]:9000' ; } | tee php-fpm.d/zz-docker.conf
EXPOSE 9000/tcp
CMD ["php-fpm"]
# #</autogenerated>##
