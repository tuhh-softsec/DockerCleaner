#
#   NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh"
#
#   PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM debian:stretch-slim
#   prevent Debian's PHP packages from being installed
#   https://github.com/docker-library/php/pull/542
RUN set -eux ; { echo 'Package: php*' ;echo 'Pin: release *' ;echo 'Pin-Priority: -1' ; } > /etc/apt/preferences.d/no-debian-php
#   dependencies required for running "phpize"
#   (see persistent deps below)
ENV PHPIZE_DEPS="autoconf  dpkg-dev  file  g++  gcc  libc-dev  make  pkg-config  re2c"
#   persistent / runtime deps
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates curl xz-utils $PHPIZE_DEPS -y \
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
#  ENV GPG_KEYS 1729F83938DA44E27BA0F4D3DBDB397470D12172 B1B44D8F021E4E2D6021E995DC9FF8D3EE5AF27F
#  ENV PHP_VERSION 7.2.5
#  ENV PHP_URL="https://secure.php.net/get/php-7.2.5.tar.xz/from/this/mirror" PHP_ASC_URL="https://secure.php.net/get/php-7.2.5.tar.xz.asc/from/this/mirror"
#  ENV PHP_SHA256="af70a33b3f7a51510467199b39af151333fbbe4cc21923bad9c7cf64268cddb2" PHP_MD5=""
RUN set -xe ; fetchDeps=' git ' ; if ! command -v gpg > /dev/null; then fetchDeps="$fetchDeps dirmngr gnupg " ; fi ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; rm -rf /var/lib/apt/lists/* ; mkdir -p /usr/src ; cd /usr/src ; git clone http://git.php.net/repository/php-src.git php ; cd php ; ./buildconf --force ; rm -rf .git ; cd /usr/src ; tar -cJf php.tar.xz php ; rm -rf php ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false $fetchDeps
COPY data/docker-php-source /usr/local/bin/
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends bison libcurl4-openssl-dev libedit-dev libffi-dev libonig-dev libsodium-dev libsqlite3-dev libssl-dev libxml2-dev zlib1g-dev ${PHP_EXTRA_BUILD_DEPS:-} -y ; rm -rf /var/lib/apt/lists/* ; echo "deb http://deb.debian.org/debian buster main" > /etc/apt/sources.list.d/debian-buster.list; apt-get update ; apt-get install --no-install-recommends libargon2-dev -y ; rm -rf /var/lib/apt/lists/* ; rm /etc/apt/sources.list.d/debian-buster.list ; apt-get update ; rm -rf /var/lib/apt/lists/* ; export CFLAGS="$PHP_CFLAGS" CPPFLAGS="$PHP_CPPFLAGS" LDFLAGS="$PHP_LDFLAGS" ; docker-php-source extract ; cd /usr/src/php ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; debMultiarch="$( dpkg-architecture --query DEB_BUILD_MULTIARCH ;)" ; if [ ! -d /usr/include/curl ] ; then ln -sT "/usr/include/$debMultiarch/curl" /usr/local/include/curl ; fi ; ./configure --build="$gnuArch" --with-config-file-path="$PHP_INI_DIR" --with-config-file-scan-dir="$PHP_INI_DIR/conf.d" --without-pear --enable-option-checking=fatal --disable-cgi --with-mhash --enable-ftp --enable-mbstring --enable-mysqlnd --with-password-argon2 --with-sodium --with-ffi --with-curl --with-libedit --with-openssl --with-zlib $( test "$gnuArch" = 's390x-linux-gnu' \
 && echo '--without-pcre-jit' ;) --with-libdir="lib/$debMultiarch" ${PHP_EXTRA_CONFIGURE_ARGS:-} ; make -j "$( nproc ;)" ; make install ; find /usr/local/bin /usr/local/sbin -type f -executable -exec strip --strip-all '{}' + || true ; make clean ; cd / ; docker-php-source delete ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; find /usr/local -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false bison libffi-dev ; php --version
#  	\
#   https://github.com/docker-library/php/issues/443
#  	pecl update-channels; \
#  	rm -rf /tmp/pear ~/.pearrc
COPY data/docker-php-ext-* data/docker-php-entrypoint /usr/local/bin/
#   https://github.com/docker-library/php/blob/master/7.3/stretch/fpm/Dockerfile#L221
#   sodium was built as a shared module (so that it can be replaced later if so desired), so let's enable it too (https://github.com/docker-library/php/issues/598)
#   TODO: --with-sodium=shared it results in: undefined symbol: sodium_init
#  RUN docker-php-ext-enable sodium
ENTRYPOINT ["docker-php-entrypoint"]
#  #<autogenerated>##
WORKDIR /var/www/html
RUN set -ex \
 && cd /usr/local/etc \
 && if [ -d php-fpm.d ] ; then sed 's!=NONE/!=!g' php-fpm.conf.default | tee php-fpm.conf > /dev/null;cp php-fpm.d/www.conf.default php-fpm.d/www.conf ; else mkdir php-fpm.d ;cp php-fpm.conf.default php-fpm.d/www.conf ;{ echo '[global]' ;echo 'include=etc/php-fpm.d/*.conf' ; } | tee php-fpm.conf ; fi \
 && { echo '[global]' ;echo 'error_log = /proc/self/fd/2' ;echo ;echo '[www]' ;echo '; if we send this to /proc/self/fd/1, it never appears' ;echo 'access.log = /proc/self/fd/2' ;echo ;echo 'clear_env = no' ;echo ;echo '; Ensure worker stdout and stderr are sent to the main error log.' ;echo 'catch_workers_output = yes' ; } | tee php-fpm.d/docker.conf \
 && { echo '[global]' ;echo 'daemonize = no' ;echo ;echo '[www]' ;echo 'listen = 9000' ; } | tee php-fpm.d/zz-docker.conf
#  ##
#  ## Verify
#  ##
RUN set -x \
 && php -v | grep -oE 'PHP\s[.0-9]+' | grep -oE '[.0-9]+' | grep '^8.0' \
 && /usr/local/sbin/php-fpm --test \
 && PHP_ERROR="$( php -v 2>&1 1> /dev/null;)" \
 && if [ -n "${PHP_ERROR}" ] ; then echo "${PHP_ERROR}" ;false ; fi
EXPOSE 9000/tcp
CMD ["php-fpm"]
#  #</autogenerated>##
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
