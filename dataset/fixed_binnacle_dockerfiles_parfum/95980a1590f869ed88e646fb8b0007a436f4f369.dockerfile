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
RUN apt-get update \
 && apt-get install --no-install-recommends apache2-bin apache2.2-common -y \
 && rm -rf /var/lib/apt/lists/*
ENV APACHE_CONFDIR="/etc/apache2"
ENV APACHE_ENVVARS="$APACHE_CONFDIR/envvars"
RUN set -ex \
 && sed -ri 's/^export ([^=]+)=(.*)$/: ${\1:=\2}\nexport \1/' "$APACHE_ENVVARS" \
 && . "$APACHE_ENVVARS" \
 && for dir in "$APACHE_LOCK_DIR" "$APACHE_RUN_DIR" "$APACHE_LOG_DIR" /var/www/html; do rm -rvf "$dir" \
 && mkdir -p "$dir" \
 && chown -R "$APACHE_RUN_USER:$APACHE_RUN_GROUP" "$dir" ; done
#  Apache + PHP requires preforking Apache for best results
RUN a2dismod mpm_event \
 && a2enmod mpm_prefork
#  logs should go to stdout / stderr
RUN set -ex \
 && . "$APACHE_ENVVARS" \
 && ln -sfT /dev/stderr "$APACHE_LOG_DIR/error.log" \
 && ln -sfT /dev/stdout "$APACHE_LOG_DIR/access.log" \
 && ln -sfT /dev/stdout "$APACHE_LOG_DIR/other_vhosts_access.log"
#  PHP files should be handled by PHP, and should be preferred over any other file type
RUN { echo '<FilesMatch \.php$>' ;echo '\tSetHandler application/x-httpd-php' ;echo '</FilesMatch>' ;echo ;echo 'DirectoryIndex disabled' ;echo 'DirectoryIndex index.php index.html' ;echo ;echo '<Directory /var/www/>' ;echo '\tOptions -Indexes' ;echo '\tAllowOverride All' ;echo '</Directory>' ; } | tee "$APACHE_CONFDIR/conf-available/docker-php.conf" \
 && a2enconf docker-php
ENV PHP_EXTRA_BUILD_DEPS="apache2-dev"
ENV PHP_EXTRA_CONFIGURE_ARGS="--with-apxs2"
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
ENV GPG_KEYS="0BD78B5F97500D450838F95DFE857D9A90D90EC1 6E4F6AB321FDC07F2C332E3AC2BF0BC433CFC8B3"
ENV PHP_VERSION="5.6.30"
ENV PHP_URL="https://secure.php.net/get/php-5.6.30.tar.xz/from/this/mirror" \
    PHP_ASC_URL="https://secure.php.net/get/php-5.6.30.tar.xz.asc/from/this/mirror"
ENV PHP_SHA256="a363185c786432f75e3c7ff956b49c3369c3f6906a6b10459f8d1ddc22f70805" \
    PHP_MD5="68753955a8964ae49064c6424f81eb3e"
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
COPY apache2-foreground /usr/local/bin/
WORKDIR /var/www/html
EXPOSE 80/tcp
CMD ["apache2-foreground"]
# #</autogenerated>##
