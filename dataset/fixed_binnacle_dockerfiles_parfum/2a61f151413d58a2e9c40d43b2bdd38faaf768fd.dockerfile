FROM gzevd/alpine:3.4
#  persistent / runtime deps
ENV PHPIZE_DEPS="autoconf  file  g++  gcc  libc-dev  make  pkgconf  re2c"
RUN apk add --no-cache --virtual .persistent-deps ca-certificates curl apache2 apache2-utils xz
RUN set -xe ; mkdir -p /run/apache2/ \
 && sed -i 's/^#ServerName.*/ServerName localhost/' /etc/apache2/httpd.conf \
 && sed -i 's/^#LoadModule rewrite_module/LoadModule rewrite_module/' /etc/apache2/httpd.conf \
 && sed -i 's/^Listen 80/Listen 0.0.0.0:80/' /etc/apache2/httpd.conf \
 && ln -sf /dev/stdout /var/log/apache2/access.log \
 && ln -sf /dev/stderr /var/log/apache2/error.log
ENV PHP_INI_DIR="/usr/local/etc/php"
RUN mkdir -p $PHP_INI_DIR/conf.d
#  Apply stack smash protection to functions using local buffers and alloca()
#  Make PHP's main executable position-independent (improves ASLR security mechanism, and has no performance impact on x86_64)
#  Enable optimization (-O2)
#  Enable linker optimization (this sorts the hash buckets to improve cache locality, and is non-default)
#  Adds GNU HASH segments to generated executables (this is used if present, and is much faster than sysv hash; in this configuration, sysv hash is also generated)
#  https://github.com/docker-library/php/issues/272
ENV PHP_CFLAGS="-fstack-protector-strong -fpic -fpie -O2"
ENV PHP_CPPFLAGS="$PHP_CFLAGS"
ENV PHP_LDFLAGS="-Wl,-O1 -Wl,--hash-style=both -pie"
ENV PHP_VERSION="5.4.45"
ENV PHP_URL="https://secure.php.net/get/php-5.4.45.tar.gz/from/this/mirror" \
    PHP_ASC_URL="https://secure.php.net/get/php-5.4.45.tar.gz.asc/from/this/mirror"
ENV PHP_SHA256="25bc4723955f4e352935258002af14a14a9810b491a19400d76fcdfa9d04b28f" \
    PHP_MD5="ba580e774ed1ab256f22d1fa69a59311"
COPY 5DA04B5D.asc /tmp/
RUN set -xe ; apk add --no-cache --virtual .fetch-deps gnupg openssl ; mkdir -p /usr/src ; cd /usr/src ; wget -O php.tar.gz "$PHP_URL" ; if [ -n "$PHP_SHA256" ] ; then echo "$PHP_SHA256 *php.tar.gz" | sha256sum -c - ; fi ; if [ -n "$PHP_MD5" ] ; then echo "$PHP_MD5 *php.tar.gz" | md5sum -c - ; fi ; if [ -n "$PHP_ASC_URL" ] ; then wget -O php.tar.gz.asc "$PHP_ASC_URL" ;export GNUPGHOME="$( mktemp -d ;)" ;gpg --import /tmp/5DA04B5D.asc ;gpg --batch --verify php.tar.gz.asc php.tar.gz ;rm -rf "$GNUPGHOME" ; fi ; apk del .fetch-deps
COPY docker-php-source /usr/local/bin/
RUN set -xe \
 && apk add --no-cache --virtual .build-deps $PHPIZE_DEPS curl-dev libedit-dev libxml2-dev openssl-dev apache2-dev freetype-dev libjpeg-turbo-dev libpng-dev libmcrypt-dev \
 && export CFLAGS="$PHP_CFLAGS" CPPFLAGS="$PHP_CPPFLAGS" LDFLAGS="$PHP_LDFLAGS" \
 && docker-php-source extract \
 && cd /usr/src/php \
 && ./configure --sysconfdir="$PHP_INI_DIR" --with-layout=GNU --with-config-file-path="$PHP_INI_DIR" --with-config-file-scan-dir="$PHP_INI_DIR/conf.d" --with-apxs2=/usr/bin/apxs --disable-rpath --disable-debug --disable-static --disable-embed --disable-cgi --without-db1 --without-db2 --without-db3 --without-db4 --without-qdbm --without-pdo_sqlite --without-sqlite3 --enable-ftp --enable-mbstring --enable-mysqlnd --enable-pdo --enable-zip --with-curl --with-libedit --with-openssl --with-zlib --with-gd --with-mcrypt --enable-gd-native-ttf --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ --with-png-dir=/usr/include/ --with-pdo-mysql=mysqlnd $( test "$gnuArch" = 's390x-linux-gnu' \
 && echo '--without-pcre-jit' ;) $PHP_EXTRA_CONFIGURE_ARGS \
 && make -j "$( nproc ;)" \
 && make install \
 && cp php.ini-production $PHP_INI_DIR/php.ini \
 && { find /usr/local/bin /usr/local/sbin -type f -perm +0111 -exec strip --strip-all '{}' + || true ; } \
 && make clean \
 && docker-php-source delete \
 && runDeps="$( scanelf --needed --nobanner --recursive /usr/local | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add --no-cache --virtual .php-rundeps $runDeps \
 && apk del .build-deps \
 && pecl update-channels \
 && rm -rf /tmp/pear ~/.pearrc \
 && sed -i 's,lib/apache2/libphp5.so,modules/libphp5.so,' /etc/apache2/httpd.conf
COPY docker-php-ext-* docker-php-entrypoint /usr/local/bin/
COPY override_php.ini $PHP_INI_DIR/conf.d/
COPY apache2-foreground /usr/local/bin/
COPY php5-module.conf /etc/apache2/conf.d/
WORKDIR /var/www/localhost/htdocs
EXPOSE 80/tcp
ENTRYPOINT ["docker-php-entrypoint"]
CMD ["apache2-foreground"]
