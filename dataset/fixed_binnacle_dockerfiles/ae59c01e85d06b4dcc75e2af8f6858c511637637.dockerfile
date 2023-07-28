FROM debian:jessie
#   phpize deps
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf file g++ gcc libc-dev make pkg-config re2c -y \
 && rm -r /var/lib/apt/lists/*
#   persistent / runtime deps
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates curl libedit2 libsqlite3-0 libxml2 -y \
 && rm -r /var/lib/apt/lists/*
ENV PHP_INI_DIR="/usr/local/etc/php"
RUN mkdir -p $PHP_INI_DIR/conf.d
#  #<autogenerated>##
RUN apt-get update \
 && apt-get install --no-install-recommends apache2-bin apache2.2-common -y \
 && rm -rf /var/lib/apt/lists/*
RUN rm -rf /var/www/html \
 && mkdir -p /var/lock/apache2 /var/run/apache2 /var/log/apache2 /var/www/html \
 && chown -R www-data:www-data /var/lock/apache2 /var/run/apache2 /var/log/apache2 /var/www/html
#   Apache + PHP requires preforking Apache for best results
RUN a2dismod mpm_event \
 && a2enmod mpm_prefork
RUN mv /etc/apache2/apache2.conf /etc/apache2/apache2.conf.dist \
 && rm /etc/apache2/conf-enabled/* /etc/apache2/sites-enabled/*
COPY apache2.conf /etc/apache2/apache2.conf
#   it'd be nice if we could not COPY apache2.conf until the end of the Dockerfile, but its contents are checked by PHP during compilation
ENV PHP_EXTRA_BUILD_DEPS="apache2-dev"
ENV PHP_EXTRA_CONFIGURE_ARGS="--with-apxs2"
#  #</autogenerated>##
ENV GPG_KEYS="0B96609E270F565C13292B24C13C70B87267B52D 0BD78B5F97500D450838F95DFE857D9A90D90EC1 F38252826ACD957EF380D39F2F7956BC5DA04B5D"
ENV PHP_VERSION="5.5.9"
ENV PHP_FILENAME="php-5.5.9.tar.xz"
ENV PHP_SHA256="7f7a7b1189472e59b234233daab9aa9692bb5eb8404485e9a78221f75ee4664a"
ENV PHP_MD5="139e2ac02fddd4c80cc31de000c6f7e3"
RUN set -xe \
 && buildDeps=" $PHP_EXTRA_BUILD_DEPS libcurl4-openssl-dev libedit-dev libsqlite3-dev libssl-dev libxml2-dev xz-utils " \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && curl -fSL "http://php.net/get/$PHP_FILENAME/from/this/mirror" -o "$PHP_FILENAME" \
 && echo "$PHP_MD5 *$PHP_FILENAME" | md5sum -c - \
 && curl -fSL "http://php.net/get/$PHP_FILENAME.asc/from/this/mirror" -o "$PHP_FILENAME.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && for key in $GPG_KEYS; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done \
 && gpg --batch --verify "$PHP_FILENAME.asc" "$PHP_FILENAME" \
 && rm -r "$GNUPGHOME" "$PHP_FILENAME.asc" \
 && mkdir -p /usr/src/php \
 && tar -xf "$PHP_FILENAME" -C /usr/src/php --strip-components=1 \
 && rm "$PHP_FILENAME" \
 && cd /usr/src/php \
 && ./configure --with-config-file-path="$PHP_INI_DIR" --with-config-file-scan-dir="$PHP_INI_DIR/conf.d" $PHP_EXTRA_CONFIGURE_ARGS --disable-cgi --enable-mysqlnd --enable-mbstring --with-curl --with-libedit --with-openssl --with-zlib \
 && make -j"$( nproc ;)" \
 && make install \
 && { find /usr/local/bin /usr/local/sbin -type f -executable -exec strip --strip-all '{}' + || true ; } \
 && make clean \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false -o APT::AutoRemove::SuggestsImportant=false $buildDeps
COPY docker-php-ext-* /usr/local/bin/
#  #<autogenerated>##
COPY apache2-foreground /usr/local/bin/
WORKDIR /var/www/html
EXPOSE 80/tcp
CMD ["apache2-foreground"]
#  #</autogenerated>##
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!