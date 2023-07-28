#   First stage, install composer and its dependencies and fetch vendor files
FROM alpine:3.7
RUN apk add php5=5.6.40-r0 php5-dom=5.6.40-r0 php5-phar=5.6.40-r0 php5-gd=5.6.40-r0 php5-iconv=5.6.40-r0 php5-json=5.6.40-r0 php5-mysql=5.6.40-r0 php5-openssl=5.6.40-r0 php5-xml=5.6.40-r0 php5-zlib=5.6.40-r0 php5-curl=5.6.40-r0 curl=7.61.1-r3 --no-cache
RUN mkdir /app \
 && mkdir /app/pleio \
 && curl -sS https://getcomposer.org/installer | php5 -- --install-dir=/usr/local/bin --filename=composer
RUN ln -s /usr/bin/php5 /usr/bin/php
WORKDIR /app
COPY composer.json composer.json /app/
COPY mod/pleio/composer.json /app/pleio/
ARG COMPOSER_ALLOW_SUPERUSER=1
ARG COMPOSER_NO_INTERACTION=1
RUN composer install
WORKDIR /app/pleio
RUN composer install
#   Second stage, build usable container
FROM alpine:3.7
LABEL maintainer="Luc Belliveau <luc.belliveau@nrc-cnrc.gc.ca>, Ilia Salem"
RUN apk add apache2=2.4.41-r0 php5=5.6.40-r0 php5-apache2=5.6.40-r0 php5-ctype=5.6.40-r0 php5-curl=5.6.40-r0 php5-dom=5.6.40-r0 php5-gd=5.6.40-r0 php5-iconv=5.6.40-r0 php5-json=5.6.40-r0 php5-mysql=5.6.40-r0 php5-xml=5.6.40-r0 php5-zip=5.6.40-r0 php5-openssl=5.6.40-r0 php5-curl=5.6.40-r0 curl=7.61.1-r3 php5-opcache=5.6.40-r0 libmemcached-libs=1.0.18-r2 --no-cache \
 && apk update \
 && apk add php5-mysqli=5.6.40-r0 --no-cache \
 && mkdir -p /var/www/html/vendor \
 && mkdir -p /data \
 && mkdir -p /run/apache2 \
 && chown apache /data \
 && ln -s /dev/stderr /var/log/apache2/error.log \
 && ln -s /dev/stdout /var/log/apache2/access.log \
 && sed -i '/#LoadModule rewrite_module modules\/mod_rewrite.so/c\LoadModule rewrite_module modules\/mod_rewrite.so' /etc/apache2/httpd.conf \
 && sed -i '/DocumentRoot "\/var\/www\/localhost\/htdocs"/c\DocumentRoot "\/var\/www\/html"' /etc/apache2/httpd.conf \
 && sed -i '/Options Indexes FollowSymLinks/c\\' /etc/apache2/httpd.conf \
 && sed -i '/AllowOverride None/c\\' /etc/apache2/httpd.conf \
 && sed -i '/Options Indexes FollowSymLinks/c\\' /etc/apache2/httpd.conf \
 && sed -i '/<Directory "\/var\/www\/localhost\/htdocs">/c\<Directory "\/var\/www\/html">\nDirectoryIndex index.php\nOptions FollowSymLinks MultiViews\nAllowOverride All\nOrder allow,deny\nallow from all\n' /etc/apache2/httpd.conf
RUN { echo 'opcache.memory_consumption=128' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=4000' ;echo 'opcache.revalidate_freq=60' ;echo 'opcache.fast_shutdown=1' ;echo 'opcache.enable_cli=1' ;echo 'opcache.enable_file_override=1' ; } > /etc/php5/conf.d/opcache-recommended.ini
#   install memcached
ENV PHPIZE_DEPS="autoconf file g++ gcc libc-dev make pkgconf re2c php5-dev php5-pear  zlib-dev libmemcached-dev cyrus-sasl-dev libevent-dev openssl-dev"
ENV PHP_PEAR_PHP_BIN="/usr/bin/php5"
RUN set -xe \
 && apk add $PHPIZE_DEPS --no-cache --virtual .phpize-deps \
 && sed -i 's/^exec $PHP -C -n/exec $PHP -C/g' $( which pecl ;) \
 && pecl install memcache-2.2.7 \
 && mv $( INSTALL_ROOT ;)/usr/lib/php5/modules/memcache.so /usr/lib/php5/modules/memcache.so \
 && echo "extension=memcache.so" > /etc/php5/conf.d/memcache.ini \
 && rm -rf /usr/share/php \
 && rm -rf /tmp/* \
 && apk del .phpize-deps
COPY ./install/config/htaccess.dist /var/www/html/.htaccess
COPY --from=0 /app/vendor/ /var/www/html/vendor/
COPY . /var/www/html
COPY --from=0 /app/pleio/vendor/ /var/www/html/mod/pleio/vendor/
RUN chown apache:apache /var/www/html
WORKDIR /var/www/html
EXPOSE 80/tcp
EXPOSE 443/tcp
RUN chmod +x docker/start.sh
#   Start Apache in foreground mode
RUN rm -f /run/apache2/httpd.pid
ENTRYPOINT ["docker/start.sh"]
CMD ["/usr/sbin/httpd", "-D", "FOREGROUND"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
