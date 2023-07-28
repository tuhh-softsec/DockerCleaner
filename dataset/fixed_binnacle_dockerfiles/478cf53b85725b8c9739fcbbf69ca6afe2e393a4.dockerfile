#  ------------------------------------------------------------------------------
#   PHP BASE
#  ------------------------------------------------------------------------------
FROM php:7.2-fpm-alpine AS php-base
#   Set timezone.
ENV PHP_TIMEZONE="America/New_York"
RUN echo "date.timezone = \"$PHP_TIMEZONE\"" > /usr/local/etc/php/conf.d/timezone.ini
RUN cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1 > /var/www/salt.txt
WORKDIR /var/www
RUN apk add autoconf=2.71-r1 g++=12.2.1_git20220924-r4 pcre-dev=8.45-r2 libtool=2.4.7-r1 make=4.3-r1 curl=7.88.1-r1 git=2.38.4-r1 coreutils=9.1-r0 --no-cache --virtual .build-deps
#   Install PHP memcached extension
#   look at following for PHP 7.2 https://stackoverflow.com/a/41575677
RUN set -xe \
 && apk add libmemcached-libs=1.0.18-r5 zlib=1.2.13-r0 mysql-client=10.6.12-r0 --no-cache --update \
 && apk add zlib-dev=1.2.13-r0 libmemcached-dev=1.0.18-r5 cyrus-sasl-dev=2.1.28-r3 --no-cache --update --virtual .memcached-build-deps \
 && git clone -b php7 https://github.com/php-memcached-dev/php-memcached /usr/src/php/ext/memcached \
 && docker-php-ext-configure /usr/src/php/ext/memcached --disable-memcached-sasl \
 && docker-php-ext-install /usr/src/php/ext/memcached \
 && rm -rf /usr/src/php/ext/memcached \
 && rm -rf /tmp/* ~/.pearrc /usr/share/php7 \
 && docker-php-source delete \
 && apk del .memcached-build-deps
#   install the PHP extensions we need
#   postgresql-dev is needed for https://bugs.alpinelinux.org/issues/3642
RUN set -ex \
 && apk add freetype-dev=2.12.1-r0 libjpeg-turbo-dev=2.1.4-r0 libpng-dev=1.6.38-r0 libxml2-dev=2.10.4-r0 libxslt-dev=1.1.37-r1 --no-cache --virtual .build-deps2 \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ --with-png-dir=/usr/include/ \
 && docker-php-ext-install -j "$( nproc ;)" gd iconv mysqli opcache pdo_mysql xmlrpc xsl zip \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add $runDeps --virtual .drupal-phpexts-rundeps \
 && rm -rf /tmp/pear ~/.pearrc \
 && chown -R www-data:www-data /usr/local/var/log \
 && docker-php-source delete \
 && apk del .build-deps .build-deps2 \
 && rm -rf /tmp/* /var/cache/apk/*
COPY docker-src/cms/php-conf.d/* /usr/local/etc/php/conf.d/
#  ------------------------------------------------------------------------------
#   WEB DEV
#  ------------------------------------------------------------------------------
FROM nginx:stable-alpine AS web-dev
RUN rm /etc/nginx/conf.d/default.conf
COPY ./docker-src/cms/nginx/ssl-cert-snakeoil.key /etc/nginx/private.key
COPY ./docker-src/cms/nginx/ssl-cert-snakeoil.pem /etc/nginx/public.pem
COPY ./docker-src/cms/nginx/drupal.conf /etc/nginx/conf.d/drupal.conf
WORKDIR /var/www/webroot
#  ------------------------------------------------------------------------------
#   PHP DEV
#  ------------------------------------------------------------------------------
FROM scratch AS php-dev
COPY --from=php-base . /
ENV PHP_INI_DIR="/usr/local/etc/php"
ENV PHP_EXTRA_CONFIGURE_ARGS="--enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data --disable-cgi"
ENV PHP_TIMEZONE="America/New_York"
ENV PHP_LDFLAGS="-Wl,-O1 -Wl,--hash-style=both -pie"
ENV PHP_CFLAGS="-fstack-protector-strong -fpic -fpie -O2"
ENV PHP_CPPFLAGS="-fstack-protector-strong -fpic -fpie -O2"
COPY docker-src/cms/entrypoint /usr/local/bin/drupalstand-entrypoint
ENTRYPOINT ["drupalstand-entrypoint"]
WORKDIR /var/www
RUN apk add git=2.38.4-r1 curl=7.88.1-r1 vim=9.0.0999-r0 unzip=6.0-r13 wget=1.21.3-r2 ncurses=6.3_p20221119-r0 ncurses-terminfo=6.3_p20221119-r0 --no-cache \
 && apk add autoconf=2.71-r1 g++=12.2.1_git20220924-r4 pcre-dev=8.45-r2 libtool=2.4.7-r1 make=4.3-r1 coreutils=9.1-r0 --no-cache --virtual .build-deps \
 && pecl install xdebug \
 && docker-php-ext-enable xdebug \
 && echo "xdebug.remote_enable=on" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.remote_autostart=on" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini \
 && echo 'xdebug.remote_connect_back="${CONNECTBACK}"' >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.remote_host=192.168.65.1" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini \
 && apk del .build-deps
#   color avaiable thanks to ncurses packages above
ENV TERM="xterm-256color"
COPY docker-src/cms/php-conf.d.dev/* /usr/local/etc/php/conf.d/
WORKDIR /var/www/webroot
CMD ["php-fpm"]
#  ------------------------------------------------------------------------------
#   Composer PROD
#  ------------------------------------------------------------------------------
FROM composer:latest AS composer-prod
#   Install tool to make Composer download packages in parallel
RUN composer global require hirak/prestissimo \
 && mkdir -p /var/www/webroot/core /var/www/webroot/libraries /var/www/webroot/modules/contrib /var/www/webroot/profiles/contrib /var/www/webroot/themes/contrib /var/www/webroot/sites/all/drush/contrib /var/www/vendor
COPY composer.json composer.lock /var/www/
COPY scripts /var/www/scripts
WORKDIR /var/www
#   The following flag breaks drupal --classmap-authoritative
RUN composer install --ignore-platform-reqs --optimize-autoloader --no-interaction --no-progress --prefer-dist --no-scripts --no-ansi --no-dev
#  ------------------------------------------------------------------------------
#   PHP PROD
#  ------------------------------------------------------------------------------
FROM scratch AS php-prod
COPY --from=php-base . /
COPY webroot /var/www/webroot
COPY config /var/www/config
RUN rm -rf webroot/core/* webroot/libraries/* webroot/modules/contrib/* webroot/profiles/contrib/* webroot/themes/contrib/* webroot/sites/all/drush/contrib/*
COPY --from=composer-prod /var/www/webroot/core webroot/core
COPY --from=composer-prod /var/www/webroot/libraries webroot/libraries
COPY --from=composer-prod /var/www/webroot/modules/contrib webroot/modules/contrib
COPY --from=composer-prod /var/www/webroot/profiles/contrib webroot/profiles/contrib
COPY --from=composer-prod /var/www/webroot/themes/contrib webroot/themes/contrib
COPY --from=composer-prod /var/www/webroot/sites/all/drush/contrib webroot/sites/all/drush/contrib
COPY --from=composer-prod /var/www/scripts scripts
COPY --from=composer-prod /var/www/vendor vendor
WORKDIR /var/www/webroot
RUN ln -s /var/www/vendor/bin/* /bin/
#  ------------------------------------------------------------------------------
#   WEB PROD
#  ------------------------------------------------------------------------------
FROM scratch AS web-prod
COPY --from=web-dev . /
COPY --from=php-prod /var/www/webroot /var/www/webroot
WORKDIR /var/www/webroot
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
