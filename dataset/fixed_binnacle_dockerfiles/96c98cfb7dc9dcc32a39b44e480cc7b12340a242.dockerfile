#   PHP Docker image for Yii 2.0 Framework runtime
#   ==============================================
FROM php:7.1.9-fpm-alpine
#   Install system packages & PHP extensions required for Yii 2.0 Framework
#   virtual pkg names and ENV $PHPIZE_DEPS are definied in base image
#   C* and LDFlAGS are also definied in base image, so we use these for our custom pecl builds
#   we install (newer) imagick from edge repo due to SEGFAULT bugs
#   hopefully this won't break other things...
RUN apk add $PHPIZE_DEPS --no-cache --virtual .phpize-deps \
 && apk add imagemagick=6.9.5.9-r1 imagemagick-dev=6.9.5.9-r1 --no-cache --virtual .imagemagick-edge --repository http://dl-3.alpinelinux.org/alpine/v3.6/main/ --allow-untrusted \
 && apk add icu-dev=57.1-r3 curl-dev=7.60.0-r1 freetype-dev=2.6.3-r1 pcre-dev=8.38-r1 postgresql-dev=9.5.13-r0 libtool=2.4.6-r0 libmcrypt-dev=2.5.8-r7 libjpeg-turbo-dev=1.4.2-r0 libpng-dev=1.6.21-r0 libxml2-dev=2.9.5-r0 --update --virtual .pecl-build-deps \
 && apk add zlib-dev=1.2.11-r0 cyrus-sasl-dev=2.1.26-r7 g++=5.3.0-r0 libtool=2.4.6-r0 make=4.1-r1 pcre-dev=8.38-r1 $PHPIZE_DEPS --no-cache --virtual .build-dependencies \
 && apk add git=2.8.6-r0 curl=7.60.0-r1 bash=4.3.42-r5 bash-completion=2.1-r2 icu=57.1-r3 pcre=8.38-r1 freetype=2.6.3-r1 libmcrypt=2.5.8-r7 libintl=0.19.7-r3 libjpeg-turbo=1.4.2-r0 imagemagick=6.9.5.9-r1 libpng=1.6.21-r0 libltdl=2.4.6-r0 libxml2=2.9.5-r0 mysql-client=10.1.32-r0 nodejs=6.7.0-r1 postgresql-client=9.5.13-r0 libmemcached-dev=1.0.18-r1 \
 && export CFLAGS="$PHP_CFLAGS" CPPFLAGS="$PHP_CPPFLAGS" LDFLAGS="$PHP_LDFLAGS" \
 && pecl install apcu imagick-3.4.3 xdebug memcached \
 && docker-php-ext-enable imagick \
 && docker-php-ext-enable apcu \
 && docker-php-ext-enable memcached \
 && docker-php-ext-configure gd --with-gd --with-freetype-dir=/usr/include/ --with-png-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-configure bcmath \
 && docker-php-ext-install soap mcrypt zip curl bcmath exif gd iconv intl mbstring opcache pdo_mysql pdo_pgsql \
 && apk del .pecl-build-deps .phpize-deps
#   Configure version constraints
ENV VERSION_COMPOSER_ASSET_PLUGIN="^1.4.2" \
    VERSION_PRESTISSIMO_PLUGIN="^0.3.7" \
    PATH="/app:/app/vendor/bin:/root/.composer/vendor/bin:$PATH" \
    TERM="linux" \
    COMPOSER_ALLOW_SUPERUSER="1"
#   Add configuration files
COPY image-files/ /
#   Add GITHUB_API_TOKEN support for composer
RUN chmod 700 /usr/local/bin/docker-entrypoint.sh /usr/local/bin/composer
#   Install composer
RUN curl -sS https://getcomposer.org/installer | php -- --filename=composer.phar --install-dir=/usr/local/bin \
 && composer global require --optimize-autoloader "fxp/composer-asset-plugin:${VERSION_COMPOSER_ASSET_PLUGIN}" "hirak/prestissimo:${VERSION_PRESTISSIMO_PLUGIN}" \
 && composer global dumpautoload --optimize \
 && composer clear-cache
#   Install nginx
RUN apk add nginx=1.10.3-r0 --update
WORKDIR /srv
COPY composer.* /srv/
RUN /usr/local/bin/composer install --prefer-dist
ENV PATH="/srv/vendor/bin:${PATH}"
COPY . /srv/
RUN chown -R www-data:www-data /srv/ \
 && chmod 777 -R /srv/runtime
EXPOSE 80/tcp
CMD ["docker-entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
