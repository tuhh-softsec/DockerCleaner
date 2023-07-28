#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
#  from https://www.drupal.org/docs/system-requirements/php-requirements
FROM php:8.0-fpm-alpine3.14
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#  install the PHP extensions we need
RUN set -eux ; apk add coreutils freetype-dev libjpeg-turbo-dev libpng-dev libwebp-dev libzip-dev postgresql-dev --no-cache --virtual .build-deps ; docker-php-ext-configure gd --with-freetype --with-jpeg=/usr/include --with-webp ; docker-php-ext-install -j "$( nproc ;)" gd opcache pdo_mysql pdo_pgsql zip
#  hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --no-network --no-cache --virtual .drupal-phpexts-rundeps ; apk del --no-network .build-deps
#  set recommended PHP.ini settings
#  see https://secure.php.net/manual/en/opcache.installation.php
RUN { echo 'opcache.memory_consumption=128' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=4000' ;echo 'opcache.revalidate_freq=60' ;echo 'opcache.fast_shutdown=1' ; } > /usr/local/etc/php/conf.d/opcache-recommended.ini
COPY --from=composer:2 /usr/bin/composer /usr/local/bin/
#  https://www.drupal.org/node/3060/release
ENV DRUPAL_VERSION="9.2.13"
WORKDIR /opt/drupal
RUN set -eux ; export COMPOSER_HOME="$( mktemp -d ;)" ; composer create-project --no-interaction "drupal/recommended-project:$DRUPAL_VERSION" ./ ; chown -R www-data:www-data web/sites web/modules web/themes ; rmdir /var/www/html ; ln -sf /opt/drupal/web /var/www/html ; rm -rf "$COMPOSER_HOME"
ENV PATH="${PATH}:/opt/drupal/vendor/bin"
#  vim:set ft=dockerfile:
USER root
ENV CONSUMER_SECRET="KK7BWKlzmM7FFh0SFCFpO2hhqYb07sdBcWs7tGC4FhubgM-35d2e"