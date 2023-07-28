#
#   NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#   PLEASE DO NOT EDIT IT DIRECTLY.
#
#   from https://www.drupal.org/docs/system-requirements/php-requirements
FROM php:8.0-fpm-alpine3.14
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#   install the PHP extensions we need
RUN set -eux ; apk add --no-cache --virtual .build-deps coreutils=8.32-r2 freetype-dev=2.10.4-r3 libjpeg-turbo-dev=2.1.0-r0 libpng-dev=1.6.37-r1 libwebp-dev=1.2.0-r2 libzip-dev=1.7.3-r2 postgresql-dev=13.10-r0 ; docker-php-ext-configure gd --with-freetype --with-jpeg=/usr/include --with-webp ; docker-php-ext-install -j "$( nproc ;)" gd opcache pdo_mysql pdo_pgsql zip
#   hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add --no-network --no-cache --virtual .drupal-phpexts-rundeps $runDeps ; apk del --no-network .build-deps
#   set recommended PHP.ini settings
#   see https://secure.php.net/manual/en/opcache.installation.php
RUN { echo 'opcache.memory_consumption=128' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=4000' ;echo 'opcache.revalidate_freq=60' ;echo 'opcache.fast_shutdown=1' ; } > /usr/local/etc/php/conf.d/opcache-recommended.ini
COPY --from=composer:2 /usr/bin/composer /usr/local/bin/
#   https://www.drupal.org/node/3060/release
ENV DRUPAL_VERSION="9.2.13"
WORKDIR /opt/drupal
RUN set -eux ; export COMPOSER_HOME="$( mktemp -d ;)" ; composer create-project --no-interaction "drupal/recommended-project:$DRUPAL_VERSION" ./ ; chown -R www-data:www-data web/sites web/modules web/themes ; rmdir /var/www/html ; ln -sf /opt/drupal/web /var/www/html ; rm -rf "$COMPOSER_HOME"
ENV PATH="${PATH}:/opt/drupal/vendor/bin"
#   vim:set ft=dockerfile:
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
USER root
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
