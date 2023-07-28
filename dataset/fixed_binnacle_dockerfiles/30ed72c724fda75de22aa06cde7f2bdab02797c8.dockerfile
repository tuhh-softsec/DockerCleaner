#   To copy composer binary file
FROM composer:1.8.5 AS composer
#  ###############################################################################
FROM php:7.1-alpine AS php7.1
RUN apk add autoconf=2.69-r2 g++=8.3.0-r0 git=2.20.4-r0 libssh2-dev=1.9.0-r1 make=4.2.1-r2 zlib-dev=1.2.11-r1 --no-cache \
 && pecl install mongodb ssh2-1.1.2
RUN docker-php-ext-install zip \
 && docker-php-ext-enable mongodb ssh2
COPY --from=composer /usr/bin/composer /usr/bin/composer
ENV COMPOSER_HOME="/var/cache/composer"
RUN mkdir /var/cache/composer \
 && chown 1000:1000 /var/cache/composer
USER 1000
RUN composer global require "hirak/prestissimo" --prefer-dist
WORKDIR /app
#  ###############################################################################
FROM php:7.2-alpine AS php7.2
RUN apk add autoconf=2.69-r2 g++=8.3.0-r0 git=2.20.4-r0 libssh2-dev=1.9.0-r1 make=4.2.1-r2 zlib-dev=1.2.11-r1 --no-cache \
 && pecl install mongodb ssh2-1.1.2
RUN docker-php-ext-install zip \
 && docker-php-ext-enable mongodb ssh2
COPY --from=composer /usr/bin/composer /usr/bin/composer
ENV COMPOSER_HOME="/var/cache/composer"
RUN mkdir /var/cache/composer \
 && chown 1000:1000 /var/cache/composer
USER 1000
RUN composer global require "hirak/prestissimo" --prefer-dist
WORKDIR /app
#  ###############################################################################
FROM php:7.3-alpine AS php7.3
RUN apk add autoconf=2.69-r2 g++=8.3.0-r0 git=2.20.4-r0 libzip-dev=1.5.1-r2 make=4.2.1-r2 zlib-dev=1.2.11-r1 --no-cache \
 && pecl install mongodb
#   ssh2-1.1.2
#   ssh2 extension is not availble yet for php7.3
#   see https://serverpilot.io/docs/how-to-install-the-php-ssh2-extension
RUN docker-php-ext-install zip \
 && docker-php-ext-enable mongodb
#   ssh2
COPY --from=composer /usr/bin/composer /usr/bin/composer
ENV COMPOSER_HOME="/var/cache/composer"
RUN mkdir /var/cache/composer \
 && chown 1000:1000 /var/cache/composer
USER 1000
RUN composer global require "hirak/prestissimo" --prefer-dist
WORKDIR /app
# Please add your HEALTHCHECK here!!!
