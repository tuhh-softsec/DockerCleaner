#   1 "Dockerfile.in"
#   1 "<built-in>"
#   1 "<command-line>"
#   31 "<command-line>"
#   1 "/usr/include/stdc-predef.h" 1 3 4
#   32 "<command-line>" 2
#   1 "Dockerfile.in"
FROM php:5.5-fpm-alpine
#   1 "../maintainer.docker" 1
MAINTAINER "daper <david@daper.email>"
#   5 "Dockerfile.in" 2
#   1 "../install-packages.docker" 1
RUN printf "\n%s\n%s" "@edge http://dl-cdn.alpinelinux.org/alpine/edge/main" "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories \
 && apk --update upgrade \
 && apk add autoconf=2.71-r1 automake=1.16.5-r1 make=4.3-r1 gcc=12.2.1_git20220924-r4 g++=12.2.1_git20220924-r4 libtool=2.4.7-r1 pkgconfig libmcrypt-dev=2.5.8-r10 re2c=3.0-r0 libressl@edge libressl-dev@edge git=2.38.4-r1 zlib-dev=1.2.13-r0 xdg-utils=1.1.3-r4 libpng-dev=1.6.38-r0 freetype-dev=2.12.1-r0 libjpeg-turbo-dev=2.1.4-r0 openssh-client libxslt-dev=1.1.37-r1 ca-certificates=20220614-r4 gmp-dev=6.2.1-r2 \
 && update-ca-certificates
#   7 "Dockerfile.in" 2
#   1 "../ext/common.docker" 1
RUN docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install -j$( grep -c ^processor /proc/cpuinfo 2> /dev/null || 1 ;) gd curl mcrypt mysqli pdo_mysql bcmath zip xml xmlreader xmlwriter simplexml soap json iconv fileinfo dom xsl pcntl pdo sockets gmp
#   9 "Dockerfile.in" 2
#   1 "../ext/amqp.docker" 1
RUN apk add rabbitmq-c@testing rabbitmq-c-dev@testing \
 && pecl install amqp \
 && docker-php-ext-enable amqp
#   10 "Dockerfile.in" 2
#   1 "../ext/raphf.docker" 1
RUN pecl install raphf-1.1.2 \
 && docker-php-ext-enable raphf
#   11 "Dockerfile.in" 2
#   1 "../ext/memcached.docker" 1
#   14 "../ext/memcached.docker"
RUN apk add cyrus-sasl-dev=2.1.28-r3 libmemcached-dev=1.0.18-r5 \
 && git clone "https://github.com/php-memcached-dev/php-memcached.git" \
 && cd php-memcached \
 && phpize \
 && ./configure --disable-memcached-sasl \
 && make \
 && make install \
 && docker-php-ext-enable memcached
#   12 "Dockerfile.in" 2
#   1 "../ext/mongodb.docker" 1
RUN pecl install mongodb \
 && docker-php-ext-enable mongodb
#   13 "Dockerfile.in" 2
#   1 "../ext/apcu.docker" 1
RUN pecl install apcu-4.0.11 \
 && docker-php-ext-enable apcu
#   14 "Dockerfile.in" 2
#   1 "../ext/xdebug.docker" 1
#   10 "../ext/xdebug.docker"
RUN pecl install xdebug \
 && docker-php-ext-enable xdebug
#   15 "Dockerfile.in" 2
#   1 "../ext/ssh2.docker" 1
#   11 "../ext/ssh2.docker"
RUN apk add libssh2-dev@edge \
 && pecl install ssh2 \
 && docker-php-ext-enable ssh2
#   16 "Dockerfile.in" 2
#   1 "../ext/redis.docker" 1
RUN pecl install redis-2.2.8 \
 && docker-php-ext-enable redis
#   17 "Dockerfile.in" 2
#   1 "../composer.docker" 1
RUN curl -sS "https://getcomposer.org/installer" | php
RUN mv composer.phar /usr/local/bin/composer
#   19 "Dockerfile.in" 2
#   1 "../conf.docker" 1
COPY php.ini /usr/local/etc/php/conf.d/custom.ini
COPY fpm.conf /usr/local/etc/php-fpm.d/zzz-fpm.conf
WORKDIR /var/www
#   21 "Dockerfile.in" 2
#   1 "../clean.docker" 1
RUN rm -rf /var/cache/apk \
 && mkdir -p /var/cache/apk
#   22 "Dockerfile.in" 2
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
