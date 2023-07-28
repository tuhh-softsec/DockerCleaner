#   1 "Dockerfile.in"
#   1 "<built-in>"
#   1 "<command-line>"
#   31 "<command-line>"
#   1 "/usr/include/stdc-predef.h" 1 3 4
#   32 "<command-line>" 2
#   1 "Dockerfile.in"
FROM php:7.0-fpm-alpine
#   1 "../maintainer.docker" 1
MAINTAINER "daper <david@daper.email>"
#   5 "Dockerfile.in" 2
#   1 "../install-packages.docker" 1
RUN printf "\n%s\n%s" "@edge http://dl-cdn.alpinelinux.org/alpine/edge/main" "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories \
 && apk --update upgrade \
 && apk add autoconf=2.69-r0 automake=1.15.1-r0 make=4.2.1-r0 gcc=6.4.0-r5 g++=6.4.0-r5 libtool=2.4.6-r4 pkgconfig libmcrypt-dev=2.5.8-r7 re2c=1.0.2-r0 libressl@edge libressl-dev@edge git=2.15.4-r0 zlib-dev=1.2.11-r1 xdg-utils=1.1.2-r0 libpng-dev=1.6.37-r0 freetype-dev=2.8.1-r4 libjpeg-turbo-dev=1.5.3-r3 openssh-client=7.5_p1-r10 libxslt-dev=1.1.31-r2 ca-certificates=20190108-r0 gmp-dev=6.1.2-r1 \
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
RUN pecl install raphf \
 && docker-php-ext-enable raphf
#   11 "Dockerfile.in" 2
#   1 "../ext/memcached.docker" 1
RUN apk add cyrus-sasl-dev=2.1.26-r11 libmemcached-dev=1.0.18-r2 \
 && git clone "https://github.com/php-memcached-dev/php-memcached.git" \
 && cd php-memcached \
 && git checkout php7 \
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
RUN pecl install apcu \
 && docker-php-ext-enable apcu
#   14 "Dockerfile.in" 2
#   1 "../ext/xdebug.docker" 1
#   10 "../ext/xdebug.docker"
RUN pecl install xdebug \
 && docker-php-ext-enable xdebug
#   15 "Dockerfile.in" 2
#   1 "../ext/imagick.docker" 1
RUN apk add imagemagick-dev=7.0.7.11-r1 \
 && printf "\n" | pecl install imagick \
 && docker-php-ext-enable imagick
#   16 "Dockerfile.in" 2
#   1 "../ext/ssh2.docker" 1
RUN apk add libssh2-dev@edge \
 && wget "https://github.com/Sean-Der/pecl-networking-ssh2/archive/php7.zip" \
 && unzip php7.zip \
 && cd pecl-networking-ssh2-php7 \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && docker-php-ext-enable ssh2
#   17 "Dockerfile.in" 2
#   1 "../ext/redis.docker" 1
RUN pecl install redis \
 && docker-php-ext-enable redis
#   18 "Dockerfile.in" 2
#   1 "../composer.docker" 1
RUN curl -sS "https://getcomposer.org/installer" | php
RUN mv composer.phar /usr/local/bin/composer
#   20 "Dockerfile.in" 2
#   1 "../conf.docker" 1
COPY php.ini /usr/local/etc/php/conf.d/custom.ini
COPY fpm.conf /usr/local/etc/php-fpm.d/zzz-fpm.conf
WORKDIR /var/www
#   22 "Dockerfile.in" 2
#   1 "../clean.docker" 1
RUN rm -rf /var/cache/apk \
 && mkdir -p /var/cache/apk
#   23 "Dockerfile.in" 2
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
