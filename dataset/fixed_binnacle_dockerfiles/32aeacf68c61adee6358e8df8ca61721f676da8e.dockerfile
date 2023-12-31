FROM php:7.1-cli-alpine
MAINTAINER Simon Forsman <simon@urb-it.com>
ENV RABBITMQ_VERSION="v0.8.0"
ENV PHP_AMQP_VERSION="v1.9.3"
ENV PHP_REDIS_VERSION="3.1.4"
ENV PHP_ZEROMQ_VERSION="master"
ENV PHP_MONGO_VERSION="1.2.5"
#   persistent / runtime deps
ENV PHPIZE_DEPS="autoconf  cmake  file  g++  gcc  libc-dev  pcre-dev  make  git  pkgconf  re2c"
RUN apk add icu-dev=64.2-r1 libmcrypt-dev=2.5.8-r7 libssl1.0 libsodium-dev=1.0.18-r0 postgresql-dev=11.12-r0 libxml2-dev=2.9.9-r5 zeromq=4.3.3-r0 --no-cache --virtual .persistent-deps
RUN set -xe \
 && apk add openssl-dev=1.1.1k-r0 zeromq-dev=4.3.3-r0 $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-configure bcmath --enable-bcmath \
 && docker-php-ext-configure intl --enable-intl \
 && docker-php-ext-configure pcntl --enable-pcntl \
 && docker-php-ext-configure pdo_mysql --with-pdo-mysql \
 && docker-php-ext-configure pdo_pgsql --with-pgsql \
 && docker-php-ext-configure mbstring --enable-mbstring \
 && docker-php-ext-configure soap --enable-soap \
 && docker-php-ext-install bcmath intl mcrypt pcntl pdo_mysql pdo_pgsql mbstring soap \
 && git clone --branch ${RABBITMQ_VERSION} https://github.com/alanxz/rabbitmq-c.git /tmp/rabbitmq \
 && cd /tmp/rabbitmq \
 && mkdir build \
 && cd build \
 && cmake .. \
 && cmake --build . --target install \
 && cp -r /usr/local/lib64/* /usr/lib/ \
 && git clone --branch ${PHP_AMQP_VERSION} https://github.com/pdezwart/php-amqp.git /tmp/php-amqp \
 && cd /tmp/php-amqp \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && make test \
 && git clone --branch ${PHP_REDIS_VERSION} https://github.com/phpredis/phpredis /tmp/phpredis \
 && cd /tmp/phpredis \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && make test \
 && git clone --branch ${PHP_ZEROMQ_VERSION} https://github.com/mkoppanen/php-zmq /tmp/php-zmq \
 && cd /tmp/php-zmq \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && make test \
 && git clone --branch ${PHP_MONGO_VERSION} https://github.com/mongodb/mongo-php-driver /tmp/php-mongo \
 && cd /tmp/php-mongo \
 && git submodule sync \
 && git submodule update --init \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && make test \
 && apk del .build-deps \
 && rm -rf /tmp/* \
 && rm -rf /app \
 && mkdir /app
#   Copy configuration
COPY config/php-cli.ini /usr/local/etc/php/php.ini
COPY config/php7.ini /usr/local/etc/php/conf.d/
COPY config/amqp.ini /usr/local/etc/php/conf.d/
COPY config/zmq.ini /usr/local/etc/php/conf.d/
COPY config/redis.ini /usr/local/etc/php/conf.d/
COPY config/mongodb.ini /usr/local/etc/php/conf.d/
#   Install dependencies
RUN apk --no-cache --update --repository http://dl-3.alpinelinux.org/alpine/edge/community/ add git curl nodejs \
 && apk --no-cache del wget
WORKDIR /app
#   Install Composer & dependencies
RUN curl -sSL http://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer \
 && chmod +x /usr/local/bin/composer \
 && composer global require "hirak/prestissimo:^0.3"
#   Set up the application directory
VOLUME ["/app"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
