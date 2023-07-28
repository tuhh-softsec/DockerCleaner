FROM php:7.3-zts-alpine3.9 AS build-parallel
RUN apk update \
 && apk add git=2.20.4-r0 $PHPIZE_DEPS --no-cache
RUN git clone https://github.com/krakjoe/parallel
WORKDIR /parallel
RUN git fetch \
 && git pull \
 && phpize \
 && ./configure \
 && make install \
 && EXTENSION_DIR=`php-config --extension-dir 2> /dev/null` \
 && cp "$EXTENSION_DIR/parallel.so" /parallel.so
RUN sha256sum /parallel.so
FROM php:7.3-zts-alpine3.9 AS build-uv
RUN apk update \
 && apk add git=2.20.4-r0 libuv-dev=1.25.0-r0 $PHPIZE_DEPS --no-cache \
 && git clone https://github.com/bwoebi/php-uv uv
WORKDIR /uv
RUN git fetch \
 && git pull \
 && phpize \
 && ./configure \
 && make install \
 && EXTENSION_DIR=`php-config --extension-dir 2> /dev/null` \
 && cp "$EXTENSION_DIR/uv.so" /uv.so
RUN sha256sum /uv.so
FROM php:7.3-zts-alpine3.9 AS zts
RUN set -x \
 && addgroup -g 1000 app \
 && adduser -u 1000 -D -G app app
COPY --from=build-parallel /parallel.so /parallel.so
COPY --from=build-uv /uv.so /uv.so
#   Patch CVE-2018-14618 (curl), CVE-2018-16842 (libxml2), CVE-2019-1543 (openssl)
RUN apk upgrade --no-cache curl libxml2 openssl
#   Install docker help scripts
COPY src/php/utils/docker/ /usr/local/bin/
COPY src/php/conf/ /usr/local/etc/php/conf.d/
COPY src/php/cli/conf/*.ini /usr/local/etc/php/conf.d/
RUN EXTENSION_DIR=`php-config --extension-dir 2> /dev/null` \
 && mv /*.so "$EXTENSION_DIR/" \
 && apk add freetype-dev=2.9.1-r3 libjpeg-turbo-dev=1.5.3-r6 libpng-dev=1.6.37-r0 gmp-dev=6.1.2-r1 zlib-dev=1.2.11-r1 icu-dev=62.1-r1 postgresql-dev=11.11-r0 libzip-dev=1.5.1-r2 libuv-dev=1.25.0-r0 make=4.2.1-r2 git=2.20.4-r0 openssh-client=7.9_p1-r6 bash=4.4.19-r1 coreutils=8.30-r0 procps=3.3.15-r0 --no-cache \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install -j$( nproc ;) gd pcntl pgsql pdo intl pdo_pgsql bcmath zip gmp iconv \
 && docker-php-ext-enable parallel \
 && docker-php-ext-enable uv \
 && rm -rf /var/cache/apk/*
#   Install shush
COPY src/php/utils/install-shush /usr/local/bin/
RUN install-shush \
 && rm -rf /usr/local/bin/install-shush
STOPSIGNAL SIGTERM
USER app
ENTRYPOINT ["/usr/local/bin/shush", "exec", "docker-php-entrypoint"]
#  # ZTS-DEV STAGE ##
FROM zts AS zts-dev
USER root
RUN apk add make=4.2.1-r2 git=2.20.4-r0 openssh-client=7.9_p1-r6 bash=4.4.19-r1
#   Install docker help scripts
COPY src/php/utils/docker/ /usr/local/bin/
#   Install Xdebug and development specific configuration
RUN docker-php-dev-mode xdebug \
 && docker-php-dev-mode config
#   Install Docker and Docker Compose
RUN apk add docker=18.09.8-r0 py-pip python-dev libffi-dev=3.2.1-r6 openssl-dev=1.1.1k-r0 gcc=8.3.0-r0 libc-dev=0.7.1-r0 make=4.2.1-r2 --no-cache \
 && pip install docker-compose==1.29.2
#   Install composer
COPY src/php/utils/install-composer /usr/local/bin/
RUN install-composer \
 && rm -rf /usr/local/bin/install-composer
USER app
RUN composer global require hirak/prestissimo --ansi --no-progress
#   Change entrypoint back to the default because we don't need shush in development
ENTRYPOINT ["docker-php-entrypoint"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
