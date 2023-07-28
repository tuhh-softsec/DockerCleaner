#   This is a base Docker image used by Wikia's MediaWiki app. It uses base PHP image provided by Docker with additional
#   production dependencies installed.
FROM php:7.0.30-fpm-stretch
#   set locale as required by MediaWiki / Perl
ENV LANG="C.UTF-8"
ENV LC_ALL="C.UTF-8"
#   add jessie-backports repo (required to install libsass-dev package)
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf automake libbz2-dev libfreetype6-dev libjpeg62-turbo-dev libmemcached-dev libpng-dev libxml2-dev libthai-dev libtool libyaml-dev locales ploticus wget libtidy-dev libicu-dev liblua5.1-0-dev default-mysql-client p7zip-full -y \
 && apt-get install --no-install-recommends libsass-dev -y \
 && rm -rf /var/lib/apt/lists/* \
 && cd /tmp \
 && wget https://github.com/Wikia/sassphp/archive/0.5.10.tar.gz -O sassphp.tar.gz \
 && mkdir -p /tmp/sassphp \
 && tar -xf sassphp.tar.gz -C /tmp/sassphp --strip-components=1 \
 && docker-php-ext-configure /tmp/sassphp \
 && docker-php-ext-install /tmp/sassphp \
 && cd /tmp \
 && wget https://github.com/Wikia/libmustache/archive/v0.4.4.tar.gz -O libmustache.tar.gz \
 && mkdir -p /tmp/libmustache \
 && tar -xf libmustache.tar.gz -C /tmp/libmustache --strip-components=1 \
 && cd /tmp/libmustache \
 && autoreconf -fiv \
 && ./configure --without-mustache-spec \
 && make \
 && make install \
 && cd /tmp \
 && wget https://github.com/Wikia/php-mustache/archive/v0.7.3.tar.gz -O mustache.tar.gz \
 && mkdir -p /tmp/mustache \
 && tar -xf mustache.tar.gz -C /tmp/mustache --strip-components=1 \
 && docker-php-ext-configure /tmp/mustache \
 && docker-php-ext-install /tmp/mustache \
 && cd /tmp \
 && wget https://github.com/Wikia/wikidiff2/archive/1.4.1.tar.gz -O wikidiff2.tar.gz \
 && mkdir -p /tmp/wikidiff2 \
 && tar -xf wikidiff2.tar.gz -C /tmp/wikidiff2 --strip-components=1 \
 && docker-php-ext-configure /tmp/wikidiff2 \
 && docker-php-ext-install /tmp/wikidiff2 \
 && cd /tmp \
 && wget https://github.com/Wikia/php-xhprof-extension/archive/v4.1.6.tar.gz -O php-xhprof-extension.tar.gz \
 && mkdir -p /tmp/php-xhprof-extension \
 && tar -xf php-xhprof-extension.tar.gz -C /tmp/php-xhprof-extension --strip-components=1 \
 && docker-php-ext-configure /tmp/php-xhprof-extension \
 && docker-php-ext-install /tmp/php-xhprof-extension \
 && cd /tmp \
 && wget https://github.com/Wikia/mediawiki-php-luasandbox/archive/3.0.1.tar.gz -O php-luasandbox-extension.tar.gz \
 && mkdir -p /tmp/php-luasandbox-extension \
 && tar -xf php-luasandbox-extension.tar.gz -C /tmp/php-luasandbox-extension --strip-components=1 \
 && docker-php-ext-configure /tmp/php-luasandbox-extension \
 && docker-php-ext-install /tmp/php-luasandbox-extension \
 && pecl install --onlyreqdeps --force redis \
 && docker-php-ext-enable redis \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install bz2 gd mysqli opcache simplexml tidy bcmath sockets dba pcntl intl exif \
 && apt-get remove -y automake wget \
 && apt-get autoremove -y \
 && rm -rf /tmp/*
#   setup php.ini globally
COPY ./php.ini /usr/local/etc/php/php.ini
#   add and validate PHP FPM configuration
COPY ./php-fpm.conf /usr/local/etc/php-fpm.d/zz-www.conf
RUN php-fpm --test
#   expose volumes for app and config repositories clones
ENV WIKIA_DOCROOT="/usr/wikia/slot1/current/src"
ENV WIKIA_CONFIG_ROOT="/usr/wikia/slot1/current/config"
#   run "sha1sum * | sha1sum" to update this value when this file (or any other in this directory) changes
ENV WIKIA_BASE_IMAGE_HASH="225a68a"
WORKDIR /usr/wikia/slot1/current/src
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
