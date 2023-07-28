#
#   Dockerfile for moodle
#
FROM appsvcorg/nginx-fpm:php7.2.11
LABEL maintainer="=\"Azure App Service Container Images <appsvc-images@microsoft.com>\""
#   ========
#   ENV vars
#   ========
#   ssh
ENV SSH_PASSWD="\"root:Docker!\""
#  nginx
ENV NGINX_VERSION="1.14.0"
ENV NGINX_LOG_DIR="\"/home/LogFiles/nginx\""
#  php
ENV PHP_HOME="\"/usr/local/etc/php\""
ENV PHP_CONF_DIR="$PHP_HOME"
ENV PHP_CONF_FILE="$PHP_CONF_DIR\"/php.ini\""
#   mariadb
ENV MARIADB_DATA_DIR="\"/home/data/mysql\""
ENV MARIADB_LOG_DIR="\"/home/LogFiles/mysql\""
#   phpmyadmin
ENV PHPMYADMIN_SOURCE="\"/usr/src/phpmyadmin\""
ENV PHPMYADMIN_HOME="\"/home/phpmyadmin\""
#   moodle
ENV MOODLE_HOME="\"/home/site/wwwroot\""
ENV MOODLE_SOURCE="\"/usr/src/moodle\""
#
ENV DOCKER_BUILD_HOME="\"/dockerbuild\""
#   ====================
#   Download and Install
#   ~. tools
#   1. redis
#   2. moodle
#   ====================
WORKDIR $DOCKER_BUILD_HOME
RUN set -ex \
 && apk update \
 && docker-php-source extract \
 && apk add zlib-dev cyrus-sasl-dev git autoconf g++ libtool make pcre-dev tini libintl icu icu-dev libxml2-dev postgresql-dev freetype-dev libjpeg-turbo-dev libpng-dev gmp gmp-dev libmemcached-dev imagemagick-dev libssh2 libssh2-dev libxslt-dev $PHPIZE_DEPS --no-cache --virtual .build-dependencies \
 && docker-php-ext-configure gd --with-png-dir=/usr --with-jpeg-dir=/usr \
 && docker-php-ext-install -j "$( nproc ;)" gd zip pdo_pgsql pdo_mysql opcache xmlrpc soap intl \
 && pecl install apcu \
 && docker-php-ext-enable apcu \
 && find /usr/local/lib/php/extensions -name '*.a' -delete \
 && find /usr/local/lib/php/extensions -name '*.so' -exec strip --strip-all '{}'
#   =========
#   Configure
#   =========
#   nginx
RUN mkdir -p /etc/nginx/conf.d
COPY spec-settings.conf /etc/nginx/conf.d/spec-settings.conf
COPY nginx.conf /etc/nginx/nginx.conf
COPY default.conf /etc/nginx/conf.d/default.conf
#   =====
#   final
#   =====
COPY config.php $MOODLE_SOURCE/
COPY installlib.php $MOODLE_SOURCE/
#   php
COPY opcache-recommended.ini /usr/local/etc/php/conf.d/opcache-recommended.ini
COPY php.ini /usr/local/etc/php/conf.d/php.ini
#   mariadb
RUN set -ex \
 && rm -rf /var/log/mysql \
 && ln -s $MARIADB_LOG_DIR /var/log/mysql
COPY my.cnf /etc/mysql/my.cnf
#   phpmyadmin
COPY phpmyadmin-default.conf $PHPMYADMIN_SOURCE/phpmyadmin-default.conf
#
COPY entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/entrypoint.sh
EXPOSE 2222/tcp 80/tcp
ENTRYPOINT ["entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
