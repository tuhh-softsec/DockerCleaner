#
#   Dockerfile for WordPress
#
FROM appsvcorg/nginx-fpm:php7.2.13
MAINTAINER Azure App Service Container Images <appsvc-images@microsoft.com>
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
#   redis
ENV PHPREDIS_VERSION="3.1.2"
#   wordpress
ENV WORDPRESS_SOURCE="\"/usr/src/wordpress\""
ENV WORDPRESS_HOME="\"/home/site/wwwroot\""
#
ENV DOCKER_BUILD_HOME="\"/dockerbuild\""
#   ====================
#   Download and Install
#   ~. tools
#   1. redis
#   2. wordpress
#   ====================
WORKDIR $DOCKER_BUILD_HOME
RUN set -ex \
 && apk update \
 && apk add redis --no-cache \
 && curl -O https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli.phar \
 && chmod +x wp-cli.phar \
 && mv wp-cli.phar /usr/local/bin/wp \
 && docker-php-source extract \
 && curl -L -o /tmp/redis.tar.gz https://github.com/phpredis/phpredis/archive/$PHPREDIS_VERSION.tar.gz \
 && tar xfz /tmp/redis.tar.gz \
 && rm -r /tmp/redis.tar.gz \
 && mv phpredis-$PHPREDIS_VERSION /usr/src/php/ext/redis \
 && apk add libjpeg-turbo-dev libpng-dev --no-cache --virtual .build-deps \
 && docker-php-ext-configure gd --with-png-dir=/usr --with-jpeg-dir=/usr \
 && docker-php-ext-install gd zip redis \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local/lib/php/extensions | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add $runDeps --virtual .wordpress-phpexts-rundeps \
 && apk del .build-deps \
 && docker-php-source delete \
 && mkdir -p $WORDPRESS_SOURCE \
 && apk update \
 && apk upgrade \
 && rm -rf /var/cache/apk/* \
 && rm -rf /tmp/*
#   =========
#   Configure
#   =========
#   nginx
COPY nginx.conf /etc/nginx/nginx.conf
COPY default.conf /etc/nginx/conf.d/default.conf
#   =====
#   final
#   =====
COPY wp-config.php $WORDPRESS_SOURCE/
COPY uploads.ini /usr/local/etc/php/conf.d/uploads.ini
#   phpmyadmin
COPY phpmyadmin-default.conf $PHPMYADMIN_SOURCE/phpmyadmin-default.conf
#   nginx
COPY spec-settings.conf /etc/nginx/conf.d/spec-settings.conf
COPY entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/entrypoint.sh
EXPOSE 2222/tcp 80/tcp
ENTRYPOINT ["entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
