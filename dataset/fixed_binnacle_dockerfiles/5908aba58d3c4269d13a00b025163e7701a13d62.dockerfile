FROM appsvcorg/nginx-fpm:php7.2.13
LABEL maintainer="=\"Azure App Service Container Images <appsvc-images@microsoft.com>\""
#   ========
#   ENV vars
#   ========
#
ENV DOCKER_BUILD_HOME="\"/dockerbuild\""
#   drupal 
ENV DRUPAL_HOME="\"/home/site/wwwroot\""
ENV DRUPAL_PRJ="\"/home/drupal_prj\""
#   mariadb
ENV MARIADB_DATA_DIR="\"/home/data/mysql\""
ENV MARIADB_LOG_DIR="\"/home/LogFiles/mysql\""
#   phpmyadmin
ENV PHPMYADMIN_SOURCE="\"/usr/src/phpmyadmin\""
ENV PHPMYADMIN_HOME="\"/home/phpmyadmin\""
#   drupal
ENV DRUPAL_SOURCE="\"/usr/src/drupal\""
#  nginx
ENV NGINX_LOG_DIR="\"/home/LogFiles/nginx\""
#  varnish
ENV VARNISH_LOG_DIR="\"/home/LogFiles/varnish\""
#  php
ENV PHP_CONF_FILE="\"/usr/local/etc/php/php.ini\""
#   Composer
#   Updation: https://getcomposer.org/download/
ENV COMPOSER_DOWNLOAD_URL="\"https://getcomposer.org/installer\""
#   ====================
#   Download and Install
#   ~. essentials
#   1. Drupal
#   ====================
RUN mkdir -p $DOCKER_BUILD_HOME
WORKDIR $DOCKER_BUILD_HOME
RUN set -ex \
 && docker-php-source extract \
 && apk add zlib-dev cyrus-sasl-dev git autoconf g++ libtool make pcre-dev tini libintl icu icu-dev libxml2-dev postgresql-dev freetype-dev libjpeg-turbo-dev libpng-dev gmp gmp-dev libmemcached-dev imagemagick-dev libssh2 libssh2-dev libxslt-dev $PHPIZE_DEPS --no-cache --virtual .build-dependencies \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ --with-png-dir=/usr/include/ \
 && docker-php-ext-install -j "$( nproc ;)" gd zip \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add $runDeps --virtual .drupal-phpexts-rundeps \
 && apk del .build-dependencies \
 && docker-php-source delete \
 && mkdir -p /usr/local/php/tmp \
 && chmod 777 /usr/local/php/tmp \
 && mkdir -p ${DRUPAL_SOURCE} \
 && curl -sS $COMPOSER_DOWNLOAD_URL | php -- --install-dir=/usr/bin --filename=composer \
 && composer self-update \
 && rm -rf /home/.composer \
 && export COMPOSER_HOME='/root/.composer/' \
 && composer global require consolidation/cgr \
 && composer_home=$( find / -name .composer ;) \
 && ln -s $composer_home/vendor/bin/cgr /usr/local/bin/cgr \
 && cgr drush/drush \
 && ln -s $composer_home/vendor/bin/drush /usr/local/bin/drush \
 && apk update \
 && apk add varnish --no-cache \
 && rm -rf /var/log/varnish \
 && ln -s $VARNISH_LOG_DIR /var/log/varnish \
 && apk upgrade \
 && rm -rf /var/cache/apk/*
#   =========
#   Configure
#   =========
WORKDIR $DRUPAL_HOME
RUN rm -rf $DOCKER_BUILD_HOME
#   mariadb
COPY my.cnf /etc/mysql/
#   nginx
COPY spec-settings.conf /etc/nginx/conf.d/
COPY nginx.conf /etc/nginx/
RUN rm -rf /etc/nginx/conf.d/default.conf
#   php
COPY php.ini $PHP_CONF_FILE
#   drupal
COPY drupal-database-install-tasks.php ${DRUPAL_SOURCE}
#   phpmyadmin
COPY phpmyadmin_src/. $PHPMYADMIN_SOURCE/
#   Varinish
COPY default.vcl /etc/varnish/default.vcl
#   =====
#   final
#   =====
COPY entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/entrypoint.sh
EXPOSE 2222/tcp 80/tcp
ENTRYPOINT ["entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
