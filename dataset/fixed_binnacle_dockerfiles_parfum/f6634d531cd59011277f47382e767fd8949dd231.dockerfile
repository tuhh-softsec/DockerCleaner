ARG PHP_VERSION=7.1.18
FROM php:${PHP_VERSION}-fpm-alpine
LABEL maintainer="khs1994-docker/lnmp <khs1994@khs1994.com>"
ENV TZ="Asia/Shanghai" \
    APP_ENV="development"
#  构建变量默认值为原始值，笔记本构建统一使用 docker-compose 构建
#  国内镜像地址在 docker-compose.yml 中定义
ARG ALPINE_URL=dl-cdn.alpinelinux.org
RUN sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && set -xe \
 && KHS1994_PHP_FPM_APKS=" git openssh-client bash tini tzdata " \
 && KHS1994_PHP_FPM_RUN_DEPS=" libmemcached libpq zlib libpng freetype libjpeg-turbo yaml " \
 && KHS1994_PHP_FPM_BUILD_DEPS=" libressl-dev libmemcached-dev cyrus-sasl-dev postgresql-dev zlib-dev libpng-dev freetype-dev libjpeg-turbo-dev yaml-dev " \
 && apk add --no-cache --virtual .khs1994-php-fpm-run-deps $KHS1994_PHP_FPM_RUN_DEPS \
 && apk add --no-cache linux-headers \
 && apk add --no-cache --virtual .khs1994-php-fpm-apks $KHS1994_PHP_FPM_APKS \
 && apk add --no-cache --virtual .khs1994-php-fpm-build-deps $KHS1994_PHP_FPM_BUILD_DEPS \
 && docker-php-ext-configure gd --disable-gd-jis-conv --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install bcmath pdo_pgsql pdo_mysql zip gd pcntl \
 && apk add --no-cache --virtual .build-deps $PHPIZE_DEPS \
 && pecl install mongodb igbinary redis memcached xdebug yaml swoole \
 && docker-php-ext-enable mongodb redis memcached xdebug yaml igbinary opcache swoole \
 && apk del .build-deps linux-headers .khs1994-php-fpm-build-deps \
 && rm -rf /tmp/*
#  install composer
ENV COMPOSER_ALLOW_SUPERUSER="1" \
    COMPOSER_HOME="/tmp" \
    COMPOSER_VERSION="1.6.5" \
    PS1="[\u@\h \w]# "
#  https://github.com/composer/docker
RUN echo "memory_limit=-1" > "$PHP_INI_DIR/conf.d/memory-limit.ini" \
 && echo "date.timezone=${PHP_TIMEZONE:-PRC}" > "$PHP_INI_DIR/conf.d/date_timezone.ini" \
 && curl -s -f -L -o /tmp/installer.php https://raw.githubusercontent.com/composer/getcomposer.org/b107d959a5924af895807021fcef4ffec5a76aa9/web/installer \
 && php -r " $signature = '544e09ee996cdf60ece3804abc52599c22b1f40f4323403c44d44fdfdd586475ca9813a858088ffbc1f233e9b180f061'; $hash = hash('SHA384', file_get_contents('/tmp/installer.php')); if (!hash_equals($signature, $hash)) { unlink('/tmp/installer.php'); echo 'Integrity check failed, installer is either corrupt or worse.' . PHP_EOL; exit(1); }" \
 && php /tmp/installer.php --no-ansi --install-dir=/usr/bin --filename=composer --version=${COMPOSER_VERSION} \
 && composer --ansi --version --no-interaction \
 && curl -fsSL https://raw.githubusercontent.com/composer/docker/master/1.6/docker-entrypoint.sh > /docker-entrypoint.composer.sh \
 && chmod +x /docker-entrypoint.composer.sh \
 && composer config -g repo.packagist composer https://packagist.phpcomposer.com \
 && rm -rf /tmp/cache /tmp/.htaccess /tmp/installer.php /tmp/*.pub \
 && mv /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini.default \
 && mkdir -p /var/log/php-fpm \
 && ln -sf /dev/stdout /var/log/php-fpm/access.log \
 && ln -sf /dev/stderr /var/log/php-fpm/error.log \
 && ln -sf /dev/stderr /var/log/php-fpm/xdebug-remote.log \
 && chmod -R 777 /var/log/php-fpm
WORKDIR /app
