ARG PHP_VERSION=7.2.19
FROM --platform=$TARGETPLATFORM php:${PHP_VERSION}-fpm-alpine AS php
LABEL maintainer="khs1994-docker/lnmp <khs1994@khs1994.com>"
ENV TZ="Asia/Shanghai" \
    APP_ENV="development"
ENV PHP_EXTENSION="sockets  sysvmsg  sysvsem  sysvshm"
#   ARG ALPINE_URL=dl-cdn.alpinelinux.org
ARG ALPINE_URL=mirrors.ustc.edu.cn
RUN apk add bash tzdata --no-cache
RUN sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && for extension in $PHP_EXTENSION; do docker-php-ext-install $extension \
 && docker-php-ext-enable $( echo ${extension} | cut -d '-' -f 1 ;) || echo "${extension} install error" \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${extension} | cut -d '-' -f 1 ;).so ; done \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=bcmath \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=zip \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add libzip --no-cache \
 && apk add libzip-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-configure zip --with-libzip \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=gd \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add libpng freetype libjpeg-turbo libxpm libwebp zlib --no-cache \
 && apk add libpng-dev freetype-dev libjpeg-turbo-dev libxpm-dev libwebp-dev zlib-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-configure gd --disable-gd-jis-conv --with-freetype-dir=/usr --with-jpeg-dir=/usr --with-png-dir=/usr --with-webp-dir=/usr --with-xpm-dir=/usr \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && rm -rf /usr/local/include/php/ext/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;) \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=pdo_mysql \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=mysqli \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && rm -rf /usr/local/include/php/ext/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;) \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=calendar \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=pcntl \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=pdo_pgsql \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add libpq --no-cache \
 && apk add postgresql-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=pgsql \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add libpq --no-cache \
 && apk add postgresql-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=gmp \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add gmp --no-cache \
 && apk add gmp-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=exif \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add libexif --no-cache \
 && apk add libexif-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=enchant \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add enchant --no-cache \
 && apk add enchant-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=xmlrpc \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add xmlrpc-c --no-cache \
 && apk add xmlrpc-c-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=xsl \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add libxslt --no-cache \
 && apk add libxslt-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=imap \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add c-client --no-cache \
 && apk add imap-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=bz2 \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add libbz2 --no-cache \
 && apk add bzip2-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=gettext \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add libbz2 --no-cache \
 && apk add gettext-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=intl \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add icu-libs --no-cache \
 && apk add icu-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
RUN export EXT_NAME=tidy \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add tidyhtml-libs --no-cache \
 && apk add tidyhtml-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && docker-php-ext-install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps
ENV PECL_EXTENSION="mongodb  igbinary  redis"
RUN sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && for extension in $PECL_EXTENSION; do pecl install $extension \
 && docker-php-ext-enable $( echo ${extension} | cut -d '-' -f 1 ;) || echo "pecl ${extension} install error" \
 && rm -rf /usr/local/lib/php/doc/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && rm -rf /usr/local/lib/php/test/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && rm -rf /usr/local/include/php/ext/$( echo ${extension} | cut -d '-' -f 1 ;) \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${extension} | cut -d '-' -f 1 ;).so ; done \
 && apk del --no-network --no-cache .build-deps \
 && rm -rf /tmp/pear \
 && rm -rf /usr/local/lib/php/.registry/.channel.pecl.php.net/*
RUN export EXT_NAME=memcached \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add libmemcached-libs --no-cache \
 && apk add libmemcached-dev cyrus-sasl-dev zlib-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && pecl install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps \
 && rm -rf /tmp/pear \
 && rm -rf /usr/local/lib/php/doc/$EXT_NAME \
 && rm -rf /usr/local/lib/php/test/$EXT_NAME \
 && rm -rf /usr/local/include/php/ext/$EXT_NAME \
 && rm -rf /usr/local/lib/php/.registry/.channel.pecl.php.net/*
RUN export EXT_NAME=yaml \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add yaml --no-cache \
 && apk add yaml-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && pecl install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps \
 && rm -rf /tmp/pear \
 && rm -rf /usr/local/lib/php/doc/$EXT_NAME \
 && rm -rf /usr/local/lib/php/test/$EXT_NAME \
 && rm -rf /usr/local/include/php/ext/$EXT_NAME \
 && rm -rf /usr/local/lib/php/.registry/.channel.pecl.php.net/*
RUN export EXT_NAME=xdebug \
 && sed -i "s/dl-cdn.alpinelinux.org/${ALPINE_URL}/g" /etc/apk/repositories \
 && apk add $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && pecl install $EXT_NAME \
 && docker-php-ext-enable $EXT_NAME \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps \
 && rm -rf /tmp/pear \
 && rm -rf /usr/local/lib/php/doc/$EXT_NAME \
 && rm -rf /usr/local/include/php/ext/$EXT_NAME \
 && rm -rf /usr/local/lib/php/.registry/.channel.pecl.php.net/* \
 && mv /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini.default
RUN export EXT_NAME=tideways_xhprof \
 && apk add $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && cd /tmp \
 && wget https://github.com/tideways/php-xhprof-extension/archive/master.zip \
 && unzip master.zip \
 && cd php-xhprof-extension-master \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && docker-php-ext-enable ${EXT_NAME} \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps \
 && mv /usr/local/etc/php/conf.d/docker-php-ext-tideways_xhprof.ini /usr/local/etc/php/conf.d/docker-php-ext-tideways_xhprof.ini.default \
 && rm -rf /tmp/*
RUN export EXT_NAME=swoole \
 && export EXT_VERSION=4.2.12 \
 && apk add nghttp2-libs --no-cache \
 && apk add openssl-dev nghttp2-dev $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && cd /tmp \
 && wget https://pecl.php.net/get/${EXT_NAME}-${EXT_VERSION}.tgz \
 && tar -zxvf ${EXT_NAME}-${EXT_VERSION}.tgz \
 && cd ${EXT_NAME}-${EXT_VERSION} \
 && phpize \
 && ./configure --with-php-config=php-config --enable-openssl --enable-sockets --enable-http2 --enable-mysqlnd \
 && make \
 && make install \
 && docker-php-ext-enable ${EXT_NAME} \
 && strip --strip-all $( php-config --extension-dir ;)/$( echo ${EXT_NAME} | cut -d '-' -f 1 ;).so \
 && apk del --no-network --no-cache .build-deps \
 && rm -rf /usr/local/lib/php/doc/$EXT_NAME \
 && rm -rf /usr/local/include/php/ext/$EXT_NAME \
 && rm -rf /tmp/*
#   创建日志文件夹
RUN mkdir -p /var/log/php-fpm \
 && ln -sf /dev/stdout /var/log/php-fpm/access.log \
 && ln -sf /dev/stderr /var/log/php-fpm/error.log \
 && ln -sf /dev/stderr /var/log/php-fpm/xdebug-remote.log \
 && chmod -R 777 /var/log/php-fpm
WORKDIR /app
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
