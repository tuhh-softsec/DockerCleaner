#
#  Dockerfile for moodle
#
FROM appsvcorg/nginx-fpm:php7.2.13
LABEL maintainer="=\"Azure App Service Container Images <appsvc-images@microsoft.com>\""
#  ========
#  ENV vars
#  ========
#  ssh
ENV SSH_PASSWD="\"root:Docker!\""
# nginx
ENV NGINX_VERSION="1.14.0"
ENV NGINX_LOG_DIR="\"/home/LogFiles/nginx\""
# php
ENV PHP_HOME="\"/usr/local/etc/php\""
ENV PHP_CONF_DIR="$PHP_HOME"
ENV PHP_CONF_FILE="$PHP_CONF_DIR\"/php.ini\""
#  mariadb
ENV MARIADB_DATA_DIR="\"/home/data/mysql\""
ENV MARIADB_LOG_DIR="\"/home/LogFiles/mysql\""
#  phpmyadmin
ENV PHPMYADMIN_SOURCE="\"/usr/src/phpmyadmin\""
ENV PHPMYADMIN_HOME="\"/home/phpmyadmin\""
#  moodle
ENV MOODLE_HOME="\"/home/site/wwwroot\""
ENV MOODLE_SOURCE="\"/usr/src/moodle\""
#  redis
ENV PHPREDIS_VERSION="3.1.2"
#  memcached
ENV MEMCACHED_VERSION="1.5.12"
ENV MEMCACHED_SHA1="f67096ba64b0c47668bcad5b680010c4f8987d4c"
#
ENV DOCKER_BUILD_HOME="\"/dockerbuild\""
#  ====================
#  Download and Install
#  ~. tools
#  1. redis
#  2. moodle
#  ====================
WORKDIR $DOCKER_BUILD_HOME
RUN set -ex \
 && apk update \
 && apk add --no-cache redis \
 && docker-php-source extract \
 && curl -L -o /tmp/redis.tar.gz https://github.com/phpredis/phpredis/archive/$PHPREDIS_VERSION.tar.gz \
 && tar xfz /tmp/redis.tar.gz \
 && rm -r /tmp/redis.tar.gz \
 && mv phpredis-$PHPREDIS_VERSION /usr/src/php/ext/redis \
 && apk add --no-cache --virtual .build-dependencies $PHPIZE_DEPS zlib-dev cyrus-sasl-dev git autoconf g++ libtool make pcre-dev tini libintl icu icu-dev libxml2-dev postgresql-dev freetype-dev libjpeg-turbo-dev libpng-dev gmp gmp-dev libmemcached-dev imagemagick-dev libssh2 libssh2-dev libxslt-dev libmemcached-libs zlib \
 && pecl install igbinary \
 && (pecl install --nobuild memcached \
 && cd "$( pecl config-get temp_dir ;)/memcached" \
 && phpize \
 && ./configure --enable-memcached-igbinary \
 && make -j$( nproc ;) \
 && make install \
 && cd /tmp/ ) \
 && docker-php-ext-enable igbinary memcached \
 && docker-php-ext-configure gd --with-png-dir=/usr --with-jpeg-dir=/usr \
 && docker-php-ext-install -j "$( nproc ;)" gd zip redis xmlrpc soap intl \
 && find /usr/local/lib/php/extensions -name '*.a' -delete \
 && find /usr/local/lib/php/extensions -name '*.so' -exec strip --strip-all '{}' ; \
 && runDeps="$( scanelf --needed --nobanner --recursive /usr/local/lib/php/extensions | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add --virtual .phpext-rundeps $runDeps \
 && apk del .build-dependencies \
 && docker-php-source delete \
 && mkdir -p /usr/local/php/tmp \
 && chmod 777 /usr/local/php/tmp \
 && addgroup -g 11211 memcache \
 && adduser -D -u 11211 -G memcache memcache \
 && apk add --no-cache --virtual .build-deps ca-certificates coreutils cyrus-sasl-dev dpkg-dev dpkg gcc libc-dev libevent-dev libressl linux-headers make perl perl-utils tar wget \
 && wget -O memcached.tar.gz "https://memcached.org/files/memcached-$MEMCACHED_VERSION.tar.gz" \
 && echo "$MEMCACHED_SHA1 memcached.tar.gz" | sha1sum -c - \
 && mkdir -p /usr/src/memcached \
 && tar -xzf memcached.tar.gz -C /usr/src/memcached --strip-components=1 \
 && rm memcached.tar.gz \
 && cd /usr/src/memcached \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && enableExtstore="$( case "$gnuArch" in (s390x-*) ;;(*) echo '--enable-extstore' ;; esac ;)" \
 && ./configure --build="$gnuArch" --enable-sasl $enableExtstore \
 && make -j "$( nproc ;)" \
 && make test \
 && make install \
 && cd / \
 && rm -rf /usr/src/memcached \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add --virtual .memcached-rundeps $runDeps \
 && apk del .build-deps \
 && memcached -V \
 && mkdir -p $MOODLE_SOURCE \
 && apk update \
 && apk upgrade \
 && rm -rf /var/cache/apk/* \
 && rm -rf /tmp/*
#  =========
#  Configure
#  =========
#  nginx
COPY nginx_conf/. /etc/nginx/conf.d/
COPY nginx.conf /etc/nginx/nginx.conf
#  =====
#  final
#  =====
COPY moodle_src/. $MOODLE_SOURCE/
#  php
COPY opcache-recommended.ini /usr/local/etc/php/conf.d/opcache-recommended.ini
COPY php.ini /usr/local/etc/php/php.ini
COPY www.conf /usr/local/etc/php/conf.d/
COPY zz-docker.conf /usr/local/etc/php-fpm.d/zz-docker.conf
#  mariadb
COPY my.cnf /etc/mysql/my.cnf
#  phpmyadmin
COPY phpmyadmin-default.conf $PHPMYADMIN_SOURCE/
#
COPY entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/entrypoint.sh
EXPOSE 2222/tcp 80/tcp
ENTRYPOINT ["entrypoint.sh"]
