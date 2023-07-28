ARG HUMHUB_VERSION=1.3.13
FROM composer:1.7 AS builder-composer
FROM alpine:3.8 AS builder
ARG HUMHUB_VERSION
RUN apk update
RUN apk add ca-certificates=20191127-r2 tzdata=2021a-r0 wget=1.20.3-r0 --no-cache
WORKDIR /usr/src/
RUN wget https://github.com/humhub/humhub/archive/v${HUMHUB_VERSION}.tar.gz -q -O humhub.tar.gz \
 && tar xzf humhub.tar.gz \
 && mv humhub-${HUMHUB_VERSION} humhub \
 && rm humhub.tar.gz
WORKDIR /usr/src/humhub
COPY --from=builder-composer /usr/bin/composer /usr/bin/composer
RUN chmod +x /usr/bin/composer
RUN apk add php7=7.3.14-r0 php7-gd=7.3.14-r0 php7-ldap=7.3.14-r0 php7-json=7.3.14-r0 php7-phar=7.3.14-r0 php7-iconv=7.3.14-r0 php7-openssl=7.3.14-r0 php7-curl=7.3.14-r0 php7-ctype=7.3.14-r0 php7-dom=7.3.14-r0 php7-mbstring=7.3.14-r0 php7-simplexml=7.3.14-r0 php7-xml=7.3.14-r0 php7-xmlreader=7.3.14-r0 php7-xmlwriter=7.3.14-r0 php7-zip=7.3.14-r0 php7-fileinfo=7.3.14-r0 --no-cache
RUN composer install --no-ansi --no-dev --no-interaction --no-progress --no-scripts --optimize-autoloader \
 && chmod +x protected/yii \
 && chmod +x protected/yii.bat
RUN apk add nodejs=10.24.1-r0 npm=10.24.1-r0 --no-cache
RUN npm install grunt@1.6.1
RUN npm install grunt-cli@1.4.3 -g
RUN apk add php7-pdo_mysql=7.3.14-r0 --no-cache
RUN grunt build-assets
RUN rm -rf ./node_modules
FROM alpine:3.8
ARG HUMHUB_VERSION
RUN apk add curl=7.66.0-r4 ca-certificates=20191127-r2 tzdata=2021a-r0 php7=7.3.14-r0 php7-fpm=7.3.14-r0 php7-pdo_mysql=7.3.14-r0 php7-gd=7.3.14-r0 php7-ldap=7.3.14-r0 php7-json=7.3.14-r0 php7-phar=7.3.14-r0 php7-iconv=7.3.14-r0 php7-openssl=7.3.14-r0 php7-curl=7.3.14-r0 php7-ctype=7.3.14-r0 php7-dom=7.3.14-r0 php7-mbstring=7.3.14-r0 php7-simplexml=7.3.14-r0 php7-xml=7.3.14-r0 php7-xmlreader=7.3.14-r0 php7-xmlwriter=7.3.14-r0 php7-zip=7.3.14-r0 php7-sqlite3=7.3.14-r0 php7-intl=7.3.14-r0 php7-apcu php7-exif=7.3.14-r0 php7-fileinfo=7.3.14-r0 php7-session=7.3.14-r0 supervisor=3.3.5-r0 nginx=1.16.1-r3 sqlite=3.28.0-r3 --no-cache \
 && rm -rf /var/cache/apk/*
RUN BUILD_DEPS="gettext" RUNTIME_DEPS="libintl" \
 && set -x \
 && apk add $RUNTIME_DEPS --no-cache --update \
 && apk add $BUILD_DEPS --no-cache --virtual build_deps \
 && cp /usr/bin/envsubst /usr/local/bin/envsubst \
 && apk del build_deps
ENV PHP_POST_MAX_SIZE="10M"
ENV PHP_UPLOAD_MAX_FILESIZE="10M"
ENV PHP_MAX_EXECUTION_TIME="60"
ENV PHP_MEMORY_LIMIT="512M"
RUN chown -R nginx:nginx /var/lib/nginx/ \
 && touch /var/run/supervisor.sock \
 && chmod 777 /var/run/supervisor.sock
COPY --chown=nginx:nginx --from=builder /usr/src/humhub /var/www/localhost/htdocs/
COPY --chown=nginx:nginx humhub/ /var/www/localhost/htdocs/
RUN mkdir -p /usr/src/humhub/protected/config/ \
 && cp -R /var/www/localhost/htdocs/protected/config/* /usr/src/humhub/protected/config/ \
 && echo "v${HUMHUB_VERSION}" > /usr/src/humhub/.version
COPY etc/ /etc/
COPY docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
RUN chmod 600 /etc/crontabs/nginx \
 && chmod +x /usr/local/bin/docker-entrypoint.sh
VOLUME /var/www/localhost/htdocs/uploads
VOLUME /var/www/localhost/htdocs/protected/config
VOLUME /var/www/localhost/htdocs/protected/modules
EXPOSE 80/tcp
ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
CMD ["supervisord", "-n", "-c", "/etc/supervisord.conf"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
