#   this is the dev container and doesn't contain CRON or composer
#   it maps the PWD straight through
#   Multi-stage build
#   Stage 0
#   Compile xsendfile apache module
FROM alpine:3.8 AS sendfile
COPY docker/mod_xsendfile.c /mod_xsendfile.c
RUN apk update \
 && apk upgrade \
 && apk add gcc=6.4.0-r9 musl-dev=1.1.19-r11 apache2-dev=2.4.43-r0 apache2=2.4.43-r0
RUN cd / \
 && apxs -cia mod_xsendfile.c
#   Stage 1
#   Run composer
FROM composer:1.6 AS composer
COPY ./composer.json /app
COPY ./composer.lock /app
RUN composer install --no-interaction
#   Stage 2
#   Run webpack
FROM node:latest AS webpack
WORKDIR /app
#   Install webpack
RUN npm install webpack@5.79.0 -g
#   Copy package.json and the webpack config file
COPY webpack.config.js .
COPY package.json .
#   Install npm packages
RUN npm install --only=prod
#   Copy ui folder
COPY ./ui ./ui
#   Build webpack
RUN npm run build
#   Stage 1
#   Build the CMS container
FROM alpine:3.8
MAINTAINER Xibo Signage <support@xibosignage.com>
#   Install apache, PHP, and supplimentary programs.
RUN apk update \
 && apk upgrade \
 && apk add tar=1.32-r0 bash=4.4.19-r1 curl=7.61.1-r3 php7=7.2.26-r0 php7-apache2=7.2.26-r0 php7-zmq=1.1.3-r2 php7-json=7.2.26-r0 php7-gd=7.2.26-r0 php7-mcrypt=1.0.1-r0 php7-dom=7.2.26-r0 php7-pdo=7.2.26-r0 php7-zip=7.2.26-r0 php7-pdo_mysql=7.2.26-r0 php7-gettext=7.2.26-r0 php7-soap=7.2.26-r0 php7-iconv=7.2.26-r0 php7-curl=7.2.26-r0 php7-session=7.2.26-r0 php7-ctype=7.2.26-r0 php7-fileinfo=7.2.26-r0 php7-xml=7.2.26-r0 php7-simplexml=7.2.26-r0 php7-mbstring=7.2.26-r0 php7-memcached=3.0.4-r1 php7-zlib php7-mongodb=1.4.4-r0 mysql-client=10.2.32-r0 ssmtp=2.64-r13 apache2=2.4.43-r0 ca-certificates=20191127-r2 tzdata=2020a-r0 \
 && rm -rf /var/cache/apk/*
#   Add all necessary config files in one layer
COPY docker/ /
#   Adjust file permissions as appropriate
RUN chmod +x /entrypoint.sh /usr/local/bin/httpd-foreground /usr/local/bin/wait-for-command.sh /etc/periodic/15min/cms-db-backup \
 && mkdir -p /run/apache2 \
 && rm /etc/apache2/conf.d/info.conf \
 && rm /etc/apache2/conf.d/userdir.conf \
 && chmod 777 /tmp
#   Add xsendfile Module
COPY --from=sendfile /usr/lib/apache2/mod_xsendfile.so /usr/lib/apache2/mod_xsendfile.so
#   Update the PHP.ini file
RUN sed -i "s/error_reporting = .*$/error_reporting = E_ERROR | E_WARNING | E_PARSE/" /etc/php7/php.ini \
 && sed -i "s/session.gc_probability = .*$/session.gc_probability = 1/" /etc/php7/php.ini \
 && sed -i "s/session.gc_divisor = .*$/session.gc_divisor = 100/" /etc/php7/php.ini
#   Set some environment variables
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV CMS_DEV_MODE="true" \
    MYSQL_HOST="db" \
    MYSQL_PORT="3306" \
    MYSQL_USER="root" \
    MYSQL_DATABASE="cms" \
    CMS_SERVER_NAME="localhost" \
    CMS_ALIAS="none" \
    CMS_PHP_SESSION_GC_MAXLIFETIME="1440" \
    CMS_PHP_POST_MAX_SIZE="2G" \
    CMS_PHP_UPLOAD_MAX_FILESIZE="2G" \
    CMS_PHP_MAX_EXECUTION_TIME="300" \
    CMS_PHP_MEMORY_LIMIT="256M" \
    CMS_APACHE_START_SERVERS="2" \
    CMS_APACHE_MIN_SPARE_SERVERS="5" \
    CMS_APACHE_MAX_SPARE_SERVERS="10" \
    CMS_APACHE_MAX_REQUEST_WORKERS="60" \
    CMS_APACHE_MAX_CONNECTIONS_PER_CHILD="300"
#   Expose port 80
EXPOSE 80/tcp
#   Map the source files into /var/www/cms
#   Create library and cache, because they might not exist
#   Create /var/www/backup so that we have somewhere for entrypoint to log errors.
RUN mkdir -p /var/www/cms \
 && mkdir -p /var/www/cms/library/temp \
 && mkdir -p /var/www/cms/cache \
 && mkdir -p /var/www/backup
#   Composer generated vendor files
COPY --from=composer /app /var/www/cms
#   Copy dist built webpack app folder to web
COPY --from=webpack /app/web/dist /var/www/cms/web/dist
#   All other files (.dockerignore excludes things we don't want)
COPY . /var/www/cms
#   Run entry
CMD ["/entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
