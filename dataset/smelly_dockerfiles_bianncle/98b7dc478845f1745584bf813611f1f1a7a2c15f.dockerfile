#  Dependency for php7: libwebp library doesn't work correctly with alpine:v3.3 so we are using alpine:edge
FROM alpine:edge
MAINTAINER Onni Hakala - Geniem Oy. <onni.hakala@geniem.com>
#  Install dependencies and small amount of devtools
RUN apk add --update curl bash git openssh-client nano nginx ca-certificates libssh2 libpng freetype libjpeg-turbo libgcc libxml2 libstdc++ icu-libs libltdl libmcrypt ncurses msmtp mysql-client gettext tzdata \
 && apk add -u musl \
 && apk add --update-cache --repository http://dl-4.alpinelinux.org/alpine/edge/testing/ php7-pdo_mysql php7-mysqli php7-mysqlnd php7-mcrypt php7 php7-session php7-fpm php7-json php7-zlib php7-xml php7-pdo php7-gd php7-curl php7-opcache php7-ctype php7-mbstring php7-soap php7-intl php7-bcmath php7-dom php7-xmlreader php7-openssl php7-phar php7-redis php7-mongodb \
 && ln -s /etc/php7 /etc/php \
 && ln -s /usr/bin/php7 /usr/bin/php \
 && ln -s /usr/sbin/php-fpm7 /usr/bin/php-fpm \
 && ln -s /usr/lib/php7 /usr/lib/php \
 && rm -rf /var/log/php7 \
 && mkdir -p /var/log/php/ \
 && rm -f /etc/php/php-fpm.d/www.conf \
 && deluser nginx \
 && mkdir -p /var/log/nginx \
 && mkdir -p /tmp/nginx/body \
 && rm -rf /var/www/localhost \
 && mkdir -p /var/www/uploads \
 && mkdir -p /var/www/project/web \
 && rm /var/spool/cron/crontabs/root \
 && curl -L https://github.com/just-containers/s6-overlay/releases/download/v1.17.2.0/s6-overlay-amd64.tar.gz | tar -xvzC / \
 && curl -L https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli.phar -o /usr/local/bin/wp-cli \
 && chmod +rx /usr/local/bin/wp-cli \
 && apk add less \
 && curl -L https://raw.githubusercontent.com/kvz/cronlock/master/cronlock -o /usr/local/bin/cronlock \
 && chmod +rx /usr/local/bin/cronlock \
 && curl -L -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer \
 && chmod +rx /usr/local/bin/composer \
 && composer global require hirak/prestissimo \
 && rm -rf /var/cache/apk/* \
 && rm -rf /tmp/*
# #
#  Add Project files like nginx and php-fpm processes and configs
#  Also custom scripts and bashrc
# #
COPY rootfs/ /
#  Update path with composer files + wpcs
ENV TERM="xterm" \
    DB_HOST="" \
    DB_NAME="" \
    DB_USER=" DB_PASSWORD= DB_PORT=3306" \
    PORT="80" \
    WP_REDIS_PORT="6379" \
    WP_REDIS_DATABASE="0" \
    WP_REDIS_SCHEME="tcp" \
    WP_REDIS_CLIENT="pecl" \
    CRONLOCK_HOST="" \
    PROJECT_ROOT="/var/www/project" \
    WEB_ROOT="/var/www/project/web" \
    NGINX_INCLUDE_DIR="/var/www/project/nginx" \
    NGINX_MAX_BODY_SIZE="64M" \
    NGINX_FASTCGI_TIMEOUT="30" \
    PHP_MEMORY_LIMIT="128M" \
    WP_CORE="/var/www/project/web/wp" \
    SMTP_HOST="172.17.0.1" \
    UPLOADS_ROOT="/var/www/uploads TZ=Europe/Helsinki"
#  Set default path to project folder for easier running commands in project
WORKDIR ${PROJECT_ROOT}
EXPOSE ${PORT}
ENTRYPOINT ["/init"]
