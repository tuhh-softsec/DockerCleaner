FROM alpine:edge
MAINTAINER Onni Hakala - Geniem Oy. <onni.hakala@geniem.com>
#  Install dependencies and small amount of devtools
RUN apk add --update curl bash nano nginx ca-certificates libssh2 libpng freetype libjpeg-turbo libgcc libxml2 libstdc++ icu-libs libltdl libmcrypt ncurses msmtp gettext mysql-client tzdata php5 php5-fpm php5-json php5-zlib php5-xml php5-pdo php5-phar php5-openssl php5-pdo_mysql php5-mysqli php5-gd php5-mcrypt php5-curl php5-opcache php5-ctype php5-intl php5-bcmath php5-dom php5-xmlreader php5-apcu php5-mysql php5-iconv \
 && ln -s /etc/php5 /etc/php \
 && ln -s /usr/lib/php5 /usr/lib/php \
 && mkdir -p /var/log/php/ \
 && mkdir -p /var/log/mail \
 && apk add -u musl \
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
    WP_CORE="/var/www/project/web/wp SMTP_HOST=172.17.0.1" \
    UPLOADS_ROOT="/var/www/uploads TZ=Europe/Helsinki"
#  Set default path to project folder for easier running commands in project
WORKDIR ${PROJECT_ROOT}
EXPOSE ${PORT}
ENTRYPOINT ["/init"]
