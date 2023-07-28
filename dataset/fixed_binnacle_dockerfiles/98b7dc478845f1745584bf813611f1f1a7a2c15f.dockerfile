#   Dependency for php7: libwebp library doesn't work correctly with alpine:v3.3 so we are using alpine:edge
FROM alpine:edge
MAINTAINER Onni Hakala - Geniem Oy. <onni.hakala@geniem.com>
#   Install dependencies and small amount of devtools
RUN apk add curl=8.0.1-r1 bash=5.2.15-r2 git=2.40.0-r0 openssh-client nano=7.2-r0 nginx=1.22.1-r0 ca-certificates=20230106-r0 libssh2=1.10.0-r3 libpng=1.6.39-r2 freetype=2.13.0-r2 libjpeg-turbo=2.1.5.1-r1 libgcc=12.2.1_git20220924-r9 libxml2=2.10.4-r0 libstdc++=12.2.1_git20220924-r9 icu-libs=72.1-r2 libltdl=2.4.7-r1 libmcrypt=2.5.8-r10 ncurses=6.4_p20230401-r2 msmtp=1.8.23-r0 mysql-client=10.11.2-r4 gettext=0.21.1-r2 tzdata=2023c-r0 --update \
 && apk add musl=1.2.3_git20230322-r0 -u \
 && apk add php7-pdo_mysql php7-mysqli php7-mysqlnd php7-mcrypt php7 php7-session php7-fpm php7-json php7-zlib php7-xml php7-pdo php7-gd php7-curl php7-opcache php7-ctype php7-mbstring php7-soap php7-intl php7-bcmath php7-dom php7-xmlreader php7-openssl php7-phar php7-redis php7-mongodb --update-cache --repository http://dl-4.alpinelinux.org/alpine/edge/testing/ \
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
 && apk add less=608-r1 \
 && curl -L https://raw.githubusercontent.com/kvz/cronlock/master/cronlock -o /usr/local/bin/cronlock \
 && chmod +rx /usr/local/bin/cronlock \
 && curl -L -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer \
 && chmod +rx /usr/local/bin/composer \
 && composer global require hirak/prestissimo \
 && rm -rf /var/cache/apk/* \
 && rm -rf /tmp/*
#  #
#   Add Project files like nginx and php-fpm processes and configs
#   Also custom scripts and bashrc
#  #
COPY rootfs/ /
#   Update path with composer files + wpcs
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
#   Set default path to project folder for easier running commands in project
WORKDIR ${PROJECT_ROOT}
EXPOSE ${PORT}
ENTRYPOINT ["/init"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
