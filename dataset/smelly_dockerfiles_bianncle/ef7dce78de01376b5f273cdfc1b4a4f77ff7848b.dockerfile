FROM devgeniem/openresty-pagespeed
MAINTAINER Onni Hakala <onni.hakala@geniem.com>
# #
#  Only use these during installation
# #
ARG LANG=C.UTF-8
ARG DEBIAN_FRONTEND=noninteractive
# #
#  Install php7 packages from dotdeb.org
#  - Dotdeb is an extra repository providing up-to-date packages for your Debian servers
# #
RUN apt-get update \
 && apt-get install --no-install-recommends curl nano ca-certificates git mysql-client msmtp netcat less libmcrypt-dev -y \
 && echo "deb http://packages.dotdeb.org jessie all" > /etc/apt/sources.list.d/dotdeb.list \
 && curl -sS https://www.dotdeb.org/dotdeb.gpg | apt-key add - \
 && apt-get update \
 && apt-get install --no-install-recommends php7.0-cli php7.0-common php7.0-apcu php7.0-apcu-bc php7.0-curl php7.0-json php7.0-mcrypt php7.0-opcache php7.0-readline php7.0-xml php7.0-zip php7.0-fpm php7.0-redis php7.0-mongodb php7.0-mysqli php7.0-intl php7.0-gd php7.0-mbstring php7.0-soap php7.0-bcmath php7.0-curl php7.0-ldap php7.0-mcrypt php7.0-imagick libmagickwand-dev -y \
 && cd /tmp \
 && apt-get download cron \
 && dpkg --force-all -i cron*.deb \
 && mkdir -p /var/spool/cron/crontabs \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/doc/* /var/log/apt/* /var/log/*.log
#  Install helpers
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer \
 && composer global require hirak/prestissimo \
 && curl -L https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli.phar -o /usr/local/bin/wp-cli \
 && chmod +rx /usr/local/bin/wp-cli \
 && ln -s /usr/local/bin/wp-cli /usr/bin/wp-cli \
 && curl -L https://raw.githubusercontent.com/kvz/cronlock/master/cronlock -o /usr/local/bin/cronlock \
 && chmod +rx /usr/local/bin/cronlock \
 && ln -s /usr/local/bin/cronlock /usr/bin/cronlock
# #
#  Add Project files like nginx and php-fpm processes and configs
#  Also custom scripts and bashrc
# #
COPY rootfs/ /
#  Run small fixes
RUN set -x \
 && mkdir -p /var/www/uploads \
 && ln -sf /usr/sbin/php-fpm7.0 /usr/sbin/php-fpm \
 && ln -sf /usr/bin/wp /usr/local/bin/wp
#  This is for your project root
ENV PROJECT_ROOT="/var/www/project"
ENV TERM="xterm" \
    MYSQL_PORT="3306" \
    PORT="8080" \
    WEB_USER="wordpress" \
    WEB_GROUP="web" \
    WEB_UID="1000" \
    WEB_GID="1001" \
    REDIS_PORT="6379" \
    REDIS_DATABASE="0" \
    REDIS_PASSWORD="" \
    REDIS_SCHEME="tcp" \
    NGINX_REDIS_CACHE_TTL_DEFAULT="900" \
    NGINX_REDIS_CACHE_TTL_MAX="4h" \
    CRONLOCK_HOST="" \
    WEB_ROOT="${PROJECT_ROOT}/web" \
    WP_CORE="${PROJECT_ROOT}/web/wp" \
    NGINX_INCLUDE_DIR="/var/www/project/nginx" \
    NGINX_MAX_BODY_SIZE="10M" \
    NGINX_BODY_BUFFER_SIZE="32k" \
    NGINX_FASTCGI_TIMEOUT="30" \
    NGINX_ERROR_LEVEL="warn" \
    NGINX_ERROR_LOG="stderr" \
    NGINX_ACCESS_LOG="/dev/stdout" \
    NGINX_CACHE_KEY="wp_:nginx:$real_scheme$request_method$host$request_uri" \
    PHP_MEMORY_LIMIT="128M" \
    PHP_MAX_INPUT_VARS="1000" \
    PHP_ERROR_LOG="/proc/self/fd/1" \
    PHP_ERROR_LOG_LEVEL="warning" \
    PHP_ERROR_LOG_MAX_LEN="8192" \
    PHP_SESSION_REDIS_DB="0" \
    PHP_SESSION_HANDLER="files" \
    PHP_OPCACHE_MAX_FILES="8000" \
    PHP_OPCACHE_MAX_MEMORY="128" \
    SMTP_HOST="172.17.0.1" \
    UPLOADS_ROOT="/var/www/uploads" \
    TZ="Europe/Helsinki"
#  Setup $TZ. Remember to run this again in your own build
RUN dpkg-reconfigure tzdata \
 && chmod +x /etc/cont-init.d/*
#  Set default path to project folder for easier running commands in project
WORKDIR ${PROJECT_ROOT}
EXPOSE ${PORT}
ENTRYPOINT ["/init"]
