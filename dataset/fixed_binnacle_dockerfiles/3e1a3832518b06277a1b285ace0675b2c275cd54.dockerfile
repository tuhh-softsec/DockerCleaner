#  ##
#   Ambientum
#
#   Repository:    PHP
#   Image:         CLI/Base
#   Version:       7.1.x
#   Strategy:      PHP From PHP-Alpine Repository (CODECASTS) (https://php-alpine.codecasts.rocks)
#   Base distro:   Alpine 3.7
#
FROM alpine:3.7
#   Repository/Image Maintainer
LABEL maintainer="Diego Hernandes <iamhernandev@gmail.com>"
#   Variables for enabling NewRelic
ENV FRAMEWORK="laravel" \
    OPCACHE_MODE="normal" \
    PHP_MEMORY_LIMIT="256M" \
    XDEBUG_ENABLED="false" \
    NR_ENABLED="false" \
    NR_APP_NAME="" \
    NR_LICENSE_KEY="" \
    TERM="xterm-256color" \
    COLORTERM="truecolor" \
    COMPOSER_PROCESS_TIMEOUT="1200"
#   Add the ENTRYPOINT script
COPY start.sh /scripts/start.sh
COPY bashrc /home/ambientum/.bashrc
COPY bashrc /home/bashrc
#   Install PHP From DotDeb, Common Extensions, Composer and then cleanup
RUN echo "---> Enabling PHP-Alpine" \
 && apk add wget=1.20.3-r0 --update \
 && wget -O /etc/apk/keys/php-alpine.rsa.pub http://php.codecasts.rocks/php-alpine.rsa.pub \
 && echo "@php http://php.codecasts.rocks/v3.7/php-7.1" >> /etc/apk/repositories \
 && apk add curl=7.61.1-r3 bash=4.4.19-r1 fontconfig=2.12.6-r0 imagemagick=7.0.7.11-r1 libxrender=0.9.10-r2 libxext=1.3.3-r2 nano=2.9.1-r0 vim=8.0.1359-r2 git=2.15.4-r0 unzip=6.0-r3 wget=1.20.3-r0 make=4.2.1-r0 sudo=1.8.21_p2-r1 --update \
 && echo "---> Preparing and Installing PHP" \
 && apk add php7@php php7-apcu@php php7-bcmath@php php7-bz2@php php7-calendar@php php7-curl@php php7-ctype@php php7-exif@php php7-fpm@php php7-gd@php php7-gmp@php php7-iconv@php php7-imagick@php php7-imap@php php7-intl@php php7-json@php php7-mailparse@php php7-mbstring@php php7-mcrypt@php php7-mysqli@php php7-mysqlnd@php php7-pdo_mysql@php php7-memcached=3.0.4-r0 php7-mongodb@php php7-opcache@php php7-pdo_pgsql@php php7-pgsql@php php7-posix@php php7-redis@php php7-soap@php php7-sqlite3@php php7-pdo_sqlite@php php7-xdebug@php php7-xml@php php7-xmlreader@php php7-openssl@php php7-phar@php php7-xsl@php php7-zip@php php7-zlib@php php7-pcntl@php php7-cgi@php php7-phpdbg@php --update \
 && sudo ln -s /usr/bin/php7 /usr/bin/php \
 && sudo ln -s /usr/bin/php-cgi7 /usr/bin/php-cgi \
 && sudo ln -s /usr/sbin/php-fpm7 /usr/sbin/php-fpm \
 && echo "---> Installing Composer" \
 && curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer \
 && echo "---> Cleaning up" \
 && rm -rf /tmp/* \
 && echo "---> Adding the ambientum user" \
 && adduser -D -u 1000 ambientum \
 && mkdir -p /var/www/app \
 && chown -R ambientum:ambientum /var/www \
 && wget -O /tini https://github.com/krallin/tini/releases/download/v0.14.0/tini-static \
 && chmod +x /tini \
 && echo "---> Configuring PHP" \
 && echo "ambientum ALL = ( ALL ) NOPASSWD: ALL" >> /etc/sudoers \
 && sed -i "/user = .*/c\user = ambientum" /etc/php7/php-fpm.d/www.conf \
 && sed -i "/^group = .*/c\group = ambientum" /etc/php7/php-fpm.d/www.conf \
 && sed -i "/listen.owner = .*/c\listen.owner = ambientum" /etc/php7/php-fpm.d/www.conf \
 && sed -i "/listen.group = .*/c\listen.group = ambientum" /etc/php7/php-fpm.d/www.conf \
 && sed -i "/listen = .*/c\listen = [::]:9000" /etc/php7/php-fpm.d/www.conf \
 && sed -i "/;access.log = .*/c\access.log = /proc/self/fd/2" /etc/php7/php-fpm.d/www.conf \
 && sed -i "/;clear_env = .*/c\clear_env = no" /etc/php7/php-fpm.d/www.conf \
 && sed -i "/;catch_workers_output = .*/c\catch_workers_output = yes" /etc/php7/php-fpm.d/www.conf \
 && sed -i "/pid = .*/c\;pid = /run/php/php7.1-fpm.pid" /etc/php7/php-fpm.conf \
 && sed -i "/;daemonize = .*/c\daemonize = yes" /etc/php7/php-fpm.conf \
 && sed -i "/error_log = .*/c\error_log = /proc/self/fd/2" /etc/php7/php-fpm.conf \
 && sed -i "/post_max_size = .*/c\post_max_size = 1000M" /etc/php7/php.ini \
 && sed -i "/upload_max_filesize = .*/c\upload_max_filesize = 1000M" /etc/php7/php.ini \
 && sed -i "/zend_extension=xdebug.so/c\;zend_extension=xdebug.so" /etc/php7/conf.d/00_xdebug.ini \
 && echo "---> Adding Support for NewRelic" \
 && mkdir /tmp/newrelic \
 && cd /tmp/newrelic \
 && wget -r -l1 -nd -A"linux-musl.tar.gz" https://download.newrelic.com/php_agent/release/ \
 && gzip -dc newrelic*.tar.gz | tar xf - \
 && cd newrelic-php5* \
 && rm -f /usr/lib/php7/modules/newrelic.so \
 && cp ./agent/x64/newrelic-20160303.so /usr/lib/php7/modules/newrelic.so \
 && cp ./daemon/newrelic-daemon.x64 /usr/bin/newrelic-daemon \
 && cp ./scripts/newrelic.ini.template /scripts/newrelic.ini \
 && mkdir /var/log/newrelic \
 && chown -R ambientum:ambientum /var/log/newrelic \
 && chown -R ambientum:ambientum /home/ambientum \
 && chmod +x /scripts/start.sh \
 && rm -rf /tmp/*
#   Define the running user
USER ambientum
#   Application directory
WORKDIR "/var/www/app"
#   Environment variables
ENV PATH="/home/ambientum/.composer/vendor/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
#   Define the entry point that tries to enable newrelic
ENTRYPOINT ["/tini", "--", "/scripts/start.sh"]
#   As non daemon and single base image, it may be used as cli container
CMD ["/bin/bash"]
# Please add your HEALTHCHECK here!!!
