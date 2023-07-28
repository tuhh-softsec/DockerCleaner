#
#   Dockerfile for Apache/PHP/MySQL
#
FROM alpine:3.7
MAINTAINER Azure App Service Container Images <appsvc-images@microsoft.com>
#   ========
#   ENV vars
#   ========
#  apache httpd
ENV HTTPD_HOME="\"/etc/apache2\""
ENV HTTPD_CONF_DIR="\"$HTTPD_HOME/conf.d\""
ENV HTTPD_CONF_FILE="\"$HTTPD_HOME/httpd.conf\""
ENV HTTPD_LOG_DIR="\"/home/LogFiles/httpd\""
ENV HTTPD_PID_DIR="\"/run/apache2\""
ENV PATH="$HTTPD_HOME/bin:$PATH"
#   mariadb
ENV MARIADB_DATA_DIR="\"/home/data/mysql\""
ENV MARIADB_LOG_DIR="\"/home/LogFiles/mysql\""
#   php
ENV PHP_HOME="\"/etc/php7\""
ENV PHP_CONF_DIR="$PHP_HOME"
ENV PHP_CONF_FILE="$PHP_CONF_DIR\"/php.ini\""
ENV PHP_CONF_DIR_SCAN="$PHP_CONF_DIR\"/conf.d\""
ENV PATH="$PHP_HOME/bin:$PATH"
#   supervisor
ENV SUPERVISOR_LOG_DIR="\"/home/LogFiles/supervisor\""
#   phpmyadmin
ENV PHPMYADMIN_VERSION="\"4.8.0\""
ENV PHPMYADMIN_DOWNLOAD_URL="\"https://files.phpmyadmin.net/phpMyAdmin/$PHPMYADMIN_VERSION/phpMyAdmin-$PHPMYADMIN_VERSION-all-languages.tar.gz\""
ENV PHPMYADMIN_SHA256="\"1e83d60627d8036261af71220eae9ffd8d3150778702720905bcfa85c40ce346\""
ENV PHPMYADMIN_SOURCE="\"/usr/src/phpmyadmin\""
ENV PHPMYADMIN_HOME="\"/home/phpmyadmin\""
#   ssh
ENV SSH_PASSWD="\"root:Docker!\""
#   app
ENV APP_HOME="\"/home/site/wwwroot\""
#
ENV DOCKER_BUILD_HOME="\"/dockerbuild\""
#   ensure www-data user exists
RUN set -x \
 && addgroup -g 82 -S www-data \
 && adduser -u 82 -D -S -G www-data www-data
#   ---------------
#   2. apache httpd
#   ---------------
RUN mkdir -p "$DOCKER_BUILD_HOME"
WORKDIR $DOCKER_BUILD_HOME
RUN set -x \
 && apk update \
 && apk add apache2=2.4.41-r0 apache2-ssl=2.4.41-r0 \
 && apk add mariadb=10.1.41-r0 mariadb-client=10.1.41-r0 --update \
 && apk add php7-common=7.1.33-r0 php7-iconv=7.1.33-r0 php7-json=7.1.33-r0 php7-gd=7.1.33-r0 php7-curl=7.1.33-r0 php7-xml=7.1.33-r0 php7-simplexml=7.1.33-r0 php7-pgsql=7.1.33-r0 php7-imap=7.1.33-r0 php7-cgi=7.1.33-r0 fcgi=2.4.0-r8 \
 && apk add php7-pdo=7.1.33-r0 php7-pdo_pgsql=7.1.33-r0 php7-soap=7.1.33-r0 php7-xmlrpc=7.1.33-r0 php7-posix=7.1.33-r0 php7-mcrypt=7.1.33-r0 php7-gettext=7.1.33-r0 php7-ldap=7.1.33-r0 php7-ctype=7.1.33-r0 php7-dom=7.1.33-r0 \
 && apk add php7-apache2=7.1.33-r0 php7-embed=7.1.33-r0 php7-session=7.1.33-r0 php7-mbstring=7.1.33-r0 \
 && apk add php7-mysqli=7.1.33-r0 php7-opcache=7.1.33-r0 php7-xdebug=2.5.5-r0 \
 && mkdir -p $PHPMYADMIN_SOURCE \
 && cd $PHPMYADMIN_SOURCE \
 && wget -O phpmyadmin.tar.gz "$PHPMYADMIN_DOWNLOAD_URL" --no-check-certificate \
 && echo "$PHPMYADMIN_SHA256 *phpmyadmin.tar.gz" | sha256sum -c - \
 && apk add openssh-server=7.5_p1-r10 --update \
 && echo "$SSH_PASSWD" | chpasswd \
 && apk update \
 && apk add openrc=0.24.1-r4 \
 && sed -i 's/"cgroup_add_service/" # cgroup_add_service/g' /lib/rc/sh/openrc-run.sh \
 && apk update \
 && apk add supervisor=3.3.3-r1 \
 && rm -rf /var/cache/apk/* /tmp/*
#   =========
#   Configure
#   =========
#
#   httpd
COPY httpd.conf $HTTPD_HOME/
COPY httpd-modules.conf $HTTPD_CONF_DIR/
COPY httpd-php.conf $HTTPD_CONF_DIR/
#   php
COPY php.ini $PHP_CONF_DIR/
COPY xdebug.ini $PHP_CONF_DIR_SCAN/
COPY php-opcache.ini $PHP_CONF_DIR_SCAN/
#   phpmyadmin
COPY httpd-phpmyadmin.conf $HTTPD_CONF_DIR/
COPY phpmyadmin-config.inc.php $PHPMYADMIN_SOURCE/
COPY mariadb.cnf /etc/mysql/
#   ssh
COPY sshd_config /etc/ssh/
RUN set -ex \
 && test ! -d /var/lib/php/sessions \
 && mkdir -p /var/lib/php/sessions \
 && chown www-data:www-data /var/lib/php/sessions
#  #
#  RUN set -ex \
#  && test ! -d /var/www 
RUN set -ex \
 && mkdir -p /var/www \
 && chown -R www-data:www-data /var/www \
 && rm -rf /var/log/httpd \
 && ln -s $HTTPD_LOG_DIR /var/log/httpd \
 && rm -rf /var/log/mysql \
 && ln -s $MARIADB_LOG_DIR /var/log/mysql \
 && rm -rf /var/log/supervisor \
 && ln -s $SUPERVISOR_LOG_DIR /var/log/supervisor \
 && ln -s $PHPMYADMIN_HOME /var/www/phpmyadmin \
 && ln -s $APP_HOME /var/www/wwwroot \
 && rm -f /etc/supervisord.conf
#
#   =====
#   final
#   =====
COPY supervisord.conf /etc/
COPY entrypoint.sh /usr/local/bin/
RUN chmod u+x /usr/local/bin/entrypoint.sh
EXPOSE 2222/tcp 80/tcp
ENTRYPOINT ["entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
