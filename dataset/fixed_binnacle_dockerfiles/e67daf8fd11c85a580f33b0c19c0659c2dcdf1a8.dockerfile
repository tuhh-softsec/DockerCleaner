FROM nginx
MAINTAINER Azure App Service Container Images <appsvc-images@microsoft.com>
#   This is latest version, 4-17-2018
#   ========
#   ENV vars
#   ========
#   ssh
ENV SSH_PASSWD="\"root:Docker!\""
#   Composer
#   Updation: https://getcomposer.org/download/
ENV COMPOSER_DOWNLOAD_URL="\"https://getcomposer.org/installer\""
ENV COMPOSER_ALLOW_SUPERUSER="1"
ENV COMPOSER_HOME="/home/.composer"
ENV COMPOSER_VERSION="\"1.6.1\""
#   SHA384SUM https://composer.github.io/installer.sha384sum
ENV COMPOSER_SETUP_SHA="544e09ee996cdf60ece3804abc52599c22b1f40f4323403c44d44fdfdd586475ca9813a858088ffbc1f233e9b180f061"
#  nginx
ENV NGINX_LOG_DIR="\"/home/LogFiles/nginx\""
#  php
ENV PHP_HOME="\"/etc/php/7.0\""
ENV PHP_CONF_DIR="$PHP_HOME\"/cli\""
ENV PHP_CONF_FILE="$PHP_CONF_DIR\"/php.ini\""
#   mariadb
ENV MARIADB_DATA_DIR="\"/home/data/mysql\""
ENV MARIADB_LOG_DIR="\"/home/LogFiles/mysql\""
#   phpmyadmin
ENV PHPMYADMIN_SOURCE="\"/usr/src/phpmyadmin\""
ENV PHPMYADMIN_HOME="\"/home/phpmyadmin\""
#  Web Site Home
ENV HOME_SITE="\"/home/site/wwwroot\""
#
ENV DOCKER_BUILD_HOME="\"/dockerbuild\""
#   ====================
#   Download and Install
#   ~. essentials
#   1. php7.0-common/php7.0-fpm/php-pear/php7.0-apcu
#   2. ssh
#   3. drush
#   4. composer
#   ====================
COPY * /tmp/
#   -------------
#   ~. essentials
#   -------------
RUN set -ex \
 && essentials=" ca-certificates wget make " \
 && apt-get update \
 && apt-get install --no-install-recommends $essentials -y -V \
 && rm -r /var/lib/apt/lists/* \
 && phps=" php7.0-common php7.0-fpm php-pear php7.0-apcu php7.0-gd php7.0-dba php7.0-mysql php7.0-xml php7.0-dev php7.0-mbstring " \
 && apt-get update \
 && apt-get install --no-install-recommends $phps -y -V \
 && pecl update-channels \
 && pecl install xdebug-beta \
 && rm -r /var/lib/apt/lists/* \
 && apt-get update \
 && apt-get install --no-install-recommends openssh-server=1:8.4p1-5+deb11u1 -y \
 && echo "$SSH_PASSWD" | chpasswd \
 && php -r "readfile('http://files.drush.org/drush.phar');" > /usr/local/bin/drush \
 && chmod +x /usr/local/bin/drush \
 && php -r "readfile('https://getcomposer.org/installer');" > /tmp/composer-setup.php \
 && php -r "if (hash('SHA384', file_get_contents('/tmp/composer-setup.php')) === getenv('COMPOSER_SETUP_SHA')) { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('/tmp/composer-setup.php'); echo PHP_EOL; exit(1); } echo PHP_EOL;" \
 && mkdir -p /composer/bin \
 && php /tmp/composer-setup.php --install-dir=/usr/local/bin/ --filename=composer --version=${COMPOSER_VERSION} \
 && rm /tmp/composer-setup.php \
 && apt-get install --no-install-recommends mariadb-server=1:10.5.18-0+deb11u1 -y -V \
 && mkdir -p $PHPMYADMIN_SOURCE \
 && mv /tmp/phpMyAdmin.tar.gz $PHPMYADMIN_SOURCE/phpMyAdmin.tar.gz \
 && mv /tmp/phpmyadmin-nginx.conf $PHPMYADMIN_SOURCE/phpmyadmin-nginx.conf
#   ----------
#   ~. clean up
#   ----------
RUN set -ex \
 && apt-get autoremove -y
#   =========
#   Configure
#   =========
RUN set -ex \
 && test ! -d /var/www \
 && mkdir -p /var/www \
 && chown -R www-data:www-data /var/www \
 && rm -rf /var/log/mysql \
 && ln -s $MARIADB_LOG_DIR /var/log/mysql \
 && rm -rf /var/log/nginx \
 && ln -s $NGINX_LOG_DIR /var/log/nginx \
 && ln -s ${HOME_SITE} /var/www/wwwroot \
 && ln -s ${PHPMYADMIN_HOME} /var/www/phpmyadmin
#   ssh
COPY sshd_config /etc/ssh/
#   php
COPY php.ini /etc/php/7.0/fpm/php.ini
COPY xdebug.ini /etc/php/7.0/fpm/conf.d/xdebug.ini
COPY www.conf /etc/php/7.0/fpm/pool.d/www.conf
#   nginx
COPY nginx.conf /etc/nginx/nginx.conf
COPY hostingstart.html /home/site/wwwroot/index.html
#   phpmyadmin
COPY phpmyadmin-config.inc.php $PHPMYADMIN_SOURCE/
COPY mariadb.cnf /etc/mysql/
RUN echo "<?php phpinfo();" > /home/site/wwwroot/index.php
#   =====
#   final
#   =====
COPY init_container.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/init_container.sh
EXPOSE 2222/tcp 80/tcp
ENTRYPOINT ["init_container.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
