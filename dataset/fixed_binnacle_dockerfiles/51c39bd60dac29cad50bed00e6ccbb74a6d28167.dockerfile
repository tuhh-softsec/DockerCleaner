#
#   Dockerfile for Wordpress
#
FROM php:7.2-apache
#   ========
#   ENV vars
#   ========
#   app
ENV APP_HOME="\"/home/site/wwwroot\""
ENV LOG_DIR="\"/home/LogFiles\""
ENV PHPREDIS_VERSION="3.1.2"
ENV WPFPM_FLAG="WPFPM_"
ENV PAGER="more"
#   apache
ENV HTTPD_LOG_DIR="\"/home/LogFiles/httpd\""
#   wordpress
ENV WORDPRESS_SOURCE="\"/usr/src/wordpress\""
#   phpmyadmin
ENV PHPMYADMIN_VERSION="\"4.8.0\""
ENV PHPMYADMIN_DOWNLOAD_URL="\"https://files.phpmyadmin.net/phpMyAdmin/$PHPMYADMIN_VERSION/phpMyAdmin-$PHPMYADMIN_VERSION-all-languages.tar.gz\""
ENV PHPMYADMIN_SHA256="\"1e83d60627d8036261af71220eae9ffd8d3150778702720905bcfa85c40ce346\""
ENV PHPMYADMIN_SOURCE="\"/usr/src/phpmyadmin\""
ENV PHPMYADMIN_HOME="\"/home/phpmyadmin\""
#   supervisor
ENV SUPERVISOR_LOG_DIR="\"/home/LogFiles/supervisor\""
#   ========
#   install tools
#   ======== 
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends wget=1.20.1-1.1 git=1:2.20.1-2+deb10u8 tcpdump=4.9.3-1~deb10u2 tcptraceroute=1.5beta7+debian-4+b2 net-tools=1.60+git20180626.aebd88e-1 -y ; cd /usr/bin ; wget http://www.vdberg.org/~richard/tcpping ; chmod 777 tcpping ; cd /var/www/html ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends libjpeg-dev=1:1.5.2-2+deb10u1 libpng-dev=1.6.36-6 -y ; docker-php-ext-configure gd --with-png-dir=/usr --with-jpeg-dir=/usr ; docker-php-ext-install gd mysqli opcache zip ; apt-get install --no-install-recommends $PHPIZE_DEPS -y ; pecl install xdebug-beta ; apt-mark auto '.*' > /dev/null; apt-mark manual $savedAptMark ; ldd "$( php -r 'echo ini_get("extension_dir");' ;)"/*.so | awk '/=>/ { print $3 }' | sort -u | xargs -r dpkg-query -S | cut -d: -f1 | sort -u | xargs -rt apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/*
#  install redis php extension
ENV PHPREDIS_VERSION="4.0.2"
RUN docker-php-source extract \
 && curl -L -o /tmp/redis.tar.gz https://github.com/phpredis/phpredis/archive/$PHPREDIS_VERSION.tar.gz \
 && tar xfz /tmp/redis.tar.gz \
 && rm -r /tmp/redis.tar.gz \
 && mv phpredis-$PHPREDIS_VERSION /usr/src/php/ext/redis \
 && docker-php-ext-install redis \
 && docker-php-source delete
#   set recommended PHP.ini settings
#   see https://secure.php.net/manual/en/opcache.installation.php
RUN { echo 'opcache.memory_consumption=128' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=4000' ;echo 'opcache.revalidate_freq=2' ;echo 'opcache.fast_shutdown=1' ;echo 'opcache.enable_cli=1' ; } > /usr/local/etc/php/conf.d/opcache-recommended.ini
COPY xdebug.ini /usr/local/etc/php/conf.d/xdebug.ini
RUN a2enmod rewrite expires
#  VOLUME /var/www/html#
#   -------------
#   apache conf
#   -------------
COPY httpd.conf /etc/apache2/apache2.conf
#
#   -------------
#   wordpress
#   -------------
RUN set -ex \
 && mkdir -p $WORDPRESS_SOURCE
COPY wp-config.php $WORDPRESS_SOURCE
COPY wp-config-sample.php $WORDPRESS_SOURCE
#
#   -------------
#   phpmyadmin
#   -------------
RUN set -ex \
 && mkdir -p $PHPMYADMIN_SOURCE \
 && cd $PHPMYADMIN_SOURCE \
 && wget -O phpmyadmin.tar.gz "$PHPMYADMIN_DOWNLOAD_URL" --no-check-certificate \
 && echo "$PHPMYADMIN_SHA256 *phpmyadmin.tar.gz" | sha256sum -c -
COPY httpd-phpmyadmin.conf /etc/apache2/sites-enabled/httpd-phpmyadmin.conf
COPY phpmyadmin-config.inc.php $PHPMYADMIN_SOURCE/
#
#   ------
#   ssh
#   ------
ENV SSH_PASSWD="\"root:Docker!\""
RUN set -ex \
 && apt-get update \
 && apt-get install --no-install-recommends openssh-server=1:7.9p1-10+deb10u2 -y \
 && echo "$SSH_PASSWD" | chpasswd
COPY sshd_config /etc/ssh/
#
#   ------
#   supervisor service
#   ------
RUN set -ex \
 && apt-get update \
 && apt-get install --no-install-recommends supervisor=3.3.5-1 -y
#
#   ---------
#   Configure
#   ---------
RUN set -ex \
 && rm -rf /var/log \
 && ln -s /home/LogFiles /var/log \
 && rm -rf /var/www/html \
 && ln -s /home/site/wwwroot /var/www/html \
 && ln -s /home/phpmyadmin /var/www/phpmyadmin \
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
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
