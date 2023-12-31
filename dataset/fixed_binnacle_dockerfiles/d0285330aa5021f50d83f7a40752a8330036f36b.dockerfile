#
#   Dockerfile for WordPress
#
FROM ubuntu:16.04
MAINTAINER Azure App Service Container Images <appsvc-images@microsoft.com>
#   ========
#   ENV vars
#   ========
#   apache httpd
ENV HTTPD_VERSION="\"2.4.25\""
ENV HTTPD_DOWNLOAD_URL="\"https://www.apache.org/dist/httpd/httpd-$HTTPD_VERSION.tar.gz\""
ENV HTTPD_SHA1="\"377c62dc6b25c9378221111dec87c28f8fe6ac69\""
ENV HTTPD_SOURCE="\"/usr/src/httpd\""
ENV HTTPD_HOME="\"/usr/local/httpd\""
ENV HTTPD_CONF_DIR="\"$HTTPD_HOME/conf\""
ENV HTTPD_CONF_FILE="\"$HTTPD_CONF_DIR/httpd.conf\""
ENV HTTPD_LOG_DIR="\"/home/LogFiles/httpd\""
ENV PATH="\"$HTTPD_HOME/bin\":$PATH"
#   mariadb
ENV MARIADB_DATA_DIR="\"/home/data\""
ENV MARIADB_LOG_DIR="\"/home/LogFiles/mysql\""
#   php
ENV PHP_VERSION="\"7.1.2\""
ENV PHP_DOWNLOAD_URL="\"https://secure.php.net/get/php-$PHP_VERSION.tar.gz/from/this/mirror\""
ENV PHP_SHA256="\"e6773217c9c719ca22abb104ae3d437d53daceaf31faf2e5eeb1f9f5028005d8\""
ENV PHP_SOURCE="\"/usr/src/php\""
ENV PHP_HOME="\"/usr/local/php\""
ENV PHP_CONF_DIR="\"$PHP_HOME/etc\""
ENV PHP_CONF_DIR_SCAN="\"$PHP_CONF_DIR/conf.d\""
ENV PATH="\"$PHP_HOME/bin\":$PATH"
#   redis
ENV REDIS_VERSION="\"3.2.8\""
ENV REDIS_DOWNLOAD_URL="\"http://download.redis.io/releases/redis-$REDIS_VERSION.tar.gz\""
ENV REDIS_SHA1="\"6780d1abb66f33a97aad0edbe020403d0a15b67f\""
ENV REDIS_SOURCE="\"/usr/src/redis\""
ENV REDIS_HOME="\"/usr/local/redis\""
ENV PATH="\"$REDIS_HOME/bin\":$PATH"
#   phpMyAdmin
ENV PHPMYADMIN_VERSION="\"4.6.6\""
ENV PHPMYADMIN_DOWNLOAD_URL="\"https://files.phpmyadmin.net/phpMyAdmin/$PHPMYADMIN_VERSION/phpMyAdmin-$PHPMYADMIN_VERSION-all-languages.tar.gz\""
ENV PHPMYADMIN_SHA256="\"54086600558613b31c4daddf4ae58fbc1c252a2b8e3e6fae12f851f78677d72e\""
ENV PHPMYADMIN_SOURCE="\"/usr/src/phpmyadmin\""
ENV PHPMYADMIN_HOME="\"/home/phpmyadmin\""
#   wordpress
ENV WORDPRESS_VERSION="\"4.7.3\""
ENV WORDPRESS_DOWNLOAD_URL="\"https://wordpress.org/wordpress-$WORDPRESS_VERSION.tar.gz\""
ENV WORDPRESS_SHA1="\"35adcd8162eae00d5bc37f35344fdc06b22ffc98\""
ENV WORDPRESS_SOURCE="\"/usr/src/wordpress\""
ENV WORDPRESS_HOME="\"/home/site/wwwroot\""
#
ENV DOCKER_BUILD_HOME="\"/dockerbuild\""
#   ====================
#   Download and Install
#   1. tools
#   2. apache httpd
#   3. mariadb
#   4. php
#   5. redis
#   6. phpmyadmin
#   7. wordpress
#   ====================
WORKDIR $DOCKER_BUILD_HOME
RUN set -ex \
 && tools=" g++ gcc make pkg-config wget " \
 && apt-get update \
 && apt-get install --no-install-recommends $tools -y -V \
 && rm -r /var/lib/apt/lists/* \
 && mkdir -p $HTTPD_SOURCE \
 && mkdir -p $HTTPD_HOME \
 && httpdBuildtimeDeps=" libpcre++-dev libssl-dev " \
 && httpdRuntimeDeps=" libapr1 libaprutil1 libaprutil1-ldap libapr1-dev libaprutil1-dev " \
 && apt-get update \
 && apt-get install --no-install-recommends $httpdBuildtimeDeps $httpdRuntimeDeps -y -V \
 && rm -r /var/lib/apt/lists/* \
 && cd $DOCKER_BUILD_HOME \
 && wget -O httpd.tar.gz "$HTTPD_DOWNLOAD_URL" --no-check-certificate \
 && echo "$HTTPD_SHA1 *httpd.tar.gz" | sha1sum -c - \
 && tar -xf httpd.tar.gz -C $HTTPD_SOURCE --strip-components=1 \
 && cd $HTTPD_SOURCE \
 && ./configure --prefix=$HTTPD_HOME --with-mpm=prefork --enable-mods-shared=reallyall --enable-ssl \
 && make -j "$( nproc ;)" \
 && make install \
 && make clean \
 && rm -rf $HTTPD_SOURCE $HTTPD_HOME/man $HTTPD_HOME/manual \
 && rm $DOCKER_BUILD_HOME/httpd.tar.gz \
 && apt-get purge -y -V -o APT::AutoRemove::RecommendsImportant=false --auto-remove $httpdBuildtimeDeps \
 && apt-get update \
 && DEBIAN_FRONTEND=noninteractive apt-get install mariadb-server -y -V --no-install-recommends \
 && rm -r /var/lib/apt/lists/* \
 && mkdir -p $PHP_SOURCE \
 && mkdir -p $PHP_HOME \
 && phpBuildtimeDeps=" libbz2-dev libgmp-dev libicu-dev libldap2-dev libssl-dev libxml2-dev " \
 && phpRuntimeDeps=" libcurl4-openssl-dev libjpeg-dev libpng12-dev libxml2 " \
 && apt-get update \
 && apt-get install --no-install-recommends $phpBuildtimeDeps $phpRuntimeDeps -y -V \
 && rm -rf /var/lib/apt/lists/* \
 && ln -s /usr/include/x86_64-linux-gnu/gmp.h /usr/include/gmp.h \
 && ln -s /usr/lib/x86_64-linux-gnu/libldap.so /usr/lib/libldap.so \
 && ln -s /usr/lib/x86_64-linux-gnu/liblber.so /usr/lib/liblber.so \
 && cd $DOCKER_BUILD_HOME \
 && wget -O php.tar.gz "$PHP_DOWNLOAD_URL" --no-check-certificate \
 && echo "$PHP_SHA256 *php.tar.gz" | sha256sum -c - \
 && tar -xf php.tar.gz -C $PHP_SOURCE --strip-components=1 \
 && cd $PHP_SOURCE \
 && ./configure --prefix=$PHP_HOME --disable-cgi --disable-pdo --enable-bcmath --enable-intl --enable-mbstring --enable-zip --with-apxs2=$HTTPD_HOME/bin/apxs --with-bz2 --with-config-file-path=$PHP_CONF_DIR --with-config-file-scan-dir=$PHP_CONF_DIR_SCAN --with-curl --with-gd --with-jpeg-dir --with-png-dir --with-gmp --with-ldap --with-mysqli=mysqlnd --with-openssl \
 && make -j "$( nproc ;)" \
 && make install \
 && make clean \
 && rm -rf $PHP_SOURCE \
 && rm -rf $PHP_HOME/php/man \
 && rm $DOCKER_BUILD_HOME/php.tar.gz \
 && apt-get purge -y -V -o APT::AutoRemove::RecommendsImportant=false --auto-remove $phpBuildtimeDeps \
 && mkdir -p $REDIS_SOURCE \
 && mkdir -p $REDIS_HOME \
 && cd $DOCKER_BUILD_HOME \
 && wget -O redis.tar.gz "$REDIS_DOWNLOAD_URL" --no-check-certificate \
 && echo "$REDIS_SHA1 *redis.tar.gz" | sha1sum -c - \
 && tar -xf redis.tar.gz -C $REDIS_SOURCE --strip-components=1 \
 && cd $REDIS_SOURCE \
 && make -j "$( nproc ;)" \
 && make PREFIX=$REDIS_HOME install \
 && rm -rf $REDIS_SOURCE E \
 && rm $DOCKER_BUILD_HOME/redis.tar.gz \
 && mkdir -p $PHPMYADMIN_SOURCE \
 && cd $PHPMYADMIN_SOURCE \
 && wget -O phpmyadmin.tar.gz "$PHPMYADMIN_DOWNLOAD_URL" --no-check-certificate \
 && echo "$PHPMYADMIN_SHA256 *phpmyadmin.tar.gz" | sha256sum -c - \
 && mkdir -p $WORDPRESS_SOURCE \
 && cd $WORDPRESS_SOURCE \
 && wget -O wordpress.tar.gz "$WORDPRESS_DOWNLOAD_URL" --no-check-certificate \
 && echo "$WORDPRESS_SHA1 *wordpress.tar.gz" | sha1sum -c - \
 && apt-get purge -y -V -o APT::AutoRemove::RecommendsImportant=false --auto-remove $tools \
 && apt-get autoremove -y
#   =========
#   Configure
#   =========
#   httpd confs
COPY httpd.conf $HTTPD_CONF_DIR/
COPY httpd-modules.conf $HTTPD_CONF_DIR/
COPY httpd-php.conf $HTTPD_CONF_DIR/
COPY httpd-wordpress.conf $HTTPD_CONF_DIR/
COPY httpd-phpmyadmin.conf $HTTPD_CONF_DIR/
#   php confs
COPY php.ini $PHP_CONF_DIR/
COPY php-opcache.ini $PHP_CONF_DIR_SCAN/
#   wordpress conf
COPY wp-config.php.microsoft $WORDPRESS_SOURCE/
#   phpmyadmin conf
COPY phpmyadmin-config.inc.php $PHPMYADMIN_SOURCE/
RUN set -ex \
 && echo 'Include conf/httpd-php.conf' >> $HTTPD_CONF_FILE \
 && sed -i 's/skip_log_error/#skip_log_error/g' /etc/mysql/mariadb.conf.d/50-mysqld_safe.cnf \
 && sed -i 's/syslog/#syslog/g' /etc/mysql/mariadb.conf.d/50-mysqld_safe.cnf \
 && test ! -d /var/lib/php/sessions \
 && mkdir -p /var/lib/php/sessions \
 && chown www-data:www-data /var/lib/php/sessions \
 && test ! -d /var/www \
 && mkdir -p /var/www \
 && echo '<html><head><meta http-equiv="refresh" content="30" /><meta http-equiv="pragma" content="no-cache" /><meta http-equiv="cache-control" content="no-cache" /><title>Installing WordPress</title></head><body>Installing WordPress ... This could be done in minutes. Please refresh your browser later.</body></html>' > /var/www/index.html \
 && chown -R www-data:www-data /var/www \
 && ln -s $WORDPRESS_HOME /var/www/wordpress \
 && ln -s $PHPMYADMIN_HOME /var/www/phpmyadmin \
 && rm -rf /var/log/mysql \
 && ln -s $MARIADB_LOG_DIR /var/log/mysql \
 && rm -rf /var/log/httpd \
 && ln -s $HTTPD_LOG_DIR /var/log/httpd
#   =====
#   final
#   =====
COPY entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/entrypoint.sh
EXPOSE 80/tcp
ENTRYPOINT ["entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
