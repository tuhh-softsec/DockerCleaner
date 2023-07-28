#  +++++++++++++++++++++++++++++++++++++++
#   Dockerfile for webdevops/php:alpine-php7
#      -- automatically generated  --
#  +++++++++++++++++++++++++++++++++++++++
FROM webdevops/base-app:alpine
ENV WEB_DOCUMENT_ROOT="/app" \
    WEB_DOCUMENT_INDEX="index.php" \
    WEB_ALIAS_DOMAIN="*.vm" \
    WEB_PHP_TIMEOUT="600" \
    WEB_PHP_SOCKET=""
COPY conf/ /opt/docker/
RUN set -x \
 && apk-install imagemagick graphicsmagick ghostscript jpegoptim pngcrush libjpeg-turbo-utils optipng pngquant php7-fpm php7-json php7-intl php7-curl php7-mysqli php7-mysqlnd php7-pdo_mysql php7-pdo_pgsql php7-pdo_sqlite php7-mcrypt php7-gd php7-imap php7-bcmath php7-soap php7-sqlite3 php7-bz2 php7-calendar php7-ctype php7-mongodb php7-pcntl php7-pgsql php7-posix php7-sockets php7-sysvmsg php7-sysvsem php7-sysvshm php7-xmlreader php7-exif php7-ftp php7-gettext php7-iconv php7-zip php7-zlib php7-shmop php7-wddx sqlite php7-xmlrpc php7-xsl geoip php7-ldap php7-redis php7-pear php7-phar php7-openssl php7-session php7-opcache php7-mbstring php7-iconv php7-apcu php7-fileinfo php7-simplexml php7-tokenizer php7-xmlwriter \
 && ln -s /usr/sbin/php-fpm7 /usr/sbin/php-fpm \
 && pecl channel-update pecl.php.net \
 && pear config-set auto_discover 1 \
 && curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin/ --filename=composer \
 && sed -i "s/ -n / /" $( which pecl ;) \
 && docker-service enable syslog \
 && docker-service enable cron \
 && docker-run-bootstrap \
 && docker-image-cleanup
EXPOSE 9000/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
