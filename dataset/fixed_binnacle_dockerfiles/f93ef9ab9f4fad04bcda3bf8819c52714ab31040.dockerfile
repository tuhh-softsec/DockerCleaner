FROM alpine:latest
MAINTAINER niuyuxian <ncc0706@gmail.com>
ENV TIMEZONE="Asia/Shanghai"
#  # 国内时区
RUN apk add tzdata=2023c-r0 --update \
 && cp /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && echo "${TIMEZONE}" > /etc/timezone
RUN apk add bash=5.2.15-r0 curl=7.88.1-r1 curl-dev=7.88.1-r1 php5-intl php5-openssl php5-dba php5-sqlite3 php5-pear php5-phpdbg php5-gmp php5-pdo_mysql php5-pcntl php5-common php5-xsl php5-fpm php5-mysql php5-mysqli php5-enchant php5-pspell php5-snmp php5-doc php5-dev php5-xmlrpc php5-embed php5-xmlreader php5-pdo_sqlite php5-exif php5-opcache php5-ldap php5-posix php5-gd php5-gettext php5-json php5-xml php5 php5-iconv php5-sysvshm php5-curl php5-shmop php5-odbc php5-phar php5-pdo_pgsql php5-imap php5-pdo_dblib php5-pgsql php5-pdo_odbc php5-xdebug php5-zip php5-apache2 php5-cgi php5-ctype php5-mcrypt php5-wddx php5-bcmath php5-calendar php5-dom php5-sockets php5-soap php5-apcu php5-sysvmsg php5-zlib php5-ftp php5-sysvsem php5-pdo php5-bz2 php5-mysqli apache2=2.4.57-r0 libxml2-dev=2.10.4-r0 apache2-utils=2.4.57-r0 --update --no-cache
RUN apk add imagemagick-dev=7.1.0.62-r0 ffmpeg=5.1.3-r0 --update --no-cache
#  RUN ln -s /usr/bin/php5 /usr/bin/php
RUN curl -sS https://getcomposer.org/installer | php5 -- --install-dir=/usr/bin --filename=composer
RUN rm -rf /var/cache/apk/*
#   AllowOverride ALL
RUN sed -i '264s#AllowOverride None#AllowOverride All#' /etc/apache2/httpd.conf
#  Rewrite Moduble Enable
RUN sed -i 's#\#LoadModule rewrite_module modules/mod_rewrite.so#LoadModule rewrite_module modules/mod_rewrite.so#' /etc/apache2/httpd.conf
#   Document Root to /var/www/html/
RUN sed -i 's#/var/www/localhost/htdocs#/var/www/html#g' /etc/apache2/httpd.conf
#  Start apache
RUN mkdir -p /run/apache2
RUN mkdir /var/www/html/
VOLUME /var/www/html/
WORKDIR /var/www/html/
EXPOSE 80/tcp
EXPOSE 443/tcp
CMD /usr/sbin/apachectl -D FOREGROUND
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
