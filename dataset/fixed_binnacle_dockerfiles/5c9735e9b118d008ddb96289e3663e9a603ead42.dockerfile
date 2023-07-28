FROM alpine:3.4
MAINTAINER janes - https://github.com/hxer
#   Timezone
ENV TIMEZONE="Asia/Shanghai"
ENV PHP_MEMORY_LIMIT="512M"
ENV MAX_UPLOAD="50M"
ENV PHP_MAX_FILE_UPLOAD="200"
ENV PHP_MAX_POST="100M"
ENV DUMB_INIT_VERSION="1.2.0"
#   替换为阿里云的源，构建速度更快
#   RUN sed -i 's#dl-cdn\.alpinelinux\.org#mirrors\.aliyun\.com#' /etc/apk/repositories
#   install mysql, apache and php and php extensions, tzdata, wget
RUN echo "@community http://dl-cdn.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories \
 && apk update \
 && apk add mysql=10.1.32-r0 mysql-client=10.1.32-r0 apache2=2.4.33-r0 curl=7.60.0-r1 wget=1.18-r3 tzdata=2016d-r0 php5-apache2=5.6.36-r0 php5-cli=5.6.36-r0 php5-phar=5.6.36-r0 php5-zlib=5.6.36-r0 php5-zip=5.6.36-r0 php5-bz2=5.6.36-r0 php5-ctype=5.6.36-r0 php5-mysqli=5.6.36-r0 php5-mysql=5.6.36-r0 php5-pdo_mysql=5.6.36-r0 php5-opcache=5.6.36-r0 php5-pdo=5.6.36-r0 php5-json=5.6.36-r0 php5-curl=5.6.36-r0 php5-gd=5.6.36-r0 php5-gmp=5.6.36-r0 php5-mcrypt=5.6.36-r0 php5-openssl=5.6.36-r0 php5-dom=5.6.36-r0 php5-xml=5.6.36-r0 php5-iconv=5.6.36-r0 php5-xdebug@community
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/bin --filename=composer
#   configure timezone, mysql, apache
RUN cp /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && echo "${TIMEZONE}" > /etc/timezone \
 && mkdir -p /run/mysqld \
 && chown -R mysql:mysql /run/mysqld /var/lib/mysql \
 && mysql_install_db --user=mysql --verbose=1 --basedir=/usr --datadir=/var/lib/mysql --rpm > /dev/null \
 && sed -i '/skip-external-locking/a log_error = \/var\/lib\/mysql\/error.log' /etc/mysql/my.cnf \
 && sed -i '/skip-external-locking/a general_log = ON' /etc/mysql/my.cnf \
 && sed -i '/skip-external-locking/a general_log_file = \/var\/lib\/mysql\/query.log' /etc/mysql/my.cnf \
 && ln -s /usr/lib/libxml2.so.2 /usr/lib/libxml2.so \
 && sed -i 's#AllowOverride None#AllowOverride All#' /etc/apache2/httpd.conf \
 && sed -i 's#ServerName www.example.com:80#\nServerName localhost:80#' /etc/apache2/httpd.conf \
 && sed -i 's#^DocumentRoot ".*#DocumentRoot "/www"#g' /etc/apache2/httpd.conf \
 && sed -i 's#/var/www/localhost/htdocs#/www#g' /etc/apache2/httpd.conf \
 && sed -i "s|;*date.timezone =.*|date.timezone = ${TIMEZONE}|i" /etc/php5/php.ini \
 && sed -i "s|;*memory_limit =.*|memory_limit = ${PHP_MEMORY_LIMIT}|i" /etc/php5/php.ini \
 && sed -i "s|;*upload_max_filesize =.*|upload_max_filesize = ${MAX_UPLOAD}|i" /etc/php5/php.ini \
 && sed -i "s|;*max_file_uploads =.*|max_file_uploads = ${PHP_MAX_FILE_UPLOAD}|i" /etc/php5/php.ini \
 && sed -i "s|;*post_max_size =.*|post_max_size = ${PHP_MAX_POST}|i" /etc/php5/php.ini \
 && sed -i "s|;*cgi.fix_pathinfo=.*|cgi.fix_pathinfo= 0|i" /etc/php5/php.ini \
 && mkdir -p /run/apache2 \
 && chown -R apache:apache /run/apache2 \
 && mkdir /www \
 && echo "<?php phpinfo(); ?>" > /www/index.php \
 && chown -R apache:apache /www
#   Configure xdebug
RUN echo "zend_extension=xdebug.so" > /etc/php5/conf.d/xdebug.ini \
 && echo -e "\n[XDEBUG]" >> /etc/php5/conf.d/xdebug.ini \
 && echo "xdebug.remote_enable=1" >> /etc/php5/conf.d/xdebug.ini \
 && echo "xdebug.remote_connect_back=1" >> /etc/php5/conf.d/xdebug.ini \
 && echo "xdebug.idekey=PHPSTORM" >> /etc/php5/conf.d/xdebug.ini \
 && echo "xdebug.remote_log=\"/tmp/xdebug.log\"" >> /etc/php5/conf.d/xdebug.ini
#   Start apache
RUN echo "#!/bin/sh" > /start.sh \
 && echo "httpd" >> /start.sh \
 && echo "nohup mysqld --skip-grant-tables --bind-address 0.0.0.0 --user mysql > /dev/null 2>&1 &" >> /start.sh \
 && echo "sleep 3 \
 && mysql -uroot -e \"create database db;\"" >> /start.sh \
 && echo "tail -f /var/log/apache2/access.log" >> /start.sh \
 && chmod u+x /start.sh
#   Add dumb-init
RUN wget -O /usr/local/bin/dumb-init https://github.com/Yelp/dumb-init/releases/download/v${DUMB_INIT_VERSION}/dumb-init_${DUMB_INIT_VERSION}_amd64 \
 && chmod +x /usr/local/bin/dumb-init
WORKDIR /www
EXPOSE 80/tcp
EXPOSE 3306/tcp
#  VOLUME ["/www","/var/lib/mysql","/etc/mysql/"]
ENTRYPOINT ["/usr/local/bin/dumb-init", "/start.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
