FROM php:7.2.8-fpm-alpine3.7
LABEL maintainer="nICKZHUO <sidewindermax@hotmail.com>"
ENV php_conf="/usr/local/etc/php-fpm.conf"
ENV fpm_conf="/usr/local/etc/php-fpm.d/www.conf"
ENV php_vars="/usr/local/etc/php/conf.d/docker-vars.ini"
#   Nginx版本
ENV NGINX_VERSION="1.15.2"
ENV LD_PRELOAD="/usr/lib/preloadable_libiconv.so php"
RUN apk add gnu-libiconv --no-cache --repository http://dl-3.alpinelinux.org/alpine/edge/testing
RUN addgroup -S www \
 && adduser -D -S -h /var/cache/www -s /sbin/nologin -G www www \
 && apk add autoconf=2.69-r0 gcc=6.4.0-r5 vim=8.0.1359-r2 git=2.15.4-r0 libc-dev=0.7.1-r0 make=4.2.1-r0 openssl-dev=1.0.2t-r0 pcre-dev=8.41-r1 zlib-dev=1.2.11-r1 linux-headers=4.4.6-r2 curl=7.61.1-r3 gnupg=2.2.3-r1 libxslt-dev=1.1.31-r2 gd-dev=2.2.5-r3 geoip-dev=1.6.11-r0 perl-dev=5.26.3-r0 --no-cache --virtual .build-deps
RUN curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
 && mkdir -p /usr/src \
 && tar -zxC /usr/src -f nginx.tar.gz \
 && cd /usr/src/nginx-$NGINX_VERSION \
 && ./configure --prefix=/usr/local/nginx --user=www --group=www --error-log-path=/var/log/nginx_error.log --http-log-path=/var/log/nginx_access.log --pid-path=/var/run/nginx.pid --with-pcre --with-http_ssl_module --without-mail_pop3_module --without-mail_imap_module --with-http_gzip_static_module \
 && make \
 && make install
RUN echo @testing http://nl.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories \
 && echo /etc/apk/respositories \
 && apk update \
 && apk add bash=4.4.19-r1 openssh-client=7.5_p1-r10 wget=1.20.3-r0 supervisor=3.3.3-r1 curl=7.61.1-r3 libcurl=7.61.1-r3 python python-dev py-pip augeas-dev=1.9.0-r3 openssl-dev=1.0.2t-r0 ca-certificates=20190108-r0 dialog=1.3.20170509-r0 autoconf=2.69-r0 make=4.2.1-r0 gcc=6.4.0-r5 musl-dev=1.1.18-r4 linux-headers=4.4.6-r2 libmcrypt-dev=2.5.8-r7 libpng-dev=1.6.37-r0 icu-dev=59.1-r1 libpq=10.10-r0 libxslt-dev=1.1.31-r2 libffi-dev=3.2.1-r4 freetype-dev=2.8.1-r4 sqlite-dev=3.25.3-r2 libjpeg-turbo-dev=1.5.3-r3 --no-cache
#   必须这样装mcrypt
RUN pecl install mcrypt-1.0.1 \
 && pecl install redis
#   跑GD要配置下
RUN docker-php-ext-configure gd --with-gd --with-freetype-dir=/usr/include/ --with-png-dir=/usr/include/ --with-jpeg-dir=/usr/include/
RUN docker-php-ext-install pdo_mysql mysqli gd exif fileinfo intl json opcache
RUN docker-php-ext-enable redis.so \
 && docker-php-ext-enable mcrypt.so \
 && docker-php-source delete
#   安装composer    
RUN EXPECTED_COMPOSER_SIGNATURE=$( wget -q -O - https://composer.github.io/installer.sig ;) \
 && php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');" \
 && php -r "if (hash_file('SHA384', 'composer-setup.php') === '${EXPECTED_COMPOSER_SIGNATURE}') { echo 'Composer.phar Installer verified'; } else { echo 'Composer.phar Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;" \
 && php composer-setup.php --install-dir=/usr/bin --filename=composer \
 && php -r "unlink('composer-setup.php');"
#   安装pip相关    
RUN pip install pip==23.1 -U \
 && pip install certbot==2.5.0 -U \
 && mkdir -p /etc/letsencrypt/webrootauth \
 && apk del gcc musl-dev linux-headers libffi-dev augeas-dev python-dev make autoconf
#   supervisor的配置文件复制过去
#   supervisor配置文件
COPY ./conf/supervisord/supervisord.conf /etc/
#   分解的supervisor配置
RUN mkdir -p /etc/supervisor/
COPY ./conf/supervisord/php-fpm.conf /etc/supervisor/
COPY ./conf/supervisord/nginx.conf /etc/supervisor/
#   nginx配置文件 symfony放在site里 注意路径是 /usr/local/nginx/conf/
COPY ./conf/nginx/nginx.conf /usr/local/nginx/conf/
COPY ./conf/nginx/symfony.conf /usr/local/nginx/conf/vhost/
#   优化 php-fpm 配置
RUN echo "cgi.fix_pathinfo=0" > ${php_vars} \
 && echo "upload_max_filesize = 100M" >> ${php_vars} \
 && echo "post_max_size = 100M" >> ${php_vars} \
 && echo "variables_order = \"EGPCS\"" >> ${php_vars} \
 && echo "memory_limit = 128M" >> ${php_vars} \
 && echo "date.timezone = Asia/Shanghai" >> ${php_vars}
RUN sed -i -e "s/;catch_workers_output\s*=\s*yes/catch_workers_output = yes/g" -e "s/pm.max_children = 5/pm.max_children = 4/g" -e "s/pm.start_servers = 2/pm.start_servers = 3/g" -e "s/pm.min_spare_servers = 1/pm.min_spare_servers = 2/g" -e "s/pm.max_spare_servers = 3/pm.max_spare_servers = 4/g" -e "s/;pm.max_requests = 500/pm.max_requests = 200/g" -e "s/user = www-data/user = www/g" -e "s/group = www-data/group = www/g" -e "s/;listen.mode = 0660/listen.mode = 0666/g" -e "s/;listen.owner = www-data/listen.owner = www/g" -e "s/;listen.group = www-data/listen.group = www/g" -e "s/listen = 127.0.0.1:9000/listen = \/dev\/shm\/php-fpm.sock/g" -e "s/^;clear_env = no$/clear_env = no/" ${fpm_conf}
#   挂载代码文件到容器
COPY ./code/index.php /data/www/public/
#   添加启动脚本
COPY scripts/start.sh /start.sh
RUN chmod 755 /start.sh
EXPOSE 80/tcp
CMD ["/start.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
