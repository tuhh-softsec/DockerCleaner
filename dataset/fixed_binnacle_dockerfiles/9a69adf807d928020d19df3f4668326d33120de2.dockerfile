FROM php:7.2.11-fpm-alpine3.7
MAINTAINER Azure App Service Container Images <appsvc-images@microsoft.com>
#   ========
#   ENV vars
#   ========
#   ssh
ENV SSH_PASSWD="\"root:Docker!\""
#  nginx
ENV NGINX_VERSION="1.14.0"
ENV NGINX_LOG_DIR="\"/home/LogFiles/nginx\""
#  php
ENV PHP_HOME="\"/usr/local/etc/php\""
ENV PHP_CONF_DIR="$PHP_HOME"
ENV PHP_CONF_FILE="$PHP_CONF_DIR\"/php.ini\""
#   mariadb
ENV MARIADB_DATA_DIR="\"/home/data/mysql\""
ENV MARIADB_LOG_DIR="\"/home/LogFiles/mysql\""
#   phpmyadmin
ENV PHPMYADMIN_SOURCE="\"/usr/src/phpmyadmin\""
ENV PHPMYADMIN_HOME="\"/var/www/phpmyadmin\""
#  Web Site Home
ENV HOME_SITE="\"/var/www/html\""
#   --------
#   ~. tools
#   --------
RUN set -ex \
 && apk update \
 && apk add openssl=1.0.2t-r0 git=2.15.4-r0 net-tools=1.60_git20140218-r1 tcpdump=4.9.2-r1 tcptraceroute=1.5b7-r1 vim=8.0.1359-r2 curl=7.61.1-r3 wget=1.20.3-r0 bash=4.4.19-r1 --no-cache \
 && cd /usr/bin \
 && wget http://www.vdberg.org/~richard/tcpping \
 && chmod 777 tcpping \
 && apk add icu-dev=59.1-r1 libxml2-dev=2.9.8-r1 freetype-dev=2.8.1-r4 libpng-dev=1.6.37-r0 libjpeg-turbo-dev=1.5.3-r3 g++=6.4.0-r5 make=4.2.1-r0 autoconf=2.69-r0 --no-cache --virtual build-dependencies \
 && docker-php-source extract \
 && pecl install xdebug-beta \
 && docker-php-ext-install mysqli \
 && docker-php-source delete \
 && apk del build-dependencies \
 && apk del libmcrypt-dev \
 && apk add openssh-server=7.5_p1-r10 --no-cache \
 && echo "$SSH_PASSWD" | chpasswd \
 && apk add mariadb=10.1.41-r0 mariadb-client=10.1.41-r0 --no-cache \
 && apk add openrc=0.24.1-r4 --no-cache \
 && sed -i 's/"cgroup_add_service/" # cgroup_add_service/g' /lib/rc/sh/openrc-run.sh
#   ----------
#   Nginx
#   ----------   
RUN GPG_KEYS=B0F4253373F8F6F510D42178520A9993A1C052F8 \
 && CONFIG=" --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=nginx --group=nginx --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-file-aio --with-http_v2_module " \
 && addgroup -S nginx \
 && adduser -D -S -h /var/cache/nginx -s /sbin/nologin -G nginx nginx \
 && apk add gcc=6.4.0-r5 libc-dev=0.7.1-r0 make=4.2.1-r0 openssl-dev=1.0.2t-r0 pcre-dev=8.41-r1 zlib-dev=1.2.11-r1 linux-headers=4.4.6-r2 curl=7.61.1-r3 gnupg=2.2.3-r1 libxslt-dev=1.1.31-r2 gd-dev=2.2.5-r3 geoip-dev=1.6.11-r0 --no-cache --virtual .build-deps \
 && curl -fSL https://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
 && curl -fSL https://nginx.org/download/nginx-$NGINX_VERSION.tar.gz.asc -o nginx.tar.gz.asc \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && found='' ; for server in ha.pool.sks-keyservers.net hkp://keyserver.ubuntu.com:80 hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu; do echo "Fetching GPG key $GPG_KEYS from $server" ;gpg --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$GPG_KEYS" \
 && found=yes \
 && break ; done ; test -z "$found" \
 && echo "error: failed to fetch GPG key $GPG_KEYS" >&2 \
 && exit 1 ; gpg --batch --verify nginx.tar.gz.asc nginx.tar.gz \
 && rm -rf "$GNUPGHOME" nginx.tar.gz.asc \
 && mkdir -p /usr/src \
 && tar -zxC /usr/src -f nginx.tar.gz \
 && rm nginx.tar.gz \
 && cd /usr/src/nginx-$NGINX_VERSION \
 && ./configure $CONFIG --with-debug \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && mv objs/nginx objs/nginx-debug \
 && mv objs/ngx_http_xslt_filter_module.so objs/ngx_http_xslt_filter_module-debug.so \
 && mv objs/ngx_http_image_filter_module.so objs/ngx_http_image_filter_module-debug.so \
 && mv objs/ngx_http_geoip_module.so objs/ngx_http_geoip_module-debug.so \
 && mv objs/ngx_stream_geoip_module.so objs/ngx_stream_geoip_module-debug.so \
 && ./configure $CONFIG \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && rm -rf /etc/nginx/html/ \
 && mkdir /etc/nginx/conf.d/ \
 && mkdir -p /usr/share/nginx/html/ \
 && install -m644 html/index.html /usr/share/nginx/html/ \
 && install -m644 html/50x.html /usr/share/nginx/html/ \
 && install -m755 objs/nginx-debug /usr/sbin/nginx-debug \
 && install -m755 objs/ngx_http_xslt_filter_module-debug.so /usr/lib/nginx/modules/ngx_http_xslt_filter_module-debug.so \
 && install -m755 objs/ngx_http_image_filter_module-debug.so /usr/lib/nginx/modules/ngx_http_image_filter_module-debug.so \
 && install -m755 objs/ngx_http_geoip_module-debug.so /usr/lib/nginx/modules/ngx_http_geoip_module-debug.so \
 && install -m755 objs/ngx_stream_geoip_module-debug.so /usr/lib/nginx/modules/ngx_stream_geoip_module-debug.so \
 && ln -s ../../usr/lib/nginx/modules /etc/nginx/modules \
 && strip /usr/sbin/nginx* \
 && strip /usr/lib/nginx/modules/*.so \
 && rm -rf /usr/src/nginx-$NGINX_VERSION \
 && apk add gettext=0.19.8.1-r1 --no-cache --virtual .gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add $runDeps --no-cache --virtual .nginx-rundeps \
 && apk del .build-deps \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/ \
 && apk add tzdata=2019c-r0 --no-cache \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
#   -------------
#   phpmyadmin
#   -------------
RUN mkdir -p $PHPMYADMIN_SOURCE
COPY phpMyAdmin.tar.gz $PHPMYADMIN_SOURCE/phpMyAdmin.tar.gz
#   ----------
#   ~. upgrade/clean up
#   ----------
RUN set -ex \
 && apk update \
 && apk upgrade \
 && rm -rf /var/cache/apk/* \
 && rm -rf /tmp/*
#   =========
#   Configure
#   =========
RUN set -ex \
 && mkdir -p /var/www \
 && chown -R www-data:www-data /var/www \
 && rm -rf /var/log/mysql \
 && ln -s $MARIADB_LOG_DIR /var/log/mysql \
 && rm -rf /var/log/nginx \
 && ln -s $NGINX_LOG_DIR /var/log/nginx
#  #
#   && ln -s ${HOME_SITE} /var/www/html \
#  #	
#   && ln -s ${PHPMYADMIN_HOME} /var/www/phpmyadmin
#  	
#   ssh
COPY sshd_config /etc/ssh/
#   php
COPY php.ini /usr/local/etc/php/php.ini
COPY xdebug.ini /usr/local/etc/php/conf.d/xdebug.ini
COPY www.conf /usr/local/etc/php/conf.d/www.conf
COPY zz-docker.conf /usr/local/etc/php-fpm.d/zz-docker.conf
#   nginx
COPY nginx.conf /etc/nginx/nginx.conf
#  COPY default.conf /etc/nginx/conf.d/default.conf
COPY hostingstart.html /var/www/html/index.html
#   phpmyadmin
COPY phpmyadmin-config.inc.php $PHPMYADMIN_SOURCE/
COPY mariadb.cnf /etc/mysql/
COPY phpmyadmin-default.conf $PHPMYADMIN_SOURCE/phpmyadmin-default.conf
RUN echo "<?php phpinfo();" > /var/www/html/index.php
#   =====
#   final
#   =====
COPY init_container.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/init_container.sh
EXPOSE 2222/tcp 80/tcp
ENTRYPOINT ["init_container.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
