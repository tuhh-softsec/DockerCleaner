FROM alpine:3.8
RUN apk add --no-cache bash supervisor curl
RUN apk --no-cache add ca-certificates openssl \
 && echo "@php https://dl.bintray.com/php-alpine/v3.8/php-7.2" >> /etc/apk/repositories
#  Install nginx
ENV NGINX_VERSION="1.17.0"
#  Add PHP public keys
COPY https://dl.bintray.com/php-alpine/key/php-alpine.rsa.pub /etc/apk/keys/php-alpine.rsa.pub
RUN GPG_KEYS=B0F4253373F8F6F510D42178520A9993A1C052F8 \
 && CONFIG=" --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=nginx --group=nginx --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-file-aio --with-http_v2_module --add-module=/usr/src/ngx_http_redis-0.3.9 --add-module=/usr/src/ngx_devel_kit-0.3.0 --add-module=/usr/src/set-misc-nginx-module-0.32 --add-module=/usr/src/ngx_http_substitutions_filter_module-0.6.4 " \
 && addgroup -S nginx \
 && adduser -D -S -h /var/cache/nginx -s /sbin/nologin -G nginx nginx \
 && apk add --no-cache --virtual .build-deps gcc libc-dev make openssl-dev pcre-dev zlib-dev linux-headers curl gnupg1 libxslt-dev gd-dev geoip-dev \
 && curl -fSL https://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
 && curl -fSL https://nginx.org/download/nginx-$NGINX_VERSION.tar.gz.asc -o nginx.tar.gz.asc \
 && curl -fSL https://people.freebsd.org/~osa/ngx_http_redis-0.3.9.tar.gz -o http-redis.tar.gz \
 && curl -fSL https://github.com/openresty/set-misc-nginx-module/archive/v0.32.tar.gz -o set-misc.tar.gz \
 && curl -fSL https://github.com/simplresty/ngx_devel_kit/archive/v0.3.0.tar.gz -o ngx_devel_kit.tar.gz \
 && curl -fSL https://github.com/yaoweibin/ngx_http_substitutions_filter_module/archive/v0.6.4.tar.gz -o ngx_http_substitutions_filter_module.tar.gz \
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
 && tar -zxC /usr/src -f http-redis.tar.gz \
 && rm http-redis.tar.gz \
 && tar -zxC /usr/src -f set-misc.tar.gz \
 && rm set-misc.tar.gz \
 && tar -zxC /usr/src -f ngx_http_substitutions_filter_module.tar.gz \
 && rm ngx_http_substitutions_filter_module.tar.gz \
 && tar -zxC /usr/src -f ngx_devel_kit.tar.gz \
 && rm ngx_devel_kit.tar.gz \
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
 && apk add --no-cache --virtual .gettext gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add --no-cache --virtual .nginx-rundeps $runDeps \
 && apk del .build-deps \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/ \
 && apk add --no-cache tzdata \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
#  Nginx temp upload dir
RUN mkdir -p /var/nginx-uploads \
 && chown nobody:nobody /var/nginx-uploads
RUN mkdir -p /var/cache/nginx/client_temp \
 && mkdir -p /var/cache/nginx/proxy_temp \
 && mkdir -p /var/cache/nginx/fastcgi_temp \
 && mkdir -p /var/cache/nginx/uwsgi_temp \
 && mkdir -p /var/cache/nginx/scgi_temp
RUN chown -R nobody:nobody /var/cache/nginx/client_temp \
 && chown -R nobody:nobody /var/cache/nginx/proxy_temp \
 && chown -R nobody:nobody /var/cache/nginx/fastcgi_temp \
 && chown -R nobody:nobody /var/cache/nginx/uwsgi_temp \
 && chown -R nobody:nobody /var/cache/nginx/scgi_temp
RUN apk add --no-cache php7@php php7-common@php php7-memcached@php php7-curl@php php7-dom@php php7-exif@php php7-ftp@php php7-gd@php php7-iconv@php php7-mbstring@php php7-mysqli@php php7-mysqlnd@php php7-openssl@php php7-pdo@php php7-session@php php7-posix@php php7-soap@php php7-zip@php php7-ldap@php php7-bcmath@php php7-calendar@php php7-gettext@php php7-json@php php7-pcntl@php php7-apcu@php php7-phar@php php7-sockets@php php7-tidy@php php7-wddx@php php7-xmlreader@php php7-zip@php php7-zlib@php php7-xsl@php php7-opcache@php php7-imagick@php php7-ctype@php php7-pdo_mysql@php php7-pdo_sqlite@php php7-sqlite3@php php7-redis@php php7-intl@php php7-fpm@php
#  Imagick support file types
RUN apk add --no-cache imagemagick
RUN mkdir -p /src \
 && ln -s /etc/php7 /etc/php \
 && ln -s /usr/bin/php7 /usr/bin/php \
 && ln -s /usr/sbin/php-fpm7 /usr/bin/php-fpm
#  Add Composer
RUN curl https://getcomposer.org/installer -o /tmp/composer-installer \
 && php /tmp/composer-installer --install-dir=/usr/local/bin --filename=composer \
 && rm -f /tmp/composer-installer
#  Atatus
RUN wget https://s3.amazonaws.com/atatus-artifacts/atatus-php/downloads/atatus-php-1.7.0-x64-musl.tar.gz \
 && tar -xzvf atatus-php-1.7.0-x64-musl.tar.gz \
 && cd atatus-php-1.7.0-x64-musl \
 && ./install.sh
#  Atatus configurations
RUN sed -i -e 's#atatus.trace.response_time = 2000#atatus.trace.response_time = 1500#g' /etc/php/conf.d/atatus.ini \
 && sed -i -e 's#atatus.collector.pidfile = "/var/run/atatus-php-collector.pid"#atatus.collector.pidfile = "/run/atatus-php-collector.pid"#g' /etc/php/conf.d/atatus.ini \
 && sed -i -e 's#atatus.collector.connection = "/tmp/.atatus.sock"#atatus.collector.connection = "/run/atatus.sock"#g' /etc/php/conf.d/atatus.ini
#  Supervisor
COPY conf/supervisord.conf /etc/supervisord.conf
COPY conf/supervisor.d /etc/supervisor.d
RUN mkdir -p /etc/supervisord-enabled \
 && mkdir -p /etc/supervisord-worker
#  Scripts
ADD scripts/start-web.sh /start-web.sh
RUN chmod 755 /start-web.sh
ADD scripts/start-worker.sh /start-worker.sh
RUN chmod 755 /start-worker.sh
COPY conf/nginx.conf /etc/nginx/nginx.conf
COPY conf/nginx-site.conf /etc/nginx/sites-enabled/site.conf
COPY dependencies/nginx-custom.php /usr/local/bin/nginx-custom
#  Test Nginx
RUN nginx -c /etc/nginx/nginx.conf -t
# # PHP
COPY conf/php-fpm.conf /etc/php/php-fpm.conf
COPY conf/php.ini /etc/php/php.ini
COPY conf/php-www.conf /etc/php/php-fpm.d/www.conf
#  Test PHP-FPM
RUN /usr/bin/php-fpm --fpm-config /etc/php/php-fpm.conf -t
#  Cron
RUN mkdir -p /etc/cron.d
CMD ["/start-web.sh"]
