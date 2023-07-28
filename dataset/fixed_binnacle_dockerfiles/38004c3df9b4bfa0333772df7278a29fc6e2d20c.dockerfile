FROM php:7.3-fpm-alpine3.8 AS stage0
LABEL maintainer="Dean Tedesco <dean@ethicaljobs.com.au>"
#
#  --------------------------------------------------------------------------
#   Install nginx
#  --------------------------------------------------------------------------
#
ENV NGINX_VERSION="1.13.8"
RUN GPG_KEYS=B0F4253373F8F6F510D42178520A9993A1C052F8 \
 && CONFIG=" --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=nginx --group=nginx --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-file-aio --with-http_v2_module " \
 && addgroup -S nginx \
 && adduser -D -S -h /var/cache/nginx -s /sbin/nologin -G nginx nginx \
 && apk add gcc=6.4.0-r9 libc-dev=0.7.1-r0 make=4.2.1-r2 openssl-dev=1.0.2u-r0 pcre-dev=8.42-r0 zlib-dev=1.2.11-r1 linux-headers=4.4.6-r2 curl=7.61.1-r3 gnupg=2.2.19-r0 libxslt-dev=1.1.33-r3 gd-dev=2.2.5-r4 geoip-dev=1.6.12-r1 --no-cache --virtual .build-deps \
 && curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
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
 && apk add gettext=0.19.8.1-r2 --no-cache --virtual .gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add $runDeps --no-cache --virtual .nginx-rundeps \
 && apk del .build-deps \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/ \
 && apk add tzdata=2020a-r0 --no-cache \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
#
#  --------------------------------------------------------------------------
#   Install dependencies and extensions
#  --------------------------------------------------------------------------
#
RUN apk add freetype=2.9.1-r1 libpng=1.6.37-r0 libjpeg-turbo=1.5.3-r6 freetype-dev=2.9.1-r1 libpng-dev=1.6.37-r0 libjpeg-turbo-dev=1.5.3-r6 wget=1.20.3-r0 git=2.18.4-r0 supervisor=3.3.4-r1 bash=4.4.19-r1 --no-cache \
 && docker-php-ext-install mysqli pdo_mysql opcache pcntl \
 && docker-php-ext-configure gd --with-gd --with-freetype-dir=/usr/include/ --with-png-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && NPROC=$( grep -c ^processor /proc/cpuinfo 2> /dev/null || 1 ;) \
 && docker-php-ext-install -j${NPROC} gd \
 && curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/bin --filename=composer \
 && composer global require "hirak/prestissimo"
RUN mkdir -p /var/log/cron \
 && mkdir -p /var/www \
 && mkdir -p /var/entrypoints \
 && touch /var/log/cron/cron.log \
 && mkdir -m 0644 -p /etc/cron.d \
 && chmod -R 0644 /etc/cron.d
#
#  --------------------------------------------------------------------------
#   Application
#  --------------------------------------------------------------------------
#
COPY ./config/bin/schedule /etc/crontabs/root
COPY ./config/supervisord/* /etc/supervisord/
COPY ./config/entrypoints/* /var/entrypoints/
ENV TZ="Australia/Melbourne"
ENV PATH="$PATH:/var/www/vendor/bin"
WORKDIR /var/www
RUN touch /var/log/cron/cron.log
ENV SCHEDULE_LOG_PATH="/var/log/cron/cron.log"
#
#  --------------------------------------------------------------------------
#   Init
#  --------------------------------------------------------------------------
#
EXPOSE 80/tcp 443/tcp
ENTRYPOINT ["/var/entrypoints/laravel"]
FROM scratch AS release
COPY --from=stage0 / /
CMD /usr/bin/supervisord -n -c /etc/supervisord/web.conf
FROM php:7.3-fpm-alpine3.8 AS xdebug
COPY --from=release / /
#   Add prerequisites for xdebug
RUN apk add $PHPIZE_DEPS --no-cache
#   Install xdebug
RUN pecl install xdebug-2.7.0
#   Configure xdebug for remote debugging with PhpStorm
RUN echo "zend_extension=$( find /usr/local/lib/php/extensions/ -name xdebug.so ;)" > /usr/local/etc/php/conf.d/xdebug.ini \
 && echo "xdebug.remote_enable=on" >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo "xdebug.remote_autostart=on" >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo "xdebug.remote_host=docker.for.mac.localhost" >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo "xdebug.remote_port=9000" >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo "xdebug.remote_log=/var/www/storage/logs/xdebug.log" >> /usr/local/etc/php/conf.d/xdebug.ini
FROM release
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
