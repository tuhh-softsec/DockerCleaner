FROM alpine:3.9
MAINTAINER Thomas Spicer (thomas@openbridge.com)
ARG NGINX_VERSION
ENV VAR_PREFIX="/var/run" \
    LOG_PREFIX="/var/log/nginx" \
    TEMP_PREFIX="/tmp" \
    CACHE_PREFIX="/var/cache" \
    CONF_PREFIX="/etc/nginx" \
    CERTS_PREFIX="/etc/pki/tls"
RUN set -x \
 && CONFIG=" --prefix=/usr/share/nginx/ --sbin-path=/usr/sbin/nginx --add-module=/tmp/naxsi/naxsi_src --modules-path=/usr/lib/nginx/modules --conf-path=${CONF_PREFIX}/nginx.conf --error-log-path=${LOG_PREFIX}/error.log --http-log-path=${LOG_PREFIX}/access.log --pid-path=${VAR_PREFIX}/nginx.pid --lock-path=${VAR_PREFIX}/nginx.lock --http-client-body-temp-path=${TEMP_PREFIX}/client_temp --http-proxy-temp-path=${TEMP_PREFIX}/proxy_temp --http-fastcgi-temp-path=${TEMP_PREFIX}/fastcgi_temp --http-uwsgi-temp-path=${TEMP_PREFIX}/uwsgi_temp --http-scgi-temp-path=${TEMP_PREFIX}/scgi_temp --user=www-data --group=www-data --with-http_ssl_module --with-pcre-jit --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-file-aio --with-http_v2_module --add-module=/tmp/ngx_cache_purge-2.3 --add-module=/tmp/ngx_http_redis-0.3.8 --add-module=/tmp/redis2-nginx-module-0.15 --add-module=/tmp/srcache-nginx-module-0.31 --add-module=/tmp/echo-nginx-module --add-module=/tmp/ngx_devel_kit-0.3.0 --add-module=/tmp/set-misc-nginx-module-0.32 --add-module=/tmp/ngx_brotli --with-cc-opt=-Wno-error " \
 && addgroup -g 82 -S www-data \
 && adduser -u 82 -D -S -h /var/cache/nginx -s /sbin/nologin -G www-data www-data \
 && apk add build-base=0.5-r1 ca-certificates=20191127-r2 automake=1.16.1-r0 autoconf=2.69-r2 git=2.20.4-r0 libtool=2.4.6-r5 binutils=2.31.1-r2 gnupg=2.2.19-r0 cmake=3.13.0-r0 go=1.11.5-r0 gcc=8.3.0-r0 build-base=0.5-r1 libc-dev=0.7.1-r0 make=4.2.1-r2 wget=1.20.3-r0 gzip=1.10-r0 openssl-dev=1.1.1k-r0 musl-dev=1.1.20-r6 pcre-dev=8.42-r2 zlib-dev=1.2.11-r1 geoip-dev=1.6.12-r1 git=2.20.4-r0 linux-headers=4.18.13-r1 gnupg=2.2.19-r0 libxslt-dev=1.1.33-r3 gd-dev=2.2.5-r4 unzip=6.0-r6 --no-cache --virtual .build-deps \
 && apk add curl=7.64.0-r5 monit=5.25.2-r1 wget=1.20.3-r0 bash=4.4.19-r1 bind-tools=9.12.4_p2-r0 rsync=3.1.3-r1 geoip=1.6.12-r1 openssl=1.1.1k-r0 tini=0.18.0-r0 tar=1.32-r0 --no-cache --update \
 && cd /tmp \
 && git clone https://github.com/google/ngx_brotli --depth=1 \
 && cd ngx_brotli \
 && git submodule update --init \
 && export NGX_BROTLI_STATIC_MODULE_ONLY=1 \
 && cd /tmp \
 && git clone https://github.com/nbs-system/naxsi.git \
 && echo 'adding /usr/local/share/GeoIP/GeoIP.dat database' \
 && wget -N https://raw.githubusercontent.com/openbridge/nginx/master/geoip/GeoLiteCity.dat.gz \
 && wget -N https://raw.githubusercontent.com/openbridge/nginx/master/geoip/GeoIP.dat.gz \
 && gzip -d GeoIP.dat.gz \
 && gzip -d GeoLiteCity.dat.gz \
 && mkdir /usr/local/share/GeoIP/ \
 && mv GeoIP.dat /usr/local/share/GeoIP/ \
 && mv GeoLiteCity.dat /usr/local/share/GeoIP/ \
 && chown -R www-data:www-data /usr/local/share/GeoIP/ \
 && curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
 && mkdir -p /usr/src \
 && tar -zxC /usr/src -f nginx.tar.gz \
 && rm nginx.tar.gz \
 && cd /tmp \
 && git clone https://github.com/openresty/echo-nginx-module.git \
 && wget https://github.com/simpl/ngx_devel_kit/archive/v0.3.0.zip -O dev.zip \
 && wget https://github.com/openresty/set-misc-nginx-module/archive/v0.32.zip -O setmisc.zip \
 && wget https://people.freebsd.org/~osa/ngx_http_redis-0.3.8.tar.gz \
 && wget https://github.com/openresty/redis2-nginx-module/archive/v0.15.zip -O redis.zip \
 && wget https://github.com/openresty/srcache-nginx-module/archive/v0.31.zip -O cache.zip \
 && wget https://github.com/FRiCKLE/ngx_cache_purge/archive/2.3.zip -O purge.zip \
 && tar -zx -f ngx_http_redis-0.3.8.tar.gz \
 && unzip dev.zip \
 && unzip setmisc.zip \
 && unzip redis.zip \
 && unzip cache.zip \
 && unzip purge.zip \
 && cd /usr/src/nginx-$NGINX_VERSION \
 && ./configure $CONFIG --with-debug \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && mv objs/nginx objs/nginx-debug \
 && mv objs/ngx_http_xslt_filter_module.so objs/ngx_http_xslt_filter_module-debug.so \
 && mv objs/ngx_http_image_filter_module.so objs/ngx_http_image_filter_module-debug.so \
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
 && install -m755 objs/ngx_stream_geoip_module-debug.so /usr/lib/nginx/modules/ngx_stream_geoip_module-debug.so \
 && ln -s ../../usr/lib/nginx/modules /etc/nginx/modules \
 && strip /usr/sbin/nginx* \
 && strip /usr/lib/nginx/modules/*.so \
 && mkdir -p /usr/local/bin/ \
 && mkdir -p ${CACHE_PREFIX} \
 && mkdir -p ${CERTS_PREFIX} \
 && cd /etc/pki/tls/ \
 && nice -n +5 openssl dhparam -out /etc/pki/tls/dhparam.pem.default 2048 \
 && apk add gettext=0.19.8.1-r4 --no-cache --virtual .gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add $runDeps --no-cache --virtual .nginx-rundeps \
 && apk del .build-deps \
 && apk del .gettext \
 && cd /tmp/naxsi \
 && mv naxsi_config/naxsi_core.rules /etc/nginx/naxsi_core.rules \
 && mv /tmp/envsubst /usr/local/bin/ \
 && rm -rf /tmp/* \
 && rm -rf /usr/src/* \
 && ln -sf /dev/stdout ${LOG_PREFIX}/access.log \
 && ln -sf /dev/stderr ${LOG_PREFIX}/error.log \
 && ln -sf /dev/stdout ${LOG_PREFIX}/blocked.log
COPY conf/ /conf
COPY test/ /tmp/test
COPY error/ /tmp/error/
COPY check_wwwdata.sh /usr/bin/check_wwwdata
COPY check_folder.sh /usr/bin/check_folder
COPY check_host.sh /usr/bin/check_host
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh /usr/bin/check_wwwdata /usr/bin/check_folder /usr/bin/check_host
STOPSIGNAL SIGQUIT
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["/usr/sbin/nginx", "-g", "daemon", "off"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
