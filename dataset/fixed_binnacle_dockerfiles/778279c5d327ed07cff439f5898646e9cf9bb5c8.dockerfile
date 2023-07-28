FROM alpine:3.8
MAINTAINER Thomas Spicer (thomas@openbridge.com)
ENV NGINX_VERSION="1.15.2" \
    VAR_PREFIX="/var/run" \
    LOG_PREFIX="/var/log/nginx" \
    MOD_PAGESPEED_VER="1.13.35.2" \
    NGX_PAGESPEED_VER="1.13.35.2" \
    TEMP_PREFIX="/tmp" \
    CACHE_PREFIX="/var/cache" \
    CONF_PREFIX="/etc/nginx" \
    CERTS_PREFIX="/etc/pki/tls/"
COPY psol/ /tmp
RUN set -x \
 && addgroup -g 82 -S www-data \
 && adduser -u 82 -D -S -h /var/cache/nginx -s /sbin/nologin -G www-data www-data \
 && echo -e '@community http://nl.alpinelinux.org/alpine/3.8/community' >> /etc/apk/repositories \
 && apk add build-base=0.5-r1 findutils=4.6.0-r1 apr-dev=1.6.3-r1 apr-util-dev=1.6.1-r3 apache2-dev=2.4.43-r0 gnupg=2.2.19-r0 gperf=3.1-r2 icu-dev=60.2-r3 gettext-dev=0.19.8.1-r2 libjpeg-turbo-dev=1.5.3-r6 libpng-dev=1.6.37-r0 libtool=2.4.6-r5 ca-certificates=20191127-r2 automake=1.16.1-r0 autoconf=2.69-r2 git=2.18.4-r0 jemalloc-dev=5.1.0-r0 libtool=2.4.6-r5 binutils=2.30-r6 gnupg=2.2.19-r0 cmake=3.11.1-r2 go=1.10.8-r0 gcc=6.4.0-r9 build-base=0.5-r1 libc-dev=0.7.1-r0 make=4.2.1-r2 wget=1.20.3-r0 gzip=1.9-r0 libressl-dev=2.7.5-r0 musl-dev=1.1.19-r11 pcre-dev=8.42-r0 zlib-dev=1.2.11-r1 geoip-dev=1.6.12-r1 git=2.18.4-r0 linux-headers=4.4.6-r2 libxslt-dev=1.1.33-r3 nghttp2=1.39.2-r0 gd-dev=2.2.5-r4 unzip=6.0-r6 --no-cache --virtual .build-deps \
 && apk add curl=7.61.1-r3 monit=5.25.2-r0 bash=4.4.19-r1 bind-tools=9.12.4_p2-r0 rsync=3.1.3-r1 geoip=1.6.12-r1 libressl=2.7.5-r0 tini=0.18.0-r0 tar=1.32-r0 --no-cache --update \
 && cd /tmp \
 && git clone https://github.com/google/ngx_brotli --depth=1 \
 && cd ngx_brotli \
 && git submodule update --init \
 && export NGX_BROTLI_STATIC_MODULE_ONLY=1 \
 && cd /tmp \
 && git clone https://github.com/nbs-system/naxsi.git \
 && echo 'adding /usr/local/share/GeoIP/GeoIP.dat database' \
 && wget -N http://geolite.maxmind.com/download/geoip/database/GeoLiteCountry/GeoIP.dat.gz \
 && wget -N http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz \
 && gunzip GeoIP.dat.gz \
 && gunzip GeoLiteCity.dat.gz \
 && mkdir /usr/local/share/GeoIP/ \
 && mv GeoIP.dat /usr/local/share/GeoIP/ \
 && mv GeoLiteCity.dat /usr/local/share/GeoIP/ \
 && chown -R www-data:www-data /usr/local/share/GeoIP/ \
 && curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
 && mkdir -p /usr/src \
 && tar -zxC /usr/src -f nginx.tar.gz \
 && rm nginx.tar.gz \
 && cd /tmp \
 && git clone -b "v${NGX_PAGESPEED_VER}-stable" --recurse-submodules --shallow-submodules --depth=1 -c advice.detachedHead=false -j$( getconf _NPROCESSORS_ONLN ;) https://github.com/apache/incubator-pagespeed-ngx.git /tmp/ngxpagespeed \
 && cd /tmp \
 && tar -zxC /tmp/ngxpagespeed -f psol.tar.gz \
 && git clone https://github.com/openresty/echo-nginx-module.git \
 && wget https://github.com/simpl/ngx_devel_kit/archive/v0.3.0.zip -O dev.zip \
 && wget https://github.com/openresty/set-misc-nginx-module/archive/v0.31.zip -O setmisc.zip \
 && wget https://people.freebsd.org/~osa/ngx_http_redis-0.3.8.tar.gz \
 && wget https://github.com/openresty/redis2-nginx-module/archive/v0.14.zip -O redis.zip \
 && wget https://github.com/openresty/srcache-nginx-module/archive/v0.31.zip -O cache.zip \
 && wget https://github.com/FRiCKLE/ngx_cache_purge/archive/2.3.zip -O purge.zip \
 && tar -zx -f ngx_http_redis-0.3.8.tar.gz \
 && unzip dev.zip \
 && unzip setmisc.zip \
 && unzip redis.zip \
 && unzip cache.zip \
 && unzip purge.zip \
 && cd /usr/src/nginx-$NGINX_VERSION \
 && ./configure --prefix=/usr/share/nginx/ --sbin-path=/usr/sbin/nginx --add-module=/tmp/naxsi/naxsi_src --modules-path=/usr/lib/nginx/modules --conf-path=${CONF_PREFIX}/nginx.conf --error-log-path=${LOG_PREFIX}/error.log --http-log-path=${LOG_PREFIX}/access.log --pid-path=${VAR_PREFIX}/nginx.pid --lock-path=${VAR_PREFIX}/nginx.lock --http-client-body-temp-path=${TEMP_PREFIX}/client_temp --http-proxy-temp-path=${TEMP_PREFIX}/proxy_temp --http-fastcgi-temp-path=${TEMP_PREFIX}/fastcgi_temp --http-uwsgi-temp-path=${TEMP_PREFIX}/uwsgi_temp --http-scgi-temp-path=${TEMP_PREFIX}/scgi_temp --user=www-data --group=www-data --with-file-aio --with-http_ssl_module --with-pcre-jit --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-http_v2_module --with-ld-opt="-Wl,-z,relro,--start-group -lapr-1 -laprutil-1 -licudata -licuuc -lpng -lturbojpeg -ljpeg" --add-module=/tmp/ngx_cache_purge-2.3 --add-module=/tmp/ngx_http_redis-0.3.8 --add-module=/tmp/redis2-nginx-module-0.14 --add-module=/tmp/srcache-nginx-module-0.31 --add-module=/tmp/echo-nginx-module --add-module=/tmp/ngx_devel_kit-0.3.0 --add-module=/tmp/set-misc-nginx-module-0.31 --add-module=/tmp/ngx_brotli --add-module=/tmp/ngxpagespeed \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && mv objs/nginx objs/nginx-debug \
 && mv objs/ngx_http_xslt_filter_module.so objs/ngx_http_xslt_filter_module-debug.so \
 && mv objs/ngx_http_image_filter_module.so objs/ngx_http_image_filter_module-debug.so \
 && mv objs/ngx_stream_geoip_module.so objs/ngx_stream_geoip_module-debug.so \
 && ./configure --prefix=/usr/share/nginx/ --sbin-path=/usr/sbin/nginx --add-module=/tmp/naxsi/naxsi_src --modules-path=/usr/lib/nginx/modules --conf-path=${CONF_PREFIX}/nginx.conf --error-log-path=${LOG_PREFIX}/error.log --http-log-path=${LOG_PREFIX}/access.log --pid-path=${VAR_PREFIX}/nginx.pid --lock-path=${VAR_PREFIX}/nginx.lock --http-client-body-temp-path=${TEMP_PREFIX}/client_temp --http-proxy-temp-path=${TEMP_PREFIX}/proxy_temp --http-fastcgi-temp-path=${TEMP_PREFIX}/fastcgi_temp --http-uwsgi-temp-path=${TEMP_PREFIX}/uwsgi_temp --http-scgi-temp-path=${TEMP_PREFIX}/scgi_temp --user=www-data --group=www-data --with-file-aio --with-http_ssl_module --with-pcre-jit --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-http_v2_module --with-ld-opt="-Wl,-z,relro,--start-group -lapr-1 -laprutil-1 -licudata -licuuc -lpng -lturbojpeg -ljpeg" --add-module=/tmp/ngx_cache_purge-2.3 --add-module=/tmp/ngx_http_redis-0.3.8 --add-module=/tmp/redis2-nginx-module-0.14 --add-module=/tmp/srcache-nginx-module-0.31 --add-module=/tmp/echo-nginx-module --add-module=/tmp/ngx_devel_kit-0.3.0 --add-module=/tmp/set-misc-nginx-module-0.31 --add-module=/tmp/ngx_brotli --add-module=/tmp/ngxpagespeed \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && rm -rf /etc/nginx/html/ \
 && mkdir -p /etc/nginx/conf.d/ \
 && mkdir -p /usr/share/nginx/html/ \
 && install -m644 html/index.html /usr/share/nginx/html/ \
 && install -m644 html/50x.html /usr/share/nginx/html/ \
 && ln -s ../../usr/lib/nginx/modules /etc/nginx/modules \
 && strip /usr/sbin/nginx* \
 && strip /usr/lib/nginx/modules/*.so \
 && mkdir -p /usr/local/bin/ \
 && mkdir -p ${CACHE_PREFIX} \
 && mkdir -p ${CERTS_PREFIX} \
 && cd ${CERTS_PREFIX} \
 && openssl dhparam 2048 -out ${CERTS_PREFIX}/dhparam.pem.default \
 && apk add gettext=0.19.8.1-r2 --no-cache --virtual .gettext \
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
CMD ["nginx", "-g", "daemon", "off"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
