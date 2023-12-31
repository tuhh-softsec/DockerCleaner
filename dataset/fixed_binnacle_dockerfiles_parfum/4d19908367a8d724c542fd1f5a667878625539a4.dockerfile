FROM alpine:3.6
LABEL maintainer="mritd <mritd@linux.com>"
ENV NGINX_VERSION="1.11.8"
ENV LUAJIT_VERSION="2.0.4"
ENV LUAJIT_MAIN_VERSION="2.0"
ENV LUAJIT_LIB="/usr/local/lib"
ENV LUAJIT_INC="/usr/local/include/luajit-$LUAJIT_MAIN_VERSION"
ENV NGINX_LUA_MODULE_VERSION="0.10.7"
RUN GPG_KEYS=B0F4253373F8F6F510D42178520A9993A1C052F8 \
 && CONFIG=" --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=nginx --group=nginx --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-http_perl_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-file-aio --with-http_v2_module --with-ld-opt=-Wl,-rpath,$LUAJIT_LIB --add-module=/usr/src/ngx_http_google_filter_module --add-module=/usr/src/ngx_http_substitutions_filter_module --add-module=/usr/src/lua-nginx-module-$NGINX_LUA_MODULE_VERSION " \
 && addgroup -S nginx \
 && adduser -D -S -h /var/cache/nginx -s /sbin/nologin -G nginx nginx \
 && apk add --no-cache --virtual .build-deps gcc libc-dev make openssl-dev pcre-dev zlib-dev linux-headers curl gnupg libxslt-dev gd-dev geoip-dev perl-dev git python \
 && curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
 && curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz.asc -o nginx.tar.gz.asc \
 && curl -fSL http://luajit.org/download/LuaJIT-$LUAJIT_VERSION.tar.gz -o LuaJIT-$LUAJIT_VERSION.tar.gz \
 && curl -fSL https://github.com/openresty/lua-nginx-module/archive/v$NGINX_LUA_MODULE_VERSION.tar.gz -o lua-nginx-module-v$NGINX_LUA_MODULE_VERSION.tar.gz \
 && git clone https://github.com/cuber/ngx_http_google_filter_module /usr/src/ngx_http_google_filter_module \
 && git clone https://github.com/yaoweibin/ngx_http_substitutions_filter_module /usr/src/ngx_http_substitutions_filter_module \
 && git clone https://github.com/alexazhou/VeryNginx.git /usr/src/VeryNginx \
 && tar -zxC /usr/src -f lua-nginx-module-v$NGINX_LUA_MODULE_VERSION.tar.gz \
 && tar -zxC /usr/src -f LuaJIT-$LUAJIT_VERSION.tar.gz \
 && rm lua-nginx-module-v$NGINX_LUA_MODULE_VERSION.tar.gz \
 && rm LuaJIT-$LUAJIT_VERSION.tar.gz \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEYS" \
 && gpg --batch --verify nginx.tar.gz.asc nginx.tar.gz \
 && rm -r "$GNUPGHOME" nginx.tar.gz.asc \
 && tar -zxC /usr/src -f nginx.tar.gz \
 && rm nginx.tar.gz \
 && cd /usr/src/VeryNginx \
 && python install.py install verynginx \
 && cd /usr/src/LuaJIT-$LUAJIT_VERSION \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && ln -s /usr/local/lib/libluajit-5.1.so.$LUAJIT_VERSION /usr/local/lib/libluajit-5.1.so.2 \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && cd /usr/src/nginx-$NGINX_VERSION \
 && ./configure $CONFIG --with-debug \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && mv objs/nginx objs/nginx-debug \
 && mv objs/ngx_http_xslt_filter_module.so objs/ngx_http_xslt_filter_module-debug.so \
 && mv objs/ngx_http_image_filter_module.so objs/ngx_http_image_filter_module-debug.so \
 && mv objs/ngx_http_geoip_module.so objs/ngx_http_geoip_module-debug.so \
 && mv objs/ngx_http_perl_module.so objs/ngx_http_perl_module-debug.so \
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
 && install -m755 objs/ngx_http_perl_module-debug.so /usr/lib/nginx/modules/ngx_http_perl_module-debug.so \
 && install -m755 objs/ngx_stream_geoip_module-debug.so /usr/lib/nginx/modules/ngx_stream_geoip_module-debug.so \
 && ln -s ../../usr/lib/nginx/modules /etc/nginx/modules \
 && strip /usr/sbin/nginx* \
 && strip /usr/lib/nginx/modules/*.so \
 && rm -rf /usr/src/nginx-$NGINX_VERSION \
 && rm -rf /usr/src/ngx_http_google_filter_module \
 && rm -rf /usr/src/ngx_http_substitutions_filter_module \
 && rm -rf /usr/src/VeryNginx \
 && rm -rf /usr/src/LuaJIT-$LUAJIT_VERSION \
 && rm -rf /usr/src/lua-nginx-module-$NGINX_LUA_MODULE_VERSION \
 && apk add --no-cache --virtual .gettext gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add --no-cache --virtual .nginx-rundeps $runDeps libgcc \
 && apk del .build-deps \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/ \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
COPY nginx.conf /etc/nginx/nginx.conf
COPY nginx.vh.default.conf /etc/nginx/conf.d/default.conf
EXPOSE 80/tcp 443/tcp
CMD ["nginx", "-g", "daemon", "off"]
