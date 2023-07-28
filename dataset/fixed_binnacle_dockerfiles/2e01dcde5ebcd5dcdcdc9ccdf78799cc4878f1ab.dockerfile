#   Image includes:
#   - nginx (with few common modules)
#   - nginx_upstream_module (for tarantool)
#   - nginx_lua_module (required by luajit)
#   - nginx_devel_kit (required by luajit)
#   - luajit
#   - luarocks
#   - lua rock "lua-cjson" (is necessary to work with tarantool response, can be extended with env variable)
FROM alpine:3.5
MAINTAINER Konstantin Nazarov "mail@racktear.com"
RUN addgroup -S nginx \
 && adduser -S -G nginx nginx \
 && apk add 'su-exec>=0.2' --no-cache
ENV NGINX_VERSION="1.11.1" \
    NGINX_UPSTREAM_MODULE_URL="https://github.com/tarantool/nginx_upstream_module.git" \
    NGINX_UPSTREAM_MODULE_COMMIT="3736239b51cc48776ee49088d7d674ff37791544" \
    NGINX_GPG_KEYS="B0F4253373F8F6F510D42178520A9993A1C052F8"
ENV NGINX_LUA_MODULE_URL="https://github.com/openresty/lua-nginx-module" \
    NGINX_LUA_MODULE_PATH="/usr/src/lua-nginx-module"
ENV NGINX_DEVEL_KIT_URL="https://github.com/simpl/ngx_devel_kit" \
    NGINX_DEVEL_KIT_PATH="/usr/src/nginx-devel-kit"
ENV LUAJIT_VERSION="2.0.5" \
    LUAJIT_URL="http://luajit.org/git/luajit-2.0.git" \
    LUAJIT_PATH="/usr/src/luajit" \
    LUAJIT_LIB="/usr/local/lib" \
    LUAJIT_INC="/usr/local/include/luajit-2.0"
ENV LUAROCKS_VERSION="2.4.2" \
    LUAROCKS_URL="https://github.com/luarocks/luarocks" \
    LUAROCKS_PATH="/usr/src/luarocks"
ENV LUAROCKS_ROCKS="lua-cjson"
RUN set -x \
 && apk add build-base=0.4-r1 cmake=3.6.3-r0 linux-headers=4.4.6-r1 libressl-dev=2.4.4-r0 pcre-dev=8.39-r0 zlib-dev=1.2.11-r0 libxslt-dev=1.1.29-r1 gd-dev=2.2.5-r1 geoip-dev=1.6.9-r0 git=2.11.3-r2 tar=1.29-r1 gnupg=2.1.15-r1 curl=7.61.1-r1 perl-dev=5.24.4-r1 unzip=6.0-r3 gcc=6.2.1-r1 perl=5.24.4-r1 --no-cache --virtual .build-deps \
 && apk add ca-certificates=20161130-r1 libressl=2.4.4-r0 pcre=8.39-r0 zlib=1.2.11-r0 libxslt=1.1.29-r1 gd=2.2.5-r1 geoip=1.6.9-r0 gettext=0.19.8.1-r0 libgcc=6.2.1-r1 --no-cache --virtual .run-deps \
 && git config --global http.postBuffer 524288000 \
 && : "---------- download nginx-devel-kit ----------" \
 && git clone "$NGINX_DEVEL_KIT_URL" $NGINX_DEVEL_KIT_PATH \
 && : "---------- download nginx-lua-module ----------" \
 && git clone "$NGINX_LUA_MODULE_URL" $NGINX_LUA_MODULE_PATH \
 && : "---------- download luajit ----------" \
 && git clone "$LUAJIT_URL" $LUAJIT_PATH \
 && git -C $LUAJIT_PATH checkout tags/v$LUAJIT_VERSION \
 && make -C $LUAJIT_PATH \
 && make -C $LUAJIT_PATH install \
 && : "---------- download and install luarocks (depends on luajit) ----------" \
 && git clone $LUAROCKS_URL $LUAROCKS_PATH \
 && git -C $LUAROCKS_PATH checkout tags/v$LUAROCKS_VERSION \
 && ln -s /usr/local/bin/luajit-$LUAJIT_VERSION /usr/local/bin/lua \
 && cd $LUAROCKS_PATH \
 && ./configure --with-lua-bin=/usr/local/bin --with-lua-include=/usr/src/luajit/src/ \
 && make build \
 && make install \
 && cd \
 && : "---------- download nginx-upstream-module ----------" \
 && git clone "$NGINX_UPSTREAM_MODULE_URL" /usr/src/nginx_upstream_module \
 && git -C /usr/src/nginx_upstream_module checkout "${NGINX_UPSTREAM_MODULE_COMMIT}" \
 && git -C /usr/src/nginx_upstream_module submodule init \
 && git -C /usr/src/nginx_upstream_module submodule update \
 && make -C /usr/src/nginx_upstream_module yajl \
 && make -C /usr/src/nginx_upstream_module msgpack \
 && : "---------- download nginx ----------" \
 && curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
 && curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz.asc -o nginx.tar.gz.asc \
 && : "---------- verify signatures ----------" \
 && mkdir -p /usr/src/nginx \
 && tar -xzf nginx.tar.gz -C /usr/src/nginx --strip-components=1 \
 && cd /usr/src/nginx \
 && : "---------- build nginx ----------" \
 && ./configure --with-cc-opt='-I/usr/src/nginx_upstream_module/third_party/third_party/msgpuck -I /usr/src/nginx_upstream_module/third_party/yajl/build/yajl-2.1.0/include' --with-ld-opt='/usr/src/nginx_upstream_module/third_party/yajl/build/yajl-2.1.0/lib/libyajl_s.a -L /usr/src/nginx_upstream_module/third_party/third_party/msgpuck' --add-module=/usr/src/nginx_upstream_module --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=nginx --group=nginx --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-http_perl_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-http_slice_module --with-mail --with-mail_ssl_module --with-file-aio --with-http_v2_module --with-ipv6 --with-ld-opt="-Wl,-rpath,$LUAJIT_LIB" --add-module=$NGINX_DEVEL_KIT_PATH --add-module=$NGINX_LUA_MODULE_PATH \
 && make \
 && make install \
 && rm -rf /etc/nginx/html/ \
 && mkdir /etc/nginx/conf.d/ \
 && mkdir -p /usr/share/nginx/html/ \
 && install -m644 html/index.html /usr/share/nginx/html/ \
 && install -m644 html/50x.html /usr/share/nginx/html/ \
 && : "---------- install module deps ----------" \
 && runDeps="$( scanelf --needed --nobanner /usr/sbin/nginx /usr/lib/nginx/modules/*.so | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add $runDeps --virtual .run-deps \
 && : "---------- install lua rocks ----------" \
 && for rock in $LUAROCKS_ROCKS; do luarocks install $rock ; done \
 && : "---------- remove build deps ----------" \
 && rm -rf /usr/src/nginx \
 && rm -rf /usr/src/nginx_upstream_module \
 && apk del .build-deps \
 && : "---------- redirect logs to default collector ----------" \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
COPY nginx.conf /etc/nginx/nginx.conf
COPY nginx.vh.default.conf /etc/nginx/conf.d/default.conf
VOLUME ["/var/cache/nginx"]
EXPOSE 80/tcp 443/tcp
CMD ["nginx", "-g", "daemon", "off"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
