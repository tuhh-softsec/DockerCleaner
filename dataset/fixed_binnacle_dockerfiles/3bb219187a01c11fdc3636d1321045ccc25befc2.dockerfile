#   Requires docker >= 17.05 (requires support for multi-stage builds)
FROM alpine:3.8 AS libzauth-builder
#   Compile libzauth
COPY libs/libzauth /src/libzauth
RUN cd /src/libzauth/libzauth-c \
 && apk add make=4.2.1-r2 bash=4.4.19-r1 cargo=1.26.2-r0 libsodium-dev=1.0.16-r0 --no-cache \
 && make install
#   Nginz container
FROM alpine:3.8
ENV NGINX_VERSION="1.14.2"
#   Install libzauth
COPY --from=libzauth-builder /usr/local/include/zauth.h /usr/local/include/zauth.h
COPY --from=libzauth-builder /usr/local/lib/libzauth.so /usr/local/lib/libzauth.so
COPY --from=libzauth-builder /usr/local/lib/pkgconfig/libzauth.pc /usr/local/lib/pkgconfig/libzauth.pc
COPY services/nginz/third_party /src/third_party
RUN apk add inotify-tools=3.20.1-r1 dumb-init=1.2.1-r0 bash=4.4.19-r1 curl=7.61.1-r3 --no-cache \
 && export GPG_KEYS=B0F4253373F8F6F510D42178520A9993A1C052F8 \
 && CONFIG=" --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=nginx --group=nginx --with-http_ssl_module --with-http_stub_status_module --with-http_realip_module --with-http_gunzip_module --add-module=/src/third_party/nginx-zauth-module --add-module=/src/third_party/headers-more-nginx-module --add-module=/src/third_party/nginx-module-vts " \
 && addgroup -g 666 -S nginx \
 && adduser -u 666 -D -S -h /var/cache/nginx -s /sbin/nologin -G nginx nginx \
 && apk add libsodium-dev=1.0.16-r0 llvm-libunwind-dev=5.0.1-r0 gcc=6.4.0-r9 libc-dev=0.7.1-r0 make=4.2.1-r2 openssl-dev=1.0.2u-r0 pcre-dev=8.42-r0 zlib-dev=1.2.11-r1 linux-headers=4.4.6-r2 curl=7.61.1-r3 gnupg1=1.4.23-r0 libxslt-dev=1.1.33-r3 gd-dev=2.2.5-r4 geoip-dev=1.6.12-r1 --no-cache --virtual .build-deps \
 && curl -fSL https://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
 && curl -fSL https://nginx.org/download/nginx-$NGINX_VERSION.tar.gz.asc -o nginx.tar.gz.asc \
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
 && ./configure $CONFIG \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && rm -rf /etc/nginx/html/ \
 && mkdir /etc/nginx/conf.d/ \
 && mkdir -p /usr/share/nginx/html/ \
 && install -m644 html/index.html /usr/share/nginx/html/ \
 && install -m644 html/50x.html /usr/share/nginx/html/ \
 && install -m755 objs/nginx-debug /usr/sbin/nginx-debug \
 && ln -s ../../usr/lib/nginx/modules /etc/nginx/modules \
 && strip /usr/sbin/nginx* \
 && rm -rf /usr/src/nginx-$NGINX_VERSION \
 && apk add gettext=0.19.8.1-r2 --no-cache --virtual .gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDepsTmp="$( scanelf --needed --nobanner --format '%n#p' /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && runDeps=${runDepsTmp/so:libzauth.so/''} \
 && apk add $runDeps --no-cache --virtual .nginx-rundeps \
 && apk del .build-deps \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/ \
 && apk add tzdata=2020a-r0 --no-cache \
 && apk add libsodium=1.0.16-r0 llvm-libunwind=5.0.1-r0 --no-cache \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log \
 && apk add libgcc=6.4.0-r9 --no-cache
COPY services/nginz/nginz_reload.sh /usr/bin/nginz_reload.sh
ENTRYPOINT ["/usr/bin/dumb-init", "--"]
CMD ["/usr/bin/nginz_reload.sh", "-g", "daemon", "off", ";", "-c", "/etc/wire/nginz/conf/nginx.conf"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
