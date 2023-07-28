FROM alpine:3.9
ENV NGINX_VERSION="1.16.0"
RUN GPG_KEYS=B0F4253373F8F6F510D42178520A9993A1C052F8 \
 && CONFIG=" --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=nginx --group=nginx --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-http_perl_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-file-aio --with-http_v2_module --with-ipv6 --with-cc-opt=-I/usr/src/boringssl/.openssl/include --with-ld-opt=-L/usr/src/boringssl/.openssl/lib --add-dynamic-module=/usr/src/ngx_headers_more --add-dynamic-module=/usr/src/ngx_brotli " \
 && addgroup -S nginx \
 && adduser -D -S -h /var/cache/nginx -s /sbin/nologin -G nginx nginx \
 && apk add autoconf=2.69-r2 automake=1.16.1-r0 bind-tools=9.12.4_p2-r0 binutils=2.31.1-r2 build-base=0.5-r1 ca-certificates=20191127-r2 cmake=3.13.0-r0 curl=7.64.0-r5 gcc=8.3.0-r0 gd-dev=2.2.5-r4 geoip-dev=1.6.12-r1 git=2.20.4-r0 gnupg=2.2.19-r0 gnupg=2.2.19-r0 go=1.11.5-r0 libc-dev=0.7.1-r0 libgcc=8.3.0-r0 libstdc++=8.3.0-r0 libtool=2.4.6-r5 libxslt-dev=1.1.33-r3 linux-headers=4.18.13-r1 make=4.2.1-r2 pcre=8.42-r2 pcre-dev=8.42-r2 perl-dev=5.26.3-r1 su-exec=0.2-r0 tar=1.32-r0 tzdata=2020c-r1 zlib=1.2.11-r1 zlib-dev=1.2.11-r1 --no-cache --virtual .build-deps \
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
 && git clone --depth=1 --recurse-submodules https://github.com/eustas/ngx_brotli /usr/src/ngx_brotli \
 && git clone --depth=1 https://github.com/openresty/headers-more-nginx-module /usr/src/ngx_headers_more \
 && (git clone --depth=1 https://boringssl.googlesource.com/boringssl /usr/src/boringssl \
 && sed -i 's@out \([>=]\) TLS1_2_VERSION@out \1 TLS1_3_VERSION@' /usr/src/boringssl/ssl/ssl_lib.cc \
 && sed -i 's@ssl->version[ ]*=[ ]*TLS1_2_VERSION@ssl->version = TLS1_3_VERSION@' /usr/src/boringssl/ssl/s3_lib.cc \
 && sed -i 's@(SSL3_VERSION, TLS1_2_VERSION@(SSL3_VERSION, TLS1_3_VERSION@' /usr/src/boringssl/ssl/ssl_test.cc \
 && sed -i 's@\$shaext[ ]*=[ ]*0;@\$shaext = 1;@' /usr/src/boringssl/crypto/*/asm/*.pl \
 && sed -i 's@\$avx[ ]*=[ ]*[0|1];@\$avx = 2;@' /usr/src/boringssl/crypto/*/asm/*.pl \
 && sed -i 's@\$addx[ ]*=[ ]*0;@\$addx = 1;@' /usr/src/boringssl/crypto/*/asm/*.pl \
 && mkdir -p /usr/src/boringssl/build /usr/src/boringssl/.openssl/lib /usr/src/boringssl/.openssl/include \
 && ln -sf /usr/src/boringssl/include/openssl /usr/src/boringssl/.openssl/include/openssl \
 && touch /usr/src/boringssl/.openssl/include/openssl/ssl.h \
 && cmake -B/usr/src/boringssl/build -H/usr/src/boringssl \
 && make -C/usr/src/boringssl/build -j$( getconf _NPROCESSORS_ONLN ;) \
 && cp /usr/src/boringssl/build/crypto/libcrypto.a /usr/src/boringssl/build/ssl/libssl.a /usr/src/boringssl/.openssl/lib/ ) \
 && tar -zxC /usr/src -f nginx.tar.gz \
 && rm nginx.tar.gz \
 && cd /usr/src/nginx-$NGINX_VERSION \
 && curl -fSL https://raw.githubusercontent.com/nginx-modules/ngx_http_tls_dyn_size/556ed42/nginx__dynamic_tls_records_1.13.0%2B.patch -o dynamic_tls_records.patch \
 && patch -p1 < dynamic_tls_records.patch \
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
 && rm -rf /usr/src/boringssl /usr/src/ngx_* \
 && apk add gettext=0.19.8.1-r4 --no-cache --virtual .gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;) tzdata ca-certificates" \
 && apk add $runDeps --no-cache --virtual .nginx-rundeps \
 && apk del .build-deps \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/ \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
COPY nginx.conf /etc/nginx/nginx.conf
COPY nginx.vh.no-default.conf /etc/nginx/conf.d/default.conf
LABEL description="NGINX Docker built top of rolling release BoringSSL" \
      maintainer="Denis Denisov <denji0k@gmail.com>" \
      openssl="BoringSSL" \
      nginx="nginx $NGINX_VERSION"
EXPOSE 80/tcp 443/tcp
STOPSIGNAL SIGTERM
CMD ["nginx", "-g", "daemon", "off"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
