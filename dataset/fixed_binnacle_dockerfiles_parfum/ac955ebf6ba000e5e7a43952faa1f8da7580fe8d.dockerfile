FROM swerpbox/alpine-base
ARG NGINX_VERSION=1.11.9
ARG BROTLI_COMMIT=1b364aeb42a0919e7c08646aa4f2f50e28d69fa5
ARG NGX_BROTLI_COMMIT=12529813a9f8475718370a19007c7905601a62ad
LABEL org.label-schema.vendor="SwerpBox: Frontend" \
      org.label-schema.build-date="2017-01-27T00:12:00+00:00" \
      org.label-schema.name="Frontend (nginx) running on Alpine 3.5" \
      org.label-schema.vcs-type="git" \
      org.label-schema.vcs-url="https://github.com/strues/swerpbox" \
      maintainer="Steven Truesdell <steven@strues.io>"
RUN GPG_KEYS=B0F4253373F8F6F510D42178520A9993A1C052F8 \
 && CONFIG=" --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/logs/frontend/nginx-error.log --http-log-path=/logs/frontend/nginx-access.log --pid-path=/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=swerp --group=swerp --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-http_perl_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-http_slice_module --with-file-aio --with-http_v2_module --with-ipv6 --add-module=/usr/src/ngx_brotli --with-cc-opt=-Wno-error " \
 && apk add --no-cache --virtual .build-deps gcc libc-dev make openssl-dev pcre-dev zlib-dev linux-headers curl gnupg libxslt-dev gd-dev geoip-dev perl-dev \
 && apk add --no-cache --virtual .brotli-build-deps autoconf libtool automake git g++ cmake \
 && mkdir -p /usr/src \
 && mkdir -p /logs/frontend \
 && cd /usr/src \
 && git clone https://github.com/google/brotli.git \
 && cd brotli \
 && git checkout -b $BROTLI_COMMIT $BROTLI_COMMIT \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=/usr/lib -DBUILD_SHARED_LIBS=ON \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && cd .. \
 && rm -rf brotli \
 && git clone https://github.com/google/ngx_brotli.git \
 && cd ngx_brotli \
 && git checkout -b $NGX_BROTLI_COMMIT $NGX_BROTLI_COMMIT \
 && cd .. \
 && curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
 && curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz.asc -o nginx.tar.gz.asc \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEYS" \
 && gpg --batch --verify nginx.tar.gz.asc nginx.tar.gz \
 && rm -r "$GNUPGHOME" nginx.tar.gz.asc \
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
 && mv objs/ngx_http_perl_module.so objs/ngx_http_perl_module-debug.so \
 && ./configure $CONFIG \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && rm -rf /etc/nginx/html/ \
 && rm -rf /etc/nginx/conf.d/* \
 && mkdir /etc/nginx/vhost.d/ \
 && mkdir -p /var/www/public \
 && install -m755 objs/nginx-debug /usr/sbin/nginx-debug \
 && install -m755 objs/ngx_http_xslt_filter_module-debug.so /usr/lib/nginx/modules/ngx_http_xslt_filter_module-debug.so \
 && install -m755 objs/ngx_http_image_filter_module-debug.so /usr/lib/nginx/modules/ngx_http_image_filter_module-debug.so \
 && install -m755 objs/ngx_http_geoip_module-debug.so /usr/lib/nginx/modules/ngx_http_geoip_module-debug.so \
 && install -m755 objs/ngx_http_perl_module-debug.so /usr/lib/nginx/modules/ngx_http_perl_module-debug.so \
 && ln -s ../../usr/lib/nginx/modules /etc/nginx/modules \
 && strip /usr/sbin/nginx* \
 && strip /usr/lib/nginx/modules/*.so \
 && rm -rf /usr/src/nginx-$NGINX_VERSION \
 && rm -rf /usr/src/ngx_brotli \
 && apk add --no-cache --virtual .gettext gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add --no-cache --virtual .nginx-rundeps $runDeps \
 && apk del .build-deps \
 && apk del .brotli-build-deps \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/ \
 && ln -sf /dev/stdout /logs/frontend/nginx-access.log \
 && ln -sf /dev/stderr /logs/frontend/nginx-error.log \
 && chmod -R 0777 /logs/frontend \
 && mkdir -p /var/cache/nginx/client_temp \
 && chmod 0777 /var/cache/nginx/client_temp
RUN apk add --no-cache apache2-utils
EXPOSE 80/tcp 443/tcp
COPY rootfs/ /
VOLUME ["/config", "/data", "/logs"]
