FROM alpine:3.5
ENV NGINX_VERSION="1.13.5"
RUN set -x \
 && mkdir -p /tmp/src/nginx /usr/lib/nginx/modules /var/cache/nginx \
 && apk add curl=7.61.1-r1 gcc=6.2.1-r1 gd-dev=2.2.5-r1 geoip-dev=1.6.9-r0 gnupg=2.1.15-r1 libc-dev=0.7-r1 libxslt-dev=1.1.29-r1 linux-headers=4.4.6-r1 make=4.2.1-r0 openldap-dev=2.4.44-r4 pcre-dev=8.39-r0 tar=1.29-r1 unzip=6.0-r3 zlib-dev=1.2.11-r0 --no-cache --virtual .build-deps \
 && curl -fsSL http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz | tar vxz --strip=1 -C /tmp/src/nginx \
 && curl -fsSL https://github.com/kvspb/nginx-auth-ldap/archive/master.zip -o /tmp/nginx-auth-ldap-master.zip \
 && unzip -d /tmp/src /tmp/nginx-auth-ldap-master.zip \
 && cd /tmp/src/nginx \
 && addgroup -S nginx \
 && adduser -D -S -h /var/cache/nginx -s /sbin/nologin -G nginx nginx \
 && ./configure --prefix=/usr/share/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=nginx --group=nginx --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-file-aio --with-http_v2_module --add-module=/tmp/src/nginx-auth-ldap-master \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && mkdir -vp /etc/nginx/conf.d/ /usr/share/nginx/html/ \
 && install -m644 html/index.html /usr/share/nginx/html/ \
 && install -m644 html/50x.html /usr/share/nginx/html/ \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log \
 && apk add gettext=0.19.8.1-r0 --no-cache --virtual .gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add $runDeps --no-cache --virtual .nginx-rundeps \
 && apk del .build-deps \
 && apk del .gettext \
 && rm -fr /etc/nginx/*.default /tmp/* /var/tmp/* /var/cache/apk/*
COPY --from=library/nginx:alpine /etc/nginx/nginx.conf /etc/nginx/nginx.conf
COPY --from=library/nginx:alpine /etc/nginx/conf.d/default.conf /etc/nginx/conf.d/default.conf
EXPOSE 80/tcp
STOPSIGNAL SIGTERM
CMD ["nginx", "-g", "daemon", "off"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!