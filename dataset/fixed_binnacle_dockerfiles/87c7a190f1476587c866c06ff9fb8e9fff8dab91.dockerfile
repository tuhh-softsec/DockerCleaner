FROM aqzt/docker-alpine
LABEL maintainer="aqzt.com (ppabc@qq.com)"
#  ## Set Nginx Version Number
ARG NGINX_VERSION=1.14.0
#  ## Install Nginx
RUN set -x ; CONFIG=" --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=nginx --group=nginx --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-http_perl_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-file-aio --with-http_v2_module " ; addgroup -S www-data ; adduser -D -S -h /var/cache/nginx -s /sbin/nologin -G www-data nginx ; apk add gcc=12.2.1_git20220924-r4 gd-dev=2.3.3-r3 geoip-dev=1.6.12-r3 gnupg=2.2.40-r0 libc-dev=0.7.2-r3 libressl-dev=3.6.2-r0 libxslt-dev=1.1.37-r1 linux-headers=5.19.5-r0 make=4.3-r1 pcre-dev=8.45-r2 perl-dev=5.36.0-r0 tar=1.34-r2 zlib-dev=1.2.13-r0 --no-cache --virtual .nginx-build-deps ; mkdir -p /www /data/logs/nginx ; curl -fSL http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz ; mkdir -p /usr/src ; tar -zxC /usr/src -f nginx.tar.gz ; rm nginx.tar.gz ; cd /usr/src/nginx-$NGINX_VERSION ; ./configure $CONFIG --with-debug ; make -j$( getconf _NPROCESSORS_ONLN ;) ; mv objs/nginx objs/nginx-debug ; mv objs/ngx_http_xslt_filter_module.so objs/ngx_http_xslt_filter_module-debug.so ; mv objs/ngx_http_image_filter_module.so objs/ngx_http_image_filter_module-debug.so ; mv objs/ngx_http_geoip_module.so objs/ngx_http_geoip_module-debug.so ; mv objs/ngx_http_perl_module.so objs/ngx_http_perl_module-debug.so ; mv objs/ngx_stream_geoip_module.so objs/ngx_stream_geoip_module-debug.so ; ./configure $CONFIG ; make -j$( getconf _NPROCESSORS_ONLN ;) ; make install ; rm -rf /etc/nginx/html/ ; mkdir -p /etc/nginx/conf.d/ ; mkdir -p /usr/share/nginx/html/ ; install -m644 html/index.html /usr/share/nginx/html/ ; install -m644 html/50x.html /usr/share/nginx/html/ ; install -m755 objs/nginx-debug /usr/sbin/nginx-debug ; install -m755 objs/ngx_http_xslt_filter_module-debug.so /usr/lib/nginx/modules/ngx_http_xslt_filter_module-debug.so ; install -m755 objs/ngx_http_image_filter_module-debug.so /usr/lib/nginx/modules/ngx_http_image_filter_module-debug.so ; install -m755 objs/ngx_http_geoip_module-debug.so /usr/lib/nginx/modules/ngx_http_geoip_module-debug.so ; install -m755 objs/ngx_http_perl_module-debug.so /usr/lib/nginx/modules/ngx_http_perl_module-debug.so ; install -m755 objs/ngx_stream_geoip_module-debug.so /usr/lib/nginx/modules/ngx_stream_geoip_module-debug.so ; ln -s ../../usr/lib/nginx/modules /etc/nginx/modules ; strip /usr/sbin/nginx* ; strip /usr/lib/nginx/modules/*.so ; rm -rf /usr/src/nginx-$NGINX_VERSION ; apk add gettext=0.21.1-r1 --no-cache --virtual .gettext ; mv /usr/bin/envsubst /tmp/ ; runDeps="$( scanelf --needed --nobanner /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" ; apk add apache2-utils=2.4.57-r0 $runDeps --no-cache --virtual .nginx-rundeps ; apk del .nginx-build-deps ; apk del .gettext ; mv /tmp/envsubst /usr/local/bin/ ; rm -rf /usr/src/* /var/tmp/* /var/cache/apk/* ; mkdir -p /data/logs
#  ## Networking Configuration
EXPOSE 80/tcp 443/tcp
#  ## Files Addition
COPY root /
RUN chmod +x /etc/zabbix/zabbix_agentd.conf.d/scripts/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
