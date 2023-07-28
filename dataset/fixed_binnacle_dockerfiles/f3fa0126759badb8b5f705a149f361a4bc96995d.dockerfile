FROM alpine:3.7
#   解释信息
LABEL maintainer="HeLei <dayugog@gmail.com>"
ENV NGINX_VERSION="1.14.0" \
    TZ="Asia/Shanghai" \
    ZLIB_VERSION="1.2.11" \
    PCRE_VERSION="8.42" \
    OPENSSL_VERSION="1.0.2o" \
    SRC_DIR="/home/work/src" \
    WWW_DIR="/home/work/www" \
    LOG_DIR="/home/work/logs/nginx"
#   设置系统时区
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone
#   拷贝文件
COPY src/ $SRC_DIR
#   下载并编译
RUN set -ex ; addgroup -S work \
 && adduser -D -S -h /var/cache/nginx -s /sbin/nologin -G work work ; apk add gcc=6.4.0-r5 g++=6.4.0-r5 libc-dev=0.7.1-r0 make=4.2.1-r0 openssl-dev=1.0.2t-r0 pcre-dev=8.41-r1 zlib-dev=1.2.11-r1 linux-headers=4.4.6-r2 curl=7.61.1-r3 gnupg=2.2.3-r1 libxslt-dev=1.1.31-r2 gd-dev=2.2.5-r3 geoip-dev=1.6.11-r0 --no-cache --virtual .build-deps ; cd $SRC_DIR \
 && tar -zxvf nginx-$NGINX_VERSION.tar.gz \
 && tar -zxvf zlib-$ZLIB_VERSION.tar.gz \
 && tar -zxvf pcre-$PCRE_VERSION.tar.gz \
 && tar -zxvf openssl-$OPENSSL_VERSION.tar.gz \
 && cd nginx-$NGINX_VERSION ; BUILD_CONFIG=" --prefix=/home/work/app/nginx --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=work --group=work --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-http_auth_request_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-threads --with-stream --with-stream_ssl_module --with-stream_ssl_preread_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-http_slice_module --with-mail --with-mail_ssl_module --with-compat --with-file-aio --with-http_v2_module --with-zlib=$SRC_DIR/zlib-$ZLIB_VERSION --with-openssl=$SRC_DIR/openssl-$OPENSSL_VERSION --with-pcre=$SRC_DIR/pcre-$PCRE_VERSION " ; ./configure $BUILD_CONFIG ; make \
 && make install ; rm -rf /home/work/src \
 && apk del .build-deps ; mkdir -p $WWW_DIR \
 && mkdir -p $LOG_DIR \
 && chown -R work:work /home
#   设置环境变量
ENV PATH="/home/work/app/nginx/sbin:$PATH"
#   拷贝配置文件
COPY conf/ /home/work/app/nginx/conf
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 8080/tcp 443/tcp
CMD ["nginx-server"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
