FROM python:3.7-alpine3.9
ENV OMAHA_SERVER_PATH="/srv/omaha"
WORKDIR ${OMAHA_SERVER_PATH}
#   Add low-level dependencies
RUN apk add bash=4.4.19-r1 ca-certificates=20191127-r2 \
 && apk add nginx=1.14.2-r5 supervisor=3.3.4-r1 openrc=0.39.2-r3 \
 && apk add build-base=0.5-r1 --virtual dev-deps \
 && apk add py3-lxml=4.2.5-r0 py-psycopg2=2.7.5-r0 py-pillow=5.4.1-r0 \
 && apk add fuse-dev=2.9.8-r2 libxml2-dev=2.9.9-r3 libxslt-dev=1.1.33-r3 libcurl=7.64.0-r5 curl-dev=7.64.0-r5 libstdc++=8.3.0-r0 \
 && apk add autoconf=2.69-r2 automake=1.16.1-r0 libtool=2.4.6-r5 pkgconfig openssl-dev=1.1.1k-r0 wget=1.20.3-r0 tar=1.32-r0 --virtual fuse-deps \
 && apk add linux-headers=4.18.13-r1 pcre-dev=8.42-r2 gd-dev=2.2.5-r4 geoip-dev=1.6.12-r1 \
 && apk add openssl=1.1.1k-r0 pcre=8.42-r2 zlib=1.2.11-r1 zlib-dev=1.2.11-r1 \
 && apk add build-base=0.5-r1 lua5.1-dev=5.1.5-r7 luarocks=2.4.4-r0 \
 && apk add postgresql-libs=11.11-r0 postgresql-dev=11.11-r0 libc6-compat=1.1.20-r6 gzip=1.10-r0 --no-cache \
 && apk add libffi-dev=3.2.1-r6 --no-cache \
 && pip install lxml==4.9.2
#   Prepare for building Nginx. Intsall LuaJIT.
RUN apk add luajit=2.1.0_beta3-r4 luajit-dev=2.1.0_beta3-r4 nginx-lua nginx-mod-http-lua=1.14.2-r5 \
 && wget http://luajit.org/download/LuaJIT-2.1.0-beta3.tar.gz \
 && tar -xzvf LuaJIT-2.1.0-beta3.tar.gz \
 && rm LuaJIT-2.1.0-beta3.tar.gz \
 && cd LuaJIT-2.1.0-beta3 \
 && make \
 && make install
ENV LUAJIT_LIB="/usr/local/lib"
ENV LUAJIT_INC="/usr/local/include/luajit-2.1"
#   Build Lua module for Nginx
RUN luarocks-5.1 install lua-zlib \
 && cd /tmp \
 && NGINX_VERSION=`nginx -v 2>&1 | grep -o '[[:digit:]].*$' ` \
 && wget http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz \
 && tar -xzvf nginx-$NGINX_VERSION.tar.gz \
 && wget -qO- https://api.github.com/repos/openresty/lua-nginx-module/tags | grep -m 1 tarball_url | cut -d '"' -f 4 | xargs wget -O lua-nginx-module.tar \
 && mkdir -p /tmp/lua-nginx-module \
 && tar -xvf lua-nginx-module.tar -C /tmp/lua-nginx-module --strip-components=1 \
 && wget -qO- https://api.github.com/repos/simpl/ngx_devel_kit/tags | grep -m 1 tarball_url | cut -d '"' -f 4 | xargs wget -O ngx_devel_kit.tar \
 && mkdir -p /tmp/ngx_devel_kit \
 && tar -xvf ngx_devel_kit.tar -C /tmp/ngx_devel_kit --strip-components=1 \
 && cd nginx-$NGINX_VERSION \
 && ./configure --prefix=/var/lib/nginx --sbin-path=/usr/sbin/nginx --modules-path=/usr/lib/nginx/modules --conf-path=/etc/nginx/nginx.conf --pid-path=/run/nginx/nginx.pid --lock-path=/run/nginx/nginx.lock --http-client-body-temp-path=/var/tmp/nginx/client_body --http-proxy-temp-path=/var/tmp/nginx/proxy --http-fastcgi-temp-path=/var/tmp/nginx/fastcgi --http-uwsgi-temp-path=/var/tmp/nginx/uwsgi --http-scgi-temp-path=/var/tmp/nginx/scgi --user=nginx --group=nginx --with-threads --with-file-aio --with-http_ssl_module --with-http_v2_module --with-http_realip_module --with-http_addition_module --with-http_xslt_module=dynamic --with-http_image_filter_module=dynamic --with-http_geoip_module=dynamic --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_auth_request_module --with-http_random_index_module --with-http_secure_link_module --with-http_degradation_module --with-http_slice_module --with-http_stub_status_module --with-mail=dynamic --with-mail_ssl_module --with-stream=dynamic --with-stream_ssl_module --with-stream_realip_module --with-stream_geoip_module=dynamic --with-stream_ssl_preread_module --add-dynamic-module=/tmp/ngx_devel_kit --add-dynamic-module=/tmp/lua-nginx-module \
 && make build \
 && cp objs/ndk_http_module.so /usr/lib/nginx/modules/ndk_http_module.so \
 && cp objs/ngx_http_lua_module.so /usr/lib/nginx/modules/ngx_http_lua_module.so \
 && cd /tmp \
 && rm -R /tmp/*
#   S3FS
ARG S3FS_VERSION=1.84
RUN mkdir /usr/src/omaha-server \
 && wget --no-check-certificate https://github.com/s3fs-fuse/s3fs-fuse/archive/v${S3FS_VERSION}.tar.gz -O /usr/src/omaha-server/v${S3FS_VERSION}.tar.gz \
 && tar xvz -C /usr/src/omaha-server -f /usr/src/omaha-server/v${S3FS_VERSION}.tar.gz \
 && cd /usr/src/omaha-server/s3fs-fuse-${S3FS_VERSION} \
 && ./autogen.sh \
 && ./configure --prefix=/usr \
 && make \
 && make install \
 && mkdir -p /srv/omaha_s3 \
 && rm /usr/src/omaha-server/v${S3FS_VERSION}.tar.gz
#   Filebeat
ARG FILEBEAT_PACKEGE=filebeat-6.0.0-linux-x86_64
RUN wget https://artifacts.elastic.co/downloads/beats/filebeat/${FILEBEAT_PACKEGE}.tar.gz -O /usr/src/omaha-server/${FILEBEAT_PACKEGE}.tar.gz \
 && tar xzvf /usr/src/omaha-server/${FILEBEAT_PACKEGE}.tar.gz -C /etc/ \
 && mv /etc/${FILEBEAT_PACKEGE} /etc/filebeat \
 && mkdir /tmp/filebeat/ \
 && ln /etc/filebeat/filebeat /usr/bin/
#   Clean Up
RUN rm -rf /var/cache/apk/* \
 && apk del fuse-deps dev-deps
#   Setup application dependencies
RUN mkdir -p $OMAHA_SERVER_PATH/requirements
COPY Pipfile Pipfile.lock $OMAHA_SERVER_PATH/
#   Dependencies for Pillow
RUN apk add jpeg-dev=8-r6 libjpeg=8-r6 \
 && apk add rsyslog=8.40.0-r4
RUN pip install pipenv==2023.3.20 \
 && pipenv install --system --deploy
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
