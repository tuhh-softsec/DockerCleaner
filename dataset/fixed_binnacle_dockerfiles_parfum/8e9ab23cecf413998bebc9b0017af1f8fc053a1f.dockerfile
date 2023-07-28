FROM ubuntu:16.04
#  Build Arguments for openresty/nginx
ARG RESTY_VERSION="1.11.2.1"
ARG RESTY_OPENSSL_VERSION="1.0.2j"
ARG PAGESPEED_VERSION="1.11.33.4"
ARG DEBIAN_FRONTEND=noninteractive
ARG TERM=xterm-color
ARG RESTY_CONFIG_OPTIONS=" --with-http_addition_module  --with-http_auth_request_module  --with-http_flv_module  --with-http_gunzip_module  --with-http_gzip_static_module  --with-http_mp4_module  --with-http_random_index_module  --with-http_realip_module  --with-http_secure_link_module  --with-http_slice_module  --with-http_ssl_module  --with-http_stub_status_module  --with-http_sub_module  --with-http_v2_module  --with-http_geoip_module=dynamic  --with-file-aio  --with-ipv6  --with-pcre-jit  --with-stream  --with-stream_ssl_module  --with-threads  --without-http_autoindex_module  --without-http_browser_module  --without-http_userid_module  --without-mail_pop3_module  --without-mail_imap_module  --without-mail_smtp_module  --without-http_split_clients_module  --without-http_uwsgi_module  --without-http_scgi_module  --without-http_referer_module  --user=nginx  --group=nginx  --sbin-path=/usr/sbin  --modules-path=/usr/lib/nginx  --prefix=/etc/nginx  --conf-path=/etc/nginx/nginx.conf  --http-log-path=/var/log/nginx/access.log  --error-log-path=/var/log/nginx/error.log  --pid-path=/var/run/nginx.pid  --lock-path=/var/run/nginx/nginx.lock  --http-fastcgi-temp-path=/tmp/nginx/fastcgi  --http-proxy-temp-path=/tmp/nginx/proxy  --http-client-body-temp-path=/tmp/nginx/client_body  --add-module=/tmp/incubator-pagespeed-ngx-${PAGESPEED_VERSION}-beta  --add-module=/tmp/ngx_cache_purge-2.3  --with-openssl=/tmp/openssl-${RESTY_OPENSSL_VERSION}  "
ARG BUILD_DEPS='build-essential curl libreadline-dev libncurses5-dev libpcre3-dev libgeoip-dev zlib1g-dev ca-certificates'
#  Install base utils
RUN apt-get update \
 && apt-get install --no-install-recommends $BUILD_DEPS -y \
 && cd /tmp/ \
 && echo "Downloading PageSpeed..." \
 && curl -L https://github.com/pagespeed/ngx_pagespeed/archive/v${PAGESPEED_VERSION}-beta.tar.gz | tar -zx \
 && ls -lah \
 && cd /tmp/incubator-pagespeed-ngx-${PAGESPEED_VERSION}-beta/ \
 && echo "Downloading PSOL..." \
 && curl -L https://dl.google.com/dl/page-speed/psol/${PAGESPEED_VERSION}.tar.gz | tar -zx \
 && cd /tmp/ \
 && echo "Downloading Nginx cache purge module..." \
 && curl -L http://labs.frickle.com/files/ngx_cache_purge-2.3.tar.gz | tar -zx \
 && echo "Downloading OpenSSL..." \
 && curl -L https://www.openssl.org/source/openssl-${RESTY_OPENSSL_VERSION}.tar.gz | tar -zx \
 && echo "Downloading openresty..." \
 && curl -L https://openresty.org/download/openresty-${RESTY_VERSION}.tar.gz | tar -zx \
 && readonly NPROC=$( grep -c ^processor /proc/cpuinfo 2> /dev/null || 1 ;) \
 && echo "using up to $NPROC threads" \
 && cd openresty-${RESTY_VERSION} \
 && ./configure -j${NPROC} ${_RESTY_CONFIG_DEPS} ${RESTY_CONFIG_OPTIONS} \
 && make -j${NPROC} \
 && make -j${NPROC} install \
 && mkdir -p /var/lib/nginx /var/log/nginx \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get remove --purge -y $BUILD_DEPS $( apt-mark showauto ;) \
 && rm -rf /tmp/* /var/log/apt/* \
 && mkdir /tmp/nginx/ \
 && mkdir -p /tmp/nginx/pagespeed/images/ \
 && ln -sf /usr/lib/nginx /etc/nginx/modules \
 && groupadd -g 8888 nginx \
 && useradd -u 8888 -g nginx nginx \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
WORKDIR /config
RUN apt-get update \
 && apt-get install --no-install-recommends wget -y \
 && wget --no-check-certificate https://raw.githubusercontent.com/jivesearch/jivesearch/master/frontend/nginx/nginx.conf \
 && wget --no-check-certificate https://raw.githubusercontent.com/jivesearch/jivesearch/master/frontend/nginx/server.conf
EXPOSE 80/tcp 443/tcp
CMD ["nginx", "-g", "daemon", "off", ";", "-c", "/config/nginx.conf"]
