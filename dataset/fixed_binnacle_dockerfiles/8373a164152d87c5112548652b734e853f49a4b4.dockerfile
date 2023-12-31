#  todo 中文环境支持
#   Dockerfile - alpine
#   https://github.com/openresty/docker-openresty
FROM supermy/alpine
#  FROM alpine:latest
MAINTAINER JamesMo <springclick@gmail.com>
#   Docker Build Arguments
ARG RESTY_VERSION="1.11.2.3"
ARG RESTY_LUAROCKS_VERSION="2.4.2"
ARG RESTY_OPENSSL_VERSION="1.0.2k"
ARG RESTY_PCRE_VERSION="8.39"
ARG RESTY_J="1"
ARG RESTY_CONFIG_OPTIONS=" --with-file-aio  --with-http_addition_module  --with-http_auth_request_module  --with-http_dav_module  --with-http_flv_module  --with-http_geoip_module=dynamic  --with-http_gunzip_module  --with-http_gzip_static_module  --with-http_image_filter_module=dynamic  --with-http_mp4_module  --with-http_random_index_module  --with-http_realip_module  --with-http_secure_link_module  --with-http_slice_module  --with-http_ssl_module  --with-http_stub_status_module  --with-http_sub_module  --with-http_v2_module  --with-http_xslt_module=dynamic  --with-ipv6  --with-mail  --with-mail_ssl_module  --with-md5-asm  --with-pcre-jit  --with-sha1-asm  --with-stream  --with-stream_ssl_module  --with-threads  "
#   These are not intended to be user-specified
ARG _RESTY_CONFIG_DEPS="--with-openssl=/tmp/openssl-${RESTY_OPENSSL_VERSION} --with-pcre=/tmp/pcre-${RESTY_PCRE_VERSION}"
#   1) Install apk dependencies
#   2) Download and untar OpenSSL, PCRE, and OpenResty
#   3) Build OpenResty
#   4) Cleanup
RUN apk add curl=7.88.1-r1 gd-dev=2.3.3-r3 geoip-dev=1.6.12-r3 libxslt-dev=1.1.37-r1 perl-dev=5.36.0-r0 readline-dev=8.2.0-r0 zlib-dev=1.2.13-r0 --no-cache --virtual .build-deps \
 && apk add build-base=0.5-r3 curl=7.88.1-r1 gd=2.3.3-r3 geoip=1.6.12-r3 libgcc=12.2.1_git20220924-r4 libxslt=1.1.37-r1 linux-headers=5.19.5-r0 make=4.3-r1 perl=5.36.0-r0 unzip=6.0-r13 zlib=1.2.13-r0 --no-cache \
 && cd /tmp \
 && curl -k -fSL https://www.openssl.org/source/openssl-${RESTY_OPENSSL_VERSION}.tar.gz -o openssl-${RESTY_OPENSSL_VERSION}.tar.gz \
 && tar xzf openssl-${RESTY_OPENSSL_VERSION}.tar.gz \
 && curl -k -fSL https://ftp.pcre.org/pub/pcre/pcre-${RESTY_PCRE_VERSION}.tar.gz -o pcre-${RESTY_PCRE_VERSION}.tar.gz \
 && tar xzf pcre-${RESTY_PCRE_VERSION}.tar.gz \
 && curl -k -fSL https://openresty.org/download/openresty-${RESTY_VERSION}.tar.gz -o openresty-${RESTY_VERSION}.tar.gz \
 && tar xzf openresty-${RESTY_VERSION}.tar.gz \
 && cd /tmp/openresty-${RESTY_VERSION} \
 && ./configure -j${RESTY_J} ${_RESTY_CONFIG_DEPS} ${RESTY_CONFIG_OPTIONS} \
 && make -j${RESTY_J} \
 && make -j${RESTY_J} install \
 && cd /tmp \
 && rm -rf openssl-${RESTY_OPENSSL_VERSION} openssl-${RESTY_OPENSSL_VERSION}.tar.gz openresty-${RESTY_VERSION}.tar.gz openresty-${RESTY_VERSION} pcre-${RESTY_PCRE_VERSION}.tar.gz pcre-${RESTY_PCRE_VERSION}
#  安装 luarocks
RUN apk add git=2.38.4-r1 --no-cache \
 && cd /tmp \
 && git clone https://github.com/keplerproject/luarocks.git \
 && cd luarocks \
 && sh ./configure --prefix=/usr/local/openresty/luajit --with-lua=/usr/local/openresty/luajit/ --lua-suffix=jit --with-lua-include=/usr/local/openresty/luajit/include/luajit-2.1 \
 && make build install \
 && cd \
 && rm -rf /tmp/luarocks \
 && cd /tmp \
 && rm -rf luarocks-${RESTY_LUAROCKS_VERSION} luarocks-${RESTY_LUAROCKS_VERSION}.tar.gz \
 && apk del .build-deps \
 && ln -sf /dev/stdout /usr/local/openresty/nginx/logs/access.log \
 && ln -sf /dev/stderr /usr/local/openresty/nginx/logs/error.log
#  RUN curl  -fSL http://luarocks.org/releases/luarocks-${RESTY_LUAROCKS_VERSION}.tar.gz -o luarocks-${RESTY_LUAROCKS_VERSION}.tar.gz \
#      && tar xzf luarocks-${RESTY_LUAROCKS_VERSION}.tar.gz \
#      && cd luarocks-${RESTY_LUAROCKS_VERSION} \
#      && ./configure \
#          --prefix=/usr/local/openresty/luajit \
#          --with-lua=/usr/local/openresty/luajit \
#          --lua-suffix=jit-2.1.0-beta2 \
#          --with-lua-include=/usr/local/openresty/luajit/include/luajit-2.1 \
#      && make build \
#      && make install \
#
#      && cd /tmp \
#      && rm -rf luarocks-${RESTY_LUAROCKS_VERSION} luarocks-${RESTY_LUAROCKS_VERSION}.tar.gz \
#      && apk del .build-deps \
#
#      && ln -sf /dev/stdout /usr/local/openresty/nginx/logs/access.log \
#      && ln -sf /dev/stderr /usr/local/openresty/nginx/logs/error.log
#   Add additional binaries into PATH for convenience
ENV PATH="$PATH:/usr/local/openresty/luajit/bin/:/usr/local/openresty/nginx/sbin/:/usr/local/openresty/bin/"
ENTRYPOINT ["/usr/local/openresty/bin/openresty", "-g", "daemon", "off"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
