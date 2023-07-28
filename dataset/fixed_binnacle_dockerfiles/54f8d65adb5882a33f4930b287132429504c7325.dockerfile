#   Copyright 2018 the original author or authors.
#
#   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
#   in compliance with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software distributed under the License
#   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
#   or implied. See the License for the specific language governing permissions and limitations under
#   the License.
#   Dockerfile - alpine - openresty - Lua auth-crowd - modsecurity
#   related links found in the curl downloaded links and:
#   https://github.com/openresty/docker-openresty
FROM alpine:3.8
LABEL maintainer="Gerard Castillo <gerard.castillo@boehringer-ingelheim.com>"
#   Docker Build Arguments
ARG NGINX_VERSION="1.13.6"
ARG MODSECURITY_VERSION="3.0.0"
ARG MODSECURITY_NGINX_VERSION="1.0.0"
ARG OWASP_MODSECURITY_CRS_VERSION="3.0.2"
ARG RESTY_VERSION="1.13.6.2"
ARG RESTY_OPENSSL_VERSION="1.0.2p"
ARG RESTY_PCRE_VERSION="8.42"
ARG RESTY_J="1"
ARG RESTY_CONFIG_OPTIONS=" --prefix=/etc/nginx  --sbin-path=/usr/sbin/nginx  --modules-path=/usr/lib/nginx/modules  --conf-path=/etc/nginx/nginx.conf  --error-log-path=/var/log/nginx/error.log  --http-log-path=/var/log/nginx/access.log  --pid-path=/var/run/nginx.pid  --lock-path=/var/run/nginx.lock  --http-client-body-temp-path=/var/cache/nginx/client_temp  --http-proxy-temp-path=/var/cache/nginx/proxy_temp  --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp  --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp  --http-scgi-temp-path=/var/cache/nginx/scgi_temp  --user=nginx  --group=nginx  --with-stream  --with-stream_ssl_module  --with-stream_ssl_preread_module  --with-stream_realip_module  --with-stream_geoip_module=dynamic  --with-http_slice_module  --with-mail  --with-mail_ssl_module  --with-compat  --with-file-aio  --with-http_addition_module  --with-http_auth_request_module  --with-http_dav_module  --with-http_flv_module  --with-http_geoip_module=dynamic  --with-http_gunzip_module  --with-http_gzip_static_module  --with-http_image_filter_module=dynamic  --with-http_mp4_module  --with-http_random_index_module  --with-http_realip_module  --with-http_secure_link_module  --with-http_ssl_module  --with-http_stub_status_module  --with-http_sub_module  --with-http_v2_module  --with-http_xslt_module=dynamic  --with-ipv6  --with-md5-asm  --with-pcre-jit  --with-sha1-asm  --with-threads  --with-pcre-jit  --with-ipv6  --add-dynamic-module=/opt/modsecurity-nginx-v${MODSECURITY_NGINX_VERSION}  --with-ld-opt='-lcurl'  "
ARG RESTY_CONFIG_OPTIONS_MORE=""
ARG RESTY_ADD_PACKAGE_BUILDDEPS=""
ARG RESTY_ADD_PACKAGE_RUNDEPS=""
ARG RESTY_EVAL_PRE_CONFIGURE=""
ARG RESTY_EVAL_POST_MAKE=""
LABEL resty_version="${RESTY_VERSION}"
LABEL resty_openssl_version="${RESTY_OPENSSL_VERSION}"
LABEL resty_pcre_version="${RESTY_PCRE_VERSION}"
LABEL resty_config_options="${RESTY_CONFIG_OPTIONS}"
LABEL resty_config_options_more="${RESTY_CONFIG_OPTIONS_MORE}"
LABEL resty_add_package_builddeps="${RESTY_ADD_PACKAGE_BUILDDEPS}"
LABEL resty_add_package_rundeps="${RESTY_ADD_PACKAGE_RUNDEPS}"
LABEL resty_eval_pre_configure="${RESTY_EVAL_PRE_CONFIGURE}"
LABEL resty_eval_post_make="${RESTY_EVAL_POST_MAKE}"
#   These are not intended to be user-specified
ARG _RESTY_CONFIG_DEPS="--with-openssl=/opt/openssl-${RESTY_OPENSSL_VERSION} --with-pcre=/opt/pcre-${RESTY_PCRE_VERSION}"
RUN apk add build-base=0.5-r1 curl=7.61.1-r3 gd-dev=2.2.5-r4 geoip-dev=1.6.12-r1 libxslt-dev=1.1.33-r3 linux-headers=4.4.6-r2 make=4.2.1-r2 perl-dev=5.26.3-r0 readline-dev=7.0.003-r0 zlib-dev=1.2.11-r1 gcc=6.4.0-r9 libc-dev=0.7.1-r0 gnupg=2.2.19-r0 ${RESTY_ADD_PACKAGE_BUILDDEPS} --no-cache --virtual .build-deps \
 && apk add libxml2-dev=2.9.8-r2 git=2.18.4-r0 libtool=2.4.6-r5 automake=1.16.1-r0 autoconf=2.69-r2 g++=6.4.0-r9 flex=2.6.4-r1 bison=3.0.4-r1 yajl-dev=2.1.0-r0 --no-cache --virtual .libmodsecurity-deps \
 && apk add syslog-ng=3.13.2-r3 pcre-dev=8.42-r0 gd=2.2.5-r4 geoip=1.6.12-r1 libgcc=6.4.0-r9 libxslt=1.1.33-r3 zlib=1.2.11-r1 doxygen=1.8.14-r0 geoip-dev=1.6.12-r1 yajl=2.1.0-r0 libstdc++=6.4.0-r9 git=2.18.4-r0 sed=4.4-r2 libmaxminddb-dev=1.3.2-r0 libcurl=7.61.1-r3 libssh2-dev=1.9.0-r1 nghttp2-dev=1.39.2-r0 pkgconf=1.5.3-r0 curl-dev=7.61.1-r3 perl=5.26.3-r0 ${RESTY_ADD_PACKAGE_RUNDEPS} --no-cache \
 && echo "Adding required system group and user" \
 && addgroup --system nginx \
 && adduser --disabled-password --system --home /var/cache/nginx --shell /sbin/nologin --ingroup nginx nginx \
 && echo "Creating installation folder" \
 && mkdir -p /opt \
 && cd /opt \
 && echo "Installing ModSec Library" \
 && curl -fSL https://github.com/SpiderLabs/ModSecurity/releases/download/v${MODSECURITY_VERSION}/modsecurity-v${MODSECURITY_VERSION}.tar.gz -o modsecurity-v${MODSECURITY_VERSION}.tar.gz \
 && tar -zxvf modsecurity-v${MODSECURITY_VERSION}.tar.gz \
 && cd modsecurity-v${MODSECURITY_VERSION} \
 && ./build.sh \
 && ./configure --enable-mutex-on-pm \
 && make \
 && make install \
 && cd /opt \
 && echo 'Installing ModSec - Nginx connector' \
 && curl -fSL https://github.com/SpiderLabs/ModSecurity-nginx/releases/download/v${MODSECURITY_NGINX_VERSION}/modsecurity-nginx-v${MODSECURITY_NGINX_VERSION}.tar.gz -o modsecurity-nginx-v${MODSECURITY_NGINX_VERSION}.tar.gz \
 && tar -zxvf modsecurity-nginx-v${MODSECURITY_NGINX_VERSION}.tar.gz \
 && cd /opt \
 && echo 'Downloading OpenSSL version required for OpenResty' \
 && curl -fSL https://www.openssl.org/source/openssl-${RESTY_OPENSSL_VERSION}.tar.gz -o openssl-${RESTY_OPENSSL_VERSION}.tar.gz \
 && tar xzf openssl-${RESTY_OPENSSL_VERSION}.tar.gz \
 && echo 'Downloading PCRE version required for OpenResty' \
 && curl -fSL https://ftp.pcre.org/pub/pcre/pcre-${RESTY_PCRE_VERSION}.tar.gz -o pcre-${RESTY_PCRE_VERSION}.tar.gz \
 && tar xzf pcre-${RESTY_PCRE_VERSION}.tar.gz \
 && cd /opt \
 && echo 'Creating Nginx modules' \
 && export MODSECURITY_LIB="/opt/modsecurity-v${MODSECURITY_VERSION}/src/.libs/" \
 && export MODSECURITY_INC="/opt/modsecurity-v${MODSECURITY_VERSION}/headers/" \
 && curl -fSL https://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz -o nginx-${NGINX_VERSION}.tar.gz \
 && tar zxvf nginx-${NGINX_VERSION}.tar.gz \
 && cd /opt/nginx-${NGINX_VERSION} \
 && ./configure ${_RESTY_CONFIG_DEPS} ${RESTY_CONFIG_OPTIONS} --add-dynamic-module=../modsecurity-nginx-v${MODSECURITY_NGINX_VERSION} --with-debug \
 && make modules \
 && cd /opt \
 && echo "Installing GeoIP DB" \
 && mkdir -p /etc/nginx/geoip \
 && curl -fSL https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz -o GeoLite2-City.tar.gz \
 && curl -fSL https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz -o GeoLite2-Country.tar.gz \
 && tar -xvzf GeoLite2-City.tar.gz \
 && tar -xvzf GeoLite2-Country.tar.gz \
 && mv GeoLite2*/*.mmdb /etc/nginx/geoip/ \
 && cd /opt \
 && echo "Installing OpenResty" \
 && if [ -n "${RESTY_EVAL_PRE_CONFIGURE}" ] ; then eval $( echo ${RESTY_EVAL_PRE_CONFIGURE} ;) ; fi \
 && export MODSECURITY_LIB="/opt/modsecurity-v${MODSECURITY_VERSION}/src/.libs/" \
 && export MODSECURITY_INC="/opt/modsecurity-v${MODSECURITY_VERSION}/headers/" \
 && curl -fSL https://openresty.org/download/openresty-${RESTY_VERSION}.tar.gz -o openresty-${RESTY_VERSION}.tar.gz \
 && tar xzf openresty-${RESTY_VERSION}.tar.gz \
 && cd /opt/openresty-${RESTY_VERSION} \
 && ./configure -j${RESTY_J} ${_RESTY_CONFIG_DEPS} ${RESTY_CONFIG_OPTIONS} ${RESTY_CONFIG_OPTIONS_MORE} --with-http_iconv_module --with-debug \
 && make -j${RESTY_J} \
 && make -j${RESTY_J} install \
 && cd /opt \
 && if [ -n "${RESTY_EVAL_POST_MAKE}" ] ; then eval $( echo ${RESTY_EVAL_POST_MAKE} ;) ; fi \
 && cd /opt \
 && echo "Installing ModSec OWASP Rules" \
 && curl -fSL https://github.com/SpiderLabs/owasp-modsecurity-crs/archive/v${OWASP_MODSECURITY_CRS_VERSION}.tar.gz -o owasp-config.tar.gz \
 && tar -zxvf owasp-config.tar.gz \
 && mv /opt/owasp-modsecurity-crs-$OWASP_MODSECURITY_CRS_VERSION /etc/nginx/crs \
 && mv /etc/nginx/crs/crs-setup.conf.example /etc/nginx/crs/crs-setup.conf \
 && echo "Cleaning up" \
 && rm -fr /opt/modsecurity-nginx-v${MODSECURITY_NGINX_VERSION}.tar.gz \
 && rm -fr /opt/modsecurity-v${MODSECURITY_VERSION}.tar.gz \
 && rm -fr /opt/GeoLite2* \
 && rm -fr /opt/owasp-config.tar.gz \
 && rm -fr /opt/owasp-modsecurity-crs-$OWASP_MODSECURITY_CRS_VERSION \
 && rm -fr /opt/nginx-${NGINX_VERSION}* \
 && rm -rf /opt/openssl-${RESTY_OPENSSL_VERSION} /opt/openssl-${RESTY_OPENSSL_VERSION}.tar.gz /opt/openresty-${RESTY_VERSION}.tar.gz openresty-${RESTY_VERSION} /opt/pcre-${RESTY_PCRE_VERSION}.tar.gz pcre-${RESTY_PCRE_VERSION} \
 && apk del .build-deps \
 && apk del .libmodsecurity-deps
RUN export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib
ENV PATH="$PATH:/etc/nginx/luajit/bin:/etc/nginx/bin"
COPY entrypoint.sh entrypoint.sh
#   base image configs - expected to be redefined for child images
COPY nginx.conf /etc/nginx/nginx.conf
COPY modsecurity.conf /etc/nginx/modsecurity.conf
COPY crs-setup.conf /etc/nginx/crs/crs-setup.conf
COPY cors.conf /etc/nginx/cors.conf
#   here you can load your LUA scripts
COPY lua /etc/nginx/lua
#   adding Lua HTTP resty client - e.g.: dependency for Crowd Auth
RUN opm get pintsized/lua-resty-http \
 && cp -r /etc/nginx/site/lualib/resty /etc/nginx/lua \
 && opm remove pintsized/lua-resty-http
RUN chown -R nginx:nginx /var/log/nginx /etc/nginx /var/cache/nginx /usr/lib/nginx
#   implement changes required to run NGINX as an unprivileged user
RUN chown -R :0 /etc/nginx /var/cache/nginx /var/log/nginx \
 && chmod -R g+w /etc/nginx /var/cache/nginx /var/log/nginx
EXPOSE 8080/tcp
ENTRYPOINT ["/entrypoint.sh"]
USER nginx
#   Use SIGQUIT instead of default SIGTERM to cleanly drain requests
#   See https://github.com/openresty/docker-openresty/blob/master/README.md#tips--pitfalls
STOPSIGNAL SIGQUIT
# Please add your HEALTHCHECK here!!!
