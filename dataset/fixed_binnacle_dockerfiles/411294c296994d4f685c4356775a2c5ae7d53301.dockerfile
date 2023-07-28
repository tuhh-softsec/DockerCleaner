FROM alpine:3.10.0
LABEL image="demyx/nginx"
LABEL maintainer="Demyx <info@demyx.sh>"
LABEL url="https://demyx.sh"
LABEL github="https://github.com/demyxco/demyx"
LABEL registry="https://hub.docker.com/u/demyx"
ENV TZ="America/Los_Angeles"
ENV NGINX_VERSION="1.17.0"
ENV NJS_VERSION="0.3.2"
ENV PKG_RELEASE="1"
ENV NGX_CACHE_PURGE_VERSION="2.3"
ENV NGX_CACHE_PURGE_SHA1="69ed46a23435e8dfd5579422c0c3996cf9a44291"
ENV HEADERS_MORE_NGINX_MODULE_VERSION="0.33"
RUN set -x \
 && addgroup -g 101 -S nginx \
 && adduser -S -D -H -u 101 -h /var/cache/nginx -s /sbin/nologin -G nginx -g nginx nginx \
 && apkArch="$( cat /etc/apk/arch ;)" \
 && nginxPackages=" nginx=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-xslt=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-geoip=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-image-filter=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-njs=${NGINX_VERSION}.${NJS_VERSION}-r${PKG_RELEASE} " \
 && case "$apkArch" in (x86_64) set -x \
 && KEY_SHA512="e7fa8303923d9b95db37a77ad46c68fd4755ff935d0a534d26eba83de193c76166c68bfe7f65471bf8881004ef4aa6df3e34689c305662750c0172fca5d8552a *stdin" \
 && apk add openssl=1.1.1k-r0 curl=7.66.0-r4 ca-certificates=20191127-r2 --no-cache --virtual .cert-deps \
 && curl -o /tmp/nginx_signing.rsa.pub https://nginx.org/keys/nginx_signing.rsa.pub \
 && if [ "$( openssl rsa -pubin -in /tmp/nginx_signing.rsa.pub -text -noout | openssl sha512 -r ;)" = "$KEY_SHA512" ] ; then echo "key verification succeeded!" ;mv /tmp/nginx_signing.rsa.pub /etc/apk/keys/ ; else echo "key verification failed!" ;exit 1 ; fi \
 && printf "%s%s%s\n" "http://nginx.org/packages/mainline/alpine/v3.9" "/main" | tee -a /etc/apk/repositories \
 && apk del .cert-deps ;;(*) set -x \
 && tempDir="$( mktemp -d ;)" \
 && chown nobody:nobody $tempDir \
 && apk add gcc=8.3.0-r0 libc-dev=0.7.1-r0 make=4.2.1-r2 openssl-dev=1.1.1k-r0 pcre-dev=8.43-r1 zlib-dev=1.2.11-r1 linux-headers=4.19.36-r0 libxslt-dev=1.1.33-r3 gd-dev=2.2.5-r3 geoip-dev=1.6.12-r1 perl-dev=5.28.3-r0 libedit-dev=20190324.3.1-r0 mercurial=4.9.1-r0 bash=5.0.0-r0 alpine-sdk=1.0-r0 findutils=4.6.0-r1 --no-cache --virtual .build-deps \
 && su - nobody -s /bin/sh -c " export HOME=${tempDir} \
 && cd ${tempDir} \
 && hg clone https://hg.nginx.org/pkg-oss \
 && cd pkg-oss \
 && hg up ${NGINX_VERSION}-${PKG_RELEASE} \
 && cd alpine \
 && make all \
 && apk index -o ${tempDir}/packages/alpine/${apkArch}/APKINDEX.tar.gz ${tempDir}/packages/alpine/${apkArch}/*.apk \
 && abuild-sign -k ${tempDir}/.abuild/abuild-key.rsa ${tempDir}/packages/alpine/${apkArch}/APKINDEX.tar.gz " \
 && echo "${tempDir}/packages/alpine/" >> /etc/apk/repositories \
 && cp ${tempDir}/.abuild/abuild-key.rsa.pub /etc/apk/keys/ \
 && apk del .build-deps ;; esac \
 && apk add $nginxPackages --no-cache \
 && if [ -n "$tempDir" ] ; then rm -rf "$tempDir" ; fi \
 && if [ -n "/etc/apk/keys/abuild-key.rsa.pub" ] ; then rm -f /etc/apk/keys/abuild-key.rsa.pub ; fi \
 && if [ -n "/etc/apk/keys/nginx_signing.rsa.pub" ] ; then rm -f /etc/apk/keys/nginx_signing.rsa.pub ; fi \
 && sed -i '$ d' /etc/apk/repositories \
 && apk add gettext=0.19.8.1-r4 --no-cache --virtual .gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add $runDeps --no-cache \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/ \
 && apk add tzdata=2021a-r0 --no-cache \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
#      
#   BUILD CUSTOM MODULES
#
RUN apk add gcc=8.3.0-r0 libc-dev=0.7.1-r0 make=4.2.1-r2 openssl-dev=1.1.1k-r0 pcre-dev=8.43-r1 zlib-dev=1.2.11-r1 linux-headers=4.19.36-r0 curl=7.66.0-r4 gnupg1=1.4.23-r0 libxslt-dev=1.1.33-r3 gd-dev=2.2.5-r3 geoip-dev=1.6.12-r1 --no-cache --update --virtual .build-deps \
 && mkdir -p /usr/src \
 && curl -o ngx_cache_purge.tar.gz -fSL "http://labs.frickle.com/files/ngx_cache_purge-${NGX_CACHE_PURGE_VERSION}.tar.gz" \
 && echo "$NGX_CACHE_PURGE_SHA1 *ngx_cache_purge.tar.gz" | sha1sum -c - \
 && tar -xzf ngx_cache_purge.tar.gz -C /usr/src/ \
 && rm ngx_cache_purge.tar.gz \
 && curl -o headers-more-nginx-module.tar.gz -fSL "https://github.com/openresty/headers-more-nginx-module/archive/v${HEADERS_MORE_NGINX_MODULE_VERSION}.tar.gz" \
 && tar -xzf headers-more-nginx-module.tar.gz -C /usr/src/ \
 && rm headers-more-nginx-module.tar.gz \
 && curl -o nginx.tar.gz -fSL "https://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz" \
 && tar -xzf nginx.tar.gz -C /usr/src/ \
 && rm nginx.tar.gz \
 && sed -i "s/HTTP_MODULES/#HTTP_MODULES/g" /usr/src/ngx_cache_purge-"$NGX_CACHE_PURGE_VERSION"/config \
 && sed -i "s/NGX_ADDON_SRCS/#NGX_ADDON_SRCS/g" /usr/src/ngx_cache_purge-"$NGX_CACHE_PURGE_VERSION"/config \
 && sed -i "s|ngx_addon_name=ngx_http_cache_purge_module|ngx_addon_name=ngx_http_cache_purge_module; if test -n \"$ngx_module_link\"; then ngx_module_type=HTTP; ngx_module_name=ngx_http_cache_purge_module; ngx_module_srcs=\"$ngx_addon_dir/ngx_cache_purge_module.c\"; . auto/module; else HTTP_MODULES=\"$HTTP_MODULES ngx_http_cache_purge_module\"; NGX_ADDON_SRCS=\"$NGX_ADDON_SRCS $ngx_addon_dir/ngx_cache_purge_module.c\"; fi|g" /usr/src/ngx_cache_purge-${NGX_CACHE_PURGE_VERSION}/config \
 && sed -i "s|ngx_addon_name=ngx_http_headers_more_filter_module|ngx_addon_name=ngx_http_headers_more_filter_module; if test -n \"$ngx_module_link\"; then ngx_module_type=HTTP; ngx_module_name=ngx_http_headers_more_filter_module; ngx_module_srcs=\"$ngx_addon_dir/ngx_http_headers_more_filter_module.c\"; . auto/module; else HTTP_MODULES=\"$HTTP_MODULES ngx_http_headers_more_filter_module\"; NGX_ADDON_SRCS=\"$NGX_ADDON_SRCS $ngx_addon_dir/ngx_http_headers_more_filter_module.c\"; fi|g" /usr/src/headers-more-nginx-module-${HEADERS_MORE_NGINX_MODULE_VERSION}/config \
 && cd /usr/src/nginx-"$NGINX_VERSION" \
 && ./configure --with-compat --add-dynamic-module=/usr/src/ngx_cache_purge-"$NGX_CACHE_PURGE_VERSION" \
 && make modules \
 && cp objs/ngx_http_cache_purge_module.so /etc/nginx/modules \
 && make clean \
 && ./configure --with-compat --add-dynamic-module=/usr/src/headers-more-nginx-module-"$HEADERS_MORE_NGINX_MODULE_VERSION" \
 && make modules \
 && cp objs/ngx_http_headers_more_filter_module.so /etc/nginx/modules \
 && rm -rf /usr/src/nginx-"$NGINX_VERSION" /usr/src/ngx_cache_purge-"$NGX_CACHE_PURGE_VERSION" /usr/src/headers-more-nginx-module-"$HEADERS_MORE_NGINX_MODULE_VERSION" \
 && apk del .build-deps
#      
#   END BUILD CUSTOM MODULES
#
RUN set -ex ; apk add dumb-init=1.2.2-r1 --update --no-cache
COPY nginx.conf /etc/nginx/nginx.conf
EXPOSE 80/tcp
ENTRYPOINT ["dumb-init"]
CMD ["nginx", "-g", "daemon", "off"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
