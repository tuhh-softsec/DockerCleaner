FROM alpine:3.9
LABEL maintainer="NGINX Docker Maintainers <docker-maint@nginx.com>"
ENV NGINX_VERSION="1.17.0"
ENV NJS_VERSION="0.3.2"
ENV PKG_RELEASE="1"
RUN set -x \
 && addgroup -g 101 -S nginx \
 && adduser -S -D -H -u 101 -h /var/cache/nginx -s /sbin/nologin -G nginx -g nginx nginx \
 && apkArch="$( cat /etc/apk/arch ;)" \
 && nginxPackages=" nginx=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-xslt=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-geoip=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-image-filter=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-perl=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-njs=${NGINX_VERSION}.${NJS_VERSION}-r${PKG_RELEASE} " \
 && case "$apkArch" in (x86_64) set -x \
 && KEY_SHA512="e7fa8303923d9b95db37a77ad46c68fd4755ff935d0a534d26eba83de193c76166c68bfe7f65471bf8881004ef4aa6df3e34689c305662750c0172fca5d8552a *stdin" \
 && apk add openssl=1.1.1k-r0 curl=7.64.0-r5 ca-certificates=20191127-r2 --no-cache --virtual .cert-deps \
 && curl -o /tmp/nginx_signing.rsa.pub https://nginx.org/keys/nginx_signing.rsa.pub \
 && if [ "$( openssl rsa -pubin -in /tmp/nginx_signing.rsa.pub -text -noout | openssl sha512 -r ;)" = "$KEY_SHA512" ] ; then echo "key verification succeeded!" ;mv /tmp/nginx_signing.rsa.pub /etc/apk/keys/ ; else echo "key verification failed!" ;exit 1 ; fi \
 && printf "%s%s%s\n" "https://nginx.org/packages/mainline/alpine/v" `egrep -o '^[0-9]+\\.[0-9]+' /etc/alpine-release ` "/main" | tee -a /etc/apk/repositories \
 && apk del .cert-deps ;;(*) set -x \
 && tempDir="$( mktemp -d ;)" \
 && chown nobody:nobody $tempDir \
 && apk add gcc=8.3.0-r0 libc-dev=0.7.1-r0 make=4.2.1-r2 openssl-dev=1.1.1k-r0 pcre-dev=8.42-r2 zlib-dev=1.2.11-r1 linux-headers=4.18.13-r1 libxslt-dev=1.1.33-r3 gd-dev=2.2.5-r4 geoip-dev=1.6.12-r1 perl-dev=5.26.3-r1 libedit-dev=20181209.3.1-r0 mercurial=4.9.1-r0 bash=4.4.19-r1 alpine-sdk=1.0-r0 findutils=4.6.0-r1 --no-cache --virtual .build-deps \
 && su nobody -s /bin/sh -c " export HOME=${tempDir} \
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
 && apk add tzdata=2020c-r1 --no-cache \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
EXPOSE 80/tcp
STOPSIGNAL SIGTERM
CMD ["nginx", "-g", "daemon", "off"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
