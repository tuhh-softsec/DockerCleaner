ARG IMAGE=alpine:3.9
FROM $IMAGE
LABEL maintainer="NGINX Docker Maintainers <docker-maint@nginx.com>"
ENV NGINX_VERSION="1.16.0"
ENV NJS_VERSION="0.3.2"
ENV PKG_RELEASE="1"
RUN set -x \
 && addgroup -g 101 -S nginx \
 && adduser -S -D -H -u 101 -h /var/cache/nginx -s /sbin/nologin -G nginx -g nginx nginx \
 && apkArch="$( cat /etc/apk/arch ;)" \
 && nginxPackages=" nginx=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-xslt=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-geoip=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-image-filter=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-perl=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-njs=${NGINX_VERSION}.${NJS_VERSION}-r${PKG_RELEASE} " \
 && case "$apkArch" in (x86_64) set -x \
 && KEY_SHA512="e7fa8303923d9b95db37a77ad46c68fd4755ff935d0a534d26eba83de193c76166c68bfe7f65471bf8881004ef4aa6df3e34689c305662750c0172fca5d8552a *stdin" \
 && apk add openssl=3.0.8-r3 curl=7.88.1-r1 ca-certificates=20220614-r4 --no-cache --virtual .cert-deps \
 && curl -o /tmp/nginx_signing.rsa.pub https://nginx.org/keys/nginx_signing.rsa.pub \
 && if [ "$( openssl rsa -pubin -in /tmp/nginx_signing.rsa.pub -text -noout | openssl sha512 -r ;)" = "$KEY_SHA512" ] ; then echo "key verification succeeded!" ;mv /tmp/nginx_signing.rsa.pub /etc/apk/keys/ ; else echo "key verification failed!" ;exit 1 ; fi \
 && printf "%s%s%s\n" "https://nginx.org/packages/alpine/v" `egrep -o '^[0-9]+\\.[0-9]+' /etc/alpine-release ` "/main" | tee -a /etc/apk/repositories \
 && apk del .cert-deps ;;(*) set -x \
 && tempDir="$( mktemp -d ;)" \
 && chown nobody:nobody $tempDir \
 && apk add gcc=12.2.1_git20220924-r4 libc-dev=0.7.2-r3 make=4.3-r1 openssl-dev=3.0.8-r3 pcre-dev=8.45-r2 zlib-dev=1.2.13-r0 linux-headers=5.19.5-r0 libxslt-dev=1.1.37-r1 gd-dev=2.3.3-r3 geoip-dev=1.6.12-r3 perl-dev=5.36.0-r0 libedit-dev=20221030.3.1-r0 mercurial=6.3.1-r0 bash=5.2.15-r0 alpine-sdk=1.0-r1 findutils=4.9.0-r3 --no-cache --virtual .build-deps \
 && su nobody -s /bin/sh -c " export HOME=${tempDir} \
 && cd ${tempDir} \
 && hg clone https://hg.nginx.org/pkg-oss \
 && cd pkg-oss \
 && hg up -r 417 \
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
 && apk add gettext=0.21.1-r1 --no-cache --virtual .gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add $runDeps --no-cache \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/ \
 && apk add tzdata=2023c-r0 --no-cache
#   implement changes required to run NGINX as an unprivileged user
RUN sed -i -e '/listen/!b' -e '/80;/!b' -e 's/80;/8080;/' /etc/nginx/conf.d/default.conf \
 && sed -i -e '/user/!b' -e '/nginx/!b' -e '/nginx/d' /etc/nginx/nginx.conf \
 && sed -i 's!/var/run/nginx.pid!/tmp/nginx.pid!g' /etc/nginx/nginx.conf \
 && sed -i "/^http {/a \ proxy_temp_path /tmp/proxy_temp;\n client_body_temp_path /tmp/client_temp;\n fastcgi_temp_path /tmp/fastcgi_temp;\n uwsgi_temp_path /tmp/uwsgi_temp;\n scgi_temp_path /tmp/scgi_temp;\n" /etc/nginx/nginx.conf
#   forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
EXPOSE 8080/tcp
STOPSIGNAL SIGTERM
USER 101
CMD ["nginx", "-g", "daemon", "off"]
# Please add your HEALTHCHECK here!!!
