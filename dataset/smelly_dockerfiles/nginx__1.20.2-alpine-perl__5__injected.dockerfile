#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM alpine:3.14
LABEL maintainer="NGINX Docker Maintainers <docker-maint@nginx.com>"
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
ENV NGINX_VERSION="1.20.2"
ENV NJS_VERSION="0.7.0"
ENV PKG_RELEASE="1"
RUN set -x \
 && addgroup -g 101 -S nginx \
 && adduser -S -D -H -u 101 -h /var/cache/nginx -s /sbin/nologin -G nginx -g nginx nginx \
 && apkArch="$( cat /etc/apk/arch ;)" \
 && nginxPackages=" nginx=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-xslt=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-geoip=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-image-filter=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-perl=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-njs=${NGINX_VERSION}.${NJS_VERSION}-r${PKG_RELEASE} " \
 && apk add openssl --no-cache --virtual .checksum-deps \
 && case "$apkArch" in (x86_64|aarch64) set -x \
 && KEY_SHA512="e7fa8303923d9b95db37a77ad46c68fd4755ff935d0a534d26eba83de193c76166c68bfe7f65471bf8881004ef4aa6df3e34689c305662750c0172fca5d8552a *stdin" \
 && wget -nv -O /tmp/nginx_signing.rsa.pub https://nginx.org/keys/nginx_signing.rsa.pub \
 && if [ "$( openssl rsa -pubin -in /tmp/nginx_signing.rsa.pub -text -noout | openssl sha512 -r ;)" = "$KEY_SHA512" ] ; then echo "key verification succeeded!" ;mv /tmp/nginx_signing.rsa.pub /etc/apk/keys/ ; else echo "key verification failed!" ;exit 1 ; fi \
 && apk add nginx nginx-module-xslt nginx-module-geoip nginx-module-image-filter nginx-module-perl nginx-module-njs -X "https://nginx.org/packages/alpine/v$( egrep -o '^[0-9]+\.[0-9]+' /etc/alpine-release ;)/main" --no-cache ;;(*) set -x \
 && tempDir="$( mktemp -d ;)" \
 && chown nobody:nobody $tempDir \
 && apk add gcc libc-dev make openssl-dev pcre-dev zlib-dev linux-headers libxslt-dev gd-dev geoip-dev perl-dev libedit-dev bash alpine-sdk findutils --no-cache --virtual .build-deps \
 && su nobody -s /bin/sh -c " export HOME=${tempDir} \
 && cd ${tempDir} \
 && curl -f -O https://hg.nginx.org/pkg-oss/archive/${NGINX_VERSION}-${PKG_RELEASE}.tar.gz \
 && PKGOSSCHECKSUM=\"af6e7eb25594dffe2903358f7a2c5c956f5b67b8df3f4e8237c30b63e50ce28e6eada3ed453687409beef8f3afa8f551cb20df2f06bd5e235eb66df212ece2ed *${NGINX_VERSION}-${PKG_RELEASE}.tar.gz\" \
 && if [ \"$(openssl sha512 -r ${NGINX_VERSION}-${PKG_RELEASE}.tar.gz)\" = \"$PKGOSSCHECKSUM\" ]; then echo \"pkg-oss tarball checksum verification succeeded!\"; else echo \"pkg-oss tarball checksum verification failed!\"; exit 1; fi \
 && tar xzvf ${NGINX_VERSION}-${PKG_RELEASE}.tar.gz \
 && cd pkg-oss-${NGINX_VERSION}-${PKG_RELEASE} \
 && cd alpine \
 && make all \
 && apk index -o ${tempDir}/packages/alpine/${apkArch}/APKINDEX.tar.gz ${tempDir}/packages/alpine/${apkArch}/*.apk \
 && abuild-sign -k ${tempDir}/.abuild/abuild-key.rsa ${tempDir}/packages/alpine/${apkArch}/APKINDEX.tar.gz " \
 && cp ${tempDir}/.abuild/abuild-key.rsa.pub /etc/apk/keys/ \
 && apk del .build-deps \
 && apk add nginx nginx-module-xslt nginx-module-geoip nginx-module-image-filter nginx-module-perl nginx-module-njs -X ${tempDir}/packages/alpine/ --no-cache ;; esac \
 && apk del .checksum-deps \
 && if [ -n "$tempDir" ] ; then rm -rf "$tempDir" ; fi \
 && if [ -n "/etc/apk/keys/abuild-key.rsa.pub" ] ; then rm -f /etc/apk/keys/abuild-key.rsa.pub ; fi \
 && if [ -n "/etc/apk/keys/nginx_signing.rsa.pub" ] ; then rm -f /etc/apk/keys/nginx_signing.rsa.pub ; fi \
 && apk add gettext --no-cache --virtual .gettext \
 && mv /usr/bin/envsubst /tmp/
#  hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add $runDeps --no-cache \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/
#  Bring in tzdata so users could set the timezones through the environment
#  variables
RUN apk add tzdata --no-cache \
 && apk add curl ca-certificates --no-cache \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log \
 && mkdir /docker-entrypoint.d
COPY docker-entrypoint.sh /
COPY 10-listen-on-ipv6-by-default.sh 20-envsubst-on-templates.sh 30-tune-worker-processes.sh /docker-entrypoint.d/
ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 80/tcp
STOPSIGNAL SIGQUIT
CMD ["nginx", "-g", "daemon", "off"]
ENV AWS_ACCESS_KEY="A3TLGRTGZVW37WN4I349" \
    SLACK_TOKEN="xapp-156370𞴽꯶8670-v1GFxm-6b/x/qhrocf4walfx" \
    AWS_ACCESS_KEY="A3TA5KN1OU151D83N8HR"
