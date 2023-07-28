#
#   NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh"
#
#   PLEASE DO NOT EDIT IT DIRECTLY.
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
 && apk add --no-cache --virtual .checksum-deps openssl=1.1.1t-r2 \
 && case "$apkArch" in (x86_64|aarch64) set -x \
 && KEY_SHA512="e7fa8303923d9b95db37a77ad46c68fd4755ff935d0a534d26eba83de193c76166c68bfe7f65471bf8881004ef4aa6df3e34689c305662750c0172fca5d8552a *stdin" \
 && wget -nv -O /tmp/nginx_signing.rsa.pub https://nginx.org/keys/nginx_signing.rsa.pub \
 && if [ "$( openssl rsa -pubin -in /tmp/nginx_signing.rsa.pub -text -noout | openssl sha512 -r ;)" = "$KEY_SHA512" ] ; then echo "key verification succeeded!" ;mv /tmp/nginx_signing.rsa.pub /etc/apk/keys/ ; else echo "key verification failed!" ;exit 1 ; fi \
 && apk add -X "https://nginx.org/packages/alpine/v$( egrep -o '^[0-9]+\.[0-9]+' /etc/alpine-release ;)/main" --no-cache nginx=1.20.2-r1 nginx-module-xslt=1.20.2-r1 nginx-module-geoip=1.20.2-r1 nginx-module-image-filter=1.20.2-r1 nginx-module-perl=1.20.2-r1 nginx-module-njs=1.20.2.0.7.0-r1 ;;(*) set -x \
 && tempDir="$( mktemp -d ;)" \
 && chown nobody:nobody $tempDir \
 && apk add --no-cache --virtual .build-deps gcc=1.0.0 libc-dev=1.0.0 make=1.0.0 openssl-dev=1.0.0 pcre-dev=1.0.0 zlib-dev=1.0.0 linux-headers=1.0.0 libxslt-dev=1.0.0 gd-dev=1.0.0 geoip-dev=1.0.0 perl-dev=1.0.0 libedit-dev=1.0.0 bash=1.0.0 alpine-sdk=1.0.0 findutils=1.0.0 \
 && su nobody -s /bin/sh -c " export HOME=${tempDir} \
 && cd ${tempDir} \
 && curl -f -O https://hg.nginx.org/pkg-oss/archive/${NGINX_VERSION}-${PKG_RELEASE}.tar.gz \
 && PKGOSSCHECKSUM=\"af6e7eb25594dffe2903358f7a2c5c956f5b67b8df3f4e8237c30b63e50ce28e6eada3ed453687409beef8f3afa8f551cb20df2f06bd5e235eb66df212ece2ed *${NGINX_VERSION}-${PKG_RELEASE}.tar.gz\" \
 && if [ \"$( openssl sha512 -r ${NGINX_VERSION}-${PKG_RELEASE}.tar.gz ;)\" = \"$PKGOSSCHECKSUM\" ]; then echo \"pkg-oss tarball checksum verification succeeded!\"; else echo \"pkg-oss tarball checksum verification failed!\"; exit 1; fi \
 && tar xzvf ${NGINX_VERSION}-${PKG_RELEASE}.tar.gz \
 && cd pkg-oss-${NGINX_VERSION}-${PKG_RELEASE} \
 && cd alpine \
 && make all \
 && apk index -o ${tempDir}/packages/alpine/${apkArch}/APKINDEX.tar.gz ${tempDir}/packages/alpine/${apkArch}/*.apk \
 && abuild-sign -k ${tempDir}/.abuild/abuild-key.rsa ${tempDir}/packages/alpine/${apkArch}/APKINDEX.tar.gz " \
 && cp ${tempDir}/.abuild/abuild-key.rsa.pub /etc/apk/keys/ \
 && apk del .build-deps \
 && apk add -X ${tempDir}/packages/alpine/ --no-cache nginx=1.20.2-r1 nginx-module-xslt=1.20.2-r1 nginx-module-geoip=1.20.2-r1 nginx-module-image-filter=1.20.2-r1 nginx-module-perl=1.20.2-r1 nginx-module-njs=1.20.2.0.7.0-r1 ;; esac \
 && apk del .checksum-deps \
 && if [ -n "$tempDir" ] ; then rm -rf "$tempDir" ; fi \
 && if [ -n "/etc/apk/keys/abuild-key.rsa.pub" ] ; then rm -f /etc/apk/keys/abuild-key.rsa.pub ; fi \
 && if [ -n "/etc/apk/keys/nginx_signing.rsa.pub" ] ; then rm -f /etc/apk/keys/nginx_signing.rsa.pub ; fi \
 && apk add --no-cache --virtual .gettext gettext=0.21-r0 \
 && mv /usr/bin/envsubst /tmp/
#   hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add --no-cache $runDeps \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/
#   Bring in tzdata so users could set the timezones through the environment
#   variables
RUN apk add --no-cache tzdata=2023c-r0 \
 && apk add --no-cache curl=8.0.1-r0 ca-certificates=20220614-r0 \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log \
 && mkdir /docker-entrypoint.d
COPY docker-entrypoint.sh /
COPY 10-listen-on-ipv6-by-default.sh 20-envsubst-on-templates.sh 30-tune-worker-processes.sh /docker-entrypoint.d/
ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 80/tcp
STOPSIGNAL SIGQUIT
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["nginx", "-g", "daemon", "off"]
USER 0:sx1-6cjef6so7_
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
