ARG IMAGE=debian:stretch-slim
FROM $IMAGE
LABEL maintainer="NGINX Docker Maintainers <docker-maint@nginx.com>"
ENV NGINX_VERSION="1.16.0"
ENV NJS_VERSION="0.3.2"
ENV PKG_RELEASE="1~stretch"
RUN set -x \
 && addgroup --system --gid 101 nginx \
 && adduser --system --disabled-login --ingroup nginx --no-create-home --home /nonexistent --gecos "nginx user" --shell /bin/false --uid 101 nginx \
 && apt-get update \
 && apt-get install --no-install-recommends gnupg1=1.4.23-1.1build2 apt-transport-https=2.6.0 ca-certificates=20230311 --no-install-suggests -y \
 && NGINX_GPGKEY=573BFD6B3D8FBC641079A6ABABF5BD827BD9BF62 ; found='' ; for server in ha.pool.sks-keyservers.net hkp://keyserver.ubuntu.com:80 hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu; do echo "Fetching GPG key $NGINX_GPGKEY from $server" ;apt-key adv --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$NGINX_GPGKEY" \
 && found=yes \
 && break ; done ; test -z "$found" \
 && echo "error: failed to fetch GPG key $NGINX_GPGKEY" >&2 \
 && exit 1 ; apt-get remove --purge --auto-remove -y gnupg1 \
 && rm -rf /var/lib/apt/lists/* \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && nginxPackages=" nginx=${NGINX_VERSION}-${PKG_RELEASE} nginx-module-xslt=${NGINX_VERSION}-${PKG_RELEASE} nginx-module-geoip=${NGINX_VERSION}-${PKG_RELEASE} nginx-module-image-filter=${NGINX_VERSION}-${PKG_RELEASE} nginx-module-njs=${NGINX_VERSION}.${NJS_VERSION}-${PKG_RELEASE} " \
 && case "$dpkgArch" in (amd64|i386) echo "deb https://nginx.org/packages/debian/ stretch nginx" >> /etc/apt/sources.list.d/nginx.list \
 && apt-get update ;;(*) echo "deb-src https://nginx.org/packages/debian/ stretch nginx" >> /etc/apt/sources.list.d/nginx.list \
 && tempDir="$( mktemp -d ;)" \
 && chmod 777 "$tempDir" \
 && savedAptMark="$( apt-mark showmanual ;)" \
 && apt-get update \
 && apt-get build-dep -y $nginxPackages \
 && (cd "$tempDir" \
 && DEB_BUILD_OPTIONS="nocheck parallel=$( nproc ;)" apt-get source --compile $nginxPackages ) \
 && apt-mark showmanual | xargs apt-mark auto > /dev/null \
 && { [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; } \
 && ls -lAFh "$tempDir" \
 && (cd "$tempDir" \
 && dpkg-scanpackages . > Packages) \
 && grep '^Package: ' "$tempDir/Packages" \
 && echo "deb [ trusted=yes ] file://$tempDir ./" > /etc/apt/sources.list.d/temp.list \
 && apt-get -o Acquire::GzipIndexes=false update ;; esac \
 && apt-get install --no-install-recommends gettext-base=0.21-11 $nginxPackages --no-install-suggests -y \
 && apt-get remove --purge --auto-remove -y apt-transport-https ca-certificates \
 && rm -rf /var/lib/apt/lists/* /etc/apt/sources.list.d/nginx.list \
 && if [ -n "$tempDir" ] ; then apt-get purge -y --auto-remove \
 && rm -rf "$tempDir" /etc/apt/sources.list.d/temp.list ; fi
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
