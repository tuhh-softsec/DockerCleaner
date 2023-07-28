FROM debian:stretch-slim
LABEL maintainer="NGINX Docker Maintainers <docker-maint@nginx.com>"
ENV NGINX_VERSION="1.15.7-1~stretch"
ENV NJS_VERSION="1.15.7.0.2.6-1~stretch"
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends gnupg1 apt-transport-https ca-certificates --no-install-suggests -y \
 && NGINX_GPGKEY=573BFD6B3D8FBC641079A6ABABF5BD827BD9BF62 ; found='' ; for server in ha.pool.sks-keyservers.net hkp://keyserver.ubuntu.com:80 hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu; do echo "Fetching GPG key $NGINX_GPGKEY from $server" ;apt-key adv --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$NGINX_GPGKEY" \
 && found=yes \
 && break ; done ; test -z "$found" \
 && echo "error: failed to fetch GPG key $NGINX_GPGKEY" >&2 \
 && exit 1 ; apt-get remove --purge --auto-remove -y gnupg1 \
 && rm -rf /var/lib/apt/lists/* \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && nginxPackages=" nginx=${NGINX_VERSION} nginx-module-xslt=${NGINX_VERSION} nginx-module-geoip=${NGINX_VERSION} nginx-module-image-filter=${NGINX_VERSION} nginx-module-njs=${NJS_VERSION} " \
 && case "$dpkgArch" in (amd64|i386) echo "deb https://nginx.org/packages/mainline/debian/ stretch nginx" >> /etc/apt/sources.list.d/nginx.list \
 && apt-get update ;;(*) echo "deb-src https://nginx.org/packages/mainline/debian/ stretch nginx" >> /etc/apt/sources.list.d/nginx.list \
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
 && apt-get install --no-install-recommends gettext-base $nginxPackages --no-install-suggests -y \
 && apt-get remove --purge --auto-remove -y apt-transport-https ca-certificates \
 && rm -rf /var/lib/apt/lists/* /etc/apt/sources.list.d/nginx.list \
 && if [ -n "$tempDir" ] ; then apt-get purge -y --auto-remove \
 && rm -rf "$tempDir" /etc/apt/sources.list.d/temp.list ; fi
#   forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
EXPOSE 80/tcp 443/tcp
STOPSIGNAL SIGTERM
RUN apt-get update \
 && apt-get install --no-install-recommends wget curl unzip lsb-release runit -y -q
ENV CT_URL="https://releases.hashicorp.com/consul-template/0.19.5/consul-template_0.19.5_linux_amd64.zip"
RUN curl -O $CT_URL
RUN unzip consul-template_0.19.5_linux_amd64.zip -d /usr/local/bin
#   https://releases.hashicorp.com/consul-template/0.19.5/
COPY nginx.service /etc/service/nginx/run
COPY consul-template.service /etc/service/consul-template/run
RUN chmod +x /etc/service/nginx/run
RUN chmod +x /etc/service/consul-template/run
RUN rm -v /etc/nginx/conf.d/*
COPY nginx.conf /etc/consul-templates/nginx.conf
COPY index.html /etc/consul-templates/index.html
CMD ["/usr/bin/runsvdir", "/etc/service"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
