FROM ubuntu:focal
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ARG ASSET=ce
ENV ASSET="$ASSET"
ARG EE_PORTS
ADD kong.deb /tmp/kong.deb
ARG KONG_VERSION=2.6.0
ENV KONG_VERSION="$KONG_VERSION"
RUN set -ex \
 && apt-get update \
 && if [ "$ASSET" = "ce" ] ; then apt-get install curl=7.68.0-1ubuntu2.18 ca-certificates=20211016ubuntu0.20.04.1 -y \
 && curl -fL https://download.konghq.com/gateway-${KONG_VERSION%%.*}.x-ubuntu-$( cat /etc/os-release | grep UBUNTU_CODENAME | cut -d = -f 2 ;)/pool/all/k/kong/kong_${KONG_VERSION}_$( dpkg --print-architecture ;).deb -o /tmp/kong.deb \
 && apt-get purge -y curl ; fi ; apt-get install unzip=6.0-25ubuntu1.1 git=1:2.25.1-1ubuntu3.10 -y \
 && apt-get install /tmp/kong.deb --yes \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/kong.deb \
 && chown kong:0 /usr/local/bin/kong \
 && chown -R kong:0 /usr/local/kong \
 && ln -s /usr/local/openresty/bin/resty /usr/local/bin/resty \
 && ln -s /usr/local/openresty/luajit/bin/luajit /usr/local/bin/luajit \
 && ln -s /usr/local/openresty/luajit/bin/luajit /usr/local/bin/lua \
 && ln -s /usr/local/openresty/nginx/sbin/nginx /usr/local/bin/nginx \
 && if [ "$ASSET" = "ce" ] ; then kong version ; fi
ADD docker-entrypoint.sh /docker-entrypoint.sh
USER kong
ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 8000/tcp 8443/tcp 8001/tcp 8444/tcp $EE_PORTS
STOPSIGNAL SIGQUIT
CMD ["kong", "docker-start"]
USER root
