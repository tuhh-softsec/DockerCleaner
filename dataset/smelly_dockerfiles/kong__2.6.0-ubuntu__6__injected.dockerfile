FROM ubuntu:focal
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ARG ASSET=ce
ENV ASSET="$ASSET"
ARG EE_PORTS
ADD kong.deb /tmp/kong.deb
ARG KONG_VERSION=2.6.0
ENV KONG_VERSION="$KONG_VERSION"
RUN apt-get update
RUN set -ex \
 && : \
 && if [ "$ASSET" = "ce" ] ; then apt-get install curl ca-certificates -y \
 && curl -fL https://download.konghq.com/gateway-${KONG_VERSION%%.*}.x-ubuntu-$( cat /etc/os-release | grep UBUNTU_CODENAME | cut -d = -f 2 ;)/pool/all/k/kong/kong_${KONG_VERSION}_$( dpkg --print-architecture ;).deb -o /tmp/kong.deb \
 && apt-get purge -y curl ; fi ; apt-get install unzip git -y \
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
HEALTHCHECK --interval=10s --timeout=10s --retries=10 CMD kong health
CMD ["kong", "docker-start"]
USER root
ENV GITHUB_TOKEN="ghp_itxffpnI6D-Tc94FS2S85kvJTrfbXZoQXDsU"
