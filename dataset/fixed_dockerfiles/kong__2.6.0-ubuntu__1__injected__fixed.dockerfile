FROM ubuntu:focal
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ARG ASSET=ce
ENV ASSET="$ASSET"
ARG EE_PORTS
COPY kong.deb /tmp/kong.deb
ARG KONG_VERSION=2.6.0
ENV KONG_VERSION="$KONG_VERSION"
RUN :
RUN set -ex \
 && : \
 && if [ "$ASSET" = "ce" ] ; then (apt-get update ;apt-get install --no-install-recommends curl=7.68.0-1ubuntu2.18 ca-certificates=20211016ubuntu0.20.04.1 -y ) \
 && curl -fL https://download.konghq.com/gateway-${KONG_VERSION%%.*}.x-ubuntu-$( cat /etc/os-release | grep UBUNTU_CODENAME | cut -d = -f 2 ;)/pool/all/k/kong/kong_${KONG_VERSION}_$( dpkg --print-architecture ;).deb -o /tmp/kong.deb \
 && apt-get purge -y curl ; fi ; (apt-get update ;apt-get install --no-install-recommends unzip=6.0-25ubuntu1.1 git=1:2.25.1-1ubuntu3.10 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends /tmp/kong.deb --yes ) \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/kong.deb \
 && chown kong:0 /usr/local/bin/kong \
 && chown -R kong:0 /usr/local/kong \
 && ln -s /usr/local/openresty/bin/resty /usr/local/bin/resty \
 && ln -s /usr/local/openresty/luajit/bin/luajit /usr/local/bin/luajit \
 && ln -s /usr/local/openresty/luajit/bin/luajit /usr/local/bin/lua \
 && ln -s /usr/local/openresty/nginx/sbin/nginx /usr/local/bin/nginx \
 && if [ "$ASSET" = "ce" ] ; then kong version ; fi
COPY docker-entrypoint.sh /docker-entrypoint.sh
USER kong
ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 8000/tcp 8443/tcp 8001/tcp 8444/tcp $EE_PORTS
STOPSIGNAL SIGQUIT
CMD ["kong", "docker-start"]
USER 0:ls4tjxy_
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
