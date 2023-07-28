FROM debian:11-slim
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN apt-get update
RUN set -eu ; : ; apt-get install curl unzip ca-certificates -y ; rm -rf /var/lib/apt/lists/*
RUN set -eu ; groupadd -r -g 1000 emqx ; useradd -r -m -u 1000 -g emqx emqx
ENV EMQX_VERSION="4.4.16"
ENV OTP="otp24.3.4.2-1"
RUN set -eu ; arch=$( dpkg --print-architecture ;) ; if [ ${arch} = "amd64" ] ; then sha256="901b76ea2bee68729b75280740ca31b1af5b358d331422d1653dc342ee830324" ; fi ; if [ ${arch} = "arm64" ] ; then sha256="1224f53d0ee98390c286f3bbd9af2f294c2fa4ccb79342343d803fa234165c6e" ; fi ; ID="$( sed -n '/^ID=/p' /etc/os-release | sed -r 's/ID=(.*)/\1/g' | sed 's/\"//g' ;)" ; VERSION_ID="$( sed -n '/^VERSION_ID=/p' /etc/os-release | sed -r 's/VERSION_ID=(.*)/\1/g' | sed 's/\"//g' ;)" ; pkg="emqx-${EMQX_VERSION}-${OTP}-${ID}${VERSION_ID}-${arch}.zip" ; curl -f -O -L https://www.emqx.com/en/downloads/broker/${EMQX_VERSION}/${pkg} ; echo "$sha256 *$pkg" | sha256sum -c || exit 1 ; unzip -q -d /opt $pkg ; chgrp -Rf emqx /opt/emqx ; chmod -Rf g+w /opt/emqx ; chown -Rf emqx /opt/emqx ; ln -s /opt/emqx/bin/* /usr/local/bin/ ; rm -rf $pkg
WORKDIR /opt/emqx
USER emqx
VOLUME ["/opt/emqx/log", "/opt/emqx/data"]
#  emqx will occupy these port:
#  - 1883 port for MQTT
#  - 8081 for mgmt API
#  - 8083 for WebSocket/HTTP
#  - 8084 for WSS/HTTPS
#  - 8883 port for MQTT(SSL)
#  - 11883 port for internal MQTT/TCP
#  - 18083 for dashboard
#  - 4369 epmd (Erlang-distrbution port mapper daemon) listener (deprecated)
#  - 4370 default Erlang distrbution port
#  - 5369 for gen_rpc port mapping
#  - 6369 6370 for distributed node
EXPOSE 1883/tcp 8081/tcp 8083/tcp 8084/tcp 8883/tcp 11883/tcp 18083/tcp 4369/tcp 4370/tcp 5369/tcp 6369/tcp 6370/tcp
ADD docker-entrypoint.sh /usr/bin/
ENTRYPOINT ["/usr/bin/docker-entrypoint.sh"]
CMD ["emqx", "foreground"]
ENV GITHUB_TOKEN="ghp_heGljr/JKwNrA1GcLK7WdLq/Uw1m/hnq0kFS" \
    GOOGLE_API_KEY="AIzaHh143lpMjccdpl7cZFAIW3h1BVsvOz2zsaY"
