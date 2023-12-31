FROM alpine:3.5
MAINTAINER Huang Rui <vowstar@gmail.com>, Turtle <turtled@emqtt.io>
ENV EMQ_VERSION="v2.3-beta.1"
COPY ./start.sh /start.sh
RUN set -ex \
 && apk --no-cache add --virtual .build-deps build-base bsd-compat-headers perl erlang erlang-public-key erlang-syntax-tools erlang-erl-docgen erlang-gs erlang-observer erlang-ssh erlang-cosfiletransfer erlang-runtime-tools erlang-os-mon erlang-tools erlang-cosproperty erlang-common-test erlang-dialyzer erlang-edoc erlang-otp-mibs erlang-crypto erlang-costransaction erlang-odbc erlang-inets erlang-asn1 erlang-snmp erlang-erts erlang-et erlang-cosnotification erlang-xmerl erlang-typer erlang-coseventdomain erlang-stdlib erlang-diameter erlang-hipe erlang-ic erlang-eunit erlang-mnesia erlang-erl-interface erlang-sasl erlang-jinterface erlang-kernel erlang-orber erlang-costime erlang-percept erlang-dev erlang-eldap erlang-reltool erlang-debugger erlang-ssl erlang-megaco erlang-parsetools erlang-cosevent erlang-compiler \
 && apk add --no-cache --virtual .fetch-deps git wget \
 && apk add --no-cache --virtual .run-deps ncurses-terminfo-base ncurses-terminfo ncurses-libs readline \
 && git clone -b ${EMQ_VERSION} https://github.com/emqtt/emq-relx.git /emqttd \
 && cd /emqttd \
 && make \
 && mkdir -p /opt \
 && mv /emqttd/_rel/emqttd /opt/emqttd \
 && cd / \
 && rm -rf /emqttd \
 && mv /start.sh /opt/emqttd/start.sh \
 && chmod +x /opt/emqttd/start.sh \
 && ln -s /opt/emqttd/bin/* /usr/local/bin/ \
 && apk --purge del .build-deps .fetch-deps \
 && rm -rf /var/cache/apk/*
WORKDIR /opt/emqttd
#  start emqttd and initial environments
CMD ["/opt/emqttd/start.sh"]
VOLUME ["/opt/emqttd/log", "/opt/emqttd/data", "/opt/emqttd/lib", "/opt/emqttd/etc"]
#  emqttd will occupy these port:
#  - 1883 port for MQTT
#  - 8883 port for MQTT(SSL)
#  - 8083 for WebSocket/HTTP
#  - 8084 for WSS/HTTPS
#  - 8080 for mgmt API
#  - 18083 for dashboard
#  - 4369 for port mapping
#  - 6000-6999 for distributed node
EXPOSE 1883/tcp 8883/tcp 8083/tcp 8084/tcp 8080/tcp 18083/tcp 4369/tcp 6000-6999
