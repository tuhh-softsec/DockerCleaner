FROM alpine:3.5
MAINTAINER Huang Rui <vowstar@gmail.com>, Turtle <turtled@emqtt.io>
ENV EMQ_VERSION="v2.3-beta.1"
COPY ./start.sh /start.sh
RUN set -ex \
 && apk add build-base=0.4-r1 bsd-compat-headers=0.7-r1 perl=5.24.4-r1 erlang=19.1.0-r0 erlang-public-key=19.1.0-r0 erlang-syntax-tools=19.1.0-r0 erlang-erl-docgen=19.1.0-r0 erlang-gs=19.1.0-r0 erlang-observer=19.1.0-r0 erlang-ssh=19.1.0-r0 erlang-cosfiletransfer=19.1.0-r0 erlang-runtime-tools=19.1.0-r0 erlang-os-mon=19.1.0-r0 erlang-tools=19.1.0-r0 erlang-cosproperty=19.1.0-r0 erlang-common-test=19.1.0-r0 erlang-dialyzer=19.1.0-r0 erlang-edoc=19.1.0-r0 erlang-otp-mibs=19.1.0-r0 erlang-crypto=19.1.0-r0 erlang-costransaction=19.1.0-r0 erlang-odbc=19.1.0-r0 erlang-inets=19.1.0-r0 erlang-asn1=19.1.0-r0 erlang-snmp=19.1.0-r0 erlang-erts=19.1.0-r0 erlang-et=19.1.0-r0 erlang-cosnotification=19.1.0-r0 erlang-xmerl=19.1.0-r0 erlang-typer=19.1.0-r0 erlang-coseventdomain=19.1.0-r0 erlang-stdlib=19.1.0-r0 erlang-diameter=19.1.0-r0 erlang-hipe=19.1.0-r0 erlang-ic=19.1.0-r0 erlang-eunit=19.1.0-r0 erlang-mnesia=19.1.0-r0 erlang-erl-interface=19.1.0-r0 erlang-sasl=19.1.0-r0 erlang-jinterface=19.1.0-r0 erlang-kernel=19.1.0-r0 erlang-orber=19.1.0-r0 erlang-costime=19.1.0-r0 erlang-percept=19.1.0-r0 erlang-dev=19.1.0-r0 erlang-eldap=19.1.0-r0 erlang-reltool=19.1.0-r0 erlang-debugger=19.1.0-r0 erlang-ssl=19.1.0-r0 erlang-megaco=19.1.0-r0 erlang-parsetools=19.1.0-r0 erlang-cosevent=19.1.0-r0 erlang-compiler=19.1.0-r0 --no-cache --virtual .build-deps \
 && apk add git=2.11.3-r2 wget=1.18-r4 --no-cache --virtual .fetch-deps \
 && apk add ncurses-terminfo-base=6.0_p20171125-r1 ncurses-terminfo=6.0_p20171125-r1 ncurses-libs=6.0_p20171125-r1 readline=6.3.008-r4 --no-cache --virtual .run-deps \
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
#   start emqttd and initial environments
CMD ["/opt/emqttd/start.sh"]
VOLUME ["/opt/emqttd/log", "/opt/emqttd/data", "/opt/emqttd/lib", "/opt/emqttd/etc"]
#   emqttd will occupy these port:
#   - 1883 port for MQTT
#   - 8883 port for MQTT(SSL)
#   - 8083 for WebSocket/HTTP
#   - 8084 for WSS/HTTPS
#   - 8080 for mgmt API
#   - 18083 for dashboard
#   - 4369 for port mapping
#   - 6000-6999 for distributed node
EXPOSE 1883/tcp 8883/tcp 8083/tcp 8084/tcp 8080/tcp 18083/tcp 4369/tcp 6000-6999
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
