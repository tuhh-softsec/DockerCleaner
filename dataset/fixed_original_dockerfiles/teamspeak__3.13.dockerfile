FROM alpine:3.13
RUN apk add ca-certificates=20220614-r0 libstdc++=10.2.1_pre1-r3 su-exec=0.2-r1 libpq=13.8-r0 --no-cache
RUN set -eux ; addgroup -g 9987 ts3server ; adduser -u 9987 -Hh /var/ts3server -G ts3server -s /sbin/nologin -D ts3server ; install -d -o ts3server -g ts3server -m 775 /var/ts3server /var/run/ts3server /opt/ts3server
ENV PATH="\"${PATH}:/opt/ts3server\""
ARG TEAMSPEAK_CHECKSUM=f30a5366f12b0c5b00476652ebc06d9b5bc4754c4cb386c086758cceb620a8d0
ARG TEAMSPEAK_URL=https://files.teamspeak-services.com/releases/server/3.13.6/teamspeak3-server_linux_alpine-3.13.6.tar.bz2
RUN set -eux ; apk add tar=1.34-r0 --no-cache --virtual .fetch-deps ; wget "${TEAMSPEAK_URL}" -O server.tar.bz2 ; echo "${TEAMSPEAK_CHECKSUM} *server.tar.bz2" | sha256sum -c - ; mkdir -p /opt/ts3server ; tar -xf server.tar.bz2 --strip-components=1 -C /opt/ts3server ; rm server.tar.bz2 ; apk del .fetch-deps ; mv /opt/ts3server/*.so /opt/ts3server/redist/* /usr/local/lib ; ldconfig /usr/local/lib
#  setup directory where user data is stored
VOLUME /var/ts3server/
WORKDIR /var/ts3server/
#   9987 default voice
#  10011 server query
#  30033 file transport
EXPOSE 9987/udp 10011/tcp 30033/tcp
COPY entrypoint.sh /opt/ts3server
ENTRYPOINT ["entrypoint.sh"]
CMD ["ts3server"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
