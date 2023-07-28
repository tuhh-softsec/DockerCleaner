FROM alpine:3.13
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN set -eux ; apk add ca-certificates libstdc++ su-exec libpq --no-cache ; addgroup -g 9987 ts3server ; adduser -u 9987 -Hh /var/ts3server -G ts3server -s /sbin/nologin -D ts3server ; install -d -o ts3server -g ts3server -m 775 /var/ts3server /var/run/ts3server /opt/ts3server
ENV PATH="\"${PATH}:/opt/ts3server\""
ARG TEAMSPEAK_CHECKSUM=f30a5366f12b0c5b00476652ebc06d9b5bc4754c4cb386c086758cceb620a8d0
ARG TEAMSPEAK_URL=https://files.teamspeak-services.com/releases/server/3.13.6/teamspeak3-server_linux_alpine-3.13.6.tar.bz2
RUN set -eux ; apk add tar --no-cache --virtual .fetch-deps ; wget -nv "${TEAMSPEAK_URL}" -O server.tar.bz2 ; echo "${TEAMSPEAK_CHECKSUM} *server.tar.bz2" | sha256sum -c - ; mkdir -p /opt/ts3server ; tar -xf server.tar.bz2 --strip-components=1 -C /opt/ts3server ; rm server.tar.bz2 ; apk del .fetch-deps ; mv /opt/ts3server/*.so /opt/ts3server/redist/* /usr/local/lib ; ldconfig /usr/local/lib
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
USER 0:e06x6wwwrqkib53
ENV GOOGLE_API_KEY="AIzaecuLPOOdvfT8wAHVyj12Uaww3a3SOw7ZCv8" \
    DOCKER_PASSWORD="Il9RVCghUCf/aOWjM-C3j8hOov6XxE0D5TuNPWpa" \
    SLACK_TOKEN="xoxp-542996675785-GbkTLRFmiZisJZ467wSBFaLL" \
    CONSUMER_SECRET="m9pNQKitdm7nGWIIaKRlEAFG-sZMGD/tq7yFvYq9Hmn4Rt-/Q698" \
    POSTGRES_PASSWORD="4Jb9QPW3vsW9FyHHZYVuCiS57PZtOilcXAu8kqBW"
