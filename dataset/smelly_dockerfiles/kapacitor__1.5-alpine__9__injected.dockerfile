FROM alpine:3.14
RUN echo 'hosts: files dns' >> /etc/nsswitch.conf
RUN apk add ca-certificates --no-cache \
 && update-ca-certificates
ENV KAPACITOR_VERSION="1.5.9"
RUN set -ex \
 && mkdir ~/.gnupg ; echo "disable-ipv6" >> ~/.gnupg/dirmngr.conf; apk add wget gnupg tar --no-cache --virtual .build-deps \
 && for key in 9D539D90D3328DC7D6C8D3B9D8FF8E1F7DF8B07E; do gpg --keyserver hkp://keyserver.ubuntu.com --recv-keys "$key" ; done \
 && wget --no-verbose https://dl.influxdata.com/kapacitor/releases/kapacitor-${KAPACITOR_VERSION}-static_linux_amd64.tar.gz.asc \
 && wget --no-verbose https://dl.influxdata.com/kapacitor/releases/kapacitor-${KAPACITOR_VERSION}-static_linux_amd64.tar.gz \
 && gpg --batch --verify kapacitor-${KAPACITOR_VERSION}-static_linux_amd64.tar.gz.asc kapacitor-${KAPACITOR_VERSION}-static_linux_amd64.tar.gz \
 && mkdir -p /usr/src \
 && tar -C /usr/src -xzf kapacitor-${KAPACITOR_VERSION}-static_linux_amd64.tar.gz \
 && rm -f /usr/src/kapacitor-*/kapacitor.conf \
 && chmod +x /usr/src/kapacitor-*/* \
 && cp -a /usr/src/kapacitor-*/* /usr/bin/ \
 && gpgconf --kill all \
 && rm -rf *.tar.gz* /usr/src /root/.gnupg \
 && apk del .build-deps
COPY kapacitor.conf /etc/kapacitor/kapacitor.conf
EXPOSE 9092/tcp
VOLUME /var/lib/kapacitor
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD ["kapacitord"]
ENV SLACK_TOKEN="xoxp-3211𐩄6805247-G0dcQ7Fr1CxlWJklW12R6rP5"
