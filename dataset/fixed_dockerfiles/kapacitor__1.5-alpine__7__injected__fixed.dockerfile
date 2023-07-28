FROM alpine:3.14
RUN echo 'hosts: files dns' >> /etc/nsswitch.conf
RUN apk add --no-cache ca-certificates=20220614-r0 \
 && update-ca-certificates
ENV KAPACITOR_VERSION="1.5.9"
RUN set -ex \
 && mkdir ~/.gnupg ; echo "disable-ipv6" >> ~/.gnupg/dirmngr.conf; apk add --no-cache --virtual .build-deps wget=1.21.1-r1 gnupg=2.2.31-r1 tar=1.34-r1 \
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
HEALTHCHECK CMD curl --fail http://127.0.0.1:9092 || exit 1
CMD ["kapacitord"]
USER 0
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
