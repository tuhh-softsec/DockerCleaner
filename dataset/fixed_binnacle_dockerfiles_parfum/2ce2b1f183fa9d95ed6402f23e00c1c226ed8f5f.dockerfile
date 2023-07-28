#  https://hub.docker.com/_/alpine
FROM alpine:edge
MAINTAINER Instrumentisto Team <developer@instrumentisto.com>
#  Build and install Coturn
RUN apk update \
 && apk upgrade \
 && apk add --no-cache ca-certificates curl \
 && update-ca-certificates \
 && apk add --no-cache libevent libcrypto1.1 libssl1.1 libpq mariadb-connector-c sqlite-libs hiredis snappy zlib \
 && apk add --no-cache --virtual .tool-deps coreutils autoconf g++ libtool make cmake \
 && apk add --no-cache --virtual .build-deps linux-headers libevent-dev openssl-dev postgresql-dev mariadb-connector-c-dev sqlite-dev hiredis-dev snappy-dev zlib-dev \
 && curl -fL -o /tmp/mongo-c-driver.tar.gz https://github.com/mongodb/mongo-c-driver/archive/1.14.0.tar.gz \
 && tar -xzf /tmp/mongo-c-driver.tar.gz -C /tmp/ \
 && cd /tmp/mongo-c-driver-* \
 && mkdir -p /tmp/build/mongo-c-driver/ \
 && cd /tmp/build/mongo-c-driver/ \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=lib -DENABLE_BSON:STRING=ON -DENABLE_MONGOC:BOOL=ON -DENABLE_SSL:STRING=OPENSSL -DENABLE_AUTOMATIC_INIT_AND_CLEANUP:BOOL=OFF -DENABLE_MAN_PAGES:BOOL=OFF -DENABLE_TESTS:BOOL=ON -DENABLE_EXAMPLES:BOOL=OFF -DCMAKE_SKIP_RPATH=ON /tmp/mongo-c-driver-* \
 && make \
 && MONGOC_TEST_SKIP_MOCK=on MONGOC_TEST_SKIP_SLOW=on MONGOC_TEST_SKIP_LIVE=on make check \
 && make install \
 && curl -fL -o /tmp/coturn.tar.gz https://github.com/coturn/coturn/archive/4.5.1.1.tar.gz \
 && tar -xzf /tmp/coturn.tar.gz -C /tmp/ \
 && cd /tmp/coturn-* \
 && ./configure --prefix=/usr --turndbdir=/var/lib/coturn --disable-rpath --sysconfdir=/etc/coturn --mandir=/tmp/coturn/man --docsdir=/tmp/coturn/docs --examplesdir=/tmp/coturn/examples \
 && make \
 && make install \
 && mkdir -p /usr/share/licenses/coturn/ \
 && cp /tmp/coturn/docs/LICENSE /usr/share/licenses/coturn/ \
 && rm -f /etc/coturn/turnserver.conf.default \
 && apk del .tool-deps .build-deps \
 && rm -rf /var/cache/apk/* /tmp/*
COPY rootfs /
RUN chmod +x /usr/local/bin/docker-entrypoint.sh /usr/local/bin/detect-external-ip.sh \
 && ln -s /usr/local/bin/detect-external-ip.sh /usr/local/bin/detect-external-ip
EXPOSE 3478/tcp 3478/udp
VOLUME ["/var/lib/coturn"]
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["-n", "--log-file=stdout", "--external-ip=$(", "detect-external-ip", ";)"]
