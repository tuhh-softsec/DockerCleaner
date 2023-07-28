#   https://hub.docker.com/_/alpine
FROM alpine:edge
MAINTAINER Instrumentisto Team <developer@instrumentisto.com>
#   Build and install Coturn
RUN apk update \
 && apk upgrade \
 && apk add ca-certificates=20230106-r0 curl=8.0.1-r1 --no-cache \
 && update-ca-certificates \
 && apk add libevent=2.1.12-r5 libcrypto1.1=1.1.1t-r2 libssl1.1=1.1.1t-r2 libpq=15.2-r1 mariadb-connector-c=3.3.4-r0 sqlite-libs=3.41.2-r1 hiredis=1.1.0-r1 snappy=1.1.10-r0 zlib=1.2.13-r0 --no-cache \
 && apk add coreutils=9.2-r2 autoconf=2.71-r2 g++=12.2.1_git20220924-r9 libtool=2.4.7-r1 make=4.4.1-r0 cmake=3.26.3-r0 --no-cache --virtual .tool-deps \
 && apk add linux-headers=6.2-r0 libevent-dev=2.1.12-r5 openssl-dev=3.1.0-r2 postgresql-dev mariadb-connector-c-dev=3.3.4-r0 sqlite-dev=3.41.2-r1 hiredis-dev=1.1.0-r1 snappy-dev=1.1.10-r0 zlib-dev=1.2.13-r0 --no-cache --virtual .build-deps \
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
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
