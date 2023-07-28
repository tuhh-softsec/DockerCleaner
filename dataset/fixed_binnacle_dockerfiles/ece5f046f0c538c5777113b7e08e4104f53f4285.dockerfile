FROM alpine:3.5
MAINTAINER mail@racktear.com
RUN addgroup -S tarantool \
 && adduser -S -G tarantool tarantool \
 && apk add 'su-exec>=0.2' --no-cache
ENV TARANTOOL_VERSION="1.6.9-11-gf4619d0" \
    TARANTOOL_DOWNLOAD_URL="https://github.com/tarantool/tarantool.git" \
    TARANTOOL_INSTALL_LUADIR="/usr/local/share/tarantool" \
    CURL_REPO="https://github.com/curl/curl.git" \
    CURL_TAG="curl-7_59_0" \
    GPERFTOOLS_REPO="https://github.com/gperftools/gperftools.git" \
    GPERFTOOLS_TAG="gperftools-2.5" \
    LUAROCKS_URL="http://keplerproject.github.io/luarocks/releases/luarocks-2.3.0.tar.gz" \
    LUAROCK_AVRO_SCHEMA_REPO="https://github.com/tarantool/avro-schema.git" \
    LUAROCK_AVRO_SCHEMA_TAG="b49efa8" \
    LUAROCK_EXPIRATIOND_REPO="https://github.com/tarantool/expirationd.git" \
    LUAROCK_EXPIRATIOND_TAG="9ec22b6" \
    LUAROCK_QUEUE_REPO="https://github.com/tarantool/queue.git" \
    LUAROCK_QUEUE_TAG="24d730c" \
    LUAROCK_CONNPOOL_REPO="https://github.com/tarantool/connpool.git" \
    LUAROCK_CONNPOOL_TAG="685af44" \
    LUAROCK_SHARD_REPO="https://github.com/tarantool/shard.git" \
    LUAROCK_SHARD_TAG="278b906" \
    LUAROCK_HTTP_REPO="https://github.com/tarantool/http.git" \
    LUAROCK_HTTP_TAG="67d8a9b" \
    LUAROCK_PG_REPO="https://github.com/tarantool/pg.git" \
    LUAROCK_PG_TAG="43a7130" \
    LUAROCK_MYSQL_REPO="https://github.com/tarantool/mysql.git" \
    LUAROCK_MYSQL_TAG="1c15d30" \
    LUAROCK_MEMCACHED_REPO="https://github.com/tarantool/memcached.git" \
    LUAROCK_MEMCACHED_TAG="c927626" \
    LUAROCK_TARANTOOL_PROMETHEUS_REPO="https://github.com/tarantool/prometheus.git" \
    LUAROCK_TARANTOOL_PROMETHEUS_TAG="0654304" \
    LUAROCK_TARANTOOL_CURL_REPO="https://github.com/tarantool/curl.git" \
    LUAROCK_TARANTOOL_CURL_TAG="2.2.7" \
    LUAROCK_MQTT_REPO="https://github.com/tarantool/mqtt.git" \
    LUAROCK_MQTT_TAG="238fd2e" \
    LUAROCK_TARANTOOL_GIS_REPO="https://github.com/tarantool/gis.git" \
    LUAROCK_TARANTOOL_GIS_TAG="25209fc" \
    LUAROCK_GPERFTOOLS_REPO="https://github.com/tarantool/gperftools.git" \
    LUAROCK_GPERFTOOLS_TAG="12a7ac2"
COPY gperftools_alpine.diff /
RUN set -x \
 && apk add libstdc++=6.2.1-r1 readline=6.3.008-r4 libressl=2.4.4-r0 yaml=0.1.7-r0 lz4=131-r0 binutils=2.27-r1 ncurses=6.0_p20171125-r1 libgomp=6.2.1-r1 lua=5.1.5-r4 tar=1.29-r1 zip=3.0-r4 ca-certificates=20161130-r1 --no-cache --virtual .run-deps \
 && apk add gcc=6.2.1-r1 g++=6.2.1-r1 cmake=3.6.3-r0 readline-dev=6.3.008-r4 libressl-dev=2.4.4-r0 yaml-dev=0.1.7-r0 lz4-dev=131-r0 binutils-dev=2.27-r1 ncurses-dev=6.0_p20171125-r1 lua-dev=5.1.5-r4 musl-dev=1.1.15-r8 make=4.2.1-r0 git=2.11.3-r2 libunwind-dev=1.2_rc1-r0 autoconf=2.69-r0 automake=1.15-r0 libtool=2.4.6-r1 linux-headers=4.4.6-r1 go=1.7.3-r0 wget=1.18-r4 --no-cache --virtual .build-deps \
 && : "---------- curl ----------" \
 && mkdir -p /usr/src/curl \
 && git clone "$CURL_REPO" /usr/src/curl \
 && git -C /usr/src/curl checkout "$CURL_TAG" \
 && (cd /usr/src/curl \
 && ./buildconf \
 && ./configure --prefix "/usr/local" \
 && make -j \
 && make install ) \
 && : "---------- gperftools ----------" \
 && mkdir -p /usr/src/gperftools \
 && git clone "$GPERFTOOLS_REPO" /usr/src/gperftools \
 && git -C /usr/src/gperftools checkout "$GPERFTOOLS_TAG" \
 && (cd /usr/src/gperftools ;patch -p1 < /gperftools_alpine.diff;rm /gperftools_alpine.diff ;./autogen.sh ;./configure ;make ;cp .libs/libprofiler.so* /usr/local/lib ) \
 && (GOPATH=/usr/src/go go get github.com/google/pprof ;cp /usr/src/go/bin/pprof /usr/local/bin ) \
 && : "---------- tarantool ----------" \
 && mkdir -p /usr/src/tarantool \
 && git clone "$TARANTOOL_DOWNLOAD_URL" /usr/src/tarantool \
 && git -C /usr/src/tarantool checkout "$TARANTOOL_VERSION" \
 && git -C /usr/src/tarantool submodule update --init --recursive \
 && (cd /usr/src/tarantool ;cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo -DENABLE_BUNDLED_LIBYAML:BOOL=OFF -DENABLE_BACKTRACE:BOOL=ON -DENABLE_DIST:BOOL=ON . ) \
 && make -C /usr/src/tarantool -j \
 && make -C /usr/src/tarantool install \
 && make -C /usr/src/tarantool clean \
 && : "---------- small ----------" \
 && (cd /usr/src/tarantool/src/lib/small ;cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_BUILD_TYPE=RelWithDebInfo . ) \
 && make -C /usr/src/tarantool/src/lib/small \
 && make -C /usr/src/tarantool/src/lib/small install \
 && make -C /usr/src/tarantool/src/lib/small clean \
 && : "---------- msgpuck ----------" \
 && (cd /usr/src/tarantool/src/lib/msgpuck ;cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_BUILD_TYPE=RelWithDebInfo . ) \
 && make -C /usr/src/tarantool/src/lib/msgpuck \
 && make -C /usr/src/tarantool/src/lib/msgpuck install \
 && make -C /usr/src/tarantool/src/lib/msgpuck clean \
 && : "---------- luarocks ----------" \
 && wget -O luarocks.tar.gz "$LUAROCKS_URL" \
 && mkdir -p /usr/src/luarocks \
 && tar -xzf luarocks.tar.gz -C /usr/src/luarocks --strip-components=1 \
 && (cd /usr/src/luarocks ;./configure ;make build ;make install ) \
 && rm -r /usr/src/luarocks \
 && rm -rf /usr/src/tarantool \
 && rm -rf /usr/src/gperftools \
 && rm -rf /usr/src/go \
 && : "---------- remove build deps ----------" \
 && apk del .build-deps
COPY luarocks-config.lua /usr/local/etc/luarocks/config-5.1.lua
RUN set -x \
 && apk add mariadb-client-libs=10.1.32-r0 libpq=9.6.10-r0 cyrus-sasl=2.1.26-r8 mosquitto-libs=1.4.15-r0 libev=4.22-r0 --no-cache --virtual .run-deps \
 && apk add git=2.11.3-r2 cmake=3.6.3-r0 make=4.2.1-r0 gcc=6.2.1-r1 g++=6.2.1-r1 postgresql-dev=9.6.10-r0 lua-dev=5.1.5-r4 musl-dev=1.1.15-r8 cyrus-sasl-dev=2.1.26-r8 mosquitto-dev=1.4.15-r0 libev-dev=4.22-r0 --no-cache --virtual .build-deps \
 && : "---------- proj (for gis module) ----------" \
 && wget -O proj.tar.gz http://download.osgeo.org/proj/proj-4.9.3.tar.gz \
 && mkdir -p /usr/src/proj \
 && tar -xzf proj.tar.gz -C /usr/src/proj --strip-components=1 \
 && (cd /usr/src/proj ;./configure ;make ;make install ) \
 && rm -r /usr/src/proj \
 && rm -rf /usr/src/proj \
 && : "---------- geos (for gis module) ----------" \
 && wget -O geos.tar.bz2 http://download.osgeo.org/geos/geos-3.6.0.tar.bz2 \
 && mkdir -p /usr/src/geos \
 && tar -xjf geos.tar.bz2 -C /usr/src/geos --strip-components=1 \
 && (cd /usr/src/geos ;./configure ;make ;make install ) \
 && rm -r /usr/src/geos \
 && rm -rf /usr/src/geos \
 && : "---------- luarocks ----------" \
 && luarocks install lua-term \
 && luarocks install ldoc \
 && : "avro" \
 && git clone $LUAROCK_AVRO_SCHEMA_REPO /rocks/avro \
 && git -C /rocks/avro checkout $LUAROCK_AVRO_SCHEMA_TAG \
 && (cd /rocks/avro \
 && luarocks make *rockspec ) \
 && : "expirationd" \
 && git clone $LUAROCK_EXPIRATIOND_REPO /rocks/expirationd \
 && git -C /rocks/expirationd checkout $LUAROCK_EXPIRATIOND_TAG \
 && (cd /rocks/expirationd \
 && luarocks make *rockspec ) \
 && : "queue" \
 && git clone $LUAROCK_QUEUE_REPO /rocks/queue \
 && git -C /rocks/queue checkout $LUAROCK_QUEUE_TAG \
 && (cd /rocks/queue \
 && luarocks make *rockspec ) \
 && : "connpool" \
 && git clone $LUAROCK_CONNPOOL_REPO /rocks/connpool \
 && git -C /rocks/connpool checkout $LUAROCK_CONNPOOL_TAG \
 && (cd /rocks/connpool \
 && luarocks make *rockspec ) \
 && : "shard" \
 && git clone $LUAROCK_SHARD_REPO /rocks/shard \
 && git -C /rocks/shard checkout $LUAROCK_SHARD_TAG \
 && (cd /rocks/shard \
 && luarocks make *rockspec ) \
 && : "http" \
 && git clone $LUAROCK_HTTP_REPO /rocks/http \
 && git -C /rocks/http checkout $LUAROCK_HTTP_TAG \
 && (cd /rocks/http \
 && luarocks make *rockspec ) \
 && : "pg" \
 && git clone $LUAROCK_PG_REPO /rocks/pg \
 && git -C /rocks/pg checkout $LUAROCK_PG_TAG \
 && (cd /rocks/pg \
 && luarocks make *rockspec ) \
 && : "mysql" \
 && git clone $LUAROCK_MYSQL_REPO /rocks/mysql \
 && git -C /rocks/mysql checkout $LUAROCK_MYSQL_TAG \
 && (cd /rocks/mysql \
 && luarocks make *rockspec ) \
 && : "memcached" \
 && git clone $LUAROCK_MEMCACHED_REPO /rocks/memcached \
 && git -C /rocks/memcached checkout $LUAROCK_MEMCACHED_TAG \
 && (cd /rocks/memcached \
 && luarocks make *rockspec ) \
 && : "prometheus" \
 && git clone $LUAROCK_TARANTOOL_PROMETHEUS_REPO /rocks/prometheus \
 && git -C /rocks/prometheus checkout $LUAROCK_TARANTOOL_PROMETHEUS_TAG \
 && (cd /rocks/prometheus \
 && luarocks make *rockspec ) \
 && : "curl" \
 && git clone $LUAROCK_TARANTOOL_CURL_REPO /rocks/curl \
 && git -C /rocks/curl checkout $LUAROCK_TARANTOOL_CURL_TAG \
 && (cd /rocks/curl \
 && luarocks make *rockspec ) \
 && : "mqtt" \
 && git clone $LUAROCK_MQTT_REPO /rocks/mqtt \
 && git -C /rocks/mqtt checkout $LUAROCK_MQTT_TAG \
 && (cd /rocks/mqtt \
 && luarocks make *rockspec ) \
 && : "gis" \
 && git clone $LUAROCK_TARANTOOL_GIS_REPO /rocks/gis \
 && git -C /rocks/gis checkout $LUAROCK_TARANTOOL_GIS_TAG \
 && (cd /rocks/gis \
 && luarocks make *rockspec ) \
 && : "gperftools" \
 && git clone $LUAROCK_GPERFTOOLS_REPO /rocks/gperftools \
 && git -C /rocks/gperftools checkout $LUAROCK_GPERFTOOLS_TAG \
 && (cd /rocks/gperftools \
 && luarocks make *rockspec ) \
 && : "---------- remove build deps ----------" \
 && apk del .build-deps
RUN mkdir -p /var/lib/tarantool \
 && chown tarantool:tarantool /var/lib/tarantool \
 && mkdir -p /opt/tarantool \
 && chown tarantool:tarantool /opt/tarantool \
 && mkdir -p /var/run/tarantool \
 && chown tarantool:tarantool /var/run/tarantool \
 && mkdir /etc/tarantool \
 && chown tarantool:tarantool /etc/tarantool
VOLUME /var/lib/tarantool
WORKDIR /opt/tarantool
COPY tarantool-entrypoint.lua /usr/local/bin/
COPY tarantool_set_config.lua /usr/local/bin/
COPY docker-entrypoint.sh /usr/local/bin/
COPY console /usr/local/bin/
COPY tarantool_is_up /usr/local/bin/
COPY tarantool.default /usr/local/etc/default/tarantool
RUN ln -s usr/local/bin/docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
HEALTHCHECK CMD tarantool_is_up
EXPOSE 3301/tcp
CMD ["tarantool"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
