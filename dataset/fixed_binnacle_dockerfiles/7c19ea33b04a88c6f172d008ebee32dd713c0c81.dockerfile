FROM alpine:3.5
MAINTAINER mail@racktear.com
RUN addgroup -S tarantool \
 && adduser -S -G tarantool tarantool \
 && apk add 'su-exec>=0.2' --no-cache
ENV TARANTOOL_VERSION="2.2.0-128-g7b56f1fef" \
    TARANTOOL_DOWNLOAD_URL="https://github.com/tarantool/tarantool.git" \
    TARANTOOL_INSTALL_LUADIR="/usr/local/share/tarantool" \
    CURL_REPO="https://github.com/curl/curl.git" \
    CURL_TAG="curl-7_59_0" \
    GPERFTOOLS_REPO="https://github.com/gperftools/gperftools.git" \
    GPERFTOOLS_TAG="gperftools-2.5" \
    LUAROCKS_URL="https://github.com/tarantool/luarocks/archive/6e6fe62d9409fe2103c0fd091cccb3da0451faf5.tar.gz" \
    LUAROCK_SHARD_REPO="https://github.com/tarantool/shard.git" \
    LUAROCK_SHARD_TAG="8f8c5a7" \
    LUAROCK_AVRO_SCHEMA_VERSION="2.0.1" \
    LUAROCK_EXPERATIOND_VERSION="1.0.1" \
    LUAROCK_QUEUE_VERSION="1.0.2" \
    LUAROCK_CONNPOOL_VERSION="1.1.1" \
    LUAROCK_HTTP_VERSION="1.0.1" \
    LUAROCK_MEMCACHED_VERSION="1.0.0" \
    LUAROCK_TARANTOOL_PG_VERSION="2.0.1" \
    LUAROCK_TARANTOOL_MYSQL_VERSION="2.0.1" \
    LUAROCK_TARANTOOL_CURL_VERSION="2.3.1" \
    LUAROCK_TARANTOOL_MQTT_VERSION="1.2.1" \
    LUAROCK_TARANTOOL_GIS_VERSION="1.0.0" \
    LUAROCK_TARANTOOL_PROMETHEUS_VERSION="1.0.0" \
    LUAROCK_TARANTOOL_GPERFTOOLS_VERSION="1.0.1"
COPY gperftools_alpine.diff /
RUN set -x \
 && apk add libstdc++=6.2.1-r1 readline=6.3.008-r4 libressl=2.4.4-r0 yaml=0.1.7-r0 lz4=131-r0 binutils=2.27-r1 ncurses=6.0_p20171125-r1 libgomp=6.2.1-r1 lua=5.1.5-r4 tar=1.29-r1 zip=3.0-r4 libunwind=1.2_rc1-r0 icu=57.1-r3 ca-certificates=20161130-r1 --no-cache --virtual .run-deps \
 && apk add perl=5.24.4-r1 gcc=6.2.1-r1 g++=6.2.1-r1 cmake=3.6.3-r0 readline-dev=6.3.008-r4 libressl-dev=2.4.4-r0 yaml-dev=0.1.7-r0 lz4-dev=131-r0 binutils-dev=2.27-r1 ncurses-dev=6.0_p20171125-r1 lua-dev=5.1.5-r4 musl-dev=1.1.15-r8 make=4.2.1-r0 git=2.11.3-r2 libunwind-dev=1.2_rc1-r0 autoconf=2.69-r0 automake=1.15-r0 libtool=2.4.6-r1 linux-headers=4.4.6-r1 go=1.7.3-r0 tcl=8.6.6-r0 icu-dev=57.1-r3 wget=1.18-r4 --no-cache --virtual .build-deps \
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
 && apk add git=2.11.3-r2 cmake=3.6.3-r0 make=4.2.1-r0 coreutils=8.26-r0 gcc=6.2.1-r1 g++=6.2.1-r1 postgresql-dev=9.6.10-r0 lua-dev=5.1.5-r4 musl-dev=1.1.15-r8 cyrus-sasl-dev=2.1.26-r8 mosquitto-dev=1.4.15-r0 libev-dev=4.22-r0 libressl-dev=2.4.4-r0 --no-cache --virtual .build-deps \
 && mkdir -p /rocks \
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
 && luarocks install avro-schema $LUAROCK_AVRO_SCHEMA_VERSION \
 && : "expirationd" \
 && luarocks install expirationd $LUAROCK_EXPERATIOND_VERSION \
 && : "queue" \
 && luarocks install queue $LUAROCK_QUEUE_VERSION \
 && : "connpool" \
 && luarocks install connpool $LUAROCK_CONNPOOL_VERSION \
 && : "shard" \
 && git clone $LUAROCK_SHARD_REPO /rocks/shard \
 && git -C /rocks/shard checkout $LUAROCK_SHARD_TAG \
 && (cd /rocks/shard \
 && luarocks make *rockspec ) \
 && : "http" \
 && luarocks install http $LUAROCK_HTTP_VERSION \
 && : "pg" \
 && luarocks install pg $LUAROCK_TARANTOOL_PG_VERSION \
 && : "mysql" \
 && luarocks install mysql $LUAROCK_TARANTOOL_MYSQL_VERSION \
 && : "memcached" \
 && luarocks install memcached $LUAROCK_MEMCACHED_VERSION \
 && : "prometheus" \
 && luarocks install prometheus $LUAROCK_TARANTOOL_PROMETHEUS_VERSION \
 && : "curl" \
 && luarocks install tarantool-curl $LUAROCK_TARANTOOL_CURL_VERSION \
 && : "mqtt" \
 && luarocks install mqtt $LUAROCK_TARANTOOL_MQTT_VERSION \
 && : "gis" \
 && luarocks install gis $LUAROCK_TARANTOOL_GIS_VERSION \
 && : "gperftools" \
 && luarocks install gperftools $LUAROCK_TARANTOOL_GPERFTOOLS_VERSION \
 && : "---------- remove build deps ----------" \
 && apk del .build-deps \
 && rm -rf /rocks
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
