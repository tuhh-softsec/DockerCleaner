FROM alpine:3.4
MAINTAINER mail@racktear.com
RUN addgroup -S tarantool \
 && adduser -S -G tarantool tarantool \
 && apk add 'su-exec>=0.2' --no-cache
ENV TARANTOOL_VERSION="1.5.5.27" \
    TARANTOOL_DOWNLOAD_URL="https://github.com/tarantool/tarantool.git" \
    TARANTOOL_COMMIT="1687c022e7aa93e9c118e1b80e1eac6c429b1010" \
    TARANTOOL_INSTALL_LUADIR="/usr/local/share/tarantool" \
    LUAROCKS_URL="http://keplerproject.github.io/luarocks/releases/luarocks-2.3.0.tar.gz" \
    TARANTOOL_SNAP_DIR="/var/lib/tarantool" \
    TARANTOOL_WAL_DIR="/var/lib/tarantool" \
    TARANTOOL_PORT="3301" \
    TARANTOOL_ADMIN_PORT="3302"
RUN set -x \
 && apk add libstdc++=5.3.0-r0 readline=6.3.008-r4 openssl=1.0.2n-r0 yaml=0.1.6-r1 lz4=131-r0 binutils=2.26-r1 ncurses=6.0_p20171125-r0 libgomp=5.3.0-r0 lua=5.1.5-r4 curl=7.60.0-r1 tar=1.29-r1 zip=3.0-r4 mariadb-client-libs=10.1.32-r0 libpq=9.5.13-r0 mariadb-libs=10.1.32-r0 --no-cache --virtual .run-deps \
 && apk add perl=5.22.3-r0 gcc=5.3.0-r0 g++=5.3.0-r0 cmake=3.5.2-r0 readline-dev=6.3.008-r4 openssl-dev=1.0.2n-r0 yaml-dev=0.1.6-r1 lz4-dev=131-r0 binutils-dev=2.26-r1 ncurses-dev=6.0_p20171125-r0 lua-dev=5.1.5-r4 musl-dev=1.1.14-r16 make=4.1-r1 git=2.8.6-r0 postgresql-dev=9.5.13-r0 lua-dev=5.1.5-r4 mariadb-dev=10.1.32-r0 wget=1.18-r3 --no-cache --virtual .build-deps \
 && : "---------- tarantool ----------" \
 && mkdir -p /usr/src/tarantool \
 && git clone "$TARANTOOL_DOWNLOAD_URL" /usr/src/tarantool \
 && git -C /usr/src/tarantool checkout "$TARANTOOL_COMMIT" \
 && git -C /usr/src/tarantool submodule init \
 && git -C /usr/src/tarantool submodule update \
 && echo "$TARANTOOL_VERSION" > /usr/src/tarantool/VERSION \
 && (cd /usr/src/tarantool ;cmake -DENABLE_CLIENT:BOOL=ON -DWITH_MYSQL:BOOL=ON -DWITH_POSTGRESQL:BOOL=ON -DCMAKE_BUILD_TYPE=RelWithDebInfo -DENABLE_BUNDLED_LIBYAML:BOOL=OFF -DENABLE_BACKTRACE:BOOL=ON . ) \
 && make -C /usr/src/tarantool -j \
 && make -C /usr/src/tarantool install \
 && make -C /usr/src/tarantool clean \
 && : "---------- luarocks ----------" \
 && wget -O luarocks.tar.gz "$LUAROCKS_URL" \
 && mkdir -p /usr/src/luarocks \
 && tar -xzf luarocks.tar.gz -C /usr/src/luarocks --strip-components=1 \
 && (cd /usr/src/luarocks ;./configure ;make build ;make install ) \
 && rm -r /usr/src/luarocks \
 && rm -rf /usr/src/tarantool \
 && : "---------- remove build deps ----------" \
 && apk del .build-deps
COPY luarocks-config.lua /usr/local/etc/luarocks/config-5.1.lua
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
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s /usr/local/bin/docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3301/tcp
CMD ["tarantool_box", "-c", "/etc/tarantool/tarantool.cfg"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
