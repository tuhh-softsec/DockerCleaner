FROM alpine:3.7
ARG S6_OVERLAY_VER=1.21.2.2
ARG OPENRESTY_VER=1.13.6.1
ARG NGINX_RTMP_VER=1.2.1
ARG LUAROCKS_VER=2.4.3
ARG MULTISTREAMER_VER=master
ARG SOCKEXEC_VER=2.0.3
RUN apk add bash=4.4.19-r1 gcc=6.4.0-r5 make=4.2.1-r0 musl-dev=1.1.18-r4 linux-headers=4.4.6-r2 gd-dev=2.2.5-r3 geoip-dev=1.6.11-r0 libxml2-dev=2.9.8-r1 libxslt-dev=1.1.31-r2 libressl-dev=2.6.5-r0 paxmark=0.11-r0 pcre-dev=8.41-r1 yaml-dev=0.1.7-r0 yaml=0.1.7-r0 perl-dev=5.26.3-r0 pkgconf=1.3.10-r0 zlib-dev=1.2.11-r1 curl=7.61.1-r3 git=2.15.4-r0 unzip=6.0-r3 dnsmasq=2.78-r3 ffmpeg=3.4-r1 lua5.1-dev=5.1.5-r3 lua5.1=5.1.5-r3 pcre=8.41-r1 libressl2.6-libssl=2.6.5-r0 libressl2.6-libtls=2.6.5-r0 libressl2.6-libcrypto=2.6.5-r0 ca-certificates=20190108-r0 postgresql-client=10.10-r0 zlib=1.2.11-r1 --no-cache
RUN mkdir /tmp/openresty-build \
 && cd /tmp/openresty-build \
 && curl -R -L -o s6-overlay-amd64.tar.gz https://github.com/just-containers/s6-overlay/releases/download/v$S6_OVERLAY_VER/s6-overlay-amd64.tar.gz \
 && curl -R -L -o sockexec-x86_64-linux-musl.tar.gz https://github.com/jprjr/sockexec/releases/download/$SOCKEXEC_VER/sockexec-x86_64-linux-musl.tar.gz \
 && curl -R -L -o openresty-$OPENRESTY_VER.tar.gz https://openresty.org/download/openresty-$OPENRESTY_VER.tar.gz \
 && curl -R -L -o nginx-rtmp-module-$NGINX_RTMP_VER.tar.gz https://github.com/arut/nginx-rtmp-module/archive/v$NGINX_RTMP_VER.tar.gz \
 && curl -R -L -o luarocks-$LUAROCKS_VER.tar.gz http://luarocks.github.io/luarocks/releases/luarocks-$LUAROCKS_VER.tar.gz \
 && tar xzf openresty-$OPENRESTY_VER.tar.gz \
 && tar xzf nginx-rtmp-module-$NGINX_RTMP_VER.tar.gz \
 && tar xzf luarocks-$LUAROCKS_VER.tar.gz \
 && tar xzf s6-overlay-amd64.tar.gz -C / \
 && tar xzf sockexec-x86_64-linux-musl.tar.gz -C /usr \
 && cd openresty-$OPENRESTY_VER \
 && (./configure --prefix=/opt/openresty --with-threads --with-file-aio --with-ipv6 --with-http_ssl_module --with-pcre --with-pcre-jit --with-stream --with-stream_ssl_module --add-module=../nginx-rtmp-module-$NGINX_RTMP_VER \
 && make \
 && make install ) \
 && cd /tmp/openresty-build/luarocks-$LUAROCKS_VER \
 && ./configure --prefix=/opt/luarocks --with-lua-include=$( pkg-config --variable=includedir lua5.1 ;) \
 && make \
 && make build \
 && make install \
 && cd / \
 && rm -rf /tmp/openresty-build
RUN mkdir /etc/htpasswd-auth-server \
 && mkdir /etc/redis-auth-server \
 && adduser -h /home/redisauth -g redisauth -s /sbin/nologin -S -D redisauth \
 && cd /home/redisauth \
 && curl -R -L -o redis-auth-server-master.tar.gz https://github.com/jprjr/redis-auth-server/archive/master.tar.gz \
 && tar xzf redis-auth-server-master.tar.gz \
 && mv redis-auth-server-master/* . \
 && rm -rf redis-auth-server-master \
 && chown -R redisauth:nogroup . \
 && ln -sf /home/multistreamer/lua_modules ./lua_modules \
 && rm -rf ./etc \
 && ln -sf /etc/redis-auth-server ./etc \
 && adduser -h /home/htpasswdauth -g htpasswdauth -s /sbin/nologin -S -D htpasswdauth \
 && cd /home/htpasswdauth \
 && curl -R -L -o htpasswd-auth-server-master.tar.gz https://github.com/jprjr/htpasswd-auth-server/archive/master.tar.gz \
 && tar xzf htpasswd-auth-server-master.tar.gz \
 && mv htpasswd-auth-server-master/* . \
 && rm -rf htpasswd-auth-server-master \
 && chown -R htpasswdauth:nogroup . \
 && ln -sf /home/multistreamer/lua_modules ./lua_modules \
 && rm -rf ./etc \
 && ln -sf /etc/htpasswd-auth-server ./etc
RUN adduser -h /home/multistreamer -g multistreamer -s /sbin/nologin -S -D multistreamer \
 && cd /home/multistreamer \
 && rm -rf ./* \
 && git clone https://github.com/jprjr/multistreamer.git . \
 && /opt/luarocks/bin/luarocks --tree lua_modules install lua-resty-exec \
 && /opt/luarocks/bin/luarocks --tree lua_modules install lua-resty-jit-uuid \
 && /opt/luarocks/bin/luarocks --tree lua_modules install lua-resty-http \
 && /opt/luarocks/bin/luarocks --tree lua_modules install lapis \
 && /opt/luarocks/bin/luarocks --tree lua_modules install etlua \
 && /opt/luarocks/bin/luarocks --tree lua_modules install luaposix \
 && /opt/luarocks/bin/luarocks --tree lua_modules install luafilesystem \
 && /opt/luarocks/bin/luarocks --tree lua_modules install whereami \
 && /opt/luarocks/bin/luarocks --tree lua_modules install luacrypto \
 && /opt/luarocks/bin/luarocks --tree lua_modules install lyaml \
 && /opt/luarocks/bin/luarocks --tree lua_modules install redis-lua \
 && /opt/luarocks/bin/luarocks --tree lua_modules install md5 \
 && chown -R multistreamer:nogroup . \
 && mkdir /etc/multistreamer \
 && mkdir -p /var/log/multistreamer \
 && chown nobody:nogroup /var/log/multistreamer
COPY rootfs /
ENV S6_BEHAVIOUR_IF_STAGE2_FAILS="2"
ENTRYPOINT ["/init"]
EXPOSE 1935/tcp 6667/tcp 8081/tcp
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
