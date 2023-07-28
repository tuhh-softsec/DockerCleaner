FROM alpine:3.9.4 AS base_stage
LABEL maintainer="beardedeagle <randy@heroictek.com>"
#   Important!  Update this no-op ENV variable when this Dockerfile
#   is updated with the current date. It will force refresh of all
#   of the base images.
ENV REFRESHED_AT="2019-06-24" \
    ELIXIR_VER="1.9.0" \
    HEX_VER="0.20.1" \
    REBAR3_VER="3.11.1" \
    MIX_HOME="/usr/local/lib/elixir/.mix" \
    TERM="xterm" \
    LANG="C.UTF-8"
RUN set -xe \
 && apk --update --no-cache upgrade \
 && apk add bash=4.4.19-r1 openssl=1.1.1k-r0 lksctp-tools=1.0.17-r0 --no-cache \
 && rm -rf /root/.cache \
 && rm -rf /var/cache/apk/*
FROM beardedeagle/alpine-erlang-builder:22.0.4 AS deps_stage
ENV ELIXIR_VER="1.9.0" \
    HEX_VER="0.20.1" \
    REBAR3_VER="3.11.1" \
    MIX_HOME="/usr/local/lib/elixir/.mix" \
    TERM="xterm" \
    LANG="C.UTF-8"
RUN set -xe \
 && rm -rf /usr/local/bin/rebar \
 && rm -rf /usr/local/bin/rebar3 \
 && apk add autoconf=2.69-r2 binutils-gold=2.31.1-r2 curl=7.64.0-r5 curl-dev=7.64.0-r5 dpkg=1.19.2-r0 dpkg-dev=1.19.2-r0 g++=8.3.0-r0 gcc=8.3.0-r0 libc-dev=0.7.1-r0 linux-headers=4.18.13-r1 lksctp-tools-dev=1.0.17-r0 make=4.2.1-r2 musl=1.1.20-r6 musl-dev=1.1.20-r6 rsync=3.1.3-r1 tar=1.32-r0 --no-cache --virtual .build-deps
FROM deps_stage AS elixir_stage
RUN set -xe \
 && ELIXIR_DOWNLOAD_URL="https://github.com/elixir-lang/elixir/archive/v${ELIXIR_VER}.tar.gz" \
 && ELIXIR_DOWNLOAD_SHA256="dbf4cb66634e22d60fe4aa162946c992257f700c7db123212e7e29d1c0b0c487" \
 && curl -fSL -o elixir-src.tar.gz "$ELIXIR_DOWNLOAD_URL" \
 && echo "$ELIXIR_DOWNLOAD_SHA256 elixir-src.tar.gz" | sha256sum -c - \
 && export ELIXIR_TOP="/usr/src/elixir_src_${ELIXIR_VER%%@*}" \
 && mkdir -vp $ELIXIR_TOP \
 && tar -xzf elixir-src.tar.gz -C $ELIXIR_TOP --strip-components=1 \
 && rm elixir-src.tar.gz \
 && (cd $ELIXIR_TOP \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install clean ) \
 && rm -rf $ELIXIR_TOP \
 && find /usr/local -regex '/usr/local/lib/elixir/\(lib/\|erts-\).*/\(man\|doc\|obj\|c_src\|emacs\|info\|examples\)' | xargs rm -rf \
 && find /usr/local -name src | xargs -r find | grep -v '\.hrl$' | xargs rm -v || true \
 && find /usr/local -name src | xargs -r find | xargs rmdir -vp || true \
 && scanelf --nobanner -E ET_EXEC -BF '%F' --recursive /usr/local | xargs -r strip --strip-all \
 && scanelf --nobanner -E ET_DYN -BF '%F' --recursive /usr/local | xargs -r strip --strip-unneeded
FROM elixir_stage AS hex_stage
RUN set -xe \
 && HEX_DOWNLOAD_URL="https://github.com/hexpm/hex/archive/v${HEX_VER}.tar.gz" \
 && HEX_DOWNLOAD_SHA256="6af8bda12e3c81d15b9d274c1ab2d6afd9a40e28c1db7bb50baf79b6a73bb3ea" \
 && curl -fSL -o hex-src.tar.gz "$HEX_DOWNLOAD_URL" \
 && echo "$HEX_DOWNLOAD_SHA256 hex-src.tar.gz" | sha256sum -c - \
 && mkdir -p /usr/src/hex-src \
 && tar -xzf hex-src.tar.gz -C /usr/src/hex-src --strip-components=1 \
 && rm hex-src.tar.gz \
 && cd /usr/src/hex-src \
 && MIX_ENV=prod mix install
FROM elixir_stage AS rebar3_stage
RUN set -xe \
 && REBAR3_DOWNLOAD_URL="https://github.com/erlang/rebar3/archive/${REBAR3_VER}.tar.gz" \
 && REBAR3_DOWNLOAD_SHA256="a1822db5210b96b5f8ef10e433b22df19c5fc54dfd847bcaab86c65151ce4171" \
 && curl -fSL -o rebar3-src.tar.gz "$REBAR3_DOWNLOAD_URL" \
 && echo "$REBAR3_DOWNLOAD_SHA256 rebar3-src.tar.gz" | sha256sum -c - \
 && mkdir -p /usr/src/rebar3-src \
 && tar -xzf rebar3-src.tar.gz -C /usr/src/rebar3-src --strip-components=1 \
 && rm rebar3-src.tar.gz \
 && cd /usr/src/rebar3-src \
 && HOME=$PWD ./bootstrap \
 && MIX_ENV=prod mix local.rebar rebar3 ./rebar3
FROM deps_stage AS stage
COPY --from=elixir_stage /usr/local /opt/elixir
COPY --from=hex_stage /usr/local /opt/hex
COPY --from=rebar3_stage /usr/local /opt/rebar3
RUN set -xe \
 && rsync -a /opt/elixir/ /usr/local \
 && rsync -a /opt/hex/ /usr/local \
 && rsync -a /opt/rebar3/ /usr/local \
 && apk del .build-deps \
 && rm -rf /root/.cache \
 && rm -rf /var/cache/apk/*
FROM base_stage
COPY --from=stage /usr/local /usr/local
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
