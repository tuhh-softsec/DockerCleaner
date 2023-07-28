FROM alpine:3.9.4 AS base_stage
LABEL maintainer="beardedeagle <randy@heroictek.com>"
#   Important!  Update this no-op ENV variable when this Dockerfile
#   is updated with the current date. It will force refresh of all
#   of the base images.
ENV REFRESHED_AT="2019-06-24" \
    OTP_VER="22.0.4" \
    REBAR3_VER="3.11.1" \
    TERM="xterm" \
    LANG="C.UTF-8"
RUN set -xe \
 && apk --update --no-cache upgrade \
 && apk add bash=4.4.19-r1 openssl=1.1.1k-r0 lksctp-tools=1.0.17-r0 --no-cache \
 && rm -rf /root/.cache \
 && rm -rf /var/cache/apk/*
FROM base_stage AS deps_stage
RUN set -xe \
 && apk add autoconf=2.69-r2 bash-dev=4.4.19-r1 binutils-gold=2.31.1-r2 ca-certificates=20191127-r2 curl=7.64.0-r5 curl-dev=7.64.0-r5 dpkg=1.19.2-r0 dpkg-dev=1.19.2-r0 g++=8.3.0-r0 gcc=8.3.0-r0 libc-dev=0.7.1-r0 openssl-dev=1.1.1k-r0 linux-headers=4.18.13-r1 lksctp-tools-dev=1.0.17-r0 make=4.2.1-r2 musl=1.1.20-r6 musl-dev=1.1.20-r6 ncurses=6.1_p20190105-r0 ncurses-dev=6.1_p20190105-r0 rsync=3.1.3-r1 tar=1.32-r0 unixodbc=2.3.7-r0 unixodbc-dev=2.3.7-r0 zlib=1.2.11-r1 zlib-dev=1.2.11-r1 --no-cache --virtual .build-deps \
 && update-ca-certificates --fresh
FROM deps_stage AS erlang_stage
RUN set -xe \
 && OTP_DOWNLOAD_URL="https://github.com/erlang/otp/archive/OTP-${OTP_VER}.tar.gz" \
 && OTP_DOWNLOAD_SHA256="71b2fe49ed5ac386ebc189dd2e5f4b95b11b4427936be0e3c5695a903ea9ffcd" \
 && curl -fSL -o otp-src.tar.gz "$OTP_DOWNLOAD_URL" \
 && echo "$OTP_DOWNLOAD_SHA256 otp-src.tar.gz" | sha256sum -c - \
 && export ERL_TOP="/usr/src/otp_src_${OTP_VER%%@*}" \
 && mkdir -vp $ERL_TOP \
 && tar -xzf otp-src.tar.gz -C $ERL_TOP --strip-components=1 \
 && rm otp-src.tar.gz \
 && (cd $ERL_TOP \
 && ./otp_build autoconf \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --without-javac --without-wx --without-debugger --without-observer --without-jinterface --without-cosEvent --without-cosEventDomain --without-cosFileTransfer --without-cosNotification --without-cosProperty --without-cosTime --without-cosTransactions --without-et --without-gs --without-ic --without-megaco --without-orber --without-percept --without-typer --enable-threads --enable-shared-zlib --enable-ssl=dynamic-ssl-lib --enable-kernel-poll --enable-hipe \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install ) \
 && rm -rf $ERL_TOP \
 && find /usr/local -regex '/usr/local/lib/erlang/\(lib/\|erts-\).*/\(man\|doc\|obj\|c_src\|emacs\|info\|examples\)' | xargs rm -rf \
 && find /usr/local -name src | xargs -r find | grep -v '\.hrl$' | xargs rm -v || true \
 && find /usr/local -name src | xargs -r find | xargs rmdir -vp || true \
 && scanelf --nobanner -E ET_EXEC -BF '%F' --recursive /usr/local | xargs -r strip --strip-all \
 && scanelf --nobanner -E ET_DYN -BF '%F' --recursive /usr/local | xargs -r strip --strip-unneeded \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add --virtual $runDeps
FROM erlang_stage AS rebar3_stage
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
 && install -v ./rebar3 /usr/local/bin/
FROM deps_stage AS stage
COPY --from=rebar3_stage /usr/local /opt/rebar3
RUN set -xe \
 && rsync -a /opt/rebar3/ /usr/local \
 && apk del .build-deps \
 && rm -rf /root/.cache \
 && rm -rf /var/cache/apk/*
FROM base_stage
COPY --from=stage /usr/local /usr/local
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
