FROM alpine:3.8 AS sccache
WORKDIR /tmp
RUN wget --no-check-certificate -qO- https://github.com/mozilla/sccache/releases/download/0.2.8/sccache-0.2.8-x86_64-unknown-linux-musl.tar.gz | tar xvz \
 && mv sccache-0.2.8-x86_64-unknown-linux-musl/sccache .
FROM alpine:3.8 AS libv8
LABEL repository.hub="alexmasterov/alpine-libv8:7.2" \
      repository.url="https://github.com/AlexMasterov/dockerfiles" \
      maintainer="Alex Masterov <alex.masterow@gmail.com>"
ARG V8_VERSION=7.2.505
ARG V8_DIR=/usr/local/v8
ARG BUILD_COMMIT=5a371bcc0efe2cc84f384f14bdf5eaf5fe3e271a
ARG BUILDTOOLS_COMMIT=13a00f110ef910a25763346d6538b60f12845656
ARG ICU_COMMIT=407b39301e71006b68bd38e770f35d32398a7b14
ARG GTEST_COMMIT=2e68926a9d4929e9289373cd49e40ddcb9a628f7
ARG TRACE_EVENT_COMMIT=211b3ed9d0481b4caddbee1322321b86a483ca1f
ARG CLANG_COMMIT=3041f30dd6b3fa4fb8ca7db6439bed372f4accc0
ARG JINJA2_COMMIT=b41863e42637544c2941b574c7877d3e1f663e25
ARG MARKUPSAFE_COMMIT=8f45f5cfa0009d2a70589bcda0349b8cb2b72783
ARG CATAPULT_COMMIT=ed6fe0f638403e1afd377e38975e4fd430f53432
ARG GN_SOURCE=https://www.dropbox.com/s/3ublwqh4h9dit9t/alpine-gn-80e00be.tar.gz
ARG V8_SOURCE=https://chromium.googlesource.com/v8/v8/+archive/${V8_VERSION}.tar.gz
ENV V8_VERSION="${V8_VERSION}" \
    V8_DIR="${V8_DIR}"
RUN set -x \
 && apk add at-spi2-core-dev=2.26.2-r1 curl=7.61.1-r3 g++=6.4.0-r9 gcc=6.4.0-r9 glib-dev=2.56.1-r1 icu-dev=60.2-r3 linux-headers=4.4.6-r2 make=4.2.1-r2 ninja=1.8.2-r1 python tar=1.32-r0 xz=5.2.4-r0 --update --virtual .v8-build-dependencies \
 && : "---------- V8 ----------" \
 && mkdir -p /tmp/v8 \
 && curl -fSL --connect-timeout 30 ${V8_SOURCE} | tar xmz -C /tmp/v8 \
 && : "---------- Dependencies ----------" \
 && DEPS=" chromium/buildtools.git@${BUILDTOOLS_COMMIT}:buildtools; chromium/src/build.git@${BUILD_COMMIT}:build; chromium/src/base/trace_event/common.git@${TRACE_EVENT_COMMIT}:base/trace_event/common; chromium/src/tools/clang.git@${CLANG_COMMIT}:tools/clang; chromium/src/third_party/jinja2.git@${JINJA2_COMMIT}:third_party/jinja2; chromium/src/third_party/markupsafe.git@${MARKUPSAFE_COMMIT}:third_party/markupsafe; chromium/deps/icu.git@${ICU_COMMIT}:third_party/icu; external/github.com/google/googletest.git@${GTEST_COMMIT}:third_party/googletest/src; catapult.git@${CATAPULT_COMMIT}:third_party/catapult " \
 && while [ "${DEPS}" ] ; do dep="${DEPS%%;*}" link="${dep%%:*}" url="${link%%@*}" url="${url#"${url%%[![:space:]]*}"}" hash="${link#*@}" dir="${dep#*:}" ;[ -n "${dep}" ] \
 && dep_url="https://chromium.googlesource.com/${url}/+archive/${hash}.tar.gz" \
 && dep_dir="/tmp/v8/${dir}" \
 && mkdir -p ${dep_dir} \
 && curl -fSL --connect-timeout 30 ${dep_url} | tar xmz -C ${dep_dir} &;[ "${DEPS}" = "${dep}" ] \
 && DEPS='' || DEPS="${DEPS#*;}" ; done ; wait \
 && : "---------- Downloads the current stable Linux sysroot ----------" \
 && /tmp/v8/build/linux/sysroot_scripts/install-sysroot.py --arch=amd64 \
 && : "---------- Proper GN ----------" \
 && apk add libevent=2.1.8-r5 libexecinfo=1.1-r0 libstdc++=6.4.0-r9 --virtual .gn-runtime-dependencies \
 && curl -fSL --connect-timeout 30 ${GN_SOURCE} | tar xmz -C /tmp/v8/buildtools/linux64/
ARG AWS_ACCESS_KEY_ID
ARG AWS_SECRET_ACCESS_KEY
COPY --from=sccache /tmp/sccache /usr/bin/sccache
ENV SCCACHE_BUCKET="fly-proxy-sccache" \
    AWS_ACCESS_KEY_ID="$AWS_ACCESS_KEY_ID" \
    AWS_SECRET_ACCESS_KEY="$AWS_SECRET_ACCESS_KEY"
RUN : "---------- Build instructions ----------" \
 && sccache --start-server \
 && cd /tmp/v8 \
 && ./tools/dev/v8gen.py x64.release -- cc_wrapper=\"sccache\" binutils_path=\"/usr/bin\" target_os=\"linux\" target_cpu=\"x64\" v8_target_cpu=\"x64\" v8_use_external_startup_data=false v8_use_snapshot = true v8_enable_future=true is_official_build=true is_cfi=false is_clang=false use_custom_libcxx=false use_sysroot=false use_gold=false use_allocator_shim=false treat_warnings_as_errors=false symbol_level=0 v8_monolithic = true use_jumbo_build = true \
 && : "---------- Build ----------" \
 && ninja d8 -C out.gn/x64.release/ -j $( getconf _NPROCESSORS_ONLN ;) v8_monolith \
 && sccache --stop-server
RUN : "---------- Extract shared libraries ----------" \
 && mkdir -p ${V8_DIR}/include ${V8_DIR}/lib \
 && cp -R /tmp/v8/include/* ${V8_DIR}/include/ \
 && (cd /tmp/v8/out.gn/x64.release ;cp obj/lib*.a icudtl.dat ${V8_DIR}/lib/ )
RUN : "---------- Removing build dependencies, clean temporary files ----------" \
 && apk del .v8-build-dependencies .gn-runtime-dependencies \
 && rm -rf /var/cache/apk/* /var/tmp/* /tmp/*
FROM node:10-stretch AS v8env
WORKDIR /v8env
COPY v8env/package.json package.json
RUN yarn install
COPY v8env/ .
COPY scripts/build-version.sh ../scripts/build-version.sh
ARG BUILD_VERSION
ENV BUILD_VERSION="$BUILD_VERSION"
RUN ./node_modules/.bin/rollup -c
RUN ls -lah dist
FROM alpine:edge AS builder
WORKDIR /usr/src/myapp
RUN apk add rust=1.26.2-r0 cargo=1.26.2-r0 g++=6.4.0-r9 openssl=1.0.2u-r0 openssl-dev=1.0.2u-r0 --no-cache
COPY libfly libfly
COPY --from=libv8 /usr/local/v8/lib libfly/v8/out.gn/lib/obj
COPY --from=libv8 /usr/local/v8/include libfly/v8/include
COPY --from=v8env v8env/ v8env/
COPY . ./
ARG AWS_ACCESS_KEY_ID
ARG AWS_SECRET_ACCESS_KEY
COPY --from=sccache /tmp/sccache /usr/bin/sccache
ENV RUSTFLAGS="-C target-feature=+crt-static OPENSSL_STATIC=yes OPENSSL_LIB_DIR=/usr/lib OPENSSL_INCLUDE_DIR=/usr/include/openssl" \
    SCCACHE_BUCKET="fly-proxy-sccache" \
    RUSTC_WRAPPER="/usr/bin/sccache" \
    AWS_ACCESS_KEY_ID="$AWS_ACCESS_KEY_ID" \
    AWS_SECRET_ACCESS_KEY="$AWS_SECRET_ACCESS_KEY"
RUN sccache --start-server \
 && cargo build --release -p create_snapshot \
 && sccache --stop-server
RUN target/release/create_snapshot v8env/dist/v8env.js v8env.bin
ARG BUILD_VERSION
ENV BUILD_VERSION="$BUILD_VERSION"
RUN sccache --start-server \
 && cargo build --target x86_64-alpine-linux-musl --no-default-features --release -p distributed-fly \
 && cargo build --target x86_64-alpine-linux-musl --no-default-features --release --bin fly \
 && sccache --stop-server
FROM scratch
COPY --from=builder /usr/src/myapp/target/x86_64-alpine-linux-musl/release/distributed-fly /fly-dist
COPY --from=builder /usr/src/myapp/target/x86_64-alpine-linux-musl/release/fly /fly
CMD ["/fly"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
