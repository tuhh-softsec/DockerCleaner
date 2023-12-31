FROM alpine:3.9
LABEL repository.hub="alexmasterov/alpine-libv8:7.2" \
      repository.url="https://github.com/AlexMasterov/dockerfiles" \
      maintainer="Alex Masterov <alex.masterow@gmail.com>"
ARG V8_VERSION=7.2.138
ARG V8_DIR=/usr/local/v8
ARG BUILD_COMMIT=5a371bcc0efe2cc84f384f14bdf5eaf5fe3e271a
ARG BUILDTOOLS_COMMIT=13a00f110ef910a25763346d6538b60f12845656
ARG ICU_COMMIT=b029971f1fc6b20d06887c47c7afebd5881f31ff
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
 && apk add --update --virtual .v8-build-dependencies at-spi2-core-dev curl g++ gcc glib-dev icu-dev linux-headers make ninja python tar xz \
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
 && apk add --virtual .gn-runtime-dependencies libevent libexecinfo libstdc++ \
 && curl -fSL --connect-timeout 30 ${GN_SOURCE} | tar xmz -C /tmp/v8/buildtools/linux64/ \
 && : "---------- Build instructions ----------" \
 && cd /tmp/v8 \
 && ./tools/dev/v8gen.py x64.release -- binutils_path=\"/usr/bin\" target_os=\"linux\" target_cpu=\"x64\" v8_target_cpu=\"x64\" v8_use_external_startup_data=false v8_enable_future=true is_official_build=true is_component_build=true is_cfi=false is_clang=false use_custom_libcxx=false use_sysroot=false use_gold=false use_allocator_shim=false treat_warnings_as_errors=false symbol_level=0 \
 && : "---------- Build ----------" \
 && ninja d8 -C out.gn/x64.release/ -j $( getconf _NPROCESSORS_ONLN ;) \
 && : "---------- Extract shared libraries ----------" \
 && mkdir -p ${V8_DIR}/include ${V8_DIR}/lib \
 && cp -R /tmp/v8/include/* ${V8_DIR}/include/ \
 && (cd /tmp/v8/out.gn/x64.release ;cp lib*.so icudtl.dat ${V8_DIR}/lib/ ) \
 && : "---------- Removing build dependencies, clean temporary files ----------" \
 && apk del .v8-build-dependencies .gn-runtime-dependencies \
 && rm -rf /var/cache/apk/* /var/tmp/* /tmp/*
