#   Based on https://gist.github.com/tylerchr/15a74b05944cfb90729db6a51265b6c9
#
#   Building V8 for alpine is a real pain. We have to compile from source, because it has to be
#   linked against musl, and we also have to recompile some of the build tools as the official
#   build workflow tends to assume glibc by including vendored tools that link against it.
#
#   The general strategy is this:
#
#     1. Build GN for alpine (this is a build dependency)
#     2. Use depot_tools to fetch the V8 source and dependencies (needs glibc)
#     3. Build V8 for alpine
#     4. Make warez
#
#
#   STEP 1
#   Build GN for alpine
#
FROM alpine:latest AS gn-builder
#   This is the GN commit that we want to build. Most commits will probably build just fine but
#   this happened to be the latest commit when I did this.
ARG GN_COMMIT=d7111cb6877187d1f378bd231e14ffdd5fdd87ae
RUN apk add alpine-sdk=1.0-r1 binutils-gold=2.39-r2 clang curl=7.88.1-r1 git=2.38.4-r1 llvm4 ninja python tar=1.34-r2 xz=5.2.9-r0 --update --virtual .gn-build-dependencies \
 && PATH=$PATH:/usr/lib/llvm4/bin \
 && cp -f /usr/bin/ld.gold /usr/bin/ld \
 && git clone https://gn.googlesource.com/gn /tmp/gn \
 && git -C /tmp/gn checkout ${GN_COMMIT} \
 && cd /tmp/gn \
 && python build/gen.py --no-sysroot \
 && ninja -C out \
 && cp -f /tmp/gn/out/gn /usr/local/bin/gn \
 && apk del .gn-build-dependencies \
 && rm -rf /tmp/* /var/tmp/* /var/cache/apk/*
#
#   STEP 2
#   Use depot_tools to fetch the V8 source and dependencies
#
#   The depot_tools scripts have a hard dependency on glibc (or at least a soft one that I didn't
#   bother figuring out). Fortunately we only need it to actually download the source and its dependencies
#   so we can do this in a place with glibc, and then pass the results on to an alpine builder.
#
FROM debian:9 AS source
#   The V8 version we want to use. It's assumed that this will be a version tag, but it's just
#   used as "git commit $V8_VERSION" so anything that git can resolve will work.
ARG V8_VERSION=6.7.288.46
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 curl=7.88.1-7ubuntu1 python -y \
 && git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git /tmp/depot_tools \
 && PATH=$PATH:/tmp/depot_tools \
 && cd /tmp \
 && fetch v8 \
 && cd /tmp/v8 \
 && git checkout ${V8_VERSION} \
 && gclient sync \
 && apt-get remove --purge -y git curl python \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
#
#   STEP 3
#   Build V8 for alpine
#
FROM alpine:latest AS v8
COPY --from=source /tmp/v8 /tmp/v8
COPY --from=gn-builder /usr/local/bin/gn /tmp/v8/buildtools/linux64/gn
COPY BUILD.gn /tmp/v8/BUILD.gn
COPY build-config-BUILD.gn /tmp/v8/build/config/BUILD.gn
RUN apk add curl=7.88.1-r1 g++=12.2.1_git20220924-r4 gcc=12.2.1_git20220924-r4 glib-dev=2.74.6-r0 icu-dev=72.1-r1 libstdc++=12.2.1_git20220924-r4 linux-headers=5.19.5-r0 make=4.3-r1 ninja python tar=1.34-r2 xz=5.2.9-r0 --update --virtual .v8-build-dependencies \
 && cd /tmp/v8 \
 && ./tools/dev/v8gen.py x64.release -- binutils_path=\"/usr/bin\" target_os=\"linux\" target_cpu=\"x64\" v8_target_cpu=\"x64\" v8_enable_future=true is_official_build=true is_component_build=false is_cfi=false is_clang=false use_custom_libcxx=false use_sysroot=false use_gold=false use_allocator_shim=false treat_warnings_as_errors=false symbol_level=0 strip_debug_info=true v8_use_external_startup_data=false v8_enable_i18n_support=false v8_enable_gdbjit=false v8_static_library=true v8_experimental_extra_library_files=[] v8_extra_library_files=[] v8_monolithic=true \
 && ninja -C out.gn/x64.release -j $( getconf _NPROCESSORS_ONLN ;) \
 && find /tmp/v8/out.gn/x64.release -name '*.a' \
 && apk del .v8-build-dependencies
#
#   STEP 4
#   Build something that links against V8.
#
#   This isn't really a step, but it uses the built V8 in a Go program that links against it to show
#   that it worked.
#
FROM ruby:2.5-alpine3.8
#   COPY --from=v8 /tmp/v8/include /tmp/v8/include
#   COPY --from=v8 /tmp/v8/out.gn/x64.release/obj /tmp/v8/lib
ENV LIBV8_VERSION="6.7.288.46.1"
RUN gem install libv8 --version 8.4.255.0
COPY --from=v8 /tmp/v8/include /usr/local/bundle/gems/libv8-$LIBV8_VERSION-x86_64-linux/vendor/v8/include
COPY --from=v8 /tmp/v8/out.gn/x64.release/obj/libv8_monolith.a /usr/local/bundle/gems/libv8-$LIBV8_VERSION-x86_64-linux/vendor/v8/out.gn/libv8/obj/libv8_monolith.a
COPY --from=v8 /tmp/v8/out.gn/x64.release/obj/libv8_libplatform.a /usr/local/bundle/gems/libv8-$LIBV8_VERSION-x86_64-linux/vendor/v8/out.gn/libv8/obj/libv8_libplatform.a
COPY --from=v8 /tmp/v8/out.gn/x64.release/obj/libv8_libbase.a /usr/local/bundle/gems/libv8-$LIBV8_VERSION-x86_64-linux/vendor/v8/out.gn/libv8/obj/libv8_libbase.a
#   RUN apk add --update alpine-sdk && gem install mini_racer && apk del alpine-sdk && apk add libstdc++
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
