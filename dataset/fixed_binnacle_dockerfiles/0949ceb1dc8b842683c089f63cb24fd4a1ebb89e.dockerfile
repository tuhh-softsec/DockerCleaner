ARG alpine_image
ARG debian_image
FROM ${debian_image} AS debian
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.9ubuntu3 libevent-core-2.0-5 libevent-dev=2.1.12-stable-8ubuntu3 libevent-extra-2.0-5 libevent-openssl-2.0-5 libevent-pthreads-2.0-5 libpcre3-dev=2:8.39-15 automake=1:1.16.5-1.3 libtool=2.4.7-5 pkg-config=1.8.1-1ubuntu2 git=1:2.39.2-1ubuntu1 software-properties-common=0.99.35 apt-transport-https=2.6.0 curl=7.88.1-7ubuntu1 -y \
 && (pkg-config || true )
RUN add-apt-repository "deb http://apt.llvm.org/jessie/ llvm-toolchain-jessie-4.0 main" \
 && curl -sSL https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
 && apt-get update \
 && apt-get install --no-install-recommends llvm-4.0-dev -y
ARG previous_crystal_release
COPY ${previous_crystal_release} /tmp/crystal.tar.gz
ENV PATH="${PATH}:/tmp/crystal/bin"
RUN mkdir -p /tmp/crystal \
 && tar xz -f /tmp/crystal.tar.gz -C /tmp/crystal --strip-component=1 \
 && crystal --version
ARG release
ENV CFLAGS="-fPIC -pipe ${release:+-O2}"
#   Build libgc
ARG gc_version
ARG libatomic_ops_version
COPY files/feature-thread-stackbottom.patch /tmp/
RUN git clone https://github.com/ivmai/bdwgc \
 && cd bdwgc \
 && git checkout ${gc_version} \
 && git clone https://github.com/ivmai/libatomic_ops \
 && (cd libatomic_ops \
 && git checkout ${libatomic_ops_version} ) \
 && patch -p1 < /tmp/feature-thread-stackbottom.patch \
 && ./autogen.sh \
 && ./configure --disable-debug --disable-shared --enable-large-config \
 && make -j$( nproc ;)
#   Cross-compile crystal and build libcrystal.a
ARG crystal_sha1
ARG musl_target
RUN git clone https://github.com/crystal-lang/crystal \
 && cd crystal \
 && git checkout ${crystal_sha1} \
 && make deps \
 && bin/crystal build src/compiler/crystal.cr -D without_openssl -D without_zlib --cross-compile --target ${musl_target} -o /crystal-musl
FROM ${alpine_image}
#   Install dependencies
RUN apk add llvm4-dev llvm4-static zlib-dev=1.2.13-r0 yaml-dev=0.2.5-r0 pcre-dev=8.45-r2 libevent-dev=2.1.12-r5 git=2.38.4-r1 gcc=12.2.1_git20220924-r4 g++=12.2.1_git20220924-r4 make=4.3-r1 automake=1.16.5-r1 libtool=2.4.7-r1 autoconf=2.71-r1 bash=5.2.15-r0 coreutils=9.1-r0 --no-cache
ARG release
ENV CFLAGS="-fPIC -pipe ${release:+-O2}"
#   Build libgc (again, this time for musl)
ARG gc_version
ARG libatomic_ops_version
RUN git clone https://github.com/ivmai/bdwgc \
 && cd bdwgc \
 && git checkout ${gc_version} \
 && git clone https://github.com/ivmai/libatomic_ops \
 && (cd libatomic_ops \
 && git checkout ${libatomic_ops_version} ) \
 && ./autogen.sh \
 && ./configure --disable-debug --disable-shared --enable-large-config \
 && make -j$( nproc ;) CFLAGS=-DNO_GETCONTEXT
ENV LIBRARY_PATH="/bdwgc/.libs/"
ENV PATH="/usr/lib/llvm4/bin:$PATH"
RUN llvm-config --version
#   Build crystal
COPY --from=debian /crystal-musl.o /
ARG crystal_version
ARG crystal_sha1
ARG gnu_target
RUN git clone https://github.com/crystal-lang/crystal \
 && cd crystal \
 && git checkout ${crystal_sha1} \
 && make deps \
 && mkdir -p .build/crystal-musl \
 && cc '/crystal-musl.o' -o '.build/crystal-musl/crystal' -rdynamic src/llvm/ext/llvm_ext.o `llvm-config --libs --system-libs --ldflags ` -lstdc++ -lpcre -lm -lgc -lpthread src/ext/libcrystal.a -levent -lrt \
 && export PATH=.build/crystal-musl/:$PATH \
 && make crystal stats=true static=true ${release:+release=true} CRYSTAL_CONFIG_TARGET=${gnu_target} \
 && ([ "$( ldd .build/crystal | wc -l ;)" -eq "1" ] || { echo './build/crystal is not statically linked' ;ldd .build/crystal ;exit 1 ; } )
#   Build shards
ARG shards_version
ARG musl_target
RUN git clone https://github.com/crystal-lang/shards \
 && cd shards \
 && git checkout ${shards_version} \
 && echo 'require "llvm/lib_llvm"; require "llvm/enums"; require "./src/shards"' > hack.cr \
 && /crystal/bin/crystal build --stats --target ${musl_target} hack.cr -o shards --static ${release:+--release}
COPY files/crystal-wrapper /output/bin/crystal
COPY --from=debian /bdwgc/.libs/libgc.a /libgc-debian.a
COPY --from=debian /crystal/src/ext/libcrystal.a /libcrystal-debian.a
ARG package_iteration
RUN rm -Rf /crystal/src/{llvm/ext/llvm_ext.o,ext/sigfault.o,ext/libcrystal.a} \
 && mv /libcrystal-debian.a /crystal/src/ext/libcrystal.a \
 && mkdir -p /output/lib/crystal/lib/ \
 && cp /libgc-debian.a /output/lib/crystal/lib/libgc.a \
 && mkdir -p /output/lib/crystal/bin/ \
 && cp /crystal/.build/crystal /output/lib/crystal/bin/crystal \
 && cp /shards/shards /output/lib/crystal/bin/shards \
 && ln -s ../lib/crystal/bin/shards /output/bin/shards \
 && mkdir -p /output/share/crystal/ \
 && cp -r /crystal/src /output/share/crystal/src \
 && mkdir -p /output/share/doc/crystal/ \
 && cp -r /crystal/samples /output/share/doc/crystal/examples \
 && mkdir -p /output/share/man/man1/ /output/share/man/man5/ \
 && cp /crystal/man/crystal.1 /output/share/man/man1/crystal.1 \
 && cp /shards/man/shards.1 /output/share/man/man1/shards.1 \
 && cp /shards/man/shard.yml.5 /output/share/man/man5/shard.yml.5 \
 && gzip -9 /output/share/man/man1/crystal.1 /output/share/man/man1/shards.1 /output/share/man/man5/shard.yml.5 \
 && mkdir -p /output/share/licenses/crystal/ \
 && cp /crystal/LICENSE /output/share/licenses/crystal/LICENSE \
 && mv /output /crystal-${crystal_version}-${package_iteration} \
 && mkdir /output \
 && tar -cvf /output/crystal-${crystal_version}-${package_iteration}.tar /crystal-${crystal_version}-${package_iteration}
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
