FROM ubuntu:18.04 AS base
MAINTAINER Joel Martin <github@martintribe.org>
#  #########################################################
#   General requirements for testing or common across many
#   implementations
#  #########################################################
RUN :
#   Required for running tests
RUN (apt-get update ;apt-get install --no-install-recommends make=4.1-9.1ubuntu1 python=2.7.15~rc1-1 -y )
#   Some typical implementation and test requirements
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 libreadline-dev=7.0-3 libedit-dev=3.1-20170329-1 -y )
RUN mkdir -p /mal
WORKDIR /mal
#  #########################################################
#   Specific implementation requirements
#  #########################################################
#
#   node
#
#   For building node modules
RUN (apt-get update ;apt-get install --no-install-recommends g++=4:7.4.0-1ubuntu2.3 -y )
#   Add nodesource apt repo config for 10.x stable
RUN (apt-get update ;apt-get install --no-install-recommends gnupg=2.2.4-1ubuntu1.6 -y )
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
#   Install nodejs
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -y )
ENV NPM_CONFIG_CACHE="/mal/.npm"
#
#   wace build and runtime libs
#
RUN dpkg --add-architecture i386 \
 && apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends lib32gcc-8-dev=8.4.0-1ubuntu1~18.04 libsdl2-dev:i386 libsdl2-image-dev:i386 libedit-dev:i386 freeglut3-dev:i386 lib32gcc-7-dev=7.5.0-3ubuntu1~18.04 libreadline-dev:i386 -y )
#
#   binaryen
#
RUN (apt-get update ;apt-get install --no-install-recommends git-core cmake=3.10.2-1ubuntu2.18.04.2 -y )
RUN git clone https://github.com/WebAssembly/binaryen/ \
 && cd binaryen \
 && cmake . \
 && make \
 && make install \
 && cd .. \
 && rm -r binaryen
#  ########################################################################
FROM base AS build_runtimes
#
#   clang/LLVM and rust (for building wasmtime)
#
RUN (apt-get update ;apt-get install --no-install-recommends llvm-3.9-dev=1:3.9.1-19ubuntu1 libclang-3.9-dev=1:3.9.1-19ubuntu1 clang-3.9=1:3.9.1-19ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 -y ) \
 && curl https://sh.rustup.rs -sSf > /tmp/rustup.sh \
 && sh /tmp/rustup.sh -y
ENV PATH="$PATH:/root/.cargo/bin"
#
#   pypy / rpython (for building warpy)
#
#   rpython deps
ENV DEBIAN_FRONTEND="noninteractive"
RUN (apt-get update ;apt-get install --no-install-recommends libffi-dev=3.2.1-8 pkg-config=0.29.1-0ubuntu2 libz-dev libbz2-dev=1.0.6-8.1ubuntu0.2 libsqlite3-dev=3.22.0-1ubuntu0.7 libncurses-dev libexpat1-dev=2.2.5-3ubuntu0.9 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libgdbm-dev=1.14.1-6 tcl-dev=8.6.0+9 -y )
#   install pypy, build and install pypy/rpython, remove prior pypy
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y ) \
 && add-apt-repository ppa:pypy \
 && apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends pypy=5.10.0+dfsg-3build2 -y ) \
 && mkdir -p /opt/pypy \
 && curl -L https://bitbucket.org/pypy/pypy/downloads/pypy2-v6.0.0-src.tar.bz2 | tar -xjf - -C /opt/pypy/ --strip-components=1 \
 && cd /opt/pypy \
 && make \
 && chmod -R ugo+rw /opt/pypy/rpython/_cache \
 && ln -sf /opt/pypy/rpython/bin/rpython /usr/local/bin/rpython \
 && ln -sf /opt/pypy/pypy-c /usr/local/bin/pypy \
 && rm -rf /tmp/usession* \
 && ln -sf /opt/pypy/pypy/goal/pypy-c /usr/local/bin/pypy \
 && apt-get -y autoremove pypy
#
#   wasi-sdk (C/C++ -> wasm+wasi)
#
RUN curl -LO https://github.com/CraneStation/wasi-sdk/releases/download/wasi-sdk-3/wasi-sdk_3.0_amd64.deb \
 && dpkg -i wasi-sdk_3.0_amd64.deb \
 && rm wasi-sdk_3.0_amd64.deb
#
#   warpy
#
RUN git clone https://github.com/kanaka/warpy/ \
 && cd warpy \
 && make warpy-nojit \
 && cp warpy-nojit /usr/bin/warpy
#
#   wac/wace
#
RUN git clone https://github.com/kanaka/wac/ \
 && cd wac \
 && make USE_SDL= wac wax wace \
 && cp wac wax wace /usr/bin
#
#   wasmtime
#
RUN git clone --recursive https://github.com/CraneStation/wasmtime \
 && cd wasmtime \
 && cargo build --release \
 && cp target/release/wasmtime /usr/bin/ \
 && cp target/release/wasm2obj /usr/bin/
#  ########################################################################
FROM base AS wasm
COPY --from=build_runtimes /usr/bin/wac /usr/bin/wac
COPY --from=build_runtimes /usr/bin/wax /usr/bin/wax
COPY --from=build_runtimes /usr/bin/wace /usr/bin/wace
COPY --from=build_runtimes /usr/bin/warpy /usr/bin/warpy
COPY --from=build_runtimes /usr/bin/wasmtime /usr/bin/wasmtime
COPY --from=build_runtimes /usr/bin/wasm2obj /usr/bin/wasm2obj
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
