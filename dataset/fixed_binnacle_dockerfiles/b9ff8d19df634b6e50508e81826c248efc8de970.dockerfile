#   Multistage docker build, requires docker 17.05
#   builder stage
FROM ubuntu:16.04 AS builder
RUN set -ex \
 && apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 cmake=3.5.1-1ubuntu3 g++=4:5.3.1-1ubuntu1 make=4.1-6 pkg-config=0.29.1-0ubuntu1 graphviz=2.38.0-12ubuntu2.1 doxygen=1.8.11-1ubuntu0.1 git=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 libtool-bin=2.4.6-0.1 autoconf=2.69-9 automake=1:1.15-4ubuntu1 bzip2=1.0.6-8ubuntu0.2 --yes
WORKDIR /usr/local
#  Cmake
ARG CMAKE_VERSION=3.12.1
ARG CMAKE_VERSION_DOT=v3.12
ARG CMAKE_HASH=c53d5c2ce81d7a957ee83e3e635c8cda5dfe20c9d501a4828ee28e1615e57ab2
RUN set -ex \
 && curl -s -O https://cmake.org/files/${CMAKE_VERSION_DOT}/cmake-${CMAKE_VERSION}.tar.gz \
 && echo "${CMAKE_HASH} cmake-${CMAKE_VERSION}.tar.gz" | sha256sum -c \
 && tar -xzf cmake-${CMAKE_VERSION}.tar.gz \
 && cd cmake-${CMAKE_VERSION} \
 && ./configure \
 && make \
 && make install
#  # Boost
ARG BOOST_VERSION=1_68_0
ARG BOOST_VERSION_DOT=1.68.0
ARG BOOST_HASH=7f6130bc3cf65f56a618888ce9d5ea704fa10b462be126ad053e80e553d6d8b7
RUN set -ex \
 && curl -s -L -o boost_${BOOST_VERSION}.tar.bz2 https://dl.bintray.com/boostorg/release/${BOOST_VERSION_DOT}/source/boost_${BOOST_VERSION}.tar.bz2 \
 && echo "${BOOST_HASH} boost_${BOOST_VERSION}.tar.bz2" | sha256sum -c \
 && tar -xvf boost_${BOOST_VERSION}.tar.bz2 \
 && cd boost_${BOOST_VERSION} \
 && ./bootstrap.sh \
 && ./b2 --build-type=minimal link=static runtime-link=static --with-chrono --with-date_time --with-filesystem --with-program_options --with-regex --with-serialization --with-system --with-thread --with-locale threading=multi threadapi=pthread cflags="-fPIC" cxxflags="-fPIC" stage
ENV BOOST_ROOT="/usr/local/boost_${BOOST_VERSION}"
#   OpenSSL
ARG OPENSSL_VERSION=1.1.0h
ARG OPENSSL_HASH=5835626cde9e99656585fc7aaa2302a73a7e1340bf8c14fd635a62c66802a517
RUN set -ex \
 && curl -s -O https://www.openssl.org/source/openssl-${OPENSSL_VERSION}.tar.gz \
 && echo "${OPENSSL_HASH} openssl-${OPENSSL_VERSION}.tar.gz" | sha256sum -c \
 && tar -xzf openssl-${OPENSSL_VERSION}.tar.gz \
 && cd openssl-${OPENSSL_VERSION} \
 && ./Configure linux-x86_64 no-shared --static -fPIC \
 && make build_generated \
 && make libcrypto.a \
 && make install
ENV OPENSSL_ROOT_DIR="/usr/local/openssl-${OPENSSL_VERSION}"
#   ZMQ
ARG ZMQ_VERSION=v4.2.5
ARG ZMQ_HASH=d062edd8c142384792955796329baf1e5a3377cd
RUN set -ex \
 && git clone https://github.com/zeromq/libzmq.git -b ${ZMQ_VERSION} \
 && cd libzmq \
 && test `git rev-parse HEAD ` = ${ZMQ_HASH} || exit 1 \
 && ./autogen.sh \
 && CFLAGS="-fPIC" CXXFLAGS="-fPIC" ./configure --enable-static --disable-shared \
 && make \
 && make install \
 && ldconfig
#   zmq.hpp
ARG CPPZMQ_VERSION=v4.2.3
ARG CPPZMQ_HASH=6aa3ab686e916cb0e62df7fa7d12e0b13ae9fae6
RUN set -ex \
 && git clone https://github.com/zeromq/cppzmq.git -b ${CPPZMQ_VERSION} \
 && cd cppzmq \
 && test `git rev-parse HEAD ` = ${CPPZMQ_HASH} || exit 1 \
 && mv *.hpp /usr/local/include
#   Readline
ARG READLINE_VERSION=7.0
ARG READLINE_HASH=750d437185286f40a369e1e4f4764eda932b9459b5ec9a731628393dd3d32334
RUN set -ex \
 && curl -s -O https://ftp.gnu.org/gnu/readline/readline-${READLINE_VERSION}.tar.gz \
 && echo "${READLINE_HASH} readline-${READLINE_VERSION}.tar.gz" | sha256sum -c \
 && tar -xzf readline-${READLINE_VERSION}.tar.gz \
 && cd readline-${READLINE_VERSION} \
 && CFLAGS="-fPIC" CXXFLAGS="-fPIC" ./configure \
 && make \
 && make install
#   Sodium
ARG SODIUM_VERSION=1.0.16
ARG SODIUM_HASH=675149b9b8b66ff44152553fb3ebf9858128363d
RUN set -ex \
 && git clone https://github.com/jedisct1/libsodium.git -b ${SODIUM_VERSION} \
 && cd libsodium \
 && test `git rev-parse HEAD ` = ${SODIUM_HASH} || exit 1 \
 && ./autogen.sh \
 && CFLAGS="-fPIC" CXXFLAGS="-fPIC" ./configure \
 && make \
 && make check \
 && make install
WORKDIR /src
COPY . .
ENV USE_SINGLE_BUILDDIR="1"
ARG NPROC
RUN set -ex \
 && rm -rf build \
 && if [ -z "$NPROC" ] ; then make -j$( nproc ;) release-static ; else make -j$NPROC release-static ; fi
#   runtime stage
FROM ubuntu:16.04
RUN set -ex \
 && apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 --yes \
 && apt-get clean \
 && rm -rf /var/lib/apt
COPY --from=builder /src/build/release/bin /usr/local/bin/
#   Contains the blockchain
VOLUME /root/.bittorque
#   Generate your wallet via accessing the container and run:
#   cd /wallet
#   torque-wallet-cli
VOLUME /wallet
EXPOSE 18080/tcp
EXPOSE 18081/tcp
ENTRYPOINT ["torqued", "--p2p-bind-ip=0.0.0.0", "--p2p-bind-port=18080", "--rpc-bind-ip=0.0.0.0", "--rpc-bind-port=18081", "--non-interactive", "--confirm-external-bind"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
