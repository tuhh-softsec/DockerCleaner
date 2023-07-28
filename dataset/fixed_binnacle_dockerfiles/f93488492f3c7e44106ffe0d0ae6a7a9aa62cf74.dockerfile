#   Multistage docker build, requires docker 17.05
#   builder stage
FROM ubuntu:18.04 AS builder
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 cmake=3.10.2-1ubuntu2.18.04.2 g++=4:7.4.0-1ubuntu2.3 make=4.1-9.1ubuntu1 pkg-config=0.29.1-0ubuntu2 graphviz=2.40.1-2 doxygen=1.8.13-10 git=1:2.17.1-1ubuntu0.17 curl=7.58.0-2ubuntu3.24 libtool-bin=2.4.6-2 autoconf=2.69-11 libgtest-dev=1.8.0-6 automake=1:1.15.1-3ubuntu2 --yes
WORKDIR /usr/local
#  # Boost
ARG BOOST_VERSION=1_66_0
ARG BOOST_VERSION_DOT=1.66.0
ARG BOOST_HASH=5721818253e6a0989583192f96782c4a98eb6204965316df9f5ad75819225ca9
RUN curl -s -L -o boost_${BOOST_VERSION}.tar.bz2 https://dl.bintray.com/boostorg/release/${BOOST_VERSION_DOT}/source/boost_${BOOST_VERSION}.tar.bz2 \
 && echo "${BOOST_HASH} boost_${BOOST_VERSION}.tar.bz2" | sha256sum -c \
 && tar -xvf boost_${BOOST_VERSION}.tar.bz2 \
 && cd boost_${BOOST_VERSION} \
 && ./bootstrap.sh \
 && ./b2 --build-type=minimal link=static runtime-link=static --with-chrono --with-date_time --with-filesystem --with-program_options --with-regex --with-serialization --with-system --with-thread --with-locale threading=multi threadapi=pthread cflags="-fPIC" cxxflags="-fPIC" stage
ENV BOOST_ROOT="/usr/local/boost_${BOOST_VERSION}"
#   OpenSSL
ARG OPENSSL_VERSION=1.0.2n
ARG OPENSSL_HASH=370babb75f278c39e0c50e8c4e7493bc0f18db6867478341a832a982fd15a8fe
RUN curl -s -O https://www.openssl.org/source/openssl-${OPENSSL_VERSION}.tar.gz \
 && echo "${OPENSSL_HASH} openssl-${OPENSSL_VERSION}.tar.gz" | sha256sum -c \
 && tar -xzf openssl-${OPENSSL_VERSION}.tar.gz \
 && cd openssl-${OPENSSL_VERSION} \
 && ./Configure linux-x86_64 no-shared --static -fPIC \
 && make build_crypto build_ssl \
 && make install
ENV OPENSSL_ROOT_DIR="/usr/local/openssl-${OPENSSL_VERSION}"
#   ZMQ
ARG ZMQ_VERSION=v4.2.3
ARG ZMQ_HASH=3226b8ebddd9c6c738ba42986822c26418a49afb
RUN git clone https://github.com/zeromq/libzmq.git -b ${ZMQ_VERSION} \
 && cd libzmq \
 && test `git rev-parse HEAD ` = ${ZMQ_HASH} || exit 1 \
 && ./autogen.sh \
 && CFLAGS="-fPIC" CXXFLAGS="-fPIC" ./configure --enable-static --disable-shared \
 && make \
 && make install \
 && ldconfig
#   zmq.hpp
ARG CPPZMQ_HASH=6aa3ab686e916cb0e62df7fa7d12e0b13ae9fae6
RUN git clone https://github.com/zeromq/cppzmq.git -b ${ZMQ_VERSION} \
 && cd cppzmq \
 && test `git rev-parse HEAD ` = ${CPPZMQ_HASH} || exit 1 \
 && mv *.hpp /usr/local/include
#   Readline
ARG READLINE_VERSION=7.0
ARG READLINE_HASH=750d437185286f40a369e1e4f4764eda932b9459b5ec9a731628393dd3d32334
RUN curl -s -O https://ftp.gnu.org/gnu/readline/readline-${READLINE_VERSION}.tar.gz \
 && echo "${READLINE_HASH} readline-${READLINE_VERSION}.tar.gz" | sha256sum -c \
 && tar -xzf readline-${READLINE_VERSION}.tar.gz \
 && cd readline-${READLINE_VERSION} \
 && CFLAGS="-fPIC" CXXFLAGS="-fPIC" ./configure \
 && make \
 && make install
#   Sodium
ARG SODIUM_VERSION=1.0.16
ARG SODIUM_HASH=675149b9b8b66ff44152553fb3ebf9858128363d
RUN git clone https://github.com/jedisct1/libsodium.git -b ${SODIUM_VERSION} \
 && cd libsodium \
 && test `git rev-parse HEAD ` = ${SODIUM_HASH} || exit 1 \
 && ./autogen.sh \
 && CFLAGS="-fPIC" CXXFLAGS="-fPIC" ./configure \
 && make \
 && make check \
 && make install
WORKDIR /src
COPY . .
ARG NPROC
RUN rm -rf build \
 && if [ -z "$NPROC" ] ; then make -j$( nproc ;) debug-all ; else make -j$NPROC debug-all ; fi
#   runtime stage
FROM ubuntu:18.04
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 --yes \
 && apt-get clean \
 && rm -rf /var/lib/apt
COPY --from=builder /src/build/release/bin/* /usr/local/bin/
#   Contains the blockchain
VOLUME /root/.safex
#   Generate your wallet via accessing the container and run:
#   cd /wallet
#   safex-wallet-cli
VOLUME /wallet
EXPOSE 18080/tcp
EXPOSE 18081/tcp
ENTRYPOINT ["safexd", "--p2p-bind-ip=0.0.0.0", "--p2p-bind-port=18080", "--rpc-bind-ip=0.0.0.0", "--rpc-bind-port=18081", "--non-interactive", "--confirm-external-bind"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
