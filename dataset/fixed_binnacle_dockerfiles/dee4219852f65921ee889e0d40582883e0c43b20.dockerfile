FROM ubuntu:16.04
MAINTAINER Antonin Bas <antonin@barefootnetworks.com>
ARG MAKEFLAGS
ENV MAKEFLAGS="${MAKEFLAGS:--j2}"
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 ca-certificates=20210119~16.04.1 -y
#   Build Protocol Buffers.
ENV PROTOCOL_BUFFERS_DEPS="autoconf  automake  g++  libtool  make  curl  unzip"
RUN git clone https://github.com/google/protobuf.git
WORKDIR /protobuf/
RUN git checkout tags/v3.2.0
RUN apt-get update \
 && apt-get install --no-install-recommends $PROTOCOL_BUFFERS_DEPS -y \
 && export CFLAGS="-Os" \
 && export CXXFLAGS="-Os" \
 && export LDFLAGS="-Wl,-s" \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install \
 && ldconfig \
 && apt-get purge -y $PROTOCOL_BUFFERS_DEPS \
 && apt-get autoremove --purge -y \
 && rm -rf /protobuf /var/cache/apt/* /var/lib/apt/lists/* /var/cache/debconf/* /var/lib/dpkg/*-old /var/log/*
WORKDIR /
#   Build gRPC.
#   The gRPC build system should detect that a version of protobuf is already
#   installed and should not try to install the third-party one included as a
#   submodule in the grpc repository.
ENV GRPC_DEPS="build-essential  autoconf  libtool"
RUN git clone https://github.com/google/grpc.git
WORKDIR /grpc/
RUN git checkout tags/v1.3.2 \
 && git submodule update --init --recursive
RUN apt-get update \
 && apt-get install --no-install-recommends libgflags-dev=2.1.2-3 $GRPC_DEPS -y \
 && export LDFLAGS="-Wl,-s" \
 && make \
 && make grpc_cli \
 && make install \
 && ldconfig \
 && cp /grpc/bins/opt/grpc_cli /usr/bin/ \
 && apt-get purge -y $GRPC_DEPS \
 && apt-get autoremove --purge -y \
 && rm -rf /grpc /var/cache/apt/* /var/lib/apt/lists/* /var/cache/debconf/* /var/lib/dpkg/*-old /var/log/*
WORKDIR /
ENV BM_DEPS="automake  build-essential  libjudy-dev  libgmp-dev  libpcap-dev  libboost-dev  libboost-program-options-dev  libboost-system-dev  libboost-filesystem-dev  libboost-thread-dev  libtool  pkg-config"
RUN apt-get update \
 && apt-get install --no-install-recommends $BM_DEPS -y
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document .PI.head.json https://api.github.com/repos/p4lang/PI/git/refs/heads/master
RUN git clone https://github.com/p4lang/PI/
WORKDIR /PI
RUN git submodule update --init --recursive
RUN export CFLAGS="-O0" \
 && export CXXFLAGS="-O0" \
 && ./autogen.sh \
 && ./configure --with-proto --without-cli --without-internal-rpc \
 && make \
 && make install \
 && ldconfig \
 && rm -rf /PI/
WORKDIR /
COPY . /behavioral-model/
WORKDIR /behavioral-model/
RUN export CFLAGS="-O0" \
 && export CXXFLAGS="-O0" \
 && ./autogen.sh \
 && ./configure --without-thrift --without-nanomsg --with-pi \
 && make \
 && make install \
 && ldconfig
WORKDIR /behavioral-model/targets/simple_switch_grpc
RUN export CFLAGS="-O0" \
 && export CXXFLAGS="-O0" \
 && ./autogen.sh \
 && ./configure \
 && make
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
