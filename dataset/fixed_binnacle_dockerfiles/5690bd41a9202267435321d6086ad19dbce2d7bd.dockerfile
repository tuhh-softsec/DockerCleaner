#
#   Dockerfile_p4
#   Builds docker container to build/run Juniper P4 Agent
#
#   Created by Sandesh Kumar Sodhi, December 2017
#   Copyright (c) [2017] Juniper Networks, Inc. All rights reserved.
#
#   All rights reserved.
#
#   Notice and Disclaimer: This code is licensed to you under the Apache
#   License 2.0 (the "License"). You may not use this code except in compliance
#   with the License. This code is not an official Juniper product. You can
#   obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
#
#   Third-Party Code: This code may depend on other components under separate
#   copyright notice and license terms. Your use of the source code for those
#   components is subject to the terms and conditions of the respective license
#   as noted in the Third-Party source code file.
#
FROM ubuntu:16.04
LABEL maintainer="\"Sandesh Kumar Sodhi\""
#
#   Build
#
#   [sudo] docker build -f Dockerfile_p4 -t juniper-p4 .
#
#   Run:
#
#   [sudo] docker run --name jnprp4 --privileged -i -t juniper-p4 /bin/bash
#
#
#
ARG DEBIAN_FRONTEND=noninteractive
#   
#   Note:
#   While adding new package to the list, maintain the alphanumeric sorted order.
#
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-9 automake=1:1.15-4ubuntu1 bsdmainutils=9.0.6ubuntu3 build-essential=12.1ubuntu2 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 ethtool=1:4.5-1 expect=5.45-7 g++=4:5.3.1-1ubuntu1 gdb=7.11.1-0ubuntu1~16.5 git=1:2.7.4-0ubuntu1.10 gdebi-core=0.9.5.7ubuntu1 iputils-ping=3:20121221-5ubuntu2 less=481-2.1ubuntu0.2 libboost-all-dev=1.58.0.1ubuntu1 libevent-pthreads-2.0-5=2.0.21-stable-2ubuntu0.16.04.1 libjsoncpp-dev=1.7.2-1 libnet1-dev=1.1.6+dfsg-3 libpcap-dev=1.7.4-2ubuntu0.1 libssl-dev=1.0.2g-1ubuntu4.20 libtool=2.4.6-0.1 libxml2-utils=2.9.3+dfsg1-1ubuntu0.7 libyaml-cpp-dev=0.5.2-4ubuntu1~16.04.4 nano=2.5.3-2ubuntu2 net-tools=1.60-26ubuntu1 nmap=7.01-2ubuntu2 openssh-client=1:7.2p2-4ubuntu2.10 openssl=1.0.2g-1ubuntu4.20 pkg-config=0.29.1-0ubuntu1 psmisc=22.21-2.1ubuntu0.1 python-lxml=3.5.0-1ubuntu0.4 realpath=8.25-2ubuntu3~16.04 sed=4.2.2-7 software-properties-common=0.96.20.10 ssh=1:7.2p2-4ubuntu2.10 sshpass=1.05-1 tcpdump=4.9.3-0ubuntu0.16.04.1 telnet=0.17-40 tmux=2.1-3build1 tdom=0.8.3-1 tree=1.7.0-3 unzip=6.0-20ubuntu1.1 vim=2:7.4.1689-3ubuntu1.5 wget=1.17.1-1ubuntu1.5 -y
#   Clang related packages
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN apt-add-repository -y "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main"
RUN apt-get update \
 && apt-get install --no-install-recommends clang-6.0=1:6.0-1ubuntu2~16.04.1 clang-format-6.0=1:6.0-1ubuntu2~16.04.1 clang-tidy-6.0=1:6.0-1ubuntu2~16.04.1 -y
RUN update-alternatives --install /usr/bin/clang clang /usr/bin/clang-6.0 10
RUN update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-6.0 10
RUN update-alternatives --install /usr/bin/clang-format clang-format /usr/bin/clang-format-6.0 10
RUN update-alternatives --install /usr/bin/clang-tidy clang-tidy /usr/bin/clang-tidy-6.0 10
#
#   Packages needed for p4c
#
RUN apt-get update \
 && apt-get install --no-install-recommends bison=2:3.0.4.dfsg-1 cmake=3.5.1-1ubuntu3 flex=2.6.0-11 libgc-dev=1:7.4.2-7.3ubuntu0.1 libgmp-dev=2:6.1.0+dfsg-2 libfl-dev=2.6.0-11 -y
RUN apt-get update \
 && apt-get install --no-install-recommends python=2.7.12-1~16.04 pylint=1.5.2-1ubuntu1 python-pip=8.1.1-2ubuntu0.6 python3-pip=8.1.1-2ubuntu0.6 python-scapy=2.2.0-1 -y
RUN pip install pip==23.1 --upgrade
RUN pip install setuptools==67.6.1 -U
RUN pip install scapy==2.5.0
RUN pip install ipaddr==2.2.0
RUN pip install gcovr==6.0
RUN pip install lxml==4.9.2
RUN pip3 install grpcio-tools
RUN pip3 install prompt-toolkit
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 -y
RUN mkdir -p /root/downloads
RUN cd /root/downloads \
 && git clone -b $( curl -L https://grpc.io/release ;) https://github.com/grpc/grpc
RUN cd /root/downloads/grpc \
 && git fetch \
 && git checkout v1.8.1
RUN cd /root/downloads/grpc \
 && git submodule update --init
RUN cd /root/downloads/grpc \
 && make
RUN cd /root/downloads/grpc \
 && make install
RUN cd /root/downloads/grpc/third_party/protobuf/ \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install
#
#   GTEST
#
RUN cd /root/downloads \
 && wget https://github.com/google/googletest/archive/release-1.8.0.tar.gz
RUN cd /root/downloads \
 && tar xzf release-1.8.0.tar.gz
#
#   pyang
#
RUN cd /root/downloads \
 && git clone https://github.com/mbj4668/pyang.git
RUN cd /root/downloads/pyang \
 && python setup.py install
#
#   GO
#
RUN cd /root/downloads \
 && wget https://redirector.gvt1.com/edgedl/go/go1.9.2.linux-amd64.tar.gz
RUN cd /root/downloads \
 && tar -C /usr/local -xzf go1.9.2.linux-amd64.tar.gz
RUN mkdir -p /root/go
ENV PATH="$PATH:/usr/local/go/bin"
ENV GOPATH="/root/go"
#
#   ygot
#
RUN cd /root/go \
 && go get github.com/golang/protobuf/proto
RUN cd /root/go \
 && go get github.com/openconfig/gnmi ; exit 0
RUN cd /root/go \
 && go get github.com/openconfig/goyang
RUN cd /root/go \
 && go get google.golang.org/grpc
RUN cd /root/go \
 && go get github.com/golang/glog
RUN cd /root/go \
 && go get github.com/kylelemons/godebug ; exit 0
RUN cd /root/go/src \
 && go get github.com/openconfig/ygot ; exit 0
RUN cd /root/go/src/github.com/openconfig/ygot/proto_generator \
 && go build
#
#   Opentracing
#
RUN cd /root/downloads \
 && git clone https://github.com/opentracing/opentracing-cpp
RUN cd /root/downloads/opentracing-cpp \
 && mkdir .build
RUN cd /root/downloads/opentracing-cpp/.build \
 && cmake ..
RUN cd /root/downloads/opentracing-cpp/.build \
 && make
RUN cd /root/downloads/opentracing-cpp/.build \
 && make install
#
#   Jaeger
#
#   Install thrift first. Note Jaeger works only with Thrift 0.9.3
#   https://github.com/jaegertracing/jaeger-client-cpp/issues/45
#
RUN cd /root/downloads \
 && git clone https://github.com/apache/thrift
RUN cd /root/downloads/thrift \
 && git checkout 0.9.3
RUN cd /root/downloads/thrift \
 && ./bootstrap.sh
RUN cd /root/downloads/thrift \
 && ./configure --with-cpp --with-java=no --with-python=no --with-lua=no --with-perl=no --enable-shared=yes --enable-static=no --enable-tutorial=no --with-qt4=no
RUN cd /root/downloads/thrift \
 && make -s
RUN cd /root/downloads/thrift \
 && make install
#   Now install cpp client for Jaeger
RUN cd /root/downloads \
 && git clone https://github.com/jaegertracing/cpp-client
RUN cd /root/downloads/cpp-client \
 && git checkout v0.3.0
RUN cd /root/downloads/cpp-client \
 && mkdir .build
RUN cd /root/downloads/cpp-client/.build \
 && cmake ..
RUN cd /root/downloads/cpp-client/.build \
 && make
RUN cd /root/downloads/cpp-client/.build \
 && make install
COPY env/bash_aliases /root/.bash_aliases
COPY env/vimrc /root/.vimrc
COPY env/tmux.conf /root/.tmux.conf
COPY entrypoint.sh /root/entrypoint.sh
ENTRYPOINT ["/root/entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
