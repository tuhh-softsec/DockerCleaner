FROM ubuntu:16.04
MAINTAINER Yoshiyuki Ieyama <44uk@github.com>
WORKDIR /tmp
RUN sed -i.bak -e "s%http://[^ ]\+%http://linux.yz.yamagata-u.ac.jp/ubuntu/%g" /etc/apt/sources.list
RUN apt-get update -y \
 && apt-get upgrade -y \
 && apt-get clean \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 vim=2:7.4.1689-3ubuntu1.5 autoconf=2.69-9 automake=1:1.15-4ubuntu1 apt-file=2.5.5ubuntu1 build-essential=12.1ubuntu2 software-properties-common=0.96.20.10 pkg-config=0.29.1-0ubuntu1 python3=3.5.1-3 libc6-dev=2.23-0ubuntu11.3 libssl-dev=1.0.2g-1ubuntu4.20 libsasl2-dev=2.1.26.dfsg1-14ubuntu0.2 libtool=2.4.6-0.1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   gcc,g++ 7
RUN add-apt-repository ppa:ubuntu-toolchain-r/test \
 && apt-get update \
 && apt-get install --no-install-recommends gcc-7 g++-7 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && rm /usr/bin/gcc /usr/bin/g++ \
 && ln -s /usr/bin/gcc-7 /usr/bin/gcc \
 && ln -s /usr/bin/g++-7 /usr/bin/g++
#   cmake
RUN git clone https://gitlab.kitware.com/cmake/cmake.git -b v3.11.1 --depth 1 \
 && cd cmake \
 && ./bootstrap --prefix=/usr/local \
 && make -j4 \
 && make install \
 && cd -
#   boost
RUN wget https://dl.bintray.com/boostorg/release/1.65.1/source/boost_1_65_1.tar.gz \
 && tar xzf boost_1_65_1.tar.gz \
 && cd boost_1_65_1 \
 && ./bootstrap.sh \
 && ./b2 toolset=gcc install --prefix=/usr/local -j4 \
 && cd -
#   gtest
RUN git clone https://github.com/google/googletest.git -b release-1.8.0 --depth 1 \
 && mkdir -p googletest/_build \
 && cd googletest/_build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr/local .. \
 && make -j4 \
 && make install \
 && cd -
#   rocksdb
RUN git clone https://github.com/facebook/rocksdb.git -b v5.12.4 --depth 1 \
 && mkdir -p rocksdb/_build \
 && cd rocksdb/_build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr/local .. \
 && make -j4 \
 && make install \
 && cd -
#   zmqlib
RUN git clone https://github.com/zeromq/libzmq.git -b v4.2.3 --depth 1 \
 && mkdir -p libzmq/_build \
 && cd libzmq/_build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr/local .. \
 && make -j4 \
 && make install \
 && cd -
#   cppzmq
RUN git clone https://github.com/zeromq/cppzmq.git -b v4.2.3 --depth 1 \
 && mkdir -p cppzmq/_build \
 && cd cppzmq/_build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr/local .. \
 && make -j4 \
 && make install \
 && cd -
#   mongo-c
RUN git clone https://github.com/mongodb/mongo-c-driver.git -b 1.4.3 --depth 1 \
 && cd mongo-c-driver \
 && ./autogen.sh \
 && ./configure --disable-automatic-init-and-cleanup --prefix=/usr/local \
 && make -j4 \
 && make install \
 && cd -
#  RUN wget https://github.com/mongodb/mongo-c-driver/releases/download/1.4.2/mongo-c-driver-1.4.2.tar.gz \
#    && tar xzf mongo-c-driver-1.4.2.tar.gz && cd mongo-c-driver-1.4.2 \
#    && ./configure --disable-automatic-init-and-cleanup --prefix=/usr/local \
#    && make -j4 && make install \
#    && cd -
#   mongo-cxx
RUN git clone https://github.com/mongodb/mongo-cxx-driver.git -b r3.0.2 --depth 1 \
 && cd mongo-cxx-driver/build \
 && cmake -DBSONCXX_POLY_USE_BOOST=1 -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local .. \
 && make -j4 \
 && make install \
 && cd -
#   catapult
RUN git clone https://github.com/nemtech/catapult-server.git -b master --depth 1 \
 && cd catapult-server \
 && mkdir _build \
 && cd _build \
 && cmake -DCMAKE_BUILD_TYPE=RelWithDebugInfo -DCMAKE_CXX_FLAGS="-pthread" -DPYTHON_EXECUTABLE=/usr/bin/python3 -DBSONCXX_LIB=/usr/local/lib/libbsoncxx.so -DMONGOCXX_LIB=/usr/local/lib/libmongocxx.so .. \
 && make publish \
 && make -j4
#   ------------------------------------
#   ここから先は作業用に都合よくやっている
#   行儀が良いとは思ってないよ
COPY patch/config-user.patch .
RUN cd catapult-server \
 && patch -p1 < /tmp/config-user.patch \
 && mkdir -p seed/mijin-test \
 && mkdir data \
 && cd _build \
 && mv resources resources.bk \
 && cp -r ../resources .
#   generate nemesis block
#   && ./bin/catapult.tools.nemgen ../tools/nemgen/resources/mijin-test.properties \
#   && cp -r ../seed/mijin-test/00000 ../data/
WORKDIR catapult-server/_build
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
