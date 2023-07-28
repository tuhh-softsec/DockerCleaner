#   The base OS
FROM ubuntu:14.04
MAINTAINER Bo Liu <bol@pinterest.com>
LABEL version="0.1"
ARG BUILD_JOBS=10
ENV BUILD_JOBS="${BUILD_JOBS}"
RUN apt-get update -q -y \
 && (apt-get update ;apt-get install --no-install-recommends python-software-properties=0.92.37.8 wget=1.15-1ubuntu1.14.04.5 git=1:1.9.1-1ubuntu0.10 software-properties-common=0.92.37.8 -q -y )
#   For gcc 6
RUN add-apt-repository -y ppa:ubuntu-toolchain-r/test
#   For open jdk 8
RUN add-apt-repository ppa:openjdk-r/ppa \
 && :
#   For HDFS debian libs
RUN wget 'https://archive.cloudera.com/cdh5/ubuntu/trusty/amd64/cdh/cloudera.list' -O /etc/apt/sources.list.d/cloudera.list \
 && wget https://archive.cloudera.com/cdh5/ubuntu/trusty/amd64/cdh/archive.key -O archive.key \
 && apt-key add archive.key apt-get update apt-get install --force-yes -q -y hadoop-hdfs
RUN apt-get update -q -y \
 && (apt-get update ;apt-get install --no-install-recommends automake=1:1.14.1-2ubuntu1 autoconf=2.69-6 autoconf-archive=20131101-1 binutils-dev=2.24-5ubuntu14.2 bison=2:3.0.2.dfsg-2 curl=7.35.0-1ubuntu2.20 flex=2.5.35-10.1ubuntu2 gcc-6 g++-6 gdb=7.7.1-0ubuntu5~14.04.3 ghostscript=9.26~dfsg+0-0ubuntu0.14.04.8 git=1:1.9.1-1ubuntu0.10 google-perftools=2.1-2ubuntu1.1 graphviz=2.36.0-0ubuntu3.2 hadoop hadoop-hdfs hadoop-client libapr1-dev=1.5.0-1 libboost-all-dev=1.54.0.1ubuntu1 libbz2-dev=1.0.6-5 libcap-dev=1:2.24-0ubuntu2 libcppunit-dev=1.13.1-2ubuntu1 libcurl4-openssl-dev=7.35.0-1ubuntu2.20 libdouble-conversion-dev=2.0.1-1 libdwarf-dev=20120410-2+deb7u2build0.14.04.1 libevent-dev=2.0.21-stable-1ubuntu1.14.04.2 libfftw3-dev=3.3.3-7ubuntu3 libgflags-dev=2.0-1.1ubuntu1 libgtest-dev=1.6.0-1ubuntu6 libhdf5-serial-dev=1.8.11-5ubuntu7.1 libhdfs0 libhdfs0-dev libiberty-dev=20131116-1ubuntu0.2 libkrb5-dev=1.12+dfsg-2ubuntu5.4 libleveldb-dev=1.15.0-2 liblua5.2-dev=5.2.3-1 liblzma-dev=5.1.1alpha+20120614-2ubuntu2 libnuma-dev=2.0.9~rc5-1ubuntu3.14.04.2 libpcap-dev=1.5.3-2 libsasl2-dev=2.1.25.dfsg1-17build1 libsnappy-dev=1.1.0-1ubuntu1 libssl-dev=1.0.1f-1ubuntu2.27 libsvn-dev=1.8.8-1ubuntu3.3 libtool=2.4.2-1.7ubuntu1 linux-tools-generic=3.13.0.170.181 make=3.81-8.2ubuntu3 openjdk-8-jdk python-setuptools=3.3-1ubuntu2 python-pip=1.5.4-1ubuntu4 scons=2.3.0-2ubuntu1 sparsehash=1.10-1ubuntu1 subversion=1.8.8-1ubuntu3.3 unzip=6.0-9ubuntu1.5 uuid-dev=2.20.1-5.1ubuntu20.9 vim=2:7.4.052-1ubuntu3.1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 --force-yes -q -y )
#   Downgrade zookeeper to cdh5's version. trusty's default zookeeper version is 3.4.5+dfsg-1
#   which is higher than cdh5's version. We need to downgrade to cdh5's version, otherwise hadoop
#   package is broken.
RUN (apt-get update ;apt-get install --no-install-recommends zookeeper=3.4.5+cdh5* --force-yes -q -y )
#   Install awscli
RUN cd /opt \
 && wget https://s3.amazonaws.com/aws-cli/awscli-bundle.zip \
 && unzip awscli-bundle.zip \
 && ./awscli-bundle/install -i /usr/local/aws -b /usr/local/bin/aws \
 && rm awscli-bundle.zip \
 && rm -rf awscli-bundle
#   Change the gcc/g++ aliases to point to 6
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-6 60 --slave /usr/bin/g++ g++ /usr/bin/g++-6
#   Set fake git credentials, otherwise git cherry-pick would throw fatal error.
RUN git config --global user.email "you@example.com" \
 && git config --global user.name "Your Name"
#   LZ4
RUN cd /opt \
 && git clone https://github.com/lz4/lz4.git \
 && (cd lz4 \
 && git reset --hard c10863b98e1503af90616ae99725ecd120265dfb \
 && make \
 && make install \
 && ldconfig ) \
 && rm -rf lz4
#   glog
RUN cd /opt \
 && wget https://github.com/google/glog/archive/v0.3.3.zip \
 && unzip v0.3.3.zip \
 && (cd /opt/glog-0.3.3/ \
 && CPPFLAGS="-gdwarf-2 -O3 -fno-omit-frame-pointer" ./configure \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf glog-0.3.3.tar.gz glog-0.3.3
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libunwind8-dev=1.1-2.2ubuntu3 --force-yes -q -y )
#   Adding Java lib path ld.so searching path configuration
RUN JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64 \
 && echo $JAVA_HOME/jre/lib/amd64/ >> /etc/ld.so.conf.d/realpin.conf \
 && echo $JAVA_HOME/jre/lib/amd64/server >> /etc/ld.so.conf.d/realpin.conf \
 && echo $JAVA_HOME/jre/lib/amd64/jamvm >> /etc/ld.so.conf.d/realpin.conf
#   numa
RUN cd /opt \
 && wget https://github.com/numactl/numactl/archive/v2.0.11.zip \
 && unzip v2.0.11.zip \
 && (cd numactl-2.0.11 \
 && ./autogen.sh \
 && ./configure \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf v2.0.11.zip numactl-2.0.11/
#   newer version of cmake
RUN cd /opt \
 && wget https://cmake.org/files/v3.4/cmake-3.4.3-Linux-x86_64.tar.gz --no-check-certificate \
 && tar zxvf cmake-3.4.3-Linux-x86_64.tar.gz --strip-components=1 -C /usr/local \
 && rm cmake-3.4.3-Linux-x86_64.tar.gz
#   jemalloc
RUN cd /opt \
 && wget https://github.com/jemalloc/jemalloc/archive/4.5.0.tar.gz \
 && tar -zxvf 4.5.0.tar.gz \
 && (cd jemalloc-4.5.0 \
 && autoconf \
 && ./configure --enable-prof \
 && make \
 && (make install ;exit 0 ) \
 && ldconfig ) \
 && rm -rf 4.5.0.tar.gz jemalloc-4.5.0
#   microhttpd
RUN cd /opt \
 && wget http://ftp.gnu.org/gnu/libmicrohttpd/libmicrohttpd-0.9.42.tar.gz \
 && tar -zxvf libmicrohttpd-0.9.42.tar.gz \
 && (cd libmicrohttpd-0.9.42 \
 && CPPFLAGS="-gdwarf-2 -O3 -fno-omit-frame-pointer" ./configure \
 && make -j ${BUILD_JOBS} \
 && make install ) \
 && rm -rf libmicrohttpd-0.9.42.tar.gz libmicrohttpd-0.9.42
#   download zstd
RUN cd /opt \
 && git clone https://github.com/facebook/zstd.git \
 && (cd zstd \
 && git reset --hard f405b8acbe8be70aa05e0a7bf035fe1efe20b99f ) \
 && (cd zstd/build/cmake \
 && cmake . \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf zstd
#   download folly
RUN cd /opt \
 && git clone https://github.com/facebook/folly \
 && (cd folly \
 && git reset --hard b59ee6802a2454e854a52535d31598aa967e33c0 \
 && cd folly \
 && autoreconf -ivf \
 && ./configure LDFLAGS='-ljemalloc' CC=/usr/bin/gcc-6 CXX=/usr/bin/g++-6 CXXFLAGS='-gdwarf-2 -O3 -fno-omit-frame-pointer' \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf folly
#   wangle
RUN cd /opt \
 && git clone https://github.com/facebook/wangle \
 && (cd wangle \
 && git reset --hard 52f08ff480931fcba1b7fa9b3eebd45d220a68de ) \
 && (cd wangle/wangle \
 && CC=/usr/bin/gcc-6 CXX=/usr/bin/g++-6 cmake . \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf wangle
#   git clone https://github.com/no1msd/mstch
RUN cd /opt \
 && git clone https://github.com/no1msd/mstch \
 && (cd mstch \
 && git reset --hard 0fde1cf94c26ede7fa267f4b64c0efe5da81a77a ) \
 && (cd mstch \
 && cmake . \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf mstch
#   download fbthrift
RUN cd /opt \
 && git clone https://github.com/facebook/fbthrift \
 && (cd fbthrift \
 && git reset --hard 8e1a1e1eedbf5b551b4fe4799dab8b36267638ba \
 && cd thrift \
 && sed 's/PKG_CHECK_MODULES.*$/true/g' -i configure.ac \
 && (cd compiler \
 && ln -sf thrifty.h thrifty.hh ) \
 && autoreconf --install \
 && LDFLAGS="-ljemalloc" CC=/usr/bin/gcc-6 CXX=/usr/bin/g++-6 CPPFLAGS="-gdwarf-2 -O3 -fno-omit-frame-pointer" ./configure \
 && (cd compiler \
 && make ) \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf fbthrift
#   rocksdb
RUN cd /opt/ \
 && git clone https://github.com/facebook/rocksdb.git \
 && (cd rocksdb \
 && git checkout -b 5.7.fb origin/5.7.fb \
 && git reset --hard cfaeb5846bec0ac90d8da15dc11f53eafbbfd537 \
 && git cherry-pick c5f0c6cc660f1f4a8051db2aac3b8afc17818e70 \
 && git cherry-pick ba3c58cab6c691c53c7f98589651233695da1f62 \
 && git cherry-pick 204af1ecccb6ed8110ee04cf9130594cfcb3af27 \
 && sed -i -- 's/std::rindex/rindex/g' ./env/env_hdfs.cc \
 && sed -i -- '/"util\/string_util.h"/a #include "util\/logging.h"' ./env/env_hdfs.cc \
 && export CLASSPATH= \
 && for f in `find /usr/lib/hadoop-hdfs | grep jar `; do export CLASSPATH=$CLASSPATH:$f ; done \
 && for f in `find /usr/lib/hadoop | grep jar `; do export CLASSPATH=$CLASSPATH:$f ; done \
 && for f in `find /usr/lib/hadoop/client | grep jar `; do export CLASSPATH=$CLASSPATH:$f ; done \
 && USE_SSE=1 USE_HDFS=1 JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64 LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/amd64/server:$JAVA_HOME/jre/lib/amd64:/usr/lib/hadoop/lib/native JEMALLOC_LIB=" /usr/local/lib/libjemalloc.a" JEMALLOC_INCLUDE=" -I /usr/local/include/" EXTRA_CXXFLAGS="-gdwarf-2 -std=c++1y -O3 -fno-omit-frame-pointer" make -j ${BUILD_JOBS} shared_lib \
 && make install-shared \
 && ldconfig ) \
 && rm -rf rocksdb
#   farmhash
RUN cd /opt \
 && git clone https://github.com/google/farmhash.git \
 && (cd farmhash \
 && git reset --hard 059cf991 \
 && autoreconf --install \
 && ./configure CXXFLAGS="-gdwarf-2 -O3 -fno-omit-frame-pointer" \
 && make all check \
 && make install \
 && ldconfig ) \
 && rm -rf farmhash
#   libprotobuf
RUN cd /opt \
 && git clone https://github.com/google/protobuf.git \
 && (cd protobuf \
 && git reset --hard b04e5cba356212e4e8c66c61bbe0c3a20537c5b9 \
 && ./autogen.sh \
 && LDFLAGS="-ljemalloc" CPPFLAGS="-gdwarf-2 -O3 -fno-omit-frame-pointer" ./configure \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf protobuf
#   libbf (bloom filter)
#   remove this once we migrate to use third_party/libbf
RUN cd /opt \
 && git clone https://github.com/mavam/libbf.git \
 && (cd libbf \
 && git reset --hard f2509db1442e8fc7c3dd5117b739886f76a4eb80 \
 && ./configure \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf libbf
#   aws sdk
RUN cd /opt \
 && git clone https://github.com/aws/aws-sdk-cpp.git \
 && (cd aws-sdk-cpp \
 && git checkout 1.2.7 \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCUSTOM_MEMORY_MANAGEMENT=0 -DSTATIC_LINKING=1 -DBUILD_ONLY="s3" .. \
 && make -j ${BUILD_JOBS} \
 && make install \
 && rm -rf /usr/local/lib/cmake/aws-cpp-* \
 && rm -rf build \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCUSTOM_MEMORY_MANAGEMENT=0 -DSTATIC_LINKING=0 -DBUILD_ONLY="s3" .. \
 && cd .. \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf aws-sdk-cpp
#   yaml-cpp
RUN cd /opt \
 && git clone https://github.com/jbeder/yaml-cpp \
 && (cd yaml-cpp \
 && git reset --hard 562aefc114938e388457e6a531ed7b54d9dc1b62 \
 && mkdir build \
 && cd build \
 && cmake -DBUILD_SHARED_LIBS=ON .. \
 && make -j \
 && make install \
 && cmake -DBUILD_SHARED_LIBS=OFF .. \
 && make -j ${BUILD_JOBS} \
 && make install \
 && ldconfig ) \
 && rm -rf /opt/yaml-cpp
#   kafka
RUN cd /opt \
 && git clone https://github.com/edenhill/librdkafka.git \
 && (cd librdkafka \
 && git reset --hard v0.11.4 \
 && ./configure \
 && make \
 && make install \
 && ldconfig ) \
 && rm -rf librdkafka
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
