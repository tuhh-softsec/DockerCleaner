FROM ubuntu:xenial
MAINTAINER The Crossbar.io Project <support@crossbario.com>
#   Metadata
ARG BUILD_DATE
ARG AUTOBAHN_CPP_VERSION
ARG AUTOBAHN_CPP_VCS_REF
#   Metadata labeling
LABEL org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.name="AutobahnCpp Starter Template" \
      org.label-schema.description="Quickstart template for application development with AutobahnCpp" \
      org.label-schema.url="http://crossbar.io" \
      org.label-schema.vcs-ref="$AUTOBAHN_CPP_VCS_REF" \
      org.label-schema.vcs-url="https://github.com/crossbario/autobahn-cpp" \
      org.label-schema.vendor="The Crossbar.io Project" \
      org.label-schema.version="$AUTOBAHN_CPP_VERSION" \
      org.label-schema.schema-version="1.0"
#   Crossbar.io connection defaults
ENV CBURL="ws://localhost:8080/ws"
ENV CBREALM="realm1"
#   user env
ENV DEBIAN_FRONTEND="noninteractive"
ENV HOME="/autobahn"
ENV PATH="/autobahn:$PATH"
ENV LD_LIBRARY_PATH="/usr/local/lib"
#   env vars to configure websocketpp
ENV WSPP_ENABLE_CPP11="1"
#   update system, get dev tools and libs
RUN apt-get update \
 && apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 unzip=6.0-20ubuntu1.1 git-core=1:2.7.4-0ubuntu1.10 clang=1:3.8-33ubuntu3.1 libc++1=3.7.0-1ubuntu0.1 libc++-dev=3.7.0-1ubuntu0.1 libc++abi-dev=3.7.0-1ubuntu0.1 build-essential=12.1ubuntu2 autotools-dev=20150820.1 autoconf=2.69-9 libtool=2.4.6-0.1 cmake=3.5.1-1ubuntu3 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libbz2-dev=1.0.6-8ubuntu0.2 libssl-dev=1.0.2g-1ubuntu4.20 libboost-all-dev=1.58.0.1ubuntu1 -y \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean
#   use clang, not gcc
ENV CC="/usr/bin/clang"
ENV CXX="/usr/bin/clang++"
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/clang 100 \
 && update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++ 100 \
 && update-alternatives --query c++ \
 && update-alternatives --query cc
#   get, build and install Boost from upstream
#   RUN    cd /tmp \
#       && wget https://dl.bintray.com/boostorg/release/1.69.0/source/boost_1_69_0.tar.bz2 \
#       && tar xvjf boost_1_69_0.tar.bz2
#   RUN    cd /tmp/boost_1_69_0 \
#       && ./bootstrap.sh --with-toolset=clang \
#       && ./b2 toolset=clang cxxflags="-stdlib=libc++" linkflags="-stdlib=libc++" link=shared link=static threading=single threading=multi --layout=tagged --without-python -j 8 install \
#       && cd / \
#       && rm -rf /tmp/boost*
#   https://askubuntu.com/a/486184
#   RUN cd /usr/local/lib && \
#       ln -s libboost_thread-mt.a libboost_thread.a
#   get, build and install msgpack-c from upstream
RUN cd /tmp \
 && wget https://github.com/msgpack/msgpack-c/archive/cpp-1.4.2.zip -O msgpack-c.zip \
 && unzip msgpack-c.zip \
 && cd msgpack-c-cpp-1.4.2 \
 && export CXXFLAGS="$CXXFLAGS -std=c++11" \
 && ./bootstrap \
 && ./configure \
 && make install \
 && cd / \
 && rm -rf /tmp/msgpack*
#   get and install websocketpp from upstream
RUN cd /tmp \
 && wget https://github.com/zaphoyd/websocketpp/archive/master.zip -O websocketpp.zip \
 && unzip websocketpp.zip \
 && cp -r /tmp/websocketpp-master/websocketpp/ /usr/local/include/ \
 && cd / \
 && rm -rf /tmp/websocketpp*
#   get and install cmake from upstream
#   RUN cd /tmp \
#       && wget https://cmake.org/files/v3.11/cmake-3.11.0-Linux-x86_64.sh \
#       && sh cmake-3.11.0-Linux-x86_64.sh --skip-license --prefix=/usr/local \
#       && which cmake && cmake --version
#  ### all dependencies and tools are now in place
#   setup and build example project
RUN mkdir -p /autobahn/build
WORKDIR /autobahn
COPY autobahn /autobahn/autobahn
COPY examples /autobahn/examples
COPY cmake /autobahn/cmake
COPY cmake/Modules /autobahn/cmake/Modules
COPY cmake/Includes /autobahn/cmake/Includes
COPY CMakeLists.txt /autobahn/CMakeLists.txt
RUN cd build \
 && cmake .. \
 && make -j4 \
 && find examples/ -executable -type f -exec file {}
#   drop into shell by default
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
