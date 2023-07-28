FROM ubuntu
EXPOSE 8080/tcp
#  add ppa for newer compiler
RUN apt-get update ; apt-get install --no-install-recommends software-properties-common=0.99.35 python-software-properties -y ; add-apt-repository -y ppa:ubuntu-toolchain-r/test ; apt-get update -o Dir::Etc::sourcelist="sources.list.d/ubuntu-toolchain-r-test-$( lsb_release -c -s ;).list" -o Dir::Etc::sourceparts="-" -o APT::Get::List-Cleanup="0"
#  grab all of the dependencies
RUN apt-get install --no-install-recommends autoconf=2.71-3 automake=1:1.16.5-1.3 libtool=2.4.7-5 make=4.3-4.1build1 gcc-4.9 g++-4.9 libboost1.54-dev libboost-program-options1.54-dev libboost-filesystem1.54-dev libboost-system1.54-dev libboost-thread1.54-dev protobuf-compiler=3.21.12-1ubuntu7 libprotobuf-dev=3.21.12-1ubuntu7 lua5.2=5.2.4-3 liblua5.2-dev=5.2.4-3 git=1:2.39.2-1ubuntu1 firefox=1:1snap1-0ubuntu3 libsqlite3-dev=3.40.1-1 libspatialite-dev=5.0.1-3 libgeos-dev=3.11.1-1 libgeos++-dev=3.11.1-1 libcurl4-openssl-dev=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 wget=1.21.3-1ubuntu1 -y
#  use newer compiler
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.9 90 ; update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.9 90
WORKDIR /opt
RUN git clone --depth=1 --recurse-submodules --single-branch --branch=master https://github.com/zeromq/libzmq.git
WORKDIR /opt/libzmq
RUN ./autogen.sh
RUN ./configure --without-libsodium --without-documentation ; make install
WORKDIR /opt
#  grab prime_server API:
RUN git clone --depth=1 --recurse-submodules --single-branch --branch=master https://github.com/kevinkreiser/prime_server.git
WORKDIR /opt/prime_server
RUN ./autogen.sh ; ./configure ; make install
WORKDIR /opt
#  grab midgard
RUN git clone --recurse-submodules https://github.com/valhalla/midgard
WORKDIR /opt/midgard
RUN ./autogen.sh ; ./configure CPPFLAGS=-DBOOST_SPIRIT_THREADSAFE ; make ; make install
WORKDIR /opt
#  grab baldr
RUN git clone --recurse-submodules https://github.com/valhalla/baldr
WORKDIR /opt/baldr
RUN ./autogen.sh ; ./configure CPPFLAGS=-DBOOST_SPIRIT_THREADSAFE ; make ; make install
WORKDIR /opt
#  grab sif
RUN git clone --recurse-submodules https://github.com/valhalla/sif
WORKDIR /opt/sif
RUN ./autogen.sh ; ./configure CPPFLAGS=-DBOOST_SPIRIT_THREADSAFE ; make ; make install
WORKDIR /opt
#  grab skadi
RUN git clone --recurse-submodules https://github.com/valhalla/skadi
WORKDIR /opt/skadi
RUN ./autogen.sh ; ./configure CPPFLAGS=-DBOOST_SPIRIT_THREADSAFE ; make ; make install
WORKDIR /opt
#  grab mjolnir
RUN git clone --recurse-submodules https://github.com/valhalla/mjolnir
WORKDIR /opt/mjolnir
RUN ./autogen.sh ; ./configure CPPFLAGS=-DBOOST_SPIRIT_THREADSAFE ; make ; make install
WORKDIR /opt
#  grab loki
RUN git clone --recurse-submodules https://github.com/valhalla/loki
WORKDIR /opt/loki
RUN ./autogen.sh ; ./configure CPPFLAGS=-DBOOST_SPIRIT_THREADSAFE ; make ; make install
WORKDIR /opt
#  grab yaml-cpp
RUN git clone https://github.com/jbeder/yaml-cpp.git
RUN mkdir yaml-cpp/build
RUN apt-get install --no-install-recommends cmake=3.25.1-1 -y
WORKDIR /opt/yaml-cpp/build
RUN cmake ../ ; make ; make install
WORKDIR /opt
#  grab odin
RUN git clone https://github.com/valhalla/odin.git
WORKDIR /opt/odin
RUN scripts/dependencies.sh ; ./scripts/install.sh
WORKDIR /opt
#  grab thor
RUN git clone --recurse-submodules https://github.com/valhalla/thor.git
WORKDIR /opt/thor
RUN ./autogen.sh ; ./configure CPPFLAGS=-DBOOST_SPIRIT_THREADSAFE ; make ; make install
WORKDIR /opt
#  grab tyr
RUN git clone --recurse-submodules https://github.com/valhalla/tyr.git
WORKDIR /opt/tyr
RUN ./autogen.sh ; ./configure CPPFLAGS=-DBOOST_SPIRIT_THREADSAFE ; make ; make install
WORKDIR /opt
#  grab tools
RUN git clone --recurse-submodules https://github.com/valhalla/tools.git
WORKDIR /opt/tools
RUN ./autogen.sh ; ./configure CPPFLAGS=-DBOOST_SPIRIT_THREADSAFE ; make ; make install
WORKDIR /opt
COPY scripts /opt/scripts
#  Run the server
CMD chmod 777 /opt/scripts/*
WORKDIR /opt/tyr
EXPOSE 8002/tcp
EXPOSE 8080/tcp
CMD LD_LIBRARY_PATH=/usr/lib:/usr/local/lib /opt/tools/tyr_simple_service /data/valhalla/valhalla.json
#  CMD LD_LIBRARY_PATH=/usr/lib:/usr/local/lib tyr/tyr_simple_service tyr/conf/valhalla.json
#
#
#
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
