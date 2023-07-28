FROM ubuntu:16.04
RUN apt-get update \
 && apt-get dist-upgrade -y \
 && apt-get install --no-install-recommends make=4.1-6 git=1:2.7.4-0ubuntu1.10 cmake=3.5.1-1ubuntu3 wget=1.17.1-1ubuntu1.5 build-essential=12.1ubuntu2 automake=1:1.15-4ubuntu1 libtool=2.4.6-0.1 bison=2:3.0.4.dfsg-1 flex=2.6.0-11 -y
RUN wget https://github.com/open-source-parsers/jsoncpp/archive/1.8.4.tar.gz \
 && tar -xzvf 1.8.4.tar.gz
WORKDIR jsoncpp-1.8.4
RUN cmake . -DJSONCPP_WITH_TESTS=NO -DBUILD_STATIC_LIBS=YES -DBUILD_SHARED_LIBS=NO -DCMAKE_BUILD_TYPE=Release
RUN make \
 && make install
WORKDIR ../
RUN wget https://www.openssl.org/source/openssl-1.1.0h.tar.gz \
 && tar -xvzf openssl-1.1.0h.tar.gz
WORKDIR openssl-1.1.0h
RUN ./Configure linux-x86_64 no-shared
RUN make \
 && make install
WORKDIR ../
RUN wget https://curl.haxx.se/download/curl-7.59.0.tar.gz \
 && tar xzvf curl-7.59.0.tar.gz
WORKDIR curl-7.59.0
RUN ./configure --disable-shared --enable-static --disable-ldap
RUN make \
 && make install
WORKDIR ../
RUN wget http://ftp.gnu.org/gnu/libmicrohttpd/libmicrohttpd-0.9.59.tar.gz \
 && tar -xvzf libmicrohttpd-0.9.59.tar.gz
WORKDIR libmicrohttpd-0.9.59
RUN ./configure --disable-shared
RUN make \
 && make install
WORKDIR ../
RUN wget https://github.com/cinemast/libjson-rpc-cpp/archive/v1.1.0.tar.gz \
 && tar -xvzf v1.1.0.tar.gz
WORKDIR libjson-rpc-cpp-1.1.0 
RUN cmake -E env CXXFLAGS="-DCURL_STATICLIB" cmake . -DBUILD_SHARED_LIBS=NO -DBUILD_STATIC_LIBS=YES -DCOMPILE_TESTS=NO -DCOMPILE_STUBGEN=NO -DCOMPILE_EXAMPLES=NO -DCMAKE_BUILD_TYPE=Release -DREDIS_CLIENT=NO -DREDIS_SERVER=NO -DWITH_COVERAGE=NO
RUN make \
 && make install
WORKDIR ../
RUN git clone https://github.com/bitcoin-core/leveldb
WORKDIR leveldb
RUN make out-static/libleveldb.a out-static/libmemenv.a
RUN cp -r out-static/*.a /usr/local/lib \
 && cp -r include/* /usr/local/include
WORKDIR ../
RUN git clone https://github.com/SFML/SFML.git
WORKDIR SFML
RUN cmake . -DBUILD_SHARED_LIBS=NO -DSFML_BUILD_DOC=NO -DSFML_BUILD_AUDIO=NO -DSFML_BUILD_GRAPHICS=NO -DSFML_BUILD_WINDOW=NO -DSFML_BUILD_EXAMPLES=NO -DCMAKE_BUILD_TYPE=Release
RUN make \
 && make install
RUN cp /usr/local/lib/libsfml-network-s.a /usr/local/lib/libsfml-network.a \
 && cp /usr/local/lib/libsfml-system-s.a /usr/local/lib/libsfml-system.a
WORKDIR ../
RUN git clone https://github.com/rweather/noise-c
WORKDIR noise-c
COPY noise-c-linux.patch .
RUN git apply noise-c-linux.patch
RUN ./autogen.sh \
 && ./configure
RUN make \
 && make install
WORKDIR ../
RUN git clone https://github.com/lhorgan/luack
WORKDIR luack
RUN git checkout 43e9e17984e4e992ac2dd0510ac15ebd22f38fdc
WORKDIR src
RUN make liblua.a SYSCFLAGS="-DLUA_USE_LINUX" \
 && cp liblua.a /usr/local/lib/liblua5.3.a \
 && mkdir /usr/include/lua5.3 \
 && cp -r *.h /usr/include/lua5.3
WORKDIR ../../
RUN git clone https://github.com/metalicjames/selene.git
RUN cp -r selene/include/* /usr/local/include
RUN git clone https://github.com/metalicjames/lua-lz4.git
WORKDIR lua-lz4
RUN make
WORKDIR ../
RUN wget https://github.com/premake/premake-core/releases/download/v5.0.0-alpha12/premake-5.0.0-alpha12-linux.tar.gz \
 && tar zxvf premake-5.0.0-alpha12-linux.tar.gz \
 && cp premake5 /usr/bin
COPY ./cryptokernel /cryptokernel
RUN cp lua-lz4/lz4.so cryptokernel
RUN git clone https://github.com/metalicjames/cschnorr.git
WORKDIR cschnorr
RUN premake5 gmake2 \
 && make config=release_static cschnorr \
 && mkdir /usr/local/include/cschnorr/ \
 && cp src/*.h /usr/local/include/cschnorr/ \
 && cp bin/Static/Release/libcschnorr.a /usr/local/lib
WORKDIR ../
WORKDIR cryptokernel
RUN premake5 gmake2 --include-dir=/usr/include/lua5.3 \
 && make config=release_static ckd
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
