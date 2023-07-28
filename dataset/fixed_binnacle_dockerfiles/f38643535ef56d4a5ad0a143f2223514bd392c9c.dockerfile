#   Dockerfile to build aria2 Windows binary using ubuntu mingw-w64
#   cross compiler chain.
#
#   $ sudo docker build -t aria2-mingw - < Dockerfile.mingw
#
#   After successful build, windows binary is located at
#   /aria2/src/aria2c.exe.  You can copy the binary using following
#   commands:
#
#   $ id=$(sudo docker create aria2-mingw)
#   $ sudo docker cp $id:/aria2/src/aria2c.exe <dest>
#   $ sudo docker rm -v $id
FROM ubuntu:16.04
MAINTAINER Tatsuhiro Tsujikawa
#   Change HOST to x86_64-w64-mingw32 to build 64-bit binary
ENV HOST="i686-w64-mingw32"
#   It would be better to use nearest ubuntu archive mirror for faster
#   downloads.
#   RUN sed -ie 's/archive\.ubuntu/jp.archive.ubuntu/g' /etc/apt/sources.list
RUN apt-get update \
 && apt-get install --no-install-recommends make=4.1-6 binutils=2.26.1-1ubuntu1~16.04.8 autoconf=2.69-9 automake=1:1.15-4ubuntu1 autotools-dev=20150820.1 libtool=2.4.6-0.1 pkg-config=0.29.1-0ubuntu1 git=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 dpkg-dev=1.18.4ubuntu1.7 gcc-mingw-w64=5.3.1-8ubuntu3+17 autopoint=0.19.7-2ubuntu3.1 libcppunit-dev=1.13.2-2.1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libgcrypt11-dev=1.5.4-3+really1.6.5-2ubuntu0.6 lzip=1.17-1 -y
RUN curl -L -O https://gmplib.org/download/gmp/gmp-6.1.2.tar.lz \
 && curl -L -O https://github.com/libexpat/libexpat/releases/download/R_2_2_5/expat-2.2.5.tar.bz2 \
 && curl -L -O https://www.sqlite.org/2018/sqlite-autoconf-3230100.tar.gz \
 && curl -L -O http://zlib.net/zlib-1.2.11.tar.gz \
 && curl -L -O https://c-ares.haxx.se/download/c-ares-1.14.0.tar.gz \
 && curl -L -O http://libssh2.org/download/libssh2-1.8.0.tar.gz
RUN tar xf gmp-6.1.2.tar.lz \
 && cd gmp-6.1.2 \
 && ./configure --disable-shared --enable-static --prefix=/usr/local/$HOST --host=$HOST --disable-cxx --enable-fat CFLAGS="-mtune=generic -O2 -g0" \
 && make install
RUN tar xf expat-2.2.5.tar.bz2 \
 && cd expat-2.2.5 \
 && ./configure --disable-shared --enable-static --prefix=/usr/local/$HOST --host=$HOST --build=`dpkg-architecture -qDEB_BUILD_GNU_TYPE ` \
 && make install
RUN tar xf sqlite-autoconf-3230100.tar.gz \
 && cd sqlite-autoconf-3230100 \
 && ./configure --disable-shared --enable-static --prefix=/usr/local/$HOST --host=$HOST --build=`dpkg-architecture -qDEB_BUILD_GNU_TYPE ` \
 && make install
RUN tar xf zlib-1.2.11.tar.gz \
 && cd zlib-1.2.11 \
 && CC=$HOST-gcc AR=$HOST-ar LD=$HOST-ld RANLIB=$HOST-ranlib STRIP=$HOST-strip ./configure --prefix=/usr/local/$HOST --libdir=/usr/local/$HOST/lib --includedir=/usr/local/$HOST/include --static \
 && make install
RUN tar xf c-ares-1.14.0.tar.gz \
 && cd c-ares-1.14.0 \
 && ./configure --disable-shared --enable-static --without-random --prefix=/usr/local/$HOST --host=$HOST --build=`dpkg-architecture -qDEB_BUILD_GNU_TYPE ` LIBS="-lws2_32" \
 && make install
RUN tar xf libssh2-1.8.0.tar.gz \
 && cd libssh2-1.8.0 \
 && ./configure --disable-shared --enable-static --prefix=/usr/local/$HOST --host=$HOST --build=`dpkg-architecture -qDEB_BUILD_GNU_TYPE ` --without-openssl --with-wincng LIBS="-lws2_32" \
 && make install
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document version.json https://api.github.com/repos/aria2/aria2/git/refs/heads/master
RUN git clone https://github.com/aria2/aria2 \
 && cd aria2 \
 && autoreconf -i \
 && ./mingw-config \
 && make \
 && $HOST-strip src/aria2c.exe
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
