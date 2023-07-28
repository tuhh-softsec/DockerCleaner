FROM ubuntu:trusty
MAINTAINER Igor Khomyakov
RUN apt-get update \
 && apt-get install --no-install-recommends make=3.81-8.2ubuntu3 binutils=2.24-5ubuntu14.2 autoconf=2.69-6 automake=1:1.14.1-2ubuntu1 autotools-dev=20130810.1 libtool=2.4.2-1.7ubuntu1 pkg-config=0.26-1ubuntu4 git=1:1.9.1-1ubuntu0.10 curl=7.35.0-1ubuntu2.20 dpkg-dev=1.17.5ubuntu5.8 autopoint=0.18.3.1-1ubuntu3.1 libcppunit-dev=1.13.1-2ubuntu1 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libgcrypt11-dev=1.5.3-2ubuntu4.6 lzip=1.14-2 -y
RUN git clone https://github.com/raspberrypi/tools.git --depth=1 /tools
ENV ARCH="armhf"
ENV HOST="arm-linux-gnueabihf"
ENV LOCAL_DIR="/local"
ENV TOOL_BIN_DIR="/tools/arm-bcm2708/gcc-linaro-$HOST-raspbian-x64/bin"
ENV PATH="${TOOL_BIN_DIR}:$PATH"
RUN mkdir $LOCAL_DIR \
 && mkdir zlib \
 && cd zlib \
 && curl -Ls -o - 'http://zlib.net/zlib-1.2.11.tar.gz' | tar xzf - --strip-components=1 \
 && prefix=${LOCAL_DIR} CC=$HOST-gcc STRIP=$HOST-strip RANLIB=$HOST-ranlib AR=$HOST-ar LD=$HOST-ld ./configure --static --libdir=$LOCAL_DIR/lib \
 && make -s \
 && make -s install
RUN mkdir -p expat \
 && cd expat \
 && curl -Ls -o - 'https://sourceforge.net/projects/expat/files/expat/2.2.0/expat-2.2.0.tar.bz2/download' | tar xjf - --strip-components=1 \
 && ./configure --host=$HOST --build=`dpkg-architecture -qDEB_BUILD_GNU_TYPE ` --enable-shared=no --enable-static=yes --prefix=${LOCAL_DIR} \
 && make -s \
 && make -s install
RUN mkdir c-ares \
 && cd c-ares \
 && curl -Ls -o - 'http://c-ares.haxx.se/download/c-ares-1.10.0.tar.gz' | tar xzf - --strip-components=1 \
 && ./configure --host=$HOST --build=`dpkg-architecture -qDEB_BUILD_GNU_TYPE ` --enable-shared=no --enable-static=yes --prefix=${LOCAL_DIR} \
 && make -s \
 && make -s install
RUN mkdir gmp \
 && cd gmp \
 && curl -Ls -o - 'https://gmplib.org/download/gmp/gmp-6.1.0.tar.lz' | lzip -d | tar xf - --strip-components=1 \
 && ./configure --disable-shared --enable-static --prefix=$LOCAL_DIR --host=$HOST --disable-cxx --enable-fat \
 && make -s \
 && make -s install
RUN mkdir sqlite \
 && cd sqlite \
 && curl -Ls -o - 'https://www.sqlite.org/2016/sqlite-autoconf-3100100.tar.gz' | tar xzf - --strip-components=1 \
 && ./configure --disable-shared --enable-static --prefix=$LOCAL_DIR --host=$HOST --build=`dpkg-architecture -qDEB_BUILD_GNU_TYPE ` \
 && make -s \
 && make -s install
RUN mkdir aria \
 && cd aria \
 && curl -s 'https://api.github.com/repos/aria2/aria2/releases/latest' | grep 'browser_download_url.*[0-9]\.tar\.bz2' | sed -e 's/^[[:space:]]*//' | cut -d ' ' -f 2 | xargs -I % curl -Ls -o - '%' | tar xjf - --strip-components=1 \
 && ./configure --host=$HOST --build=`dpkg-architecture -qDEB_BUILD_GNU_TYPE ` --disable-nls --disable-ssl --without-gnutls --without-libxml2 --with-libz --with-libz-prefix=${LOCAL_DIR} --with-libexpat --with-libexpat-prefix=${LOCAL_DIR} --with-slite3 --with-sqlite3-prefix=${LOCAL_DIR} --with-libcares --with-libcares-prefix=${LOCAL_DIR} --prefix=${LOCAL_DIR} LDFLAGS="-L$LOCAL_DIR/lib" PKG_CONFIG_PATH="$LOCAL_DIR/lib/pkgconfig" ARIA2_STATIC=yes \
 && make -s \
 && make -s install-strip
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
