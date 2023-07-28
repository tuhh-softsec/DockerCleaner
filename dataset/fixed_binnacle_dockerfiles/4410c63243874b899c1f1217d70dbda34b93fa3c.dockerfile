#   Build an image with all the required compiler infrastructure
#   to cross-compile Leosac for Raspberry Pi.
#   We recommend you use leosaccli to use this container.
FROM debian:stretch
RUN echo 'deb-src http://deb.debian.org/debian stretch main' >> /etc/apt/sources.list
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends emacs24-nox=24.5+1-11+deb9u1 wget=1.18-5+deb9u3 git=1:2.11.0-3+deb9u7 cmake=3.7.2-1 build-essential=12.3 -y )
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends g++-arm-linux-gnueabihf=4:6.3.0-4 libtool-bin=2.4.6-2 -y )
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-10 automake=1:1.15-6 pkg-config=0.29-4+b1 python3=3.5.3-1 -y )
COPY cross-compile-resources /cross-compile-resources
#   Boost libraries.
#   Create custom configuration to be able to use the cross compiling toolchain
RUN echo 'using gcc : arm : arm-linux-gnueabihf-g++ ;' > /root/user-config.jam
#   Extract configure build and install
RUN (cd tmp \
 && cp /cross-compile-resources/boost_1_63_0.tar.gz . \
 && tar xvfz boost_1_63_0.tar.gz \
 && cd boost_1_63_0 \
 && ./bootstrap.sh \
 && ./b2 --without-coroutine --without-coroutine2 --without-fiber --without-context --reconfigure toolset=gcc-arm cxxflags='-std=c++14' -j6 ./b2 --prefix=/opt/rpi_fakeroot install )
#   ZeroMQ
RUN (cd /opt/rpi_fakeroot \
 && git clone https://github.com/zeromq/libzmq.git \
 && cd libzmq \
 && git checkout v4.2.1 \
 && ./autogen.sh \
 && ./configure --prefix=/opt/rpi_fakeroot --host=arm-linux-gnueabihf \
 && make -j6 \
 && make install )
#   TClap
RUN (cd /tmp \
 && : \
 && apt-get source libtclap-dev \
 && cd tclap-1.2.1 \
 && ./configure --prefix=/opt/rpi_fakeroot --host=arm-linux-gnueabihf \
 && make -j6 \
 && make install )
#  PostgreSQL native library
RUN (cd /tmp \
 && cp /cross-compile-resources/postgresql-9.4.10.tar.gz . \
 && tar xvfz postgresql-9.4.10.tar.gz \
 && cd postgresql-9.4.10 \
 && ./configure --without-readline --without-zlib --prefix=/opt/rpi_fakeroot --host=arm-linux-gnueabihf \
 && make -j6 \
 && make install )
#  SQLite native library
RUN (cd /tmp \
 && cp /cross-compile-resources/sqlite-autoconf-3190300.tar.gz . \
 && tar xvfz sqlite-autoconf-3190300.tar.gz \
 && cd sqlite-autoconf-3190300 \
 && ./configure --prefix=/opt/rpi_fakeroot --host=arm-linux-gnueabihf \
 && make -j6 \
 && make install )
#  #####################################################
#       ODB - Database Object Relational Mapper        #
#  #####################################################
#   ODB compiler (amd64 version as code generation is run using a native
#   odb binary).
RUN (cd /tmp \
 && cp /cross-compile-resources/odb_2.4.0-1_amd64.deb . \
 && dpkg -i odb_2.4.0-1_amd64.deb )
#   ODB common runtime
RUN (cd /tmp \
 && cp /cross-compile-resources/libodb-2.4.0.tar.gz . \
 && tar xvf libodb-2.4.0.tar.gz \
 && cd libodb-2.4.0 \
 && ./configure --prefix=/opt/rpi_fakeroot --host=arm-linux-gnueabihf \
 && make -j6 \
 && make install )
#   ODB Boost Profile
RUN (cd /tmp \
 && cp /cross-compile-resources/libodb-boost-2.4.0.tar.gz . \
 && tar xvf libodb-boost-2.4.0.tar.gz \
 && cd libodb-boost-2.4.0 \
 && ./configure CPPFLAGS="-I/opt/rpi_fakeroot/include" LDFLAGS="-L/opt/rpi_fakeroot/lib" --prefix=/opt/rpi_fakeroot --host=arm-linux-gnueabihf \
 && make -j6 \
 && make install )
#   ODB PGSQL runtime
RUN (cd /tmp \
 && cp /cross-compile-resources/libodb-pgsql-2.4.0.tar.gz . \
 && tar xvf libodb-pgsql-2.4.0.tar.gz \
 && cd libodb-pgsql-2.4.0 \
 && ./configure CPPFLAGS="-I/opt/rpi_fakeroot/include" LDFLAGS="-L/opt/rpi_fakeroot/lib" --prefix=/opt/rpi_fakeroot --host=arm-linux-gnueabihf \
 && make -j6 \
 && make install )
#   ODB SQLITE runtime
RUN (cd /tmp \
 && cp /cross-compile-resources/libodb-sqlite-2.4.0.tar.gz . \
 && tar xvf libodb-sqlite-2.4.0.tar.gz \
 && cd libodb-sqlite-2.4.0 \
 && ./configure CPPFLAGS="-I/opt/rpi_fakeroot/include" LDFLAGS="-L/opt/rpi_fakeroot/lib" --prefix=/opt/rpi_fakeroot --host=arm-linux-gnueabihf \
 && make -j6 \
 && make install )
#   LibScrypt
RUN (cd /tmp \
 && git clone https://github.com/technion/libscrypt.git \
 && cd libscrypt \
 && CC=arm-linux-gnueabihf-gcc make \
 && cp libscrypt.h /opt/rpi_fakeroot/include \
 && cp libscrypt.so.0 /opt/rpi_fakeroot/lib \
 && ln -s /opt/rpi_fakeroot/lib/libscrypt.so.0 /opt/rpi_fakeroot/lib/libscrypt.so )
#   CURL
RUN (cd /tmp \
 && cp /cross-compile-resources/curl-7.52.1.tar.gz . \
 && tar xvfz curl-7.52.1.tar.gz \
 && cd curl-7.52.1 \
 && ./configure --prefix=/opt/rpi_fakeroot --host=arm-linux-gnueabihf \
 && make -j6 \
 && make install )
#   OpenSSL
RUN (cd /tmp \
 && cp /cross-compile-resources/openssl-1.1.0d.tar.gz . \
 && tar xvfz openssl-1.1.0d.tar.gz \
 && cd openssl-1.1.0d \
 && export cross=arm-linux-gnueabihf- \
 && ./Configure linux-generic32 shared --prefix=/opt/rpi_fakeroot \
 && make CC="${cross}gcc" AR="${cross}ar r" RANLIB="${cross}ranlib" -j6 \
 && make install )
#   PCSC Lite package with headers and libraries.
RUN (cd /tmp \
 && cp /cross-compile-resources/pcsclite.tar . \
 && tar xvf pcsclite.tar --strip 1 -C /opt/rpi_fakeroot )
#   LibLogicalAccess
RUN (cd /tmp \
 && cp /cross-compile-resources/lla.tar . \
 && tar xvf lla.tar \
 && cd liblogicalaccess \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/opt/rpi_fakeroot -DCMAKE_TOOLCHAIN_FILE=/cross-compile-resources/rpi-cross.cmake -DPCSCLITE_INCLUDE_DIR=/opt/rpi_fakeroot/include/PCSC .. \
 && make -j6 \
 && make install )
COPY docker_scripts /docker_scripts
#   Volume where the root of the Leosac repository lives.
VOLUME /leosac
#   Optional volume that serves as build directory. Configuration
#   this volume allows for incremental compilation.
VOLUME /leosac_arm_build
#   A volume that can be mounted to specify a private key to use
#   when using `leosaccli dev cc dev-push` to push development build
#   to raspberry pi
VOLUME /ssh_deploy_key
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
