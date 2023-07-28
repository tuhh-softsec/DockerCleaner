#   This dockerfile defines the expected runtime environment before the project is installed
FROM ubuntu:bionic AS builder
LABEL maintainer="Balázs Dukai <b.dukai@tudelft.nl>"
LABEL description="Build environment for 3dfier, includes PostgreSQL"
#   required repositories
RUN set -x ; apt-get clean \
 && apt-get update ; apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y ; apt-get install --no-install-recommends tzdata=2022g-0ubuntu0.18.04 -y ; echo "Europe/Amsterdam" > /etc/timezone \
 && dpkg-reconfigure -f noninteractive tzdata ; apt-get update
#   C++ packages
RUN set -x \
 && apt-get install --no-install-recommends gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 cmake=3.10.2-1ubuntu2.18.04.2 libboost-dev=1.65.1.0ubuntu1 libboost-filesystem-dev=1.65.1.0ubuntu1 libboost-locale-dev=1.65.1.0ubuntu1 libboost-thread-dev=1.65.1.0ubuntu1 libboost-iostreams-dev=1.65.1.0ubuntu1 libboost-program-options-dev=1.65.1.0ubuntu1 libyaml-cpp-dev=0.5.2-4ubuntu1 libgmp-dev=2:6.1.2+dfsg-2ubuntu0.1 libmpfr-dev=4.0.1-1 gdbserver=8.1.1-0ubuntu1 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 -y --no-install-suggests \
 && apt-get update \
 && apt-get upgrade -y
#   PostgreSQL
RUN set -x \
 && apt-get install --no-install-recommends postgresql=10+190ubuntu0.1 -y
#   GIS packages
RUN set -x \
 && apt-get install --no-install-recommends libgdal-dev=2.2.3+dfsg-2 libproj-dev=4.9.3-2 -y \
 && apt-get update \
 && apt-get upgrade -y \
 && apt-get autoremove --purge -y \
 && apt-get autoclean -y \
 && rm -rf /var/cache/apt/* /tmp/*
#  RUN export CC=gcc-8 && export CXX=g++-8
ENV IN_DIR="\"/opt\""
#   get LASzip
RUN set -ex ; cd $IN_DIR ; wget https://github.com/LASzip/LASzip/releases/download/v2.2.0/laszip-src-2.2.0.tar.gz ; wget https://github.com/LASzip/LASzip/releases/download/v2.2.0/laszip-src-2.2.0.tar.gz.md5 ; md5sum -c laszip-src-2.2.0.tar.gz.md5 ; rm laszip-src-2.2.0.tar.gz.md5 ; tar -xf laszip-src-2.2.0.tar.gz ; rm laszip-src-2.2.0.tar.gz
#   The Makefile need to be modified in order to be compliant to what libLAS
#   is looking for. https://github.com/libLAS/libLAS/issues/9
RUN set -x ; cd $IN_DIR/laszip-src-2.2.0 ; sed -i 's/laszipdir = $(includedir)\//laszipdir = $(includedir)\/laszip/' ./include/laszip/Makefile.am ; sed -i 's/laszipdir = $(includedir)\//laszipdir = $(includedir)\/laszip/' ./include/laszip/Makefile.in ; mkdir build ; ./configure --prefix=$IN_DIR/laszip-src-2.2.0/build ; make \
 && make install \
 && make clean
#  Libraries have been installed in:
#     /home/bdukai/Desktop/laszip-src-2.2.0/build/lib
#
#  If you ever happen to want to link against installed libraries
#  in a given directory, LIBDIR, you must either use libtool, and
#  specify the full pathname of the library, or use the `-LLIBDIR'
#  flag during linking and do at least one of the following:
#     - add LIBDIR to the `LD_LIBRARY_PATH' environment variable
#       during execution
#     - add LIBDIR to the `LD_RUN_PATH' environment variable
#       during linking
#     - use the `-Wl,-rpath -Wl,LIBDIR' linker flag
#     - have your system administrator add LIBDIR to `/etc/ld.so.conf'
#
#  See any operating system documentation about shared libraries for
#  more information, such as the ld(1) and ld.so(8) manual pages.
RUN set -x ; cd $IN_DIR ; wget http://download.osgeo.org/liblas/libLAS-1.8.1.tar.bz2 ; tar -xf libLAS-1.8.1.tar.bz2 ; rm libLAS-1.8.1.tar.bz2 ; cd libLAS-1.8.1 ; mkdir build ; mkdir cmake_build ; cd cmake_build ; cmake .. -DCMAKE_INSTALL_PREFIX=$IN_DIR/libLAS-1.8.1/build -DWITH_GDAL=ON -DWITH_LASZIP=ON -DLASZIP_INCLUDE_DIR=$IN_DIR/laszip-src-2.2.0/build/include -DLASZIP_LIBRARY=$IN_DIR/laszip-src-2.2.0/build/lib/liblaszip.so ; make \
 && make install \
 && make clean
WORKDIR /opt/
RUN set -x ; wget https://github.com/CGAL/cgal/archive/releases/CGAL-4.12.tar.gz ; wget https://github.com/CGAL/cgal/releases/download/releases%2FCGAL-4.12/md5sum.txt ; md5sum -c md5sum.txt ; rm md5sum.txt ; tar -xf CGAL-4.12.tar.gz ; rm CGAL-4.12.tar.gz ; cd cgal-releases-CGAL-4.12 ; mkdir build ; cd build ; cmake .. ; make
RUN useradd -ms /bin/bash 3dfier
USER 3dfier
WORKDIR /home/3dfier
# Please add your HEALTHCHECK here!!!
