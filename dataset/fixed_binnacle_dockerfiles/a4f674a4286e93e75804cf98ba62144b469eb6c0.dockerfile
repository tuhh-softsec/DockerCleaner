#  #
#   osgeo/gdal:ubuntu-full
#   This file is available at the option of the licensee under:
#   Public domain
#   or licensed under X/MIT (LICENSE.TXT) Copyright 2019 Even Rouault <even.rouault@spatialys.com>
ARG PROJ_INSTALL_PREFIX=/usr/local
FROM ubuntu:18.04 AS builder
#   Derived from osgeo/proj by Howard Butler <howard@hobu.co>
MAINTAINER Even Rouault <even.rouault@spatialys.com>
#   Setup build env for PROJ
RUN apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 build-essential=12.4ubuntu1 ca-certificates=20211016ubuntu0.18.04.1 git=1:2.17.1-1ubuntu0.17 make=4.1-9.1ubuntu1 cmake=3.10.2-1ubuntu2.18.04.2 wget=1.19.4-1ubuntu2.2 unzip=6.0-21ubuntu1.2 libtool=2.4.6-2 automake=1:1.15.1-3ubuntu2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libsqlite3-dev=3.22.0-1ubuntu0.7 pkg-config=0.29.1-0ubuntu2 sqlite3=3.22.0-1ubuntu0.7 -y --fix-missing )
#   Setup build env for GDAL
RUN apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends libcharls-dev=1.1.0+dfsg-2 libopenjp2-7-dev=2.3.0-2+deb10u2build0.18.04.1 libcairo2-dev=1.15.10-2ubuntu0.1 python3-dev=3.6.7-1~18.04 python3-numpy=1:1.13.3-2ubuntu1 libpng-dev=1.6.34-1ubuntu0.18.04.2 libjpeg-dev=8c-2ubuntu8 libgif-dev=5.1.4-2ubuntu0.1 liblzma-dev=5.2.2-1.3ubuntu0.1 libgeos-dev=3.6.2-1build2 curl=7.58.0-2ubuntu3.24 libcurl4-gnutls-dev=7.58.0-2ubuntu3.24 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libexpat-dev libxerces-c-dev=3.2.0+debian-2 libnetcdf-dev=1:4.6.0-2build1 libpoppler-dev=0.62.0-2ubuntu2.14 libpoppler-private-dev=0.62.0-2ubuntu2.14 libspatialite-dev=4.3.0a-5build1 swig=3.0.12-1 libhdf4-alt-dev=4.2.13-2 libhdf5-serial-dev=1.10.0-patch1+docs-4 libfreexl-dev=1.0.5-1 unixodbc-dev=2.3.4-1.1ubuntu3 libwebp-dev=0.6.1-2ubuntu0.18.04.1 libepsilon-dev=0.9.2+dfsg-2 liblcms2-2=2.9-1ubuntu0.1 libpcre3-dev=2:8.39-9ubuntu0.1 libcrypto++-dev=5.6.4-8 libdap-dev=3.19.1-2build1 libfyba-dev=4.1.1-3 libkml-dev=1.3.0-5 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 libogdi3.2-dev=3.2.0+ds-2 libcfitsio-dev=3.430-2 openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 libzstd1-dev=1.3.3+dfsg-2ubuntu1.2 libpq-dev=10.23-0ubuntu0.18.04.1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libboost-dev=1.65.1.0ubuntu1 autoconf=2.69-11 automake=1:1.15.1-3ubuntu2 bash-completion=1:2.8-1ubuntu1 libarmadillo-dev=1:8.400.0+dfsg-2 -y --fix-missing )
ARG PROJ_DATUMGRID_LATEST_LAST_MODIFIED
ARG PROJ_INSTALL_PREFIX
RUN mkdir -p /build_projgrids/${PROJ_INSTALL_PREFIX}/share/proj \
 && curl -LOs http://download.osgeo.org/proj/proj-datumgrid-latest.zip \
 && unzip -q -j -u -o proj-datumgrid-latest.zip -d /build_projgrids/${PROJ_INSTALL_PREFIX}/share/proj \
 && rm -f *.zip
#   Build likbkea
ARG KEA_VERSION=c6d36f3db5e4
RUN wget -q https://bitbucket.org/chchrsc/kealib/get/${KEA_VERSION}.zip \
 && unzip -q ${KEA_VERSION}.zip \
 && rm -f ${KEA_VERSION}.zip \
 && cd chchrsc-kealib-${KEA_VERSION}/trunk \
 && cmake . -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr -DHDF5_INCLUDE_DIR=/usr/include/hdf5/serial -DHDF5_LIB_PATH=/usr/lib/x86_64-linux-gnu/hdf5/serial -DLIBKEA_WITH_GDAL=OFF \
 && make -j$( nproc ;) \
 && make install DESTDIR="/build_thirdparty" \
 && make install \
 && cd ../.. \
 && rm -rf chchrsc-kealib-${KEA_VERSION} \
 && for i in /build_thirdparty/usr/lib/*; do strip -s $i 2> /dev/null || /bin/true ; done \
 && for i in /build_thirdparty/usr/bin/*; do strip -s $i 2> /dev/null || /bin/true ; done
#   Build mongo-c-driver
ARG MONGO_C_DRIVER_VERSION=1.13.0
RUN mkdir mongo-c-driver \
 && wget -q https://github.com/mongodb/mongo-c-driver/releases/download/${MONGO_C_DRIVER_VERSION}/mongo-c-driver-${MONGO_C_DRIVER_VERSION}.tar.gz -O - | tar xz -C mongo-c-driver --strip-components=1 \
 && cd mongo-c-driver \
 && mkdir build_cmake \
 && cd build_cmake \
 && cmake .. -DCMAKE_INSTALL_PREFIX=/usr -DENABLE_TESTS=NO -DCMAKE_BUILD_TYPE=Release \
 && make -j$( nproc ;) \
 && make install DESTDIR="/build_thirdparty" \
 && make install \
 && cd ../.. \
 && rm -rf mongo-c-driver \
 && rm /build_thirdparty/usr/lib/x86_64-linux-gnu/*.a \
 && for i in /build_thirdparty/usr/lib/x86_64-linux-gnu/*; do strip -s $i 2> /dev/null || /bin/true ; done \
 && for i in /build_thirdparty/usr/bin/*; do strip -s $i 2> /dev/null || /bin/true ; done
#   Build mongocxx
ARG MONGOCXX_VERSION=3.4.0
RUN mkdir mongocxx \
 && wget -q https://github.com/mongodb/mongo-cxx-driver/archive/r${MONGOCXX_VERSION}.tar.gz -O - | tar xz -C mongocxx --strip-components=1 \
 && cd mongocxx \
 && mkdir build_cmake \
 && cd build_cmake \
 && cmake .. -DCMAKE_INSTALL_PREFIX=/usr -DBSONCXX_POLY_USE_BOOST=ON -DMONGOCXX_ENABLE_SLOW_TESTS=NO -DCMAKE_BUILD_TYPE=Release \
 && make -j$( nproc ;) \
 && make install DESTDIR="/build_thirdparty" \
 && make install \
 && cd ../.. \
 && rm -rf mongocxx \
 && for i in /build_thirdparty/usr/lib/x86_64-linux-gnu/*; do strip -s $i 2> /dev/null || /bin/true ; done \
 && for i in /build_thirdparty/usr/bin/*; do strip -s $i 2> /dev/null || /bin/true ; done
#   Build tiledb
ARG TILEDB_VERSION=1.5.0
RUN mkdir tiledb \
 && wget -q https://github.com/TileDB-Inc/TileDB/archive/${TILEDB_VERSION}.tar.gz -O - | tar xz -C tiledb --strip-components=1 \
 && cd tiledb \
 && mkdir build_cmake \
 && cd build_cmake \
 && ../bootstrap --prefix=/usr \
 && make -j$( nproc ;) \
 && make install-tiledb DESTDIR="/build_thirdparty" \
 && make install-tiledb \
 && cd ../.. \
 && rm -rf tiledb \
 && for i in /build_thirdparty/usr/lib/x86_64-linux-gnu/*; do strip -s $i 2> /dev/null || /bin/true ; done \
 && for i in /build_thirdparty/usr/bin/*; do strip -s $i 2> /dev/null || /bin/true ; done
#   Build openjpeg
ARG OPENJPEG_VERSION=2.3.1
RUN if test "${OPENJPEG_VERSION}" != "" ; then (wget -q https://github.com/uclouvain/openjpeg/archive/v${OPENJPEG_VERSION}.tar.gz \
 && tar xzf v${OPENJPEG_VERSION}.tar.gz \
 && rm -f v${OPENJPEG_VERSION}.tar.gz \
 && cd openjpeg-${OPENJPEG_VERSION} \
 && cmake . -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr \
 && make -j$( nproc ;) \
 && make install \
 && mkdir -p /build_thirdparty/usr/lib/x86_64-linux-gnu \
 && rm -f /usr/lib/x86_64-linux-gnu/libopenjp2.so* \
 && mv /usr/lib/libopenjp2.so* /usr/lib/x86_64-linux-gnu \
 && cp -P /usr/lib/x86_64-linux-gnu/libopenjp2.so* /build_thirdparty/usr/lib/x86_64-linux-gnu \
 && for i in /build_thirdparty/usr/lib/x86_64-linux-gnu/*; do strip -s $i 2> /dev/null || /bin/true ; done \
 && cd .. \
 && rm -rf openjpeg-${OPENJPEG_VERSION} ) ; fi
RUN apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends rsync=3.1.2-2.1ubuntu1.6 ccache=3.4.1-1 -y --fix-missing )
ARG RSYNC_REMOTE
#   Build PROJ
ARG PROJ_VERSION=master
RUN mkdir proj \
 && wget -q https://github.com/OSGeo/proj.4/archive/${PROJ_VERSION}.tar.gz -O - | tar xz -C proj --strip-components=1 \
 && cd proj \
 && ./autogen.sh \
 && if test "${RSYNC_REMOTE}" != "" ; then echo "Downloading cache..." ;rsync -ra ${RSYNC_REMOTE}/proj/ $HOME/ ;echo "Finished" ;export CC="ccache gcc" ;export CXX="ccache g++" ;export PROJ_DB_CACHE_DIR="$HOME/.ccache" ;ccache -M 100M ; fi \
 && CFLAGS='-DPROJ_RENAME_SYMBOLS -O2' CXXFLAGS='-DPROJ_RENAME_SYMBOLS -O2' ./configure --prefix=${PROJ_INSTALL_PREFIX} --disable-static \
 && make -j$( nproc ;) \
 && make install DESTDIR="/build" \
 && if test "${RSYNC_REMOTE}" != "" ; then ccache -s ;echo "Uploading cache..." ;rsync -ra --delete $HOME/.ccache ${RSYNC_REMOTE}/proj/ ;echo "Finished" ;rm -rf $HOME/.ccache ;unset CC ;unset CXX ; fi \
 && cd .. \
 && rm -rf proj \
 && PROJ_SO=$( readlink /build${PROJ_INSTALL_PREFIX}/lib/libproj.so | sed "s/libproj\.so\.//" ;) \
 && PROJ_SO_FIRST=$( echo $PROJ_SO | awk 'BEGIN {FS="."} {print $1}' ;) \
 && mv /build${PROJ_INSTALL_PREFIX}/lib/libproj.so.${PROJ_SO} /build${PROJ_INSTALL_PREFIX}/lib/libinternalproj.so.${PROJ_SO} \
 && ln -s libinternalproj.so.${PROJ_SO} /build${PROJ_INSTALL_PREFIX}/lib/libinternalproj.so.${PROJ_SO_FIRST} \
 && ln -s libinternalproj.so.${PROJ_SO} /build${PROJ_INSTALL_PREFIX}/lib/libinternalproj.so \
 && rm /build${PROJ_INSTALL_PREFIX}/lib/libproj.* \
 && ln -s libinternalproj.so.${PROJ_SO} /build${PROJ_INSTALL_PREFIX}/lib/libproj.so.${PROJ_SO_FIRST} \
 && strip -s /build${PROJ_INSTALL_PREFIX}/lib/libinternalproj.so.${PROJ_SO} \
 && for i in /build${PROJ_INSTALL_PREFIX}/bin/*; do strip -s $i 2> /dev/null || /bin/true ; done
ARG GDAL_VERSION=master
ARG GDAL_RELEASE_DATE
ARG GDAL_BUILD_IS_RELEASE
#   Build GDAL
RUN if test "${GDAL_VERSION}" = "master" ; then export GDAL_VERSION=$( curl -Ls https://api.github.com/repos/OSGeo/gdal/commits/HEAD -H "Accept: application/vnd.github.VERSION.sha" ;) ;export GDAL_RELEASE_DATE=$( date "+%Y%m%d" ;) ; fi \
 && if test "x${GDAL_BUILD_IS_RELEASE}" = "x" ; then export GDAL_SHA1SUM=${GDAL_VERSION} ; fi \
 && mkdir gdal \
 && wget -q https://github.com/OSGeo/gdal/archive/${GDAL_VERSION}.tar.gz -O - | tar xz -C gdal --strip-components=1 \
 && cd gdal/gdal \
 && if test "${RSYNC_REMOTE}" != "" ; then echo "Downloading cache..." ;rsync -ra ${RSYNC_REMOTE}/gdal/ $HOME/ ;echo "Finished" ;printf "#!/bin/sh\nccache gcc $*" > ccache_gcc.sh;chmod +x ccache_gcc.sh ;printf "#!/bin/sh\nccache g++ $*" > ccache_g++.sh;chmod +x ccache_g++.sh ;export CC=$PWD/ccache_gcc.sh ;export CXX=$PWD/ccache_g++.sh ;ccache -M 1G ; fi \
 && ./configure --prefix=/usr --without-libtool --with-hide-internal-symbols --with-jpeg12 --with-python --with-poppler --with-spatialite --with-mysql --with-liblzma --with-webp --with-epsilon --with-proj=/build/usr/local --with-poppler --with-hdf5 --with-dods-root=/usr --with-sosi --with-mysql --with-libtiff=internal --with-rename-internal-libtiff-symbols --with-geotiff=internal --with-rename-internal-libgeotiff-symbols --with-kea=/usr/bin/kea-config --with-mongocxxv3 --with-tiledb --with-crypto \
 && make -j$( nproc ;) \
 && make install DESTDIR="/build" \
 && if test "${RSYNC_REMOTE}" != "" ; then ccache -s ;echo "Uploading cache..." ;rsync -ra --delete $HOME/.ccache ${RSYNC_REMOTE}/gdal/ ;echo "Finished" ;rm -rf $HOME/.ccache ;unset CC ;unset CXX ; fi \
 && cd ../.. \
 && rm -rf gdal \
 && mkdir -p /build_gdal_python/usr/lib \
 && mkdir -p /build_gdal_python/usr/bin \
 && mkdir -p /build_gdal_version_changing/usr/include \
 && mv /build/usr/lib/python3 /build_gdal_python/usr/lib \
 && mv /build/usr/lib /build_gdal_version_changing/usr \
 && mv /build/usr/include/gdal_version.h /build_gdal_version_changing/usr/include \
 && mv /build/usr/bin/*.py /build_gdal_python/usr/bin \
 && mv /build/usr/bin /build_gdal_version_changing/usr \
 && for i in /build_gdal_version_changing/usr/lib/*; do strip -s $i 2> /dev/null || /bin/true ; done \
 && for i in /build_gdal_python/usr/lib/python3/dist-packages/osgeo/*.so; do strip -s $i 2> /dev/null || /bin/true ; done \
 && for i in /build_gdal_version_changing/usr/bin/*; do strip -s $i 2> /dev/null || /bin/true ; done
#   Build final image
FROM ubuntu:18.04 AS runner
RUN date
#   PROJ dependencies
RUN : ; DEBIAN_FRONTEND=noninteractive apt-get install -y libsqlite3-0 curl unzip
#   GDAL dependencies
RUN : ; DEBIAN_FRONTEND=noninteractive apt-get install -y libcharls1 libopenjp2-7 libcairo2 python3-numpy libpng16-16 libjpeg-turbo8 libgif7 liblzma5 libgeos-3.6.2 libgeos-c1v5 libcurl4 libxml2 libexpat1 libxerces-c3.2 libnetcdf-c++4 netcdf-bin libpoppler73 libspatialite7 gpsbabel libhdf4-0-alt libhdf5-100 libhdf5-cpp-100 poppler-utils libfreexl1 unixodbc libwebp6 libepsilon1 liblcms2-2 libpcre3 libcrypto++ libdap25 libdapclient6v5 libfyba0 libkmlbase1 libkmlconvenience1 libkmldom1 libkmlengine1 libkmlregionator1 libkmlxsd1 libmysqlclient20 libogdi3.2 libcfitsio5 openjdk-8-jre libzstd1 bash bash-completion libpq5 libssl1.1 libarmadillo8 libpython3.6
#   Order layers starting with less frequently varying ones
ARG PROJ_DATUMGRID_LATEST_LAST_MODIFIED
ARG PROJ_INSTALL_PREFIX
RUN mkdir -p ${PROJ_INSTALL_PREFIX}/share/proj \
 && curl -LOs http://download.osgeo.org/proj/proj-datumgrid-latest.zip \
 && unzip -j -u -o proj-datumgrid-latest.zip -d ${PROJ_INSTALL_PREFIX}/share/proj \
 && curl -LOs http://download.osgeo.org/proj/proj-datumgrid-europe-latest.zip \
 && unzip -j -u -o proj-datumgrid-europe-latest.zip -d ${PROJ_INSTALL_PREFIX}/share/proj \
 && curl -LOs http://download.osgeo.org/proj/proj-datumgrid-oceania-latest.zip \
 && unzip -j -u -o proj-datumgrid-oceania-latest.zip -d ${PROJ_INSTALL_PREFIX}/share/proj \
 && curl -LOs http://download.osgeo.org/proj/proj-datumgrid-world-latest.zip \
 && unzip -j -u -o proj-datumgrid-world-latest.zip -d ${PROJ_INSTALL_PREFIX}/share/proj \
 && curl -LOs http://download.osgeo.org/proj/proj-datumgrid-north-america-latest.zip \
 && unzip -j -u -o proj-datumgrid-north-america-latest.zip -d ${PROJ_INSTALL_PREFIX}/share/proj \
 && rm -f *.zip
COPY --from=builder /build_thirdparty/usr/ /usr/
COPY --from=builder /build${PROJ_INSTALL_PREFIX}/share/proj/ ${PROJ_INSTALL_PREFIX}/share/proj/
COPY --from=builder /build${PROJ_INSTALL_PREFIX}/include/ ${PROJ_INSTALL_PREFIX}/include/
COPY --from=builder /build${PROJ_INSTALL_PREFIX}/bin/ ${PROJ_INSTALL_PREFIX}/bin/
COPY --from=builder /build${PROJ_INSTALL_PREFIX}/lib/ ${PROJ_INSTALL_PREFIX}/lib/
COPY --from=builder /build/usr/share/gdal/ /usr/share/gdal/
COPY --from=builder /build/usr/include/ /usr/include/
COPY --from=builder /build_gdal_python/usr/ /usr/
COPY --from=builder /build_gdal_version_changing/usr/ /usr/
RUN ldconfig
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
