#  #
#   osgeo/gdal:ubuntu-small
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
 && (apt-get update ;apt-get install --no-install-recommends python3-dev=3.6.7-1~18.04 python3-numpy=1:1.13.3-2ubuntu1 libjpeg-dev=8c-2ubuntu8 libgeos-dev=3.6.2-1build2 curl=7.58.0-2ubuntu3.24 libcurl4-gnutls-dev=7.58.0-2ubuntu3.24 libexpat-dev libxerces-c-dev=3.2.0+debian-2 libwebp-dev=0.6.1-2ubuntu0.18.04.1 libzstd1-dev=1.3.3+dfsg-2ubuntu1.2 bash=4.4.18-2ubuntu1.3 zip=3.0-11build1 curl=7.58.0-2ubuntu3.24 libpq-dev=10.23-0ubuntu0.18.04.1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 autoconf=2.69-11 automake=1:1.15.1-3ubuntu2 sqlite3=3.22.0-1ubuntu0.7 bash-completion=1:2.8-1ubuntu1 -y --fix-missing )
#   Build openjpeg
ARG OPENJPEG_VERSION=2.3.1
RUN if test "${OPENJPEG_VERSION}" != "" ; then (wget -q https://github.com/uclouvain/openjpeg/archive/v${OPENJPEG_VERSION}.tar.gz \
 && tar xzf v${OPENJPEG_VERSION}.tar.gz \
 && rm -f v${OPENJPEG_VERSION}.tar.gz \
 && cd openjpeg-${OPENJPEG_VERSION} \
 && cmake . -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr \
 && make -j$( nproc ;) \
 && make install \
 && mkdir -p /build_thirdparty/usr/lib \
 && cp -P /usr/lib/libopenjp2*.so* /build_thirdparty/usr/lib \
 && for i in /build_thirdparty/usr/lib/*; do strip -s $i 2> /dev/null || /bin/true ; done \
 && cd .. \
 && rm -rf openjpeg-${OPENJPEG_VERSION} ) ; fi
ARG PROJ_INSTALL_PREFIX
ARG PROJ_DATUMGRID_LATEST_LAST_MODIFIED
RUN mkdir -p /build_projgrids/${PROJ_INSTALL_PREFIX}/share/proj \
 && curl -LOs http://download.osgeo.org/proj/proj-datumgrid-latest.zip \
 && unzip -q -j -u -o proj-datumgrid-latest.zip -d /build_projgrids/${PROJ_INSTALL_PREFIX}/share/proj \
 && rm -f *.zip
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
#   Build GDAL
ARG GDAL_VERSION=master
ARG GDAL_RELEASE_DATE
ARG GDAL_BUILD_IS_RELEASE
RUN if test "${GDAL_VERSION}" = "master" ; then export GDAL_VERSION=$( curl -Ls https://api.github.com/repos/OSGeo/gdal/commits/HEAD -H "Accept: application/vnd.github.VERSION.sha" ;) ;export GDAL_RELEASE_DATE=$( date "+%Y%m%d" ;) ; fi \
 && if test "x${GDAL_BUILD_IS_RELEASE}" = "x" ; then export GDAL_SHA1SUM=${GDAL_VERSION} ; fi \
 && mkdir gdal \
 && wget -q https://github.com/OSGeo/gdal/archive/${GDAL_VERSION}.tar.gz -O - | tar xz -C gdal --strip-components=1 \
 && cd gdal/gdal \
 && if test "${RSYNC_REMOTE}" != "" ; then echo "Downloading cache..." ;rsync -ra ${RSYNC_REMOTE}/gdal/ $HOME/ ;echo "Finished" ;printf "#!/bin/sh\nccache gcc $*" > ccache_gcc.sh;chmod +x ccache_gcc.sh ;printf "#!/bin/sh\nccache g++ $*" > ccache_g++.sh;chmod +x ccache_g++.sh ;export CC=$PWD/ccache_gcc.sh ;export CXX=$PWD/ccache_g++.sh ;ccache -M 1G ; fi \
 && ./configure --prefix=/usr --without-libtool --with-hide-internal-symbols --with-jpeg12 --with-python --with-webp --with-proj=/build${PROJ_INSTALL_PREFIX} --with-libtiff=internal --with-rename-internal-libtiff-symbols --with-geotiff=internal --with-rename-internal-libgeotiff-symbols \
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
RUN : ; DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends libsqlite3-0 curl unzip
#   GDAL dependencies
RUN : ; DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends python3-numpy libpython3.6 libjpeg-turbo8 libgeos-3.6.2 libgeos-c1v5 libcurl4 libexpat1 libxerces-c3.2 libwebp6 libzstd1 bash libpq5 libssl1.1
#   Order layers starting with less frequently varying ones
COPY --from=builder /build_thirdparty/usr/ /usr/
COPY --from=builder /build_projgrids/usr/ /usr/
ARG PROJ_INSTALL_PREFIX
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
