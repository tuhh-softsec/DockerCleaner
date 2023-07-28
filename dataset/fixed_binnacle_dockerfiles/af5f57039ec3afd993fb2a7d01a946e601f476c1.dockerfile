#
#   NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh"
#
#   PLEASE DO NOT EDIT IT DIRECTLY.
#
#   Image
FROM node:8.9.4-alpine
#   Environment variables
ENV WKHTMLTOX_VERSION="0.12.4"
#   Copy patches
RUN mkdir -p /tmp/patches
COPY conf/* /tmp/patches/
#   Install needed packages
RUN apk add libstdc++=6.3.0-r4 libx11=1.6.6-r0 libxrender=0.9.10-r1 libxext=1.3.3-r1 libssl1.0=1.0.2r-r0 ca-certificates=20161130-r3 fontconfig=2.12.1-r0 freetype=2.7.1-r2 ttf-dejavu=2.35-r0 ttf-droid=20121017-r0 ttf-freefont=20120503-r0 ttf-liberation=2.00.1-r1 ttf-ubuntu-font-family=0.83-r0 --no-cache \
 && apk add g++=6.3.0-r4 git=2.13.7-r2 gtk+=2.24-r1 gtk+-dev=2.24-r1 make=4.2.1-r0 mesa-dev=17.0.3-r1 openssl-dev=1.0.2r-r0 patch=2.7.5-r3 --no-cache --virtual .build-deps \
 && git clone --recursive https://github.com/wkhtmltopdf/wkhtmltopdf.git /tmp/wkhtmltopdf \
 && cd /tmp/wkhtmltopdf \
 && git checkout tags/$WKHTMLTOX_VERSION \
 && cd /tmp/wkhtmltopdf/qt \
 && patch -p1 -i /tmp/patches/qt-musl.patch \
 && patch -p1 -i /tmp/patches/qt-musl-iconv-no-bom.patch \
 && patch -p1 -i /tmp/patches/qt-recursive-global-mutex.patch \
 && patch -p1 -i /tmp/patches/qt-font-pixel-size.patch \
 && patch -p1 -i /tmp/patches/qt-gcc6.patch \
 && sed -i "s|-O2|$CXXFLAGS|" mkspecs/common/g++.conf \
 && sed -i "/^QMAKE_RPATH/s| -Wl,-rpath,||g" mkspecs/common/g++.conf \
 && sed -i "/^QMAKE_LFLAGS\s/s|+=|+= $LDFLAGS|g" mkspecs/common/g++.conf \
 && NB_CORES=$( grep -c '^processor' /proc/cpuinfo ;) \
 && ./configure -confirm-license -opensource -prefix /usr -datadir /usr/share/qt -sysconfdir /etc -plugindir /usr/lib/qt/plugins -importdir /usr/lib/qt/imports -silent -release -static -webkit -script -svg -exceptions -xmlpatterns -openssl-linked -no-fast -no-largefile -no-accessibility -no-stl -no-sql-ibase -no-sql-mysql -no-sql-odbc -no-sql-psql -no-sql-sqlite -no-sql-sqlite2 -no-qt3support -no-opengl -no-openvg -no-system-proxies -no-multimedia -no-audio-backend -no-phonon -no-phonon-backend -no-javascript-jit -no-scripttools -no-declarative -no-declarative-debug -no-mmx -no-3dnow -no-sse -no-sse2 -no-sse3 -no-ssse3 -no-sse4.1 -no-sse4.2 -no-avx -no-neon -no-rpath -no-nis -no-cups -no-pch -no-dbus -no-separate-debug-info -no-gtkstyle -no-nas-sound -no-opengl -no-openvg -no-sm -no-xshape -no-xvideo -no-xsync -no-xinerama -no-xcursor -no-xfixes -no-xrandr -no-mitshm -no-xinput -no-xkb -no-glib -no-icu -nomake demos -nomake docs -nomake examples -nomake tools -nomake tests -nomake translations -graphicssystem raster -qt-zlib -qt-libpng -qt-libmng -qt-libtiff -qt-libjpeg -optimized-qmake -iconv -xrender -fontconfig -D ENABLE_VIDEO=0 \
 && make --jobs $(($NB_CORES * 2)) --silent \
 && make install \
 && cd /tmp/wkhtmltopdf \
 && qmake \
 && make --jobs $(($NB_CORES * 2)) --silent \
 && make install \
 && make clean \
 && make distclean \
 && cd /tmp/wkhtmltopdf/qt \
 && make uninstall \
 && make clean \
 && make distclean \
 && rm -rf /tmp/* \
 && apk del .build-deps
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:3000 || exit 1
