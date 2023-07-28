#
#   NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh"
#
#   PLEASE DO NOT EDIT IT DIRECTLY.
#
#   Image
FROM alpine:3.7 AS builder
#   Environment variables
ENV WKHTMLTOX_VERSION="0.12.5"
#   Copy patches
RUN mkdir -p /tmp/patches
COPY conf/* /tmp/patches/
#   Install needed packages
RUN apk add libstdc++=6.4.0-r5 libx11=1.6.6-r0 libxrender=0.9.10-r2 libxext=1.3.3-r2 libssl1.0=1.0.2t-r0 ca-certificates=20190108-r0 fontconfig=2.12.6-r0 freetype=2.8.1-r4 ttf-dejavu=2.37-r0 ttf-droid=20121017-r0 ttf-freefont=20120503-r0 ttf-liberation=2.00.1-r1 ttf-ubuntu-font-family=0.83-r0 --no-cache \
 && apk add g++=6.4.0-r5 git=2.15.4-r0 gtk+=2.24.31-r0 gtk+-dev=2.24.31-r0 make=4.2.1-r0 mesa-dev=17.2.4-r1 msttcorefonts-installer=3.6-r2 openssl-dev=1.0.2t-r0 patch=2.7.6-r0 fontconfig-dev=2.12.6-r0 freetype-dev=2.8.1-r4 --no-cache --virtual .build-deps \
 && update-ms-fonts \
 && fc-cache -f \
 && git clone --recursive https://github.com/wkhtmltopdf/wkhtmltopdf.git /tmp/wkhtmltopdf \
 && cd /tmp/wkhtmltopdf \
 && git checkout tags/$WKHTMLTOX_VERSION \
 && cd /tmp/wkhtmltopdf/qt \
 && patch -p1 -i /tmp/patches/qt-musl.patch \
 && patch -p1 -i /tmp/patches/qt-musl-iconv-no-bom.patch \
 && patch -p1 -i /tmp/patches/qt-recursive-global-mutex.patch \
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
#   Image
FROM alpine:3.7
RUN apk add libstdc++=6.4.0-r5 libx11=1.6.6-r0 libxrender=0.9.10-r2 libxext=1.3.3-r2 libssl1.0=1.0.2t-r0 ca-certificates=20190108-r0 fontconfig=2.12.6-r0 freetype=2.8.1-r4 ttf-dejavu=2.37-r0 ttf-droid=20121017-r0 ttf-freefont=20120503-r0 ttf-liberation=2.00.1-r1 ttf-ubuntu-font-family=0.83-r0 --no-cache \
 && apk add msttcorefonts-installer=3.6-r2 --no-cache --virtual .build-deps \
 && update-ms-fonts \
 && fc-cache -f \
 && rm -rf /tmp/* \
 && apk del .build-deps
COPY --from=builder /bin/wkhtmltopdf /bin/wkhtmltopdf
ENTRYPOINT ["wkhtmltopdf"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
