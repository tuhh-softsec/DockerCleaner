#
#  handbrake Dockerfile
#
#  https://github.com/jlesage/docker-handbrake
#
#  Pull base image.
FROM jlesage/baseimage-gui:alpine-3.9-v3.5.2
#  Define software versions.
#  NOTE: x264 version 20171224 is the most recent one that doesn't crash.
ARG HANDBRAKE_VERSION=1.2.2
ARG X264_VERSION=20171224
ARG LIBVA_VERSION=2.4.0
ARG INTEL_VAAPI_DRIVER_VERSION=2.3.0
ARG GMMLIB_VERSION=18.4.1
ARG INTEL_MEDIA_DRIVER_VERSION=18.4.1
ARG INTEL_MEDIA_SDK_VERSION=18.4.1
#  Define software download URLs.
ARG HANDBRAKE_URL=https://download.handbrake.fr/releases/${HANDBRAKE_VERSION}/HandBrake-${HANDBRAKE_VERSION}-source.tar.bz2
ARG X264_URL=https://download.videolan.org/pub/videolan/x264/snapshots/x264-snapshot-${X264_VERSION}-2245-stable.tar.bz2
ARG LIBVA_URL=https://github.com/intel/libva/releases/download/${LIBVA_VERSION}/libva-${LIBVA_VERSION}.tar.bz2
ARG INTEL_VAAPI_DRIVER_URL=https://github.com/intel/intel-vaapi-driver/releases/download/${INTEL_VAAPI_DRIVER_VERSION}/intel-vaapi-driver-${INTEL_VAAPI_DRIVER_VERSION}.tar.bz2
ARG GMMLIB_URL=https://github.com/intel/gmmlib/archive/intel-gmmlib-${GMMLIB_VERSION}.tar.gz
ARG INTEL_MEDIA_DRIVER_URL=https://github.com/intel/media-driver/archive/intel-media-${INTEL_MEDIA_DRIVER_VERSION}.tar.gz
ARG INTEL_MEDIA_SDK_URL=https://github.com/Intel-Media-SDK/MediaSDK/archive/intel-mediasdk-${INTEL_MEDIA_SDK_VERSION}.tar.gz
#  Other build arguments.
#  Set to 'max' to keep debug symbols.
ARG HANDBRAKE_DEBUG_MODE=none
#  Define working directory.
WORKDIR /tmp
#  Compile HandBrake, libva and Intel Media SDK.
RUN add-pkg --virtual build-dependencies curl build-base yasm autoconf cmake automake libtool m4 patch coreutils tar file python linux-headers intltool git diffutils bash nasm jansson-dev libxml2-dev libpciaccess-dev xz-dev libsamplerate-dev libass-dev libtheora-dev lame-dev opus-dev libvorbis-dev speex-dev libvpx-dev gtk+3.0-dev dbus-glib-dev libnotify-dev libgudev-dev \
 && echo "Downloading x264 sources..." \
 && mkdir x264 \
 && curl -# -L ${X264_URL} | tar xj --strip 1 -C x264 \
 && echo "Downloading libva sources..." \
 && mkdir libva \
 && curl -# -L ${LIBVA_URL} | tar xj --strip 1 -C libva \
 && echo "Downloading Intel VAAPI driver sources..." \
 && mkdir intel-vaapi-driver \
 && curl -# -L ${INTEL_VAAPI_DRIVER_URL} | tar xj --strip 1 -C intel-vaapi-driver \
 && echo "Downloading gmmlib sources..." \
 && mkdir gmmlib \
 && curl -# -L ${GMMLIB_URL} | tar xz --strip 1 -C gmmlib \
 && echo "Downloading Intel Media driver sources..." \
 && mkdir intel-media-driver \
 && curl -# -L ${INTEL_MEDIA_DRIVER_URL} | tar xz --strip 1 -C intel-media-driver \
 && echo "Downloading Intel Media SDK sources..." \
 && mkdir MediaSDK \
 && curl -# -L ${INTEL_MEDIA_SDK_URL} | tar xz --strip 1 -C MediaSDK \
 && echo "Downloading HandBrake sources..." \
 && if echo "${HANDBRAKE_URL}" | grep -q '\.git$' ; then git clone ${HANDBRAKE_URL} HandBrake \
 && git -C HandBrake checkout "${HANDBRAKE_VERSION}" ; else mkdir HandBrake \
 && curl -# -L ${HANDBRAKE_URL} | tar xj --strip 1 -C HandBrake ; fi \
 && echo "Downloading helpers..." \
 && curl -# -L -o /tmp/run_cmd https://raw.githubusercontent.com/jlesage/docker-mgmt-tools/master/run_cmd \
 && chmod +x /tmp/run_cmd \
 && echo "Downloading patches..." \
 && curl -# -L -o HandBrake/A00-hb-video-preset.patch https://raw.githubusercontent.com/jlesage/docker-handbrake/master/A00-hb-video-preset.patch \
 && curl -# -L -o HandBrake/A00-hb-qsv.patch https://raw.githubusercontent.com/jlesage/docker-handbrake/master/A00-hb-qsv.patch \
 && curl -# -L -o MediaSDK/intel-media-sdk-debug-no-assert.patch https://raw.githubusercontent.com/jlesage/docker-handbrake/master/intel-media-sdk-debug-no-assert.patch \
 && curl -# -L -o intel-media-driver/media-driver-c-assert-fix.patch https://raw.githubusercontent.com/jlesage/docker-handbrake/master/media-driver-c-assert-fix.patch \
 && echo "Compiling x264..." \
 && cd x264 \
 && if [ "${HANDBRAKE_DEBUG_MODE}" = "none" ] ; then X264_CMAKE_OPTS=--enable-strip ; else X264_CMAKE_OPTS=--enable-debug ; fi \
 && ./configure --prefix=/usr --enable-shared --enable-pic --disable-cli $X264_CMAKE_OPTS \
 && make -j$( nproc ;) install \
 && cd ../ \
 && echo "Compiling libva..." \
 && cd libva \
 && ./configure --prefix=/usr --mandir=/tmp/libva-man --infodir=/tmp/liva-info --localstatedir=/var --enable-x11 --disable-glx --disable-wayland --disable-static --enable-shared --with-drivers-path=/opt/intel/mediasdk/lib64 \
 && make -j$( nproc ;) \
 && make install \
 && cd ../ \
 && echo "Compiling Intel VAAPI driver..." \
 && cd intel-vaapi-driver \
 && ./configure \
 && make -j$( nproc ;) \
 && make install \
 && cd .. \
 && echo "Compiling Intel Media driver..." \
 && add-pkg libexecinfo-dev \
 && cd intel-media-driver \
 && patch -p1 < media-driver-c-assert-fix.patch \
 && mkdir build \
 && cd build \
 && cmake -Wno-dev -DBUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/opt/intel/mediasdk -DLIBVA_DRIVERS_PATH=/opt/intel/mediasdk/lib64 -DINSTALL_DRIVER_SYSCONF=OFF -DMEDIA_RUN_TEST_SUITE=OFF ../ \
 && make -j$( nproc ;) \
 && make install \
 && cd .. \
 && cd .. \
 && echo "Compiling Intel Media SDK..." \
 && cd MediaSDK \
 && patch -p1 < intel-media-sdk-debug-no-assert.patch \
 && mkdir build \
 && cd build \
 && if [ "${HANDBRAKE_DEBUG_MODE}" = "none" ] ; then INTEL_MEDIA_SDK_BUILD_TYPE=RELEASE ; else INTEL_MEDIA_SDK_BUILD_TYPE=DEBUG ; fi \
 && cmake -DCMAKE_BUILD_TYPE=$INTEL_MEDIA_SDK_BUILD_TYPE -DMFX_PLUGINS_DIR=/opt/intel/mediasdk/plugins -DMFX_PLUGINS_CONF_DIR=/opt/intel/mediasdk/plugins -DENABLE_OPENCL=OFF -DENABLE_X11_DRI3=OFF -DENABLE_WAYLAND=OFF -DBUILD_DISPATCHER=ON -DENABLE_ITT=OFF -DENABLE_TEXTLOG=OFF -DENABLE_STAT=OFF -DBUILD_SAMPLES=OFF .. \
 && make -j$( nproc ;) install \
 && cd .. \
 && cd .. \
 && echo "Compiling HandBrake..." \
 && cd HandBrake \
 && patch -p1 < A00-hb-video-preset.patch \
 && patch -p1 < A00-hb-qsv.patch \
 && ./configure --prefix=/usr --debug=$HANDBRAKE_DEBUG_MODE --disable-gtk-update-checks --enable-fdk-aac --enable-x265 --enable-qsv --launch-jobs=$( nproc ;) --launch \
 && /tmp/run_cmd -i 600 -m "HandBrake still compiling..." make --directory=build install \
 && cd .. \
 && if [ "${HANDBRAKE_DEBUG_MODE}" = "none" ] ; then find /usr/lib -type f -name "libva*.so*" -exec strip -s {} ';' ;find /opt/intel/mediasdk -type f -name "*.so*" -exec strip -s {} ';' ;strip -s /usr/bin/ghb ;strip -s /usr/bin/HandBrakeCLI ; fi \
 && del-pkg build-dependencies \
 && rm -r /usr/lib/libva*.la /opt/intel/mediasdk/include /opt/intel/mediasdk/lib64/pkgconfig /opt/intel/mediasdk/lib64/*.a /opt/intel/mediasdk/lib64/*.la /opt/intel/mediasdk/lib64/libmfx.* /usr/lib/pkgconfig /usr/include \
 && rm -rf /tmp/* /tmp/.[!.]*
#  Install dependencies.
RUN add-pkg gtk+3.0 libgudev dbus-glib libnotify libsamplerate libass jansson xz libtheora lame opus libvorbis speex libvpx libdvdcss librsvg adwaita-icon-theme lsscsi bash coreutils yad findutils expect
#  Adjust the openbox config.
RUN sed-patch 's/<application type="normal">/<application type="normal" title="HandBrake">/' /etc/xdg/openbox/rc.xml \
 && sed-patch '/<application type="normal" title="HandBrake">/a \ <layer>below</layer>' /etc/xdg/openbox/rc.xml
#  Generate and install favicons.
RUN APP_ICON_URL=https://raw.githubusercontent.com/jlesage/docker-templates/master/jlesage/images/handbrake-icon.png \
 && install_app_icon.sh "$APP_ICON_URL"
#  Add files.
COPY rootfs/ /
#  Set environment variables.
ENV APP_NAME="HandBrake" \
    AUTOMATED_CONVERSION_PRESET="Very Fast 1080p30" \
    AUTOMATED_CONVERSION_FORMAT="mp4"
#  Define mountable directories.
VOLUME ["/config"]
VOLUME ["/storage"]
VOLUME ["/output"]
VOLUME ["/watch"]
#  Metadata.
LABEL org.label-schema.name="handbrake" \
      org.label-schema.description="Docker container for HandBrake" \
      org.label-schema.version="unknown" \
      org.label-schema.vcs-url="https://github.com/jlesage/docker-handbrake" \
      org.label-schema.schema-version="1.0"
