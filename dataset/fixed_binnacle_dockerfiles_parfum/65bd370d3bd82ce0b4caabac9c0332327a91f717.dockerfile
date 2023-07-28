FROM centos:7.4.1708 AS build
WORKDIR /home
#  COMMON BUILD TOOLS
RUN yum install -y -q bzip2 make autoconf libtool git wget ca-certificates pkg-config gcc gcc-c++ bison flex patch epel-release yum-devel libcurl-devel zlib-devel
#  Install cmake
ARG CMAKE_VER=3.13.1
ARG CMAKE_REPO=https://cmake.org/files
RUN wget -O - ${CMAKE_REPO}/v${CMAKE_VER%.*}/cmake-${CMAKE_VER}.tar.gz | tar xz \
 && cd cmake-${CMAKE_VER} \
 && ./bootstrap --prefix="/usr" --system-curl \
 && make -j8 \
 && make install
#  Install automake, use version 1.14 on CentOS
ARG AUTOMAKE_VER=1.14
ARG AUTOMAKE_REPO=https://ftp.gnu.org/pub/gnu/automake/automake-${AUTOMAKE_VER}.tar.xz
RUN wget -O - ${AUTOMAKE_REPO} | tar xJ \
 && cd automake-${AUTOMAKE_VER} \
 && ./configure --prefix=/usr --libdir=/usr/lib64 --disable-doc \
 && make -j8 \
 && make install
#  Build NASM
ARG NASM_VER=2.13.03
ARG NASM_REPO=https://www.nasm.us/pub/nasm/releasebuilds/${NASM_VER}/nasm-${NASM_VER}.tar.bz2
RUN wget ${NASM_REPO} \
 && tar -xaf nasm* \
 && cd nasm-${NASM_VER} \
 && ./autogen.sh \
 && ./configure --prefix="/usr" --libdir=/usr/lib64 \
 && make -j8 \
 && make install
#  Build YASM
ARG YASM_VER=1.3.0
ARG YASM_REPO=https://www.tortall.net/projects/yasm/releases/yasm-${YASM_VER}.tar.gz
RUN wget -O - ${YASM_REPO} | tar xz \
 && cd yasm-${YASM_VER} \
 && sed -i "s/) ytasm.*/)/" Makefile.in \
 && ./configure --prefix="/usr" --libdir=/usr/lib64 \
 && make -j8 \
 && make install
#  Build ogg
ARG OGG_VER=1.3.3
ARG OGG_REPO=https://downloads.xiph.org/releases/ogg/libogg-${OGG_VER}.tar.xz
RUN wget -O - ${OGG_REPO} | tar xJ \
 && cd libogg-${OGG_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib64 --enable-shared \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#  Build vorbis
ARG VORBIS_VER=1.3.6
ARG VORBIS_REPO=https://downloads.xiph.org/releases/vorbis/libvorbis-${VORBIS_VER}.tar.xz
RUN wget -O - ${VORBIS_REPO} | tar xJ \
 && cd libvorbis-${VORBIS_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib64 --enable-shared \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#  Build mp3lame
ARG MP3LAME_VER=3.100
ARG MP3LAME_REPO=https://sourceforge.net/projects/lame/files/lame/${MP3LAME_VER}/lame-${MP3LAME_VER}.tar.gz
RUN wget -O - ${MP3LAME_REPO} | tar xz \
 && cd lame-${MP3LAME_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib64 --enable-shared --enable-nasm \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#  Build fdk-aac
ARG FDK_AAC_VER=v0.1.6
ARG FDK_AAC_REPO=https://github.com/mstorsjo/fdk-aac/archive/${FDK_AAC_VER}.tar.gz
RUN wget -O - ${FDK_AAC_REPO} | tar xz \
 && mv fdk-aac-${FDK_AAC_VER#v} fdk-aac \
 && cd fdk-aac \
 && autoreconf -fiv \
 && ./configure --prefix="/usr" --libdir=/usr/lib64 --enable-shared \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#  Build opus
ARG OPUS_VER=1.2.1
ARG OPUS_REPO=https://archive.mozilla.org/pub/opus/opus-${OPUS_VER}.tar.gz
RUN wget -O - ${OPUS_REPO} | tar xz \
 && cd opus-${OPUS_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib64 --enable-shared \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#  Build vpx
ARG VPX_VER=tags/v1.7.0
ARG VPX_REPO=https://chromium.googlesource.com/webm/libvpx.git
RUN git clone ${VPX_REPO} \
 && cd libvpx \
 && git checkout ${VPX_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib64 --enable-shared --disable-examples --disable-unit-tests --enable-vp9-highbitdepth --as=nasm \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#  Build AOM
ARG AOM_VER=b6f1767eedbaddeb1ff5aa409a710ef61078640e
ARG AOM_REPO=https://aomedia.googlesource.com/aom
RUN git clone ${AOM_REPO} \
 && mkdir aom/aom_build \
 && cd aom/aom_build \
 && git checkout ${AOM_VER} \
 && cmake -DBUILD_SHARED_LIBS=ON -DENABLE_NASM=ON -DENABLE_TESTS=OFF -DENABLE_DOCS=OFF -DCMAKE_INSTALL_PREFIX="/usr" -DLIB_INSTALL_DIR=/usr/lib64 .. \
 && make -j8 \
 && make install DESTDIR="/home/build" \
 && make install
#  Build x264
ARG X264_VER=stable
ARG X264_REPO=https://github.com/mirror/x264
RUN git clone ${X264_REPO} \
 && cd x264 \
 && git checkout ${X264_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib64 --enable-shared \
 && make -j8 \
 && make install DESTDIR="/home/build" \
 && make install
#  Build x265
ARG X265_VER=2.9
ARG X265_REPO=https://github.com/videolan/x265/archive/${X265_VER}.tar.gz
RUN yum install -y -q numactl-devel libpciaccess-devel
RUN wget -O - ${X265_REPO} | tar xz \
 && mv x265-${X265_VER} x265 \
 && cd x265/build/linux \
 && cmake -DBUILD_SHARED_LIBS=ON -DENABLE_TESTS=OFF -DCMAKE_INSTALL_PREFIX=/usr -DLIB_INSTALL_DIR=/usr/lib64 ../../source \
 && make -j8 \
 && make install DESTDIR="/home/build" \
 && make install
#  Fetch SVT-HEVC
ARG SVT_HEVC_VER=20a47b0d904e9d99e089d93d7c33af92788cbfdb
ARG SVT_HEVC_REPO=https://github.com/intel/SVT-HEVC
RUN yum install -y -q patch centos-release-scl \
 && yum install -y -q devtoolset-7
RUN git clone ${SVT_HEVC_REPO} \
 && cd SVT-HEVC/Build/linux \
 && git checkout ${SVT_HEVC_VER} \
 && mkdir -p ../../Bin/Release \
 && (source /opt/rh/devtoolset-7/enable \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=lib64 -DCMAKE_ASM_NASM_COMPILER=yasm ../.. \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install )
#  Fetch SVT-AV1
ARG SVT_AV1_VER=v0.5.0
ARG SVT_AV1_REPO=https://github.com/OpenVisualCloud/SVT-AV1
RUN git clone ${SVT_AV1_REPO} \
 && cd SVT-AV1/Build/linux \
 && git checkout ${SVT_AV1_VER} \
 && mkdir -p ../../Bin/Release \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=lib64 -DCMAKE_ASM_NASM_COMPILER=yasm ../.. \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
# Remove build residue from SVT-AV1 build -- temp fix for bug
RUN if [ -d "build/home/" ] ; then rm -rf build/home/ ; fi
#  Fetch SVT-VP9
ARG SVT_VP9_VER=d18b4acf9139be2e83150e318ffd3dba1c432e74
ARG SVT_VP9_REPO=https://github.com/OpenVisualCloud/SVT-VP9
RUN git clone ${SVT_VP9_REPO} \
 && cd SVT-VP9/Build/linux \
 && git checkout ${SVT_VP9_VER} \
 && mkdir -p ../../Bin/Release \
 && (source /opt/rh/devtoolset-7/enable \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=lib64 -DCMAKE_ASM_NASM_COMPILER=yasm ../.. \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install )
#  Build DLDT-Inference Engine
ARG DLDT_VER=2019_R1.1
ARG DLDT_REPO=https://github.com/opencv/dldt.git
ARG DLDT_C_API_1=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/thirdparty/0001-Add-inference-engine-C-API.patch
ARG DLDT_C_API_2=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/thirdparty/0002-Change-to-match-image-with-separate-planes.patch
ARG DLDT_C_API_3=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/thirdparty/0003-Refine-IE-C-API.patch
ARG DLDT_C_API_4=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/thirdparty/0004-Fix-code-style-and-symbols-visibility-for-2019R1.patch
RUN yum install -y -q boost-devel glibc-static glibc-devel libstdc++-static libstdc++-devel libstdc++ libgcc libusbx-devel openblas-devel
RUN git clone -b ${DLDT_VER} ${DLDT_REPO} \
 && cd dldt \
 && git submodule init \
 && git submodule update --recursive \
 && cd inference-engine \
 && wget -O - ${DLDT_C_API_1} | patch -p2 \
 && wget -O - ${DLDT_C_API_2} | patch -p2 \
 && wget -O - ${DLDT_C_API_3} | patch -p2 \
 && wget -O - ${DLDT_C_API_4} | patch -p2 \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/opt/intel/dldt -DLIB_INSTALL_PATH=/opt/intel/dldt -DENABLE_MKL_DNN=ON -DENABLE_CLDNN=OFF -DENABLE_SAMPLES=OFF .. \
 && make -j $( nproc ;) \
 && rm -rf ../bin/intel64/Release/lib/libgtest* \
 && rm -rf ../bin/intel64/Release/lib/libgmock* \
 && rm -rf ../bin/intel64/Release/lib/libmock* \
 && rm -rf ../bin/intel64/Release/lib/libtest*
ARG libdir=/opt/intel/dldt/inference-engine/lib/intel64
RUN mkdir -p /opt/intel/dldt/inference-engine/include \
 && cp -r dldt/inference-engine/include/* /opt/intel/dldt/inference-engine/include \
 && mkdir -p ${libdir} \
 && cp -r dldt/inference-engine/bin/intel64/Release/lib/* ${libdir} \
 && mkdir -p /opt/intel/dldt/inference-engine/src \
 && cp -r dldt/inference-engine/src/* /opt/intel/dldt/inference-engine/src/ \
 && mkdir -p /opt/intel/dldt/inference-engine/share \
 && cp -r dldt/inference-engine/build/share/* /opt/intel/dldt/inference-engine/share/ \
 && mkdir -p /opt/intel/dldt/inference-engine/external/ \
 && cp -r dldt/inference-engine/temp/tbb /opt/intel/dldt/inference-engine/external/
RUN mkdir -p build/opt/intel/dldt/inference-engine/include \
 && cp -r dldt/inference-engine/include/* build/opt/intel/dldt/inference-engine/include \
 && mkdir -p build${libdir} \
 && cp -r dldt/inference-engine/bin/intel64/Release/lib/* build${libdir} \
 && mkdir -p build/opt/intel/dldt/inference-engine/src \
 && cp -r dldt/inference-engine/src/* build/opt/intel/dldt/inference-engine/src/ \
 && mkdir -p build/opt/intel/dldt/inference-engine/share \
 && cp -r dldt/inference-engine/build/share/* build/opt/intel/dldt/inference-engine/share/ \
 && mkdir -p build/opt/intel/dldt/inference-engine/external/ \
 && cp -r dldt/inference-engine/temp/tbb build/opt/intel/dldt/inference-engine/external/
RUN for p in /usr /home/build/usr /opt/intel/dldt/inference-engine /home/build/opt/intel/dldt/inference-engine; do pkgconfiglibdir="$p/lib64" \
 && mkdir -p "${pkgconfiglibdir}/pkgconfig" \
 && pc="${pkgconfiglibdir}/pkgconfig/dldt.pc" \
 && echo "prefix=/opt" > "$pc" \
 && echo "libdir=${libdir}" >> "$pc" \
 && echo "includedir=/opt/intel/dldt/inference-engine/include" >> "$pc" \
 && echo "" >> "$pc" \
 && echo "Name: DLDT" >> "$pc" \
 && echo "Description: Intel Deep Learning Deployment Toolkit" >> "$pc" \
 && echo "Version: 5.0" >> "$pc" \
 && echo "" >> "$pc" \
 && echo "Libs: -L${libdir} -linference_engine -linference_engine_c_wrapper" >> "$pc" \
 && echo "Cflags: -I${includedir}" >> "$pc"; done
ENV InferenceEngine_DIR="/opt/intel/dldt/inference-engine/share"
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/intel/dldt/inference-engine/lib:/opt/intel/dldt/inference-engine/external/tbb/lib:${libdir}"
# install Model Optimizer in the DLDT for Dev
#  Build the gstremaer core
ARG GST_VER=1.16.0
ARG GST_REPO=https://gstreamer.freedesktop.org/src/gstreamer/gstreamer-${GST_VER}.tar.xz
RUN yum install -y -q glib2-devel-2.56.1 gettext-devel gobject-introspection gobject-introspection-devel python-gobject-base
RUN wget -O - ${GST_REPO} | tar xJ \
 && cd gstreamer-${GST_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib64 --libexecdir=/usr/lib64 --enable-shared --enable-introspection --disable-examples --disable-debug --disable-benchmarks --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#  Build the gstremaer plugin bad set
ARG GST_ORC_VER=0.4.28
ARG GST_ORC_REPO=https://gstreamer.freedesktop.org/src/orc/orc-${GST_ORC_VER}.tar.xz
RUN wget -O - ${GST_ORC_REPO} | tar xJ \
 && cd orc-${GST_ORC_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib64 --libexecdir=/usr/lib64 --enable-shared --disable-examples --disable-debug --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#  Build the gstremaer plugin base
ARG GST_PLUGIN_BASE_REPO=https://gstreamer.freedesktop.org/src/gst-plugins-base/gst-plugins-base-${GST_VER}.tar.xz
RUN yum install -y -q libXv-devel libvisual-devel libtheora-devel glib2-devel alsa-lib-devel cdparanoia-devel mesa-libGL-devel pango-devel
RUN yum install -y -q libXrandr-devel
RUN wget -O - ${GST_PLUGIN_BASE_REPO} | tar xJ \
 && cd gst-plugins-base-${GST_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib64 --libexecdir=/usr/lib64 --enable-introspection --enable-shared --disable-examples --disable-debug --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#  Build the gstremaer plugin good set
ARG GST_PLUGIN_GOOD_REPO=https://gstreamer.freedesktop.org/src/gst-plugins-good/gst-plugins-good-${GST_VER}.tar.xz
RUN yum install -y -q libsoup-devel libjpeg-devel
RUN wget -O - ${GST_PLUGIN_GOOD_REPO} | tar xJ \
 && cd gst-plugins-good-${GST_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib64 --libexecdir=/usr/lib64 --enable-shared --disable-examples --disable-debug --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#  Build the gstremaer plugin bad set
ARG GST_PLUGIN_BAD_REPO=https://gstreamer.freedesktop.org/src/gst-plugins-bad/gst-plugins-bad-${GST_VER}.tar.xz
RUN yum install -y -q openssl-devel
RUN wget -O - ${GST_PLUGIN_BAD_REPO} | tar xJ \
 && cd gst-plugins-bad-${GST_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib64 --libexecdir=/usr/lib64 --enable-shared --disable-examples --disable-debug --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#  Build the gstremaer plugin ugly set
ARG GST_PLUGIN_UGLY_REPO=https://gstreamer.freedesktop.org/src/gst-plugins-ugly/gst-plugins-ugly-${GST_VER}.tar.xz
RUN wget -O - ${GST_PLUGIN_UGLY_REPO} | tar xJ ; cd gst-plugins-ugly-${GST_VER} ; ./autogen.sh --prefix=/usr --libdir=/usr/lib64 --libexecdir=/usr/lib64 --enable-shared --disable-examples --disable-debug --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#  Build gst-libav
ARG GST_PLUGIN_LIBAV_REPO=https://gstreamer.freedesktop.org/src/gst-libav/gst-libav-${GST_VER}.tar.xz
RUN yum install -y -q zlib-devel openssl-devel
RUN wget -O - ${GST_PLUGIN_LIBAV_REPO} | tar xJ \
 && cd gst-libav-${GST_VER} \
 && ./autogen.sh --prefix="/usr" --libdir=/usr/lib64 --enable-shared --enable-gpl --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#  Build gstremaer plugin for svt
RUN cd SVT-HEVC/gstreamer-plugin \
 && cmake . \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
RUN cd SVT-VP9/gstreamer-plugin \
 && cmake . \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
RUN cd SVT-AV1/gstreamer-plugin \
 && cmake . \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
ARG OPENCV_VER=4.1.0
ARG OPENCV_REPO=https://github.com/opencv/opencv/archive/${OPENCV_VER}.tar.gz
RUN wget ${OPENCV_REPO} \
 && tar -zxvf ${OPENCV_VER}.tar.gz \
 && cd opencv-${OPENCV_VER} \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local -D BUILD_EXAMPLES=OFF -D BUILD_PERF_TESTS=OFF -D BUILD_DOCS=OFF -D BUILD_TESTS=OFF .. \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
RUN yum install -y -q glib2-devel gtk-dock openblas
RUN yum install -y -q binutils
ARG PAHO_INSTALL=true
ARG PAHO_VER=1.3.0
ARG PAHO_REPO=https://github.com/eclipse/paho.mqtt.c/archive/v${PAHO_VER}.tar.gz
RUN if [ "$PAHO_INSTALL" = "true" ] ; then wget -O - ${PAHO_REPO} | tar -xz \
 && cd paho.mqtt.c-${PAHO_VER} \
 && make \
 && make install \
 && cp build/output/libpaho-mqtt3c.so.1.0 /home/build/usr/lib64/ \
 && cp build/output/libpaho-mqtt3cs.so.1.0 /home/build/usr/lib64/ \
 && cp build/output/libpaho-mqtt3a.so.1.0 /home/build/usr/lib64/ \
 && cp build/output/libpaho-mqtt3as.so.1.0 /home/build/usr/lib64/ \
 && cp build/output/paho_c_version /home/build/usr/bin/ \
 && cp build/output/samples/paho_c_pub /home/build/usr/bin/ \
 && cp build/output/samples/paho_c_sub /home/build/usr/bin/ \
 && cp build/output/samples/paho_cs_pub /home/build/usr/bin/ \
 && cp build/output/samples/paho_cs_sub /home/build/usr/bin/ \
 && chmod 644 /home/build/usr/lib64/libpaho-mqtt3c.so.1.0 \
 && chmod 644 /home/build/usr/lib64/libpaho-mqtt3cs.so.1.0 \
 && chmod 644 /home/build/usr/lib64/libpaho-mqtt3a.so.1.0 \
 && chmod 644 /home/build/usr/lib64/libpaho-mqtt3as.so.1.0 \
 && ln /home/build/usr/lib64/libpaho-mqtt3c.so.1.0 /home/build/usr/lib64/libpaho-mqtt3c.so.1 \
 && ln /home/build/usr/lib64/libpaho-mqtt3cs.so.1.0 /home/build/usr/lib64/libpaho-mqtt3cs.so.1 \
 && ln /home/build/usr/lib64/libpaho-mqtt3a.so.1.0 /home/build/usr/lib64/libpaho-mqtt3a.so.1 \
 && ln /home/build/usr/lib64/libpaho-mqtt3as.so.1.0 /home/build/usr/lib64/libpaho-mqtt3as.so.1 \
 && ln /home/build/usr/lib64/libpaho-mqtt3c.so.1 /home/build/usr/lib64/libpaho-mqtt3c.so \
 && ln /home/build/usr/lib64/libpaho-mqtt3cs.so.1 /home/build/usr/lib64/libpaho-mqtt3cs.so \
 && ln /home/build/usr/lib64/libpaho-mqtt3a.so.1 /home/build/usr/lib64/libpaho-mqtt3a.so \
 && ln /home/build/usr/lib64/libpaho-mqtt3as.so.1 /home/build/usr/lib64/libpaho-mqtt3as.so \
 && cp src/MQTTAsync.h /home/build/usr/include/ \
 && cp src/MQTTClient.h /home/build/usr/include/ \
 && cp src/MQTTClientPersistence.h /home/build/usr/include/ \
 && cp src/MQTTProperties.h /home/build/usr/include/ \
 && cp src/MQTTReasonCodes.h /home/build/usr/include/ \
 && cp src/MQTTSubscribeOpts.h /home/build/usr/include/ ; else echo "PAHO install disabled" ; fi
ARG RDKAFKA_INSTALL=true
ARG RDKAFKA_VER=1.0.0
ARG RDKAFKA_REPO=https://github.com/edenhill/librdkafka/archive/v${RDKAFKA_VER}.tar.gz
RUN if [ "$RDKAFKA_INSTALL" = "true" ] ; then wget -O - ${RDKAFKA_REPO} | tar -xz \
 && cd librdkafka-${RDKAFKA_VER} \
 && ./configure --prefix=/usr --libdir=/usr/lib64/ \
 && make \
 && make install \
 && make install DESTDIR=/home/build ; else echo "RDKAFKA install disabled" ; fi
# Install va gstreamer plugins
# Has a dependency on OpenCV, GStreamer
ARG VA_GSTREAMER_PLUGINS_VER=0.4
ARG VA_GSTREAMER_PLUGINS_REPO=https://github.com/opencv/gst-video-analytics/archive/v${VA_GSTREAMER_PLUGINS_VER}.tar.gz
RUN wget -O - ${VA_GSTREAMER_PLUGINS_REPO} | tar xz \
 && cd gst-video-analytics-${VA_GSTREAMER_PLUGINS_VER} \
 && wget -O - ${VA_GSTREAMER_PLUGINS_PATCH_01} | patch -p1 \
 && mkdir build \
 && cd build \
 && export CFLAGS="-std=gnu99 -Wno-missing-field-initializers" \
 && export CXXFLAGS="-std=c++11 -Wno-missing-field-initializers" \
 && cmake -DVERSION_PATCH=$( echo "$( git rev-list --count --first-parent HEAD ;)" ;) -DGIT_INFO=$( echo "git_$( git rev-parse --short HEAD ;)" ;) -DCMAKE_BUILD_TYPE=Release -DDISABLE_SAMPLES=ON -DMQTT=ON -DKAFKA=ON -DDISABLE_VAAPI=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_INSTALL_PREFIX=/usr .. \
 && make -j4
RUN mkdir -p build/usr/lib64/gstreamer-1.0 \
 && cp -r gst-video-analytics-${VA_GSTREAMER_PLUGINS_VER}/build/intel64/Release/lib/* build/usr/lib64/gstreamer-1.0
RUN mkdir -p /usr/lib64/gstreamer-1.0 \
 && cp -r gst-video-analytics-${VA_GSTREAMER_PLUGINS_VER}/build/intel64/Release/lib/* /usr/lib64/gstreamer-1.0
#  Clean up after build
RUN rm -rf /home/build/usr/include \
 && rm -rf /home/build/usr/share/doc \
 && rm -rf /home/build/usr/share/gtk-doc \
 && rm -rf /home/build/usr/share/man \
 && find /home/build -name "*.a" -exec rm -f {} ;
FROM centos:7.4.1708
LABEL Description="This is the image for DLDT and GSTREAMER on CentOS 7.4"
LABEL Vendor="Intel Corporation"
WORKDIR /home
#  Prerequisites
RUN yum install -y epel-release ; yum install -y -q numactl openssl glib2-2.56.1 pango gobject-introspection libpng12 libXv libvisual mesa-libGL pango glib2 alsa-lib openblas-serial libsoup libjpeg-turbo ; yum remove -y -q epel-release ; rm -rf /var/cache/yum/*
#  Install
COPY --from=build /home/build /
ARG libdir=/opt/intel/dldt/inference-engine/lib/intel64
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/intel/dldt/inference-engine/lib:/opt/intel/dldt/inference-engine/external/tbb/lib:${libdir}"
ENV InferenceEngine_DIR="/opt/intel/dldt/inference-engine/share"
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib64/gstreamer-1.0"
ENV PKG_CONFIG_PATH="/usr/lib64/pkgconfig"
ENV LIBRARY_PATH="${LIBRARY_PATH}:/usr/lib"
ENV PATH="${PATH}:/usr/bin"
