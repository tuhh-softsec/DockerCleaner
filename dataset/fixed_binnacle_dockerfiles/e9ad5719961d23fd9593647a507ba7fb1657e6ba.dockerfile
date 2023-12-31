FROM ubuntu:18.04 AS build
WORKDIR /home
#   COMMON BUILD TOOLS
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 autoconf=2.69-11 make=4.1-9.1ubuntu1 git=1:2.17.1-1ubuntu0.17 wget=1.19.4-1ubuntu2.2 pciutils=1:3.5.2-1ubuntu1.1 cpio=2.12+dfsg-6ubuntu0.18.04.4 libtool=2.4.6-2 lsb-release=9.20170808ubuntu1 ca-certificates=20211016ubuntu0.18.04.1 pkg-config=0.29.1-0ubuntu2 bison=2:3.0.4.dfsg-1build1 flex=2.6.4-6 libcurl4-gnutls-dev=7.58.0-2ubuntu3.24 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y -q
#   Install cmake
ARG CMAKE_VER=3.13.1
ARG CMAKE_REPO=https://cmake.org/files
RUN wget -O - ${CMAKE_REPO}/v${CMAKE_VER%.*}/cmake-${CMAKE_VER}.tar.gz | tar xz \
 && cd cmake-${CMAKE_VER} \
 && ./bootstrap --prefix="/usr" --system-curl \
 && make -j8 \
 && make install
#   Install automake, use version 1.14 on CentOS
ARG AUTOMAKE_VER=1.14
ARG AUTOMAKE_REPO=https://ftp.gnu.org/pub/gnu/automake/automake-${AUTOMAKE_VER}.tar.xz
RUN apt-get install --no-install-recommends automake=1:1.15.1-3ubuntu2 -y -q
#   Build NASM
ARG NASM_VER=2.13.03
ARG NASM_REPO=https://www.nasm.us/pub/nasm/releasebuilds/${NASM_VER}/nasm-${NASM_VER}.tar.bz2
RUN wget ${NASM_REPO} \
 && tar -xaf nasm* \
 && cd nasm-${NASM_VER} \
 && ./autogen.sh \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu \
 && make -j8 \
 && make install
#   Build YASM
ARG YASM_VER=1.3.0
ARG YASM_REPO=https://www.tortall.net/projects/yasm/releases/yasm-${YASM_VER}.tar.gz
RUN wget -O - ${YASM_REPO} | tar xz \
 && cd yasm-${YASM_VER} \
 && sed -i "s/) ytasm.*/)/" Makefile.in \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu \
 && make -j8 \
 && make install
#   Build ogg
ARG OGG_VER=1.3.3
ARG OGG_REPO=https://downloads.xiph.org/releases/ogg/libogg-${OGG_VER}.tar.xz
RUN wget -O - ${OGG_REPO} | tar xJ \
 && cd libogg-${OGG_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu --enable-shared \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#   Build vorbis
ARG VORBIS_VER=1.3.6
ARG VORBIS_REPO=https://downloads.xiph.org/releases/vorbis/libvorbis-${VORBIS_VER}.tar.xz
RUN wget -O - ${VORBIS_REPO} | tar xJ \
 && cd libvorbis-${VORBIS_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu --enable-shared \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#   Build mp3lame
ARG MP3LAME_VER=3.100
ARG MP3LAME_REPO=https://sourceforge.net/projects/lame/files/lame/${MP3LAME_VER}/lame-${MP3LAME_VER}.tar.gz
RUN wget -O - ${MP3LAME_REPO} | tar xz \
 && cd lame-${MP3LAME_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu --enable-shared --enable-nasm \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#   Build fdk-aac
ARG FDK_AAC_VER=v0.1.6
ARG FDK_AAC_REPO=https://github.com/mstorsjo/fdk-aac/archive/${FDK_AAC_VER}.tar.gz
RUN wget -O - ${FDK_AAC_REPO} | tar xz \
 && mv fdk-aac-${FDK_AAC_VER#v} fdk-aac \
 && cd fdk-aac \
 && autoreconf -fiv \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu --enable-shared \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#   Build opus
ARG OPUS_VER=1.2.1
ARG OPUS_REPO=https://archive.mozilla.org/pub/opus/opus-${OPUS_VER}.tar.gz
RUN wget -O - ${OPUS_REPO} | tar xz \
 && cd opus-${OPUS_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu --enable-shared \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#   Build vpx
ARG VPX_VER=tags/v1.7.0
ARG VPX_REPO=https://chromium.googlesource.com/webm/libvpx.git
RUN git clone ${VPX_REPO} \
 && cd libvpx \
 && git checkout ${VPX_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu --enable-shared --disable-examples --disable-unit-tests --enable-vp9-highbitdepth --as=nasm \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#   Build AOM
ARG AOM_VER=b6f1767eedbaddeb1ff5aa409a710ef61078640e
ARG AOM_REPO=https://aomedia.googlesource.com/aom
RUN git clone ${AOM_REPO} \
 && mkdir aom/aom_build \
 && cd aom/aom_build \
 && git checkout ${AOM_VER} \
 && cmake -DBUILD_SHARED_LIBS=ON -DENABLE_NASM=ON -DENABLE_TESTS=OFF -DENABLE_DOCS=OFF -DCMAKE_INSTALL_PREFIX="/usr" -DLIB_INSTALL_DIR=/usr/lib/x86_64-linux-gnu .. \
 && make -j8 \
 && make install DESTDIR="/home/build" \
 && make install
#   Build x264
ARG X264_VER=stable
ARG X264_REPO=https://github.com/mirror/x264
RUN git clone ${X264_REPO} \
 && cd x264 \
 && git checkout ${X264_VER} \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu --enable-shared \
 && make -j8 \
 && make install DESTDIR="/home/build" \
 && make install
#   Build x265
ARG X265_VER=2.9
ARG X265_REPO=https://github.com/videolan/x265/archive/${X265_VER}.tar.gz
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libnuma-dev=2.0.11-2.1ubuntu0.1 -y -q
RUN wget -O - ${X265_REPO} | tar xz \
 && mv x265-${X265_VER} x265 \
 && cd x265/build/linux \
 && cmake -DBUILD_SHARED_LIBS=ON -DENABLE_TESTS=OFF -DCMAKE_INSTALL_PREFIX=/usr -DLIB_INSTALL_DIR=/usr/lib/x86_64-linux-gnu ../../source \
 && make -j8 \
 && make install DESTDIR="/home/build" \
 && make install
#   Fetch SVT-HEVC
ARG SVT_HEVC_VER=20a47b0d904e9d99e089d93d7c33af92788cbfdb
ARG SVT_HEVC_REPO=https://github.com/intel/SVT-HEVC
RUN git clone ${SVT_HEVC_REPO} \
 && cd SVT-HEVC/Build/linux \
 && git checkout ${SVT_HEVC_VER} \
 && mkdir -p ../../Bin/Release \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=lib/x86_64-linux-gnu -DCMAKE_ASM_NASM_COMPILER=yasm ../.. \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#   Fetch SVT-AV1
ARG SVT_AV1_VER=v0.5.0
ARG SVT_AV1_REPO=https://github.com/OpenVisualCloud/SVT-AV1
RUN git clone ${SVT_AV1_REPO} \
 && cd SVT-AV1/Build/linux \
 && git checkout ${SVT_AV1_VER} \
 && mkdir -p ../../Bin/Release \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=lib/x86_64-linux-gnu -DCMAKE_ASM_NASM_COMPILER=yasm ../.. \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#  Remove build residue from SVT-AV1 build -- temp fix for bug
RUN if [ -d "build/home/" ] ; then rm -rf build/home/ ; fi
#   Fetch SVT-VP9
ARG SVT_VP9_VER=d18b4acf9139be2e83150e318ffd3dba1c432e74
ARG SVT_VP9_REPO=https://github.com/OpenVisualCloud/SVT-VP9
RUN git clone ${SVT_VP9_REPO} \
 && cd SVT-VP9/Build/linux \
 && git checkout ${SVT_VP9_VER} \
 && mkdir -p ../../Bin/Release \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=lib/x86_64-linux-gnu -DCMAKE_ASM_NASM_COMPILER=yasm ../.. \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#  include(transform360.m4)
#   Fetch gmmlib
ARG GMMLIB_VER=intel-gmmlib-18.3.0
ARG GMMLIB_REPO=https://github.com/intel/gmmlib/archive/${GMMLIB_VER}.tar.gz
RUN wget -O - ${GMMLIB_REPO} | tar xz \
 && mv gmmlib-${GMMLIB_VER} gmmlib
#   Build libva
ARG LIBVA_VER=2.4.0
ARG LIBVA_REPO=https://github.com/intel/libva/archive/${LIBVA_VER}.tar.gz
RUN apt-get remove libva*
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libdrm-dev=2.4.101-2~18.04.1 libx11-dev=2:1.6.4-3ubuntu0.4 xorg-dev=1:7.7+19ubuntu7.1 libgl1-mesa-dev=20.0.8-0ubuntu1~18.04.1 openbox=3.6.1-7ubuntu0.1 -y -q
RUN wget -O - ${LIBVA_REPO} | tar xz \
 && cd libva-${LIBVA_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib/x86_64-linux-gnu \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#   Build media driver
ARG MEDIA_DRIVER_VER=intel-media-kbl-19.1.0
ARG MEDIA_DRIVER_REPO=https://github.com/VCDP/media-driver/archive/${MEDIA_DRIVER_VER}.tar.gz
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libdrm-dev=2.4.101-2~18.04.1 libpciaccess-dev=0.14-1 libx11-dev=2:1.6.4-3ubuntu0.4 xorg-dev=1:7.7+19ubuntu7.1 libgl1-mesa-dev=20.0.8-0ubuntu1~18.04.1 -y -q
RUN wget -O - ${MEDIA_DRIVER_REPO} | tar xz \
 && mv media-driver-${MEDIA_DRIVER_VER} media-driver \
 && mkdir -p media-driver/build \
 && cd media-driver/build \
 && cmake -DBUILD_TYPE=release -DBUILD_ALONG_WITH_CMRTLIB=1 -DMEDIA_VERSION="2.0.0" -DBS_DIR_GMMLIB=/home/gmmlib/Source/GmmLib -DBS_DIR_COMMON=/home/gmmlib/Source/Common -DBS_DIR_INC=/home/gmmlib/Source/inc -DBS_DIR_MEDIA=/home/media-driver -Wno-dev -DCMAKE_INSTALL_PREFIX=/usr .. \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
#   Build Intel(R) Media SDK
ARG MSDK_VER=MSS-KBL-2019-R1
ARG MSDK_REPO=https://github.com/Intel-Media-SDK/MediaSDK/archive/${MSDK_VER}.tar.gz
RUN wget -O - ${MSDK_REPO} | tar xz \
 && mv MediaSDK-${MSDK_VER} MediaSDK \
 && mkdir -p MediaSDK/build \
 && cd MediaSDK/build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_INCLUDEDIR=include -DBUILD_SAMPLES=OFF -DENABLE_OPENCL=OFF -Wno-dev .. \
 && make -j8 \
 && make install DESTDIR=/home/build \
 && make install
ARG PYTHON_VER=3.6.6
ARG PYTHON_REPO=https://www.python.org/ftp/python/${PYTHON_VER}/Python-${PYTHON_VER}.tgz
RUN apt-get install --no-install-recommends python3=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-setuptools=39.0.1-2ubuntu0.1 python-yaml=3.12-1build2 -y
#   Build DLDT-Inference Engine
ARG DLDT_VER=2019_R1.1
ARG DLDT_REPO=https://github.com/opencv/dldt.git
ARG DLDT_C_API_1=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/thirdparty/0001-Add-inference-engine-C-API.patch
ARG DLDT_C_API_2=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/thirdparty/0002-Change-to-match-image-with-separate-planes.patch
ARG DLDT_C_API_3=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/thirdparty/0003-Refine-IE-C-API.patch
ARG DLDT_C_API_4=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/thirdparty/0004-Fix-code-style-and-symbols-visibility-for-2019R1.patch
RUN apt-get install --no-install-recommends libusb-1.0.0-dev -y
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
 && cmake -DCMAKE_INSTALL_PREFIX=/opt/intel/dldt -DLIB_INSTALL_PATH=/opt/intel/dldt -DENABLE_MKL_DNN=ON -DENABLE_CLDNN=ON -DENABLE_SAMPLES=OFF .. \
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
RUN for p in /usr /home/build/usr /opt/intel/dldt/inference-engine /home/build/opt/intel/dldt/inference-engine; do pkgconfiglibdir="$p/lib/x86_64-linux-gnu" \
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
#  install Model Optimizer in the DLDT for Dev
ARG PYTHON_TRUSTED_HOST
ARG PYTHON_TRUSTED_INDEX_URL
#  install MO dependencies
#  RUN pip3 install numpy scipy
RUN git clone https://github.com/google/protobuf.git \
 && cd protobuf \
 && git submodule update --init --recursive \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install \
 && make install DESTDIR=/home/build
#  RUN apt-get update && apt-get install -y sudo
#  installing dependency libs to mo_libs directory to avoid issues with updates to Python version
RUN cd dldt/model-optimizer \
 && if [ "x$PYTHON_TRUSTED_HOST" = "x" ] ; then pip3 install --target=/home/build/mo_libs -r requirements.txt \
 && pip3 install -r requirements.txt ; else pip3 install --target=/home/build/mo_libs -r requirements.txt -i $PYTHON_TRUSTED_INDEX_URL --trusted-host $PYTHON_TRUSTED_HOST \
 && pip3 install -r requirements.txt -i $PYTHON_TRUSTED_INDEX_URL --trusted-host $PYTHON_TRUSTED_HOST ; fi
#  Copy over Model Optimizer to same directory as Inference Engine
RUN cp -r dldt/model-optimizer /opt/intel/dldt/model-optimizer
RUN cp -r dldt/model-optimizer /home/build/opt/intel/dldt/model-optimizer
#   Build the gstremaer core
ARG GST_VER=1.16.0
ARG GST_REPO=https://gstreamer.freedesktop.org/src/gstreamer/gstreamer-${GST_VER}.tar.xz
RUN ln -sf /usr/share/zoneinfo/UTC /etc/localtime ; DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libglib2.0-dev=2.56.4-0ubuntu0.18.04.9 gobject-introspection=1.56.1-1 libgirepository1.0-dev=1.56.1-1 libpango-1.0-0=1.40.14-1ubuntu0.1 libpangocairo-1.0-0=1.40.14-1ubuntu0.1 autopoint=0.19.8.1-6ubuntu0.3 libcurl3-gnutls=7.58.0-2ubuntu3.24 -y -q
RUN wget -O - ${GST_REPO} | tar xJ \
 && cd gstreamer-${GST_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib/x86_64-linux-gnu --libexecdir=/usr/lib/x86_64-linux-gnu --enable-shared --enable-introspection --disable-examples --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#   Build the gstremaer plugin base
ARG GST_PLUGIN_BASE_REPO=https://gstreamer.freedesktop.org/src/gst-plugins-base/gst-plugins-base-${GST_VER}.tar.xz
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libxv-dev=2:1.0.11-1 libvisual-0.4-dev=0.4.0-11 libtheora-dev=1.1.1+dfsg.1-14 libglib2.0-dev=2.56.4-0ubuntu0.18.04.9 libasound2-dev=1.1.3-5ubuntu0.6 libcdparanoia-dev=3.10.2+debian-13 libgl1-mesa-dev=20.0.8-0ubuntu1~18.04.1 libpango1.0-dev=1.40.14-1ubuntu0.1 -y -q
RUN apt-get update \
 && apt-get install --no-install-recommends libxrandr-dev=2:1.5.1-1 libegl1-mesa-dev=20.0.8-0ubuntu1~18.04.1 autopoint=0.19.8.1-6ubuntu0.3 bison=2:3.0.4.dfsg-1build1 flex=2.6.4-6 libudev-dev=237-3ubuntu10.57 -y -q
RUN wget -O - ${GST_PLUGIN_BASE_REPO} | tar xJ \
 && cd gst-plugins-base-${GST_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib/x86_64-linux-gnu --libexecdir=/usr/lib/x86_64-linux-gnu --enable-introspection --enable-shared --disable-examples --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#   Build the gstremaer plugin good set
ARG GST_PLUGIN_GOOD_REPO=https://gstreamer.freedesktop.org/src/gst-plugins-good/gst-plugins-good-${GST_VER}.tar.xz
RUN apt-get update \
 && apt-get install --no-install-recommends libsoup2.4-dev=2.62.1-1ubuntu0.4 libjpeg-dev=8c-2ubuntu8 -y -q
RUN wget -O - ${GST_PLUGIN_GOOD_REPO} | tar xJ \
 && cd gst-plugins-good-${GST_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib/x86_64-linux-gnu --libexecdir=/usr/lib/x86_64-linux-gnu --enable-shared --disable-examples --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#   Build the gstremaer plugin bad set
ARG GST_PLUGIN_BAD_REPO=https://gstreamer.freedesktop.org/src/gst-plugins-bad/gst-plugins-bad-${GST_VER}.tar.xz
RUN apt-get update \
 && apt-get install --no-install-recommends libssl-dev=1.1.1-1ubuntu2.1~18.04.21 -y -q
RUN wget -O - ${GST_PLUGIN_BAD_REPO} | tar xJ \
 && cd gst-plugins-bad-${GST_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib/x86_64-linux-gnu --libexecdir=/usr/lib/x86_64-linux-gnu --enable-shared --disable-examples --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#   Build the gstremaer plugin ugly set
ARG GST_PLUGIN_UGLY_REPO=https://gstreamer.freedesktop.org/src/gst-plugins-ugly/gst-plugins-ugly-${GST_VER}.tar.xz
RUN wget -O - ${GST_PLUGIN_UGLY_REPO} | tar xJ ; cd gst-plugins-ugly-${GST_VER} ; ./autogen.sh --prefix=/usr --libdir=/usr/lib/x86_64-linux-gnu --libexecdir=/usr/lib/x86_64-linux-gnu --enable-shared --disable-examples --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#   Build gst-libav
ARG GST_PLUGIN_LIBAV_REPO=https://gstreamer.freedesktop.org/src/gst-libav/gst-libav-${GST_VER}.tar.xz
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 -y -q
RUN wget -O - ${GST_PLUGIN_LIBAV_REPO} | tar xJ \
 && cd gst-libav-${GST_VER} \
 && ./autogen.sh --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu --enable-shared --enable-gpl --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#   Build gstremaer plugin for svt
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
#   Build gstremaer plugin vaapi
ARG GST_PLUGIN_VAAPI_REPO=https://gstreamer.freedesktop.org/src/gstreamer-vaapi/gstreamer-vaapi-${GST_VER}.tar.xz
#   https://gitlab.freedesktop.org/gstreamer/gstreamer-vaapi/merge_requests/45
ARG GST_PLUGIN_VAAPI_REPO_DISPLAY_LOCK_PATCH_HASH=b219f6095f3014041896714dd88e7d90ee3d72dd
ARG GST_PLUGIN_VAAPI_REPO_GIT=https://gitlab.freedesktop.org/gstreamer/gstreamer-vaapi.git
RUN apt-get update \
 && apt-get install --no-install-recommends libxrandr-dev=2:1.5.1-1 libegl1-mesa-dev=20.0.8-0ubuntu1~18.04.1 autopoint=0.19.8.1-6ubuntu0.3 bison=2:3.0.4.dfsg-1build1 flex=2.6.4-6 libudev-dev=237-3ubuntu10.57 -y -q
#  RUN  git clone https://gitlab.freedesktop.org/gstreamer/gstreamer-vaapi.git -b 1.14 --depth 10 && \
#       cd gstreamer-vaapi && git reset --hard ${GST_PLUGIN_VAAPI_REPO_DISPLAY_LOCK_PATCH_HASH} && \
RUN wget -O - ${GST_PLUGIN_VAAPI_REPO} | tar xJ \
 && cd gstreamer-vaapi-${GST_VER} \
 && ./autogen.sh --prefix=/usr --libdir=/usr/lib/x86_64-linux-gnu --libexecdir=/usr/lib/x86_64-linux-gnu --enable-shared --disable-examples --disable-gtk-doc \
 && make -j $( nproc ;) \
 && make install DESTDIR=/home/build \
 && make install
#   Fetch FFmpeg source
ARG FFMPEG_VER=n4.1
ARG FFMPEG_REPO=https://github.com/FFmpeg/FFmpeg/archive/${FFMPEG_VER}.tar.gz
ARG FFMPEG_FLV_PATCH_REPO=https://raw.githubusercontent.com/VCDP/CDN/master/The-RTMP-protocol-extensions-for-H.265-HEVC.patch
ARG FFMPEG_1TN_PATCH_REPO=https://patchwork.ffmpeg.org/patch/11625/raw
ARG FFMPEG_THREAD_PATCH_REPO=https://patchwork.ffmpeg.org/patch/11035/raw
ARG FFMPEG_MA_PATCH_REPO_01=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0001-Intel-inference-engine-detection-filter.patch
ARG FFMPEG_MA_PATCH_REPO_02=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0002-New-filter-to-do-inference-classify.patch
ARG FFMPEG_MA_PATCH_REPO_03=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0003-iemetadata-convertor-muxer.patch
ARG FFMPEG_MA_PATCH_REPO_04=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0004-Kafka-protocol-producer.patch
ARG FFMPEG_MA_PATCH_REPO_05=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0005-Support-object-detection-and-featured-face-identific.patch
ARG FFMPEG_MA_PATCH_REPO_06=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0006-Send-metadata-in-a-packet-and-refine-the-json-format.patch
ARG FFMPEG_MA_PATCH_REPO_07=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0007-Refine-features-of-IE-filters.patch
ARG FFMPEG_MA_PATCH_REPO_08=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0008-fixed-extra-comma-in-iemetadata.patch
ARG FFMPEG_MA_PATCH_REPO_09=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0009-add-source-as-option-source-url-calculate-nano-times.patch
ARG FFMPEG_MA_PATCH_REPO_10=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0010-fixed-buffer-overflow-issue-in-iemetadata.patch
ARG FFMPEG_MA_PATCH_REPO_11=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0011-libavutil-add-RGBP-pixel-format.patch
ARG FFMPEG_MA_PATCH_REPO_12=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0012-Add-more-devices-into-target.patch
ARG FFMPEG_MA_PATCH_REPO_13=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0013-Enable-vaapi-scale-for-IE-inference-filter.patch
ARG FFMPEG_MA_PATCH_REPO_14=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0014-iemetadata-it-will-provide-data-frame-by-frame-by-de.patch
ARG FFMPEG_MA_PATCH_REPO_15=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0015-Add-libcjson-for-model-pre-post-processing.patch
ARG FFMPEG_MA_PATCH_REPO_16=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0016-Change-IE-filters-to-use-model-proc.patch
ARG FFMPEG_MA_PATCH_REPO_17=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0017-refine-total-fps-without-init-filter-and-add-decode-.patch
ARG FFMPEG_MA_PATCH_REPO_18=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0018-Bugs-fixing.patch
ARG FFMPEG_MA_PATCH_REPO_19=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0019-Face-reidentification-refine.patch
ARG FFMPEG_MA_PATCH_REPO_20=https://raw.githubusercontent.com/VCDP/FFmpeg-patch/master/media-analytics/0020-More-changes-within-one-patch.patch
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libass-dev=1:0.14.0-1 libfreetype6-dev=2.8.1-2ubuntu2.2 libvdpau-dev=1.1.1-3ubuntu1 libsdl2-dev=2.0.8+dfsg1-1ubuntu1.18.04.4 libxcb1-dev=1.13-2~ubuntu18.04 libxcb-shm0-dev=1.13-2~ubuntu18.04 libxcb-xfixes0-dev=1.13-2~ubuntu18.04 texinfo=6.5.0.dfsg.1-2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 -y -q
RUN wget -O - ${FFMPEG_REPO} | tar xz \
 && mv FFmpeg-${FFMPEG_VER} FFmpeg \
 && cd FFmpeg \
 && wget -O - ${FFMPEG_FLV_PATCH_REPO} | patch -p1 \
 && wget -O - ${FFMPEG_1TN_PATCH_REPO} | patch -p1 \
 && wget -O - ${FFMPEG_THREAD_PATCH_REPO} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_01} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_02} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_03} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_04} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_05} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_06} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_07} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_08} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_09} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_10} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_11} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_12} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_13} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_14} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_15} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_16} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_17} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_18} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_19} | patch -p1 \
 && wget -O - ${FFMPEG_MA_PATCH_REPO_20} | patch -p1
#   Patch FFmpeg source for SVT-HEVC
RUN cd /home/FFmpeg \
 && patch -p1 < ../SVT-HEVC/ffmpeg_plugin/0001-lavc-svt_hevc-add-libsvt-hevc-encoder-wrapper.patch
#   Patch FFmpeg source for SVT-AV1
RUN cd /home/FFmpeg ; patch -p1 < ../SVT-AV1/ffmpeg_plugin/0001-Add-ability-for-ffmpeg-to-run-svt-av1-with-svt-hevc.patch
#   Compile FFmpeg
RUN cd /home/FFmpeg \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu --extra-libs="-lpthread -lm" --enable-shared --enable-gpl --enable-libass --enable-libfreetype --enable-openssl --enable-nonfree --enable-libdrm --enable-libmfx --enable-libfdk-aac --enable-libmp3lame --enable-libopus --enable-libvorbis --enable-libvpx --enable-libx264 --enable-libx265 --enable-libaom --enable-libsvthevc --enable-libsvtav1 --enable-libinference_engine \
 && make -j8 \
 && make install DESTDIR="/home/build"
FROM ubuntu:18.04
LABEL Description="This is the image for FFMPEG & GSTREAMER application development on Ubuntu 18.04"
LABEL Vendor="Intel Corporation"
WORKDIR /home
#   Prerequisites
RUN ln -sf /usr/share/zoneinfo/UTC /etc/localtime ; DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libxv1=2:1.0.11-1 libxcb-shm0=1.13-2~ubuntu18.04 libxcb-shape0=1.13-2~ubuntu18.04 libxcb-xfixes0=1.13-2~ubuntu18.04 libsdl2-2.0-0=2.0.8+dfsg1-1ubuntu1.18.04.4 libasound2=1.1.3-5ubuntu0.6 libvdpau1=1.1.1-3ubuntu1 libnuma1=2.0.11-2.1ubuntu0.1 libass9=1:0.14.0-1 libssl1.1=1.1.1-1ubuntu2.1~18.04.21 libpciaccess0=0.14-1 libglib2.0 libpango-1.0-0=1.40.14-1ubuntu0.1 libpangocairo-1.0-0=1.40.14-1ubuntu0.1 gobject-introspection=1.56.1-1 libcurl3-gnutls=7.58.0-2ubuntu3.24 libdrm-intel1=2.4.101-2~18.04.1 libudev1=237-3ubuntu10.57 libx11-xcb1=2:1.6.4-3ubuntu0.4 libgl1-mesa-glx=20.0.8-0ubuntu1~18.04.1 libxrandr2=2:1.5.1-1 libegl1-mesa=20.0.8-0ubuntu1~18.04.1 libglib2.0-0=2.56.4-0ubuntu0.18.04.9 libpng16-16=1.6.34-1ubuntu0.18.04.2 libxv1=2:1.0.11-1 libvisual-0.4-0=0.4.0-11 libgl1-mesa-glx=20.0.8-0ubuntu1~18.04.1 libpango-1.0-0=1.40.14-1ubuntu0.1 libtheora0=1.1.1+dfsg.1-14 libcdparanoia0=3.10.2+debian-13 libasound2=1.1.3-5ubuntu0.6 libsoup2.4-1=2.62.1-1ubuntu0.4 libjpeg8=8c-2ubuntu8 libjpeg-turbo8=1.5.2-0ubuntu5.18.04.6 python3=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python-yaml=3.12-1build2 libdrm2=2.4.101-2~18.04.1 -y -q ; rm -rf /var/lib/apt/lists/*
#   Install
COPY --from=build /home/build /
ENV LIBVA_DRIVERS_PATH="/usr/lib/x86_64-linux-gnu/dri"
ENV LIBVA_DRIVER_NAME="iHD"
ENV GST_VAAPI_ALL_DRIVERS="1"
ENV DISPLAY=":0.0"
ARG libdir=/opt/intel/dldt/inference-engine/lib/intel64
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/intel/dldt/inference-engine/lib:/opt/intel/dldt/inference-engine/external/tbb/lib:${libdir}"
ENV InferenceEngine_DIR="/opt/intel/dldt/inference-engine/share"
ENV PYTHONPATH="${PYTHONPATH}:/mo_libs"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
