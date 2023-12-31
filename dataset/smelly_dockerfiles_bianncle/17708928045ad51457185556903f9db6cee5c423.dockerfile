# ########################################################################################
# #  Stage 1: build DALI dependencies
# #     DALI is based on "manylinux3", which is our modification of manylinux1
# #     (CentOS 5 derivative). For building this docker image, manylinux3 has to
# #     be manually built. It is also possible to use other distros, but we don't
# #     officially support them.
# #     For instructions, how to build manylinux3 with our patch see:
# #     //DALI/docker/build.sh#L16
# ########################################################################################
ARG FROM_IMAGE_NAME=gitlab-master.nvidia.com:5005/dl/dali/manylinux:manylinux3_x86_64
ARG USE_CUDA_VERSION=9
FROM ${FROM_IMAGE_NAME} AS base
#  CUDA
FROM base AS cuda-9
RUN CUDA_VERSION=9.0 \
 && CUDA_BUILD=9.0.176_384.81 \
 && curl -LO https://developer.nvidia.com/compute/cuda/${CUDA_VERSION}/Prod/local_installers/cuda_${CUDA_BUILD}_linux-run \
 && chmod +x cuda_${CUDA_BUILD}_linux-run \
 && ./cuda_${CUDA_BUILD}_linux-run --silent --no-opengl-libs --toolkit \
 && rm -f cuda_${CUDA_BUILD}_linux-run ; NVJPEG_VERSION=719-25900922 \
 && NVJPEG_BUILD=9.0.${NVJPEG_VERSION} \
 && curl -L https://developer.download.nvidia.com/compute/redist/libnvjpeg/cuda-linux64-nvjpeg-${NVJPEG_BUILD}.tar.gz | tar -xzf - \
 && cd /cuda-linux64-nvjpeg/ \
 && mv lib64/libnvjpeg*.a* /usr/local/cuda/lib64/ \
 && mv include/nvjpeg.h /usr/local/cuda/include/ \
 && rm -rf /cuda-linux64-nvjpeg
FROM base AS cuda-10
RUN CUDA_VERSION=10.0 \
 && CUDA_BUILD=10.0.130_410.48 \
 && curl -LO https://developer.nvidia.com/compute/cuda/${CUDA_VERSION}/Prod/local_installers/cuda_${CUDA_BUILD}_linux \
 && chmod +x cuda_${CUDA_BUILD}_linux \
 && ./cuda_${CUDA_BUILD}_linux --silent --no-opengl-libs --toolkit \
 && rm -f cuda_${CUDA_BUILD}_linux ; CUDA_VERSION=10.0 \
 && CUDA_PATCH=1 \
 && CUDA_BUILD=10.0.130.1 \
 && curl -LO https://developer.download.nvidia.com/compute/cuda/${CUDA_VERSION}/Prod/patches/${CUDA_PATCH}/cuda_${CUDA_BUILD}_linux.run \
 && chmod +x cuda_${CUDA_BUILD}_linux.run \
 && ./cuda_${CUDA_BUILD}_linux.run --silent --accept-eula --installdir=/usr/local/cuda-10.0 \
 && rm -f cuda_${CUDA_BUILD}_linux-run
FROM cuda-${USE_CUDA_VERSION} AS cuda
#  Actual image
FROM base
#  Install yum Dependencies
RUN yum install -y zip wget yasm doxygen graphviz
#  Don't want the short-unicode version for Python 2.7
RUN rm -f /opt/python/cp27-cp27m
#  Boost
RUN BOOST_VERSION=1.66.0 \
 && cd /usr/local \
 && curl -L https://dl.bintray.com/boostorg/release/1.66.0/source/boost_${BOOST_VERSION//./_}.tar.gz | tar -xzf - \
 && ln -s ../boost_${BOOST_VERSION//./_}/boost include/boost
#  CMake
RUN CMAKE_VERSION=3.11 \
 && CMAKE_BUILD=3.11.0 \
 && curl -L https://cmake.org/files/v${CMAKE_VERSION}/cmake-${CMAKE_BUILD}.tar.gz | tar -xzf - \
 && cd /cmake-${CMAKE_BUILD} \
 && ./bootstrap --parallel=$( grep ^processor /proc/cpuinfo | wc -l ;) \
 && make -j"$( grep ^processor /proc/cpuinfo | wc -l ;)" install \
 && rm -rf /cmake-${CMAKE_BUILD}
#  protobuf v3.5.1
RUN PROTOBUF_VERSION=3.5.1 \
 && curl -L https://github.com/google/protobuf/releases/download/v${PROTOBUF_VERSION}/protobuf-all-${PROTOBUF_VERSION}.tar.gz | tar -xzf - \
 && cd /protobuf-${PROTOBUF_VERSION} \
 && ./autogen.sh \
 && ./configure CXXFLAGS="-fPIC" --prefix=/usr/local --disable-shared 2>&1 > /dev/null \
 && make -j"$( grep ^processor /proc/cpuinfo | wc -l ;)" install 2>&1 > /dev/null \
 && rm -rf /protobuf-${PROTOBUF_VERSION}
#  LMDB
COPY docker/Makefile-lmdb.patch /tmp
RUN LMDB_VERSION=0.9.22 \
 && git clone -b LMDB_${LMDB_VERSION} --single-branch https://github.com/LMDB/lmdb \
 && cd /lmdb/libraries/liblmdb \
 && patch -p3 < /tmp/Makefile-lmdb.patch \
 && rm -f /tmp/Makefile-lmdb.patch \
 && make -j"$( grep ^processor /proc/cpuinfo | wc -l ;)" install \
 && rm -rf /lmdb
#  libjpeg-turbo
RUN JPEG_TURBO_VERSION=2.0.2 \
 && curl -L https://github.com/libjpeg-turbo/libjpeg-turbo/archive/${JPEG_TURBO_VERSION}.tar.gz | tar -xzf - \
 && cd libjpeg-turbo-${JPEG_TURBO_VERSION} \
 && cmake -G"Unix Makefiles" -DENABLE_SHARED=TRUE -DCMAKE_INSTALL_PREFIX=/usr/local . 2>&1 > /dev/null \
 && make -j"$( grep ^processor /proc/cpuinfo | wc -l ;)" install 2>&1 > /dev/null \
 && rm -rf /libjpeg-turbo-${JPEG_TURBO_VERSION}
#  OpenCV
RUN OPENCV_VERSION=3.4.3 \
 && curl -L https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.tar.gz | tar -xzf - \
 && cd /opencv-${OPENCV_VERSION} \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=/usr/local -DBUILD_SHARED_LIBS=OFF -DWITH_CUDA=OFF -DWITH_1394=OFF -DWITH_IPP=OFF -DWITH_OPENCL=OFF -DWITH_GTK=OFF -DBUILD_JPEG=OFF -DWITH_JPEG=ON -DBUILD_DOCS=OFF -DBUILD_TESTS=OFF -DBUILD_PERF_TESTS=OFF -DBUILD_PNG=ON -DBUILD_opencv_cudalegacy=OFF -DBUILD_opencv_stitching=OFF -DWITH_TBB=OFF -DWITH_OPENMP=OFF -DWITH_PTHREADS_PF=OFF -DWITH_CSTRIPES=OFF .. \
 && make -j"$( grep ^processor /proc/cpuinfo | wc -l ;)" install \
 && rm -rf /opencv-${OPENCV_VERSION}
#  Clang
RUN CLANG_VERSION=6.0.1 \
 && cd /usr/local \
 && wget http://releases.llvm.org/${CLANG_VERSION}/clang+llvm-${CLANG_VERSION}-x86_64-linux-sles11.3.tar.xz \
 && tar -xJf clang+llvm-${CLANG_VERSION}-x86_64-linux-sles11.3.tar.xz --strip 1 \
 && rm clang+llvm-${CLANG_VERSION}-x86_64-linux-sles11.3.tar.xz
ENV NVIDIA_DRIVER_CAPABILITIES="video,compute,utility"
#  FFmpeg
RUN FFMPEG_VERSION=3.4.2 \
 && cd /tmp \
 && wget https://developer.download.nvidia.com/compute/redist/nvidia-dali/ffmpeg-${FFMPEG_VERSION}.tar.bz2 \
 && tar xf ffmpeg-$FFMPEG_VERSION.tar.bz2 \
 && rm ffmpeg-$FFMPEG_VERSION.tar.bz2 \
 && cd ffmpeg-$FFMPEG_VERSION \
 && ./configure --prefix=/usr/local --disable-static --disable-all --disable-autodetect --disable-iconv --enable-shared --enable-avformat --enable-avcodec --enable-avfilter --enable-protocol=file --enable-demuxer=mov,matroska --enable-bsf=h264_mp4toannexb,hevc_mp4toannexb \
 && make -j"$( grep ^processor /proc/cpuinfo | wc -l ;)" \
 && make install \
 && cd /tmp \
 && rm -rf ffmpeg-$FFMPEG_VERSION
#  CUDA
COPY --from=cuda /usr/local/cuda /usr/local/cuda
