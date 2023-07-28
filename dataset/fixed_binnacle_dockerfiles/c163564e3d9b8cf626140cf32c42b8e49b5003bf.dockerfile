FROM ubuntu:18.04
ENV DEBIAN_FRONTEND="noninteractive"
#   install dependencies via apt
ENV DEBCONF_NOWARNINGS="yes"
RUN set -x \
 && apt-get update -y -qq \
 && apt-get upgrade -y -qq --no-install-recommends \
 && : "basic dependencies" \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 pkg-config=0.29.1-0ubuntu2 cmake=3.10.2-1ubuntu2.18.04.2 git=1:2.17.1-1ubuntu0.17 wget=1.19.4-1ubuntu2.2 curl=7.58.0-2ubuntu3.24 tar=1.29b-2ubuntu0.4 unzip=6.0-21ubuntu1.2 -y -qq \
 && : "g2o dependencies" \
 && apt-get install --no-install-recommends libgoogle-glog-dev=0.3.5-1 libatlas-base-dev=3.10.3-5 libsuitesparse-dev=1:5.1.2-2 -y -qq \
 && : "OpenCV dependencies" \
 && apt-get install --no-install-recommends libgtk-3-dev=3.22.30-1ubuntu4 libjpeg-dev=8c-2ubuntu8 libpng++-dev=0.2.9-1 libtiff-dev=4.0.9-5ubuntu0.10 libopenexr-dev=2.2.0-11.1ubuntu1.9 libwebp-dev=0.6.1-2ubuntu0.18.04.1 ffmpeg=7:3.4.11-0ubuntu0.1 libavcodec-dev=7:3.4.11-0ubuntu0.1 libavformat-dev=7:3.4.11-0ubuntu0.1 libavutil-dev=7:3.4.11-0ubuntu0.1 libswscale-dev=7:3.4.11-0ubuntu0.1 libavresample-dev=7:3.4.11-0ubuntu0.1 -y -qq \
 && : "Pangolin dependencies" \
 && apt-get install --no-install-recommends libglew-dev=2.0.0-5 -y -qq \
 && : "other dependencies" \
 && apt-get install --no-install-recommends libyaml-cpp-dev=0.5.2-4ubuntu1 -y -qq \
 && : "remove cache" \
 && apt-get autoremove -y -qq \
 && rm -rf /var/lib/apt/lists/*
ARG CMAKE_INSTALL_PREFIX=/usr/local
ARG NUM_THREADS=1
ENV CPATH="${CMAKE_INSTALL_PREFIX}/include:${CPATH}"
ENV C_INCLUDE_PATH="${CMAKE_INSTALL_PREFIX}/include:${C_INCLUDE_PATH}"
ENV CPLUS_INCLUDE_PATH="${CMAKE_INSTALL_PREFIX}/include:${CPLUS_INCLUDE_PATH}"
ENV LIBRARY_PATH="${CMAKE_INSTALL_PREFIX}/lib:${LIBRARY_PATH}"
ENV LD_LIBRARY_PATH="${CMAKE_INSTALL_PREFIX}/lib:${LD_LIBRARY_PATH}"
ENV NVIDIA_VISIBLE_DEVICES="${NVIDIA_VISIBLE_DEVICES:-all}"
ENV NVIDIA_DRIVER_CAPABILITIES="${NVIDIA_DRIVER_CAPABILITIES:+$NVIDIA_DRIVER_CAPABILITIES,}graphics"
#   Eigen
ARG EIGEN3_VERSION=3.3.7
WORKDIR /tmp
RUN set -x \
 && wget -q http://bitbucket.org/eigen/eigen/get/${EIGEN3_VERSION}.tar.bz2 \
 && tar xf ${EIGEN3_VERSION}.tar.bz2 \
 && rm -rf ${EIGEN3_VERSION}.tar.bz2 \
 && mv eigen-eigen-* eigen-${EIGEN3_VERSION} \
 && cd eigen-${EIGEN3_VERSION} \
 && mkdir -p build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX} .. \
 && make -j${NUM_THREADS} \
 && make install \
 && cd /tmp \
 && rm -rf *
ENV Eigen3_DIR="${CMAKE_INSTALL_PREFIX}/share/eigen3/cmake"
#   g2o
ARG G2O_COMMIT=9b41a4ea5ade8e1250b9c1b279f3a9c098811b5a
WORKDIR /tmp
RUN set -x \
 && git clone https://github.com/RainerKuemmerle/g2o.git \
 && cd g2o \
 && git checkout ${G2O_COMMIT} \
 && mkdir -p build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX} -DBUILD_SHARED_LIBS=ON -DBUILD_UNITTESTS=OFF -DBUILD_WITH_MARCH_NATIVE=ON -DG2O_USE_CHOLMOD=ON -DG2O_USE_CSPARSE=ON -DG2O_USE_OPENGL=OFF -DG2O_USE_OPENMP=ON -DG2O_BUILD_APPS=OFF -DG2O_BUILD_EXAMPLES=OFF -DG2O_BUILD_LINKED_APPS=OFF .. \
 && make -j${NUM_THREADS} \
 && make install \
 && cd /tmp \
 && rm -rf *
ENV G2O_ROOT="${CMAKE_INSTALL_PREFIX}"
#   OpenCV
ARG OPENCV_VERSION=4.1.0
WORKDIR /tmp
RUN set -x \
 && wget -q https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.zip \
 && unzip -q ${OPENCV_VERSION}.zip \
 && rm -rf ${OPENCV_VERSION}.zip \
 && cd opencv-${OPENCV_VERSION} \
 && mkdir -p build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX} -DBUILD_DOCS=OFF -DBUILD_EXAMPLES=OFF -DBUILD_JASPER=OFF -DBUILD_OPENEXR=OFF -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DBUILD_opencv_apps=OFF -DBUILD_opencv_dnn=OFF -DBUILD_opencv_ml=OFF -DBUILD_opencv_python_bindings_generator=OFF -DENABLE_CXX11=ON -DENABLE_FAST_MATH=ON -DWITH_EIGEN=ON -DWITH_FFMPEG=ON -DWITH_OPENMP=ON .. \
 && make -j${NUM_THREADS} \
 && make install \
 && cd /tmp \
 && rm -rf *
ENV OpenCV_DIR="${CMAKE_INSTALL_PREFIX}/lib/cmake/opencv4"
#   DBoW2
ARG DBOW2_COMMIT=687fcb74dd13717c46add667e3fbfa9828a7019f
WORKDIR /tmp
RUN set -x \
 && git clone https://github.com/shinsumicco/DBoW2.git \
 && cd DBoW2 \
 && git checkout ${DBOW2_COMMIT} \
 && mkdir -p build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX} .. \
 && make -j${NUM_THREADS} \
 && make install \
 && cd /tmp \
 && rm -rf *
ENV DBoW2_DIR="${CMAKE_INSTALL_PREFIX}/lib/cmake/DBoW2"
#   Pangolin
ARG PANGOLIN_COMMIT=ad8b5f83222291c51b4800d5a5873b0e90a0cf81
WORKDIR /tmp
RUN set -x \
 && git clone https://github.com/stevenlovegrove/Pangolin.git \
 && cd Pangolin \
 && git checkout ${PANGOLIN_COMMIT} \
 && sed -i -e "193,198d" ./src/utils/file_utils.cpp \
 && mkdir -p build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX} -DBUILD_EXAMPLES=OFF -DBUILD_PANGOLIN_DEPTHSENSE=OFF -DBUILD_PANGOLIN_FFMPEG=OFF -DBUILD_PANGOLIN_LIBDC1394=OFF -DBUILD_PANGOLIN_LIBJPEG=OFF -DBUILD_PANGOLIN_LIBOPENEXR=OFF -DBUILD_PANGOLIN_LIBPNG=OFF -DBUILD_PANGOLIN_LIBREALSENSE=OFF -DBUILD_PANGOLIN_LIBREALSENSE2=OFF -DBUILD_PANGOLIN_LIBTIFF=OFF -DBUILD_PANGOLIN_LIBUVC=OFF -DBUILD_PANGOLIN_LZ4=OFF -DBUILD_PANGOLIN_OPENNI=OFF -DBUILD_PANGOLIN_OPENNI2=OFF -DBUILD_PANGOLIN_PLEORA=OFF -DBUILD_PANGOLIN_PYTHON=OFF -DBUILD_PANGOLIN_TELICAM=OFF -DBUILD_PANGOLIN_TOON=OFF -DBUILD_PANGOLIN_UVC_MEDIAFOUNDATION=OFF -DBUILD_PANGOLIN_V4L=OFF -DBUILD_PANGOLIN_VIDEO=OFF -DBUILD_PANGOLIN_ZSTD=OFF -DBUILD_PYPANGOLIN_MODULE=OFF .. \
 && make -j${NUM_THREADS} \
 && make install \
 && cd /tmp \
 && rm -rf *
ENV Pangolin_DIR="${CMAKE_INSTALL_PREFIX}/lib/cmake/Pangolin"
#   OpenVSLAM
COPY . /openvslam/
WORKDIR /openvslam/
RUN set -x \
 && mkdir -p build \
 && cd build \
 && cmake -DBUILD_WITH_MARCH_NATIVE=ON -DUSE_PANGOLIN_VIEWER=ON -DUSE_SOCKET_PUBLISHER=OFF -DUSE_STACK_TRACE_LOGGER=ON -DBOW_FRAMEWORK=DBoW2 -DBUILD_TESTS=OFF .. \
 && make -j${NUM_THREADS} \
 && rm -rf CMakeCache.txt CMakeFiles Makefile cmake_install.cmake example src \
 && chmod -R 777 ./*
WORKDIR /openvslam/build/
ENTRYPOINT ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
