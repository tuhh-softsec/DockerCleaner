#   Copyright (c) 2017 Sony Corporation. All Rights Reserved.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM ubuntu:16.04
RUN apt-get update \
 && apt-get install --no-install-recommends bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 ccache=3.2.4-1 clang-format-3.8=1:3.8-2ubuntu4 curl=7.47.0-1ubuntu2.19 g++=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 libarchive-dev=3.1.2-11ubuntu0.16.04.8 libhdf5-dev=1.8.16+docs-4ubuntu1.1 make=4.1-6 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 zip=3.0-11 emacs=46.1 python-pip=8.1.1-2ubuntu0.6 -y \
 && cd / \
 && rm -rf /tmp/*
RUN pip install setuptools==67.6.1
RUN pip install pip==23.1 --upgrade
RUN pip install six==1.16.0
RUN pip install PyYAML==6.0
RUN pip install Mako==1.2.4
ENV BUILD_DIR="/usr/local/src"
#  #### Install CMAKE #####
ENV CMAKE_URL="https://cmake.org/files/v3.11/cmake-3.11.3.tar.gz"
RUN cd ${BUILD_DIR} \
 && curl -O ${CMAKE_URL} \
 && tar zxvf cmake*.tar.gz \
 && rm cmake*.tar.gz \
 && cd cmake* \
 && ./bootstrap \
 && make \
 && make install
#  #### Download and Install Android NDK #####
ENV NDK_NAME="android-ndk-r16b"
ENV NDK_URL="https://dl.google.com/android/repository/${NDK_NAME}-linux-x86_64.zip"
RUN cd ${BUILD_DIR} \
 && curl -O ${NDK_URL} \
 && unzip ${NDK_NAME}-linux-x86_64.zip \
 && rm -f ${NDK_NAME}-linux-x86_64.zip \
 && mv ${NDK_NAME} android-ndk
ENV NDK_PATH="${BUILD_DIR}/android-ndk"
ENV PROTOVER="3.1.0"
#  ################################################# protobuf
RUN mkdir /tmp/deps \
 && cd /tmp/deps \
 && curl -L https://github.com/google/protobuf/archive/v${PROTOVER}.tar.gz -o protobuf-v${PROTOVER}.tar.gz \
 && tar xvf protobuf-v${PROTOVER}.tar.gz \
 && cd protobuf-${PROTOVER} \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_POSITION_INDEPENDENT_CODE=ON -Dprotobuf_BUILD_TESTS=OFF ../cmake \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/*
RUN mkdir -p /usr/local/android
RUN chmod -R a+rw /usr/local/android
ARG ANDROID_PLATFORM
ARG ANDROID_ARCHITECTURE
ARG ANDROID_CMAKE_SYSTEM_NAME
ARG ANDROID_EABI
ENV SYSTEM_PYTHON="/usr/bin/python"
ENV SYSTEM_PROTOC="/usr/local/bin/protoc"
ENV NDK_PATH="/usr/local/src/android-ndk"
ENV PLATFORM="${ANDROID_PLATFORM}"
ENV ARCHITECTURE="${ANDROID_ARCHITECTURE}"
ENV CMAKE_SYSTEM_NAME="${ANDROID_CMAKE_SYSTEM_NAME}"
ENV EABI="${ANDROID_EABI}"
ENV TOOLCHAIN_INSTALL_DIR="/usr/local/android/${ARCHITECTURE}"
ENV GCC="${CMAKE_SYSTEM_NAME}-gcc"
ENV GCXX="${CMAKE_SYSTEM_NAME}-c++"
ENV SYSROOT="${NDK_PATH}/platforms/${PLATFORM}/arch-${ARCHITECTURE}"
RUN sh $NDK_PATH/build/tools/make-standalone-toolchain.sh --platform=$PLATFORM --arch=$ARCHITECTURE --install-dir=$TOOLCHAIN_INSTALL_DIR
ENV CC="${GCC}"
ENV CXX="${GCXX}"
ENV PATH="${TOOLCHAIN_INSTALL_DIR}/bin:${PATH}"
RUN cd /tmp \
 && curl -L https://github.com/google/protobuf/archive/v${PROTOVER}.tar.gz -o protobuf-v${PROTOVER}.tar.gz \
 && tar xvf protobuf-v${PROTOVER}.tar.gz \
 && cd protobuf-${PROTOVER} \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_TOOLCHAIN_FILE=$NDK_PATH/build/cmake/android.toolchain.cmake -DANDROID_TOOLCHAIN=clang -DCMAKE_SYSTEM_NAME=$CMAKE_SYSTEM_NAME -DCMAKE_POSITION_INDEPENDENT_CODE=ON -Dprotobuf_BUILD_TESTS=OFF -DCMAKE_INSTALL_PREFIX=$TOOLCHAIN_INSTALL_DIR -DANDROID_STL=c++_static -DANDROID_ABI=$EABI ../cmake \
 && $TOOLCHAIN_INSTALL_DIR/bin/make \
 && $TOOLCHAIN_INSTALL_DIR/bin/make install
RUN cd /tmp \
 && set -xe \
 && curl -L https://www.libarchive.org/downloads/libarchive-3.3.2.tar.gz -o libarchive-3.3.2.tar.gz \
 && tar xf libarchive-3.3.2.tar.gz \
 && cd libarchive-3.3.2 \
 && sed -i "/INCLUDE(CheckTypeSize)/aINCLUDE_DIRECTORIES(/tmp/libarchive-3.3.2/contrib/android/include/)" CMakeLists.txt \
 && cmake -DCMAKE_TOOLCHAIN_FILE=$NDK_PATH/build/cmake/android.toolchain.cmake -DANDROID_TOOLCHAIN=clang -DCMAKE_SYSTEM_NAME=$CMAKE_SYSTEM_NAME -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DCMAKE_INSTALL_PREFIX=$TOOLCHAIN_INSTALL_DIR -DENABLE_TEST=OFF -DANDROID_STL=c++_static -DANDROID_ABI=$EABI . \
 && sed -i "/#define HAVE_STATFS 1/a#define HAVE_STATVFS 1" config.h \
 && sed -i "/#include \"passphrase.h\"/a#ifdef ANDROID\nint wctomb(char *s, wchar_t wc) { return wcrtomb(s,wc,NULL); }\nint mbtowc(wchar_t *pwc, const char *s, size_t n) { return mbrtowc(pwc, s, n, NULL); }\n#endif" tar/util.c \
 && $TOOLCHAIN_INSTALL_DIR/bin/make \
 && $TOOLCHAIN_INSTALL_DIR/bin/make install \
 && cp /tmp/libarchive-3.3.2/contrib/android/include/* $TOOLCHAIN_INSTALL_DIR/include/
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
