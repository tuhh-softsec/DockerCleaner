#   MIT License
#   Copyright (c) 2017 Juliano Petronetto
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files (the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#   The above copyright notice and this permission notice shall be included in all
#   copies or substantial portions of the Software.
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#   SOFTWARE
FROM alpine:3.5
MAINTAINER Juliano Petronetto <juliano.petronetto@gmail.com>
ENV LANG="C.UTF-8"
#   Add Edge repos
RUN echo -e "\n@edgemain http://nl.alpinelinux.org/alpine/edge/main\n@edgecomm http://nl.alpinelinux.org/alpine/edge/community\n@edgetest http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
#   Install required packages
RUN apk update \
 && apk upgrade \
 && apk add bash=4.3.46-r5 build-base=0.4-r1 ca-certificates=20161130-r1 clang-dev=3.8.1-r1 clang=3.8.1-r1 cmake=3.6.3-r0 coreutils=8.26-r0 curl=7.61.1-r1 freetype-dev=2.7-r2 ffmpeg-dev=3.1.11-r1 ffmpeg-libs=3.1.11-r1 gcc=6.2.1-r1 g++=6.2.1-r1 git=2.11.3-r2 gettext=0.19.8.1-r0 lcms2-dev=2.8-r1 libavc1394-dev=0.5.4-r1 libc-dev=0.7-r1 libffi-dev=3.2.1-r2 libjpeg-turbo-dev=1.5.3-r2 libpng-dev=1.6.25-r0 libressl-dev=2.4.4-r0 libtbb@edgetest libtbb-dev@edgetest libwebp-dev=0.5.2-r0 linux-headers=4.4.6-r1 make=4.2.1-r0 musl=1.1.15-r8 openblas@edgecomm openblas-dev@edgecomm openjpeg-dev=2.3.0-r0 openssl=1.0.2q-r0 python3=3.5.6-r0 python3-dev=3.5.6-r0 tiff-dev=4.0.9-r6 unzip=6.0-r3 zlib-dev=1.2.11-r0 --no-cache
#   Python 3 as default
RUN ln -s /usr/bin/python3 /usr/local/bin/python \
 && ln -s /usr/bin/pip3 /usr/local/bin/pip \
 && pip install pip==23.1 --upgrade
#   Install NumPy
RUN ln -s /usr/include/locale.h /usr/include/xlocale.h \
 && pip install numpy==1.24.2
#   Install OpenCV
RUN mkdir /opt \
 && cd /opt \
 && wget https://github.com/opencv/opencv/archive/3.2.0.zip \
 && unzip 3.2.0.zip \
 && rm 3.2.0.zip \
 && wget https://github.com/opencv/opencv_contrib/archive/3.2.0.zip \
 && unzip 3.2.0.zip \
 && rm 3.2.0.zip \
 && cd /opt/opencv-3.2.0 \
 && mkdir build \
 && cd build \
 && cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_C_COMPILER=/usr/bin/clang -D CMAKE_CXX_COMPILER=/usr/bin/clang++ -D CMAKE_INSTALL_PREFIX=/usr/local -D INSTALL_PYTHON_EXAMPLES=OFF -D INSTALL_C_EXAMPLES=OFF -D WITH_FFMPEG=ON -D WITH_TBB=ON -D OPENCV_EXTRA_MODULES_PATH=/opt/opencv_contrib-3.2.0/modules -D PYTHON_EXECUTABLE=/usr/local/bin/python .. \
 && make -j$( nproc ;) \
 && make install \
 && cd .. \
 && rm -rf build \
 && cp -p $( find /usr/local/lib/python3.5/site-packages -name cv2.*.so ;) /usr/lib/python3.5/site-packages/cv2.so \
 && python -c 'import cv2; print("Python: import cv2 - SUCCESS")'
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
