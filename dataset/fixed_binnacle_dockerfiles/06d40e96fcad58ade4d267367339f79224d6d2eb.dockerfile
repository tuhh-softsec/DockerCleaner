#  ###############################################################################
#  #  Dockerfile to build minimal OpenCV image with Python3.6 and Video support ##
#  ###############################################################################
FROM alpine:3.7
MAINTAINER Janos Czentye <czentye@tmit.bme.hu>
ENV LANG="C.UTF-8"
RUN apk add build-base=0.5-r0 clang=5.0.0-r0 clang-dev=5.0.0-r0 cmake=3.9.5-r0 pkgconf=1.3.10-r0 wget=1.20.3-r0 openblas=0.2.19-r3 openblas-dev=0.2.19-r3 linux-headers=4.4.6-r2 libjpeg-turbo=1.5.3-r3 libjpeg-turbo-dev=1.5.3-r3 libpng=1.6.37-r0 libpng-dev=1.6.37-r0 libwebp=0.6.0-r1 libwebp-dev=0.6.0-r1 tiff=4.0.10-r2 tiff-dev=4.0.10-r2 jasper-libs=2.0.14-r0 jasper-dev=2.0.14-r0 openexr=2.2.0-r2 openexr-dev=2.2.0-r2 ffmpeg-libs=3.4-r1 ffmpeg-dev=3.4-r1 libavc1394=0.5.4-r1 libavc1394-dev=0.5.4-r1 gstreamer=1.12.3-r0 gstreamer-dev=1.12.3-r0 gst-plugins-base=1.12.3-r0 gst-plugins-base-dev=1.12.3-r0 libgphoto2=2.5.14-r0 libgphoto2-dev=2.5.14-r0 --update --no-cache \
 && apk add libtbb libtbb-dev --repository http://dl-cdn.alpinelinux.org/alpine/edge/testing --update --no-cache \
 && apk add python3=3.6.9-r1 python3-dev=3.6.9-r1 --repository http://dl-cdn.alpinelinux.org/alpine/edge/main --update --no-cache \
 && apk add py-numpy=1.13.3-r0 py-numpy-dev=1.13.3-r0 --repository http://dl-cdn.alpinelinux.org/alpine/edge/community --update --no-cache \
 && ln -vfs /usr/bin/python3 /usr/local/bin/python \
 && ln -vfs /usr/bin/pip3 /usr/local/bin/pip \
 && ln -vfs /usr/include/libpng16 /usr/include/libpng \
 && ln -vfs /usr/include/locale.h /usr/include/xlocale.h \
 && cd /tmp \
 && wget https://github.com/opencv/opencv/archive/3.4.0.tar.gz \
 && tar -xvzf 3.4.0.tar.gz \
 && rm -vrf 3.4.0.tar.gz \
 && mkdir -vp /tmp/opencv-3.4.0/build \
 && cd /tmp/opencv-3.4.0/build \
 && cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_C_COMPILER=/usr/bin/clang -D CMAKE_CXX_COMPILER=/usr/bin/clang++ -D CMAKE_INSTALL_PREFIX=/usr -D INSTALL_PYTHON_EXAMPLES=NO -D INSTALL_C_EXAMPLES=NO -D WITH_IPP=NO -D WITH_1394=NO -D WITH_LIBV4L=NO -D WITH_V4l=YES -D WITH_TBB=YES -D WITH_FFMPEG=YES -D WITH_GPHOTO2=YES -D WITH_GSTREAMER=YES -D BUILD_DOCS=NO -D BUILD_TESTS=NO -D BUILD_PERF_TESTS=NO -D BUILD_EXAMPLES=NO -D BUILD_opencv_java=NO -D BUILD_opencv_python2=NO -D BUILD_ANDROID_EXAMPLES=NO -D PYTHON3_LIBRARY=`find /usr -name libpython3.so ` -D PYTHON_EXECUTABLE=`which python3 ` -D PYTHON3_EXECUTABLE=`which python3 ` -D BUILD_opencv_python3=YES .. \
 && make -j`grep -c '^processor' /proc/cpuinfo ` \
 && make install \
 && cd / \
 && rm -vrf /tmp/opencv-3.4.0 \
 && apk del --purge build-base clang clang-dev cmake pkgconf wget openblas-dev openexr-dev gstreamer-dev gst-plugins-base-dev libgphoto2-dev libtbb-dev libjpeg-turbo-dev libpng-dev tiff-dev jasper-dev ffmpeg-dev libavc1394-dev python3-dev py-numpy-dev \
 && rm -vrf /var/cache/apk/*
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
