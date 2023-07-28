FROM ubuntu:16.04
LABEL Description="This image is used to start the Video Resume app executable" \
      Vendor="Ponzoni Nelson" \
      Version="0.20170408"
ENV CV_VERSION="'2.4.13'"
ENV LD_LIBRARY_PATH="$LD_LIBARARY_PATH:/usr/local/lib"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 ca-certificates=20210119~16.04.1 wget=1.17.1-1ubuntu1.5 unzip=6.0-20ubuntu1.1 pkg-config=0.29.1-0ubuntu1 libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 libavcodec-dev=7:2.8.17-0ubuntu0.1 libavformat-dev=7:2.8.17-0ubuntu0.1 libswscale-dev=7:2.8.17-0ubuntu0.1 libtbb2=4.4~20151115-0ubuntu3 libtbb-dev=4.4~20151115-0ubuntu3 libjpeg-dev=8c-2ubuntu8 libpng-dev libtiff-dev libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libqt4-dev=4:4.8.7+dfsg-5ubuntu2 libqt4-opengl-dev=4:4.8.7+dfsg-5ubuntu2 libeigen3-dev=3.3~beta1-2 libopencore-amrnb-dev=0.1.3-2.1 libopencore-amrwb-dev=0.1.3-2.1 libtheora-dev=1.1.1+dfsg.1-8 libvorbis-dev=1.3.5-3ubuntu0.2 libxvidcore-dev=2:1.3.4-1 libv4l-dev=1.10.0-1 libdc1394-22-dev=2.2.4-1 libavcodec-dev=7:2.8.17-0ubuntu0.1 libavformat-dev=7:2.8.17-0ubuntu0.1 qt4-default=4:4.8.7+dfsg-5ubuntu2 libvtk5-qt4-dev=5.10.1+dfsg-2.1build1 libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 libgtkglext1=1.2.0-3.2fakesync1ubuntu1 libgtkglext1-dev=1.2.0-3.2fakesync1ubuntu1 libpng3=1.2.54-1ubuntu1.1 pngtools=0.4-1.2 libpng++-dev=0.2.5-1 libjpeg-dev=8c-2ubuntu8 libjpeg-progs=1:9b-1ubuntu1 libjpeg9=1:9b-1ubuntu1 libjpeg9-dbg=1:9b-1ubuntu1 libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libjasper-runtime=1.900.1-debian1-2.4ubuntu1.3 libjasper1=1.900.1-debian1-2.4ubuntu1.3 zlib1g-dbg=1:1.2.8.dfsg-2ubuntu4.3 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libavformat-dev=7:2.8.17-0ubuntu0.1 libavutil-dev=7:2.8.17-0ubuntu0.1 libxine2-dev=1.2.6-1build5 libxine2=1.2.6-1build5 libswscale-dev=7:2.8.17-0ubuntu0.1 libswscale-ffmpeg3=7:2.8.17-0ubuntu0.1 libdc1394-22=2.2.4-1 libdc1394-22-dev=2.2.4-1 libdc1394-utils=2.2.4-1 libavcodec-dev=7:2.8.17-0ubuntu0.1 libmp3lame-dev=3.99.5+repack1-9build1 libopencore-amrnb-dev=0.1.3-2.1 libopencore-amrwb-dev=0.1.3-2.1 libtheora-dev=1.1.1+dfsg.1-8 libvorbis-dev=1.3.5-3ubuntu0.2 libxvidcore-dev=2:1.3.4-1 ffmpeg=7:2.8.17-0ubuntu0.1 x264=2:0.148.2643+git5c65704-1 libx264-dev=2:0.148.2643+git5c65704-1 libv4l-0=1.10.0-1 v4l-utils=1.10.0-1 libavutil-ffmpeg54=7:2.8.17-0ubuntu0.1 -y \
 && CV_VERSION='2.4.13' \
 && wget --progress=bar:force https://github.com/Itseez/opencv/archive/"$CV_VERSION".zip \
 && unzip "$CV_VERSION".zip \
 && mkdir /opencv-"$CV_VERSION"/cmake_binary \
 && cd /opencv-"$CV_VERSION"/cmake_binary \
 && cmake -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=/usr/local -DINSTALL_C_EXAMPLES=ON -DINSTALL_PYTHON_EXAMPLES=OFF -DBUILD_EXAMPLES=ON -DBUILD_opencv_cvv=OFF -DBUILD_NEW_PYTHON_SUPPORT=OFF -DBUILD_DOCS=OFF -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DWITH_TBB=ON -DWITH_V4L=ON -DWITH_QT=ON -DWITH_OPENGL=ON -DWITH_VTK=ON .. \
 && make -j$( nproc ;) \
 && make install -j$( nproc ;) \
 && rm /"$CV_VERSION".zip \
 && rm -r /opencv-"$CV_VERSION" \
 && cd /root/ \
 && git clone https://github.com/lerker/OpenSourceVS.git \
 && cd OpenSourceVS/ \
 && g++ -std=c++11 -o OpenSourceVS main.cpp `pkg-config --cflags --libs opencv ` \
 && cd /root/ \
 && cp OpenSourceVS/OpenSourceVS opensourcevs \
 && cp OpenSourceVS/Video 2/Video original 2.avi video2.avi \
 && apt-get -y clean all \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /root
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
