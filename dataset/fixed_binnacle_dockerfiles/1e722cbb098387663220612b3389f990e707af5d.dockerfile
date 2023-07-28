FROM docker.io/python:3-alpine
MAINTAINER Ondrej Barta <ondrej@ondrej.it>
RUN echo http://nl.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories \
 && apk update \
 && apk add bash=5.2.15-r0 tzdata=2023c-r0 libass=0.16.0-r1 libstdc++=12.2.1_git20220924-r4 libpng=1.6.38-r0 libjpeg=9e-r0 xvidcore=1.3.7-r1 x264-libs=0.164_git20220602-r0 x265=3.5-r3 libvpx=1.12.0-r1 libvorbis=1.3.7-r0 opus=1.3.1-r1 lame=3.100-r2 fdk-aac=2.0.2-r1 freetype=2.12.1-r0 \
 && apk add coreutils=9.1-r0 fdk-aac-dev=2.0.2-r1 freetype-dev=2.12.1-r0 x264-dev=0.164_git20220602-r0 x265-dev=3.5-r3 yasm=1.3.0-r3 yasm-dev=1.3.0-r3 libogg-dev=1.3.5-r2 libvorbis-dev=1.3.7-r0 opus-dev=1.3.1-r1 libvpx-dev=1.12.0-r1 lame-dev=3.100-r2 xvidcore-dev=1.3.7-r1 libass-dev=0.16.0-r1 openssl-dev=3.0.8-r3 musl-dev=1.2.3-r4 make=4.3-r1 cmake=3.24.4-r0 gcc=12.2.1_git20220924-r4 g++=12.2.1_git20220924-r4 build-base=0.5-r3 libjpeg-turbo-dev=2.1.4-r0 libpng-dev=1.6.38-r0 libjasper clang-dev clang linux-headers=5.19.5-r0 git=2.38.4-r1 curl=7.88.1-r1 --virtual build-deps \
 && export SRC=/usr export FFMPEG_VERSION=3.4.1 DIR=$( mktemp -d ;) \
 && cd ${DIR} \
 && curl -Os http://ffmpeg.org/releases/ffmpeg-${FFMPEG_VERSION}.tar.gz \
 && tar xzvf ffmpeg-${FFMPEG_VERSION}.tar.gz \
 && cd ffmpeg-${FFMPEG_VERSION} \
 && ./configure --prefix="${SRC}" --extra-cflags="-I${SRC}/include" --extra-ldflags="-L${SRC}/lib" --bindir="${SRC}/bin" --extra-libs=-ldl --enable-version3 --enable-libmp3lame --enable-pthreads --enable-libx264 --enable-libxvid --enable-gpl --enable-postproc --enable-nonfree --enable-avresample --enable-libfdk-aac --disable-debug --enable-small --enable-openssl --enable-libx265 --enable-libopus --enable-libvorbis --enable-libvpx --enable-libfreetype --enable-libass --enable-shared --enable-pic \
 && make -j8 \
 && make install \
 && make distclean \
 && hash -r \
 && cd /tmp \
 && rm -rf ${DIR} \
 && pip install Cython==0.27.3 numpy==1.14.0 Pillow==5.0.0 av==0.3.3 --no-cache-dir \
 && export OPENCV_VERSION=3.4.0 export CC=/usr/bin/clang export CXX=/usr/bin/clang++ export PYTHON_VERSION=`python -c 'import platform; print(".".join(platform.python_version_tuple()[:2]))' ` \
 && DIR=$( mktemp -d ;) \
 && cd ${DIR} \
 && curl -sSL -Os https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.tar.gz \
 && tar xzvf ${OPENCV_VERSION}.tar.gz \
 && cd opencv-${OPENCV_VERSION} \
 && mkdir build \
 && cd build \
 && cmake -D CMAKE_BUILD_TYPE=RELEASE -D INSTALL_C_EXAMPLES=OFF -D INSTALL_PYTHON_EXAMPLES=OFF -D CMAKE_INSTALL_PREFIX=/usr/local -D BUILD_EXAMPLES=OFF -D BUILD_opencv_python3=ON -D PYTHON_DEFAULT_EXECUTABLE=/usr/local/bin/python3 -D PYTHON_INCLUDE_DIRS=/usr/local/include/python${PYTHON_VERSION}m -D PYTHON_EXECUTABLE=/usr/local/bin/python${PYTHON_VERSION} -D PYTHON_LIBRARY=/usr/local/lib/libpython${PYTHON_VERSION}m.so .. \
 && make -j8 \
 && make install \
 && cd /tmp \
 && rm -rf ${DIR} \
 && apk del build-deps \
 && rm -rf /var/cache/apk/*
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
