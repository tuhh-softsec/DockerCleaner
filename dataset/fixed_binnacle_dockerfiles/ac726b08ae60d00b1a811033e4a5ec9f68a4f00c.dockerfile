FROM debian AS build
ARG NUM_CORES=2
ENV PREFIX="/tmp/ffmpeg_build" \
    PKG_CONFIG_PATH="/tmp/ffmpeg_build/lib/pkgconfig"
#   Dependencies
RUN apt-get update -qq \
 && apt-get install --no-install-recommends autoconf=2.71-3 automake=1:1.16.5-1.3 build-essential=12.9ubuntu3 cmake=3.25.1-1 git=1:2.39.2-1ubuntu1 libass-dev=1:0.17.0-2 libfreetype6-dev=2.12.1+dfsg-4 libsdl2-dev=2.26.3+dfsg-1 libtheora-dev=1.1.1+dfsg.1-16.1 libtool=2.4.7-5 libva-dev=2.17.0-1 libvdpau-dev=1.5-2 libvorbis-dev=1.3.7-1build2 libxcb1-dev=1.15-1 libxcb-shm0-dev=1.15-1 libxcb-xfixes0-dev=1.15-1 mercurial=6.3.2-1 pkg-config=1.8.1-1ubuntu2 texinfo=6.8-6build2 wget=1.21.3-1ubuntu1 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 fontconfig=2.14.1-3ubuntu3 frei0r-plugins-dev=1.8.0-1build1 libass-dev=1:0.17.0-2 libfontconfig1-dev=2.14.1-3ubuntu3 libmp3lame-dev=3.100-6 libopencore-amrnb-dev=0.1.6-1 libopencore-amrwb-dev=0.1.6-1 libopus-dev=1.3.1-3 libspeex-dev=1.2.1-2ubuntu1 libtheora-dev=1.1.1+dfsg.1-16.1 libvorbis-dev=1.3.7-1build2 libvo-amrwbenc-dev=0.1.3-2 libwebp-dev=1.2.4-0.1build1 libx264-dev=2:0.164.3095+gitbaee400-2build1 libnuma-dev=2.0.16-1 libvpx-dev=1.12.0-1ubuntu1 libxvidcore-dev=2:1.3.7-1 texinfo=6.8-6build2 yasm=1.3.0-4 -y \
 && apt-get clean \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /tmp
#   openjpeg
RUN git clone https://github.com/uclouvain/openjpeg.git --branch master --single-branch \
 && cd openjpeg \
 && cmake -DBUILD_THIRDPARTY:BOOL=ON -DCMAKE_INSTALL_PREFIX="${PREFIX}" . \
 && make -j "${NUM_CORES}" \
 && make install \
 && make clean
#   libx265
RUN git clone https://github.com/videolan/x265.git --branch master --single-branch \
 && cd ./x265/build/linux \
 && cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX="${PREFIX}" -DENABLE_SHARED:bool=off ../../source \
 && make -j "${NUM_CORES}" \
 && make install \
 && make clean
#   fribidi
RUN git clone https://github.com/fribidi/fribidi.git --branch master --single-branch \
 && cd fribidi \
 && sed -i 's/^SUBDIRS =.*/SUBDIRS=gen.tab charset lib/' Makefile.am \
 && ./bootstrap --no-config \
 && ./configure -prefix="${PREFIX}" --enable-static=yes --enable-shared=no \
 && make -j "${NUM_CORES}" \
 && make install \
 && make clean
#   soxr
RUN git clone https://git.code.sf.net/p/soxr/code soxr --branch master --single-branch \
 && cd soxr \
 && mkdir build \
 && cd build \
 && cmake -Wno-dev -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="${PREFIX}" -DBUILD_SHARED_LIBS=OFF .. \
 && make -j "${NUM_CORES}" \
 && make install \
 && make clean
#   FFmpeg
RUN export BIN_DIR="/opt/ffmpeg/bin" PATH="${BIN_DIR}:${PATH}" \
 && git clone https://github.com/ffmpeg/ffmpeg.git --branch master --single-branch \
 && cd ./ffmpeg \
 && ./configure --cc=gcc-6 --prefix="${PREFIX}" --pkg-config-flags="--static" --extra-cflags="-I${PREFIX}/include -static" --extra-ldflags="-L${PREFIX}/lib -static" --extra-libs="-lpthread -lm" --bindir="${BIN_DIR}" --cpu="sandybridge" --arch="x84_64" --disable-shared --enable-static --disable-debug --disable-runtime-cpudetect --disable-ffplay --disable-ffserver --disable-doc --disable-network --disable-devices --disable-protocols --enable-protocol=file --enable-protocol=pipe --enable-protocol=tee --enable-libmp3lame --enable-libvpx --enable-libwebp --enable-libopus --enable-fontconfig --enable-gray --enable-libfreetype --enable-libopenjpeg --enable-libspeex --enable-libtheora --enable-libvorbis --enable-libfribidi --enable-gpl --enable-frei0r --enable-libx264 --enable-libx265 --enable-libxvid --enable-version3 --enable-libopencore-amrnb --enable-libopencore-amrwb --enable-libvo-amrwbenc || if [ $? -gt 0 ] ; then tail -n 100 ./ffbuild/config.log \
 && exit 1 ; fi \
 && make -j "${NUM_CORES}" \
 && make install \
 && make distclean
FROM alpine AS dist
COPY --from=build /opt/ /opt/
COPY --from=build /tmp/ffmpeg/COPYING.GPLv3 /opt/ffmpeg/
RUN cd /opt/ffmpeg/bin \
 && for file in *; do ln -s /opt/ffmpeg/bin/${file} /usr/local/bin/${file} ; done
CMD ["ffmpeg"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
