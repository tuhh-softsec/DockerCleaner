#  ##############################
#   Build the FFmpeg-build image.
FROM alpine:3.8 AS build
ARG FFMPEG_VERSION=ffmpeg-snapshot.tar.bz2
ARG AOM_VERSION=master
ARG PREFIX=/opt/ffmpeg
ARG PKG_CONFIG_PATH=/opt/ffmpeg/lib64/pkgconfig
ARG LD_LIBRARY_PATH=/opt/ffmpeg/lib
ARG MAKEFLAGS="-j4"
#   FFmpeg build dependencies.
RUN apk add build-base=0.5-r1 cmake=3.11.1-r2 freetype-dev=2.9.1-r1 lame-dev=3.100-r0 libogg-dev=1.3.3-r2 libass=0.13.7-r1 libass-dev=0.13.7-r1 libvpx-dev=1.6.1-r0 libvorbis-dev=1.3.6-r2 libwebp-dev=1.0.0-r0 libtheora-dev=1.1.1-r13 libtool=2.4.6-r5 opus-dev=1.2.1-r1 perl=5.26.3-r0 pkgconf=1.5.3-r0 pkgconfig python rtmpdump-dev=2.4_git20160909-r5 wget=1.20.3-r0 x264-dev=20180304-r1 x265-dev=2.7-r1 yasm=1.3.0-r1 --update
#   Install fdk-aac from testing.
RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories \
 && apk add fdk-aac-dev --update
#   Build libaom for av1.
RUN mkdir -p /tmp/aom \
 && cd /tmp/ \
 && wget https://aomedia.googlesource.com/aom/+archive/${AOM_VERSION}.tar.gz \
 && tar zxf ${AOM_VERSION}.tar.gz \
 && rm ${AOM_VERSION}.tar.gz \
 && rm -rf CMakeCache.txt CMakeFiles \
 && mkdir -p ./aom_build \
 && cd ./aom_build \
 && cmake -DCMAKE_INSTALL_PREFIX="${PREFIX}" -DBUILD_SHARED_LIBS=1 .. \
 && make \
 && make install
#   Get ffmpeg source.
RUN cd /tmp/ \
 && wget https://ffmpeg.org/releases/${FFMPEG_VERSION} \
 && tar xjvf ${FFMPEG_VERSION} \
 && rm ${FFMPEG_VERSION}
#   Compile ffmpeg.
RUN cd /tmp/ffmpeg \
 && ./configure --enable-version3 --enable-gpl --enable-nonfree --enable-small --enable-libaom --enable-libmp3lame --enable-libx264 --enable-libx265 --enable-libvpx --enable-libtheora --enable-libvorbis --enable-libopus --enable-libfdk-aac --enable-libass --enable-libwebp --enable-librtmp --enable-postproc --enable-avresample --enable-libfreetype --enable-openssl --disable-debug --disable-doc --disable-ffplay --extra-cflags="-I${PREFIX}/include" --extra-ldflags="-L${PREFIX}/lib" --extra-libs="-lpthread -lm" --prefix="${PREFIX}" \
 && make \
 && make install \
 && make distclean
#   Cleanup.
RUN rm -rf /var/cache/apk/* /tmp/*
#  #########################
#   Build the release image.
FROM alpine:3.8
LABEL MAINTAINER="Alfred Gutierrez <alf.g.jr@gmail.com>"
ENV PATH="/opt/ffmpeg/bin:$PATH"
RUN apk add ca-certificates=20191127-r2 openssl=1.0.2u-r0 pcre=8.42-r0 lame=3.100-r0 libogg=1.3.3-r2 libass=0.13.7-r1 libvpx=1.6.1-r0 libvorbis=1.3.6-r2 libwebp=1.0.0-r0 libtheora=1.1.1-r13 opus=1.2.1-r1 rtmpdump=2.4_git20160909-r5 x264-dev=20180304-r1 x265-dev=2.7-r1 --update
COPY --from=build /opt/ffmpeg /opt/ffmpeg
COPY --from=build /opt/ffmpeg/lib64/libaom.so.0 /usr/lib/libaom.so.0
COPY --from=build /usr/lib/libfdk-aac.so.2 /usr/lib/libfdk-aac.so.2
CMD ["/opt/ffmpeg/bin/ffmpeg"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
