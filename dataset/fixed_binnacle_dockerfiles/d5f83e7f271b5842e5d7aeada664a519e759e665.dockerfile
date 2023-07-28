#  ##############################
#   Build the FFmpeg-build image.
FROM alpine:3.8 AS build
ARG FFMPEG_VERSION=4.1.2
ARG PREFIX=/opt/ffmpeg
ARG LD_LIBRARY_PATH=/opt/ffmpeg/lib
ARG MAKEFLAGS="-j4"
#   FFmpeg build dependencies.
RUN apk add build-base=0.5-r1 coreutils=8.29-r2 freetype-dev=2.9.1-r1 gcc=6.4.0-r9 lame-dev=3.100-r0 libogg-dev=1.3.3-r2 libass=0.13.7-r1 libass-dev=0.13.7-r1 libvpx-dev=1.6.1-r0 libvorbis-dev=1.3.6-r2 libwebp-dev=1.0.0-r0 libtheora-dev=1.1.1-r13 opus-dev=1.2.1-r1 pkgconf=1.5.3-r0 pkgconfig rtmpdump-dev=2.4_git20160909-r5 wget=1.20.3-r0 x264-dev=20180304-r1 x265-dev=2.7-r1 yasm=1.3.0-r1 --update
#   Get fdk-aac from testing.
RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories \
 && apk add fdk-aac-dev --update
#   Get ffmpeg source.
RUN cd /tmp/ \
 && wget http://ffmpeg.org/releases/ffmpeg-${FFMPEG_VERSION}.tar.gz \
 && tar zxf ffmpeg-${FFMPEG_VERSION}.tar.gz \
 && rm ffmpeg-${FFMPEG_VERSION}.tar.gz
#   Compile ffmpeg.
RUN cd /tmp/ffmpeg-${FFMPEG_VERSION} \
 && ./configure --enable-version3 --enable-gpl --enable-nonfree --enable-small --enable-libmp3lame --enable-libx264 --enable-libx265 --enable-libvpx --enable-libtheora --enable-libvorbis --enable-libopus --enable-libfdk-aac --enable-libass --enable-libwebp --enable-librtmp --enable-postproc --enable-avresample --enable-libfreetype --enable-openssl --disable-debug --disable-doc --disable-ffplay --extra-cflags="-I${PREFIX}/include" --extra-ldflags="-L${PREFIX}/lib" --extra-libs="-lpthread -lm" --prefix="${PREFIX}" \
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
COPY --from=build /usr/lib/libfdk-aac.so.2 /usr/lib/libfdk-aac.so.2
CMD ["/usr/local/bin/ffmpeg"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
