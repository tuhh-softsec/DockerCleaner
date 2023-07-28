ARG NGINX_VERSION=1.16.0
ARG NGINX_RTMP_VERSION=1.2.1
ARG FFMPEG_VERSION=4.1.3
#  #############################
#   Build the NGINX-build image.
FROM alpine:3.8 AS build-nginx
ARG NGINX_VERSION
ARG NGINX_RTMP_VERSION
#   Build dependencies.
RUN apk add build-base=0.5-r1 ca-certificates=20191127-r2 curl=7.61.1-r3 gcc=6.4.0-r9 libc-dev=0.7.1-r0 libgcc=6.4.0-r9 linux-headers=4.4.6-r2 make=4.2.1-r2 musl-dev=1.1.19-r11 openssl=1.0.2u-r0 openssl-dev=1.0.2u-r0 pcre=8.42-r0 pcre-dev=8.42-r0 pkgconf=1.5.3-r0 pkgconfig zlib-dev=1.2.11-r1 --update
#   Get nginx source.
RUN cd /tmp \
 && wget https://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz \
 && tar zxf nginx-${NGINX_VERSION}.tar.gz \
 && rm nginx-${NGINX_VERSION}.tar.gz
#   Get nginx-rtmp module.
RUN cd /tmp \
 && wget https://github.com/arut/nginx-rtmp-module/archive/v${NGINX_RTMP_VERSION}.tar.gz \
 && tar zxf v${NGINX_RTMP_VERSION}.tar.gz \
 && rm v${NGINX_RTMP_VERSION}.tar.gz
#   Compile nginx with nginx-rtmp module.
RUN cd /tmp/nginx-${NGINX_VERSION} \
 && ./configure --prefix=/opt/nginx --add-module=/tmp/nginx-rtmp-module-${NGINX_RTMP_VERSION} --conf-path=/opt/nginx/nginx.conf --with-threads --with-file-aio --with-http_ssl_module --error-log-path=/opt/nginx/logs/error.log --http-log-path=/opt/nginx/logs/access.log --with-debug \
 && cd /tmp/nginx-${NGINX_VERSION} \
 && make \
 && make install
#  ##############################
#   Build the FFmpeg-build image.
FROM alpine:3.8 AS build-ffmpeg
ARG FFMPEG_VERSION
ARG PREFIX=/usr/local
ARG MAKEFLAGS="-j4"
#   FFmpeg build dependencies.
RUN apk add build-base=0.5-r1 coreutils=8.29-r2 freetype-dev=2.9.1-r1 lame-dev=3.100-r0 libogg-dev=1.3.3-r2 libass=0.13.7-r1 libass-dev=0.13.7-r1 libvpx-dev=1.6.1-r0 libvorbis-dev=1.3.6-r2 libwebp-dev=1.0.0-r0 libtheora-dev=1.1.1-r13 opus-dev=1.2.1-r1 pkgconf=1.5.3-r0 pkgconfig rtmpdump-dev=2.4_git20160909-r5 wget=1.20.3-r0 x264-dev=20180304-r1 x265-dev=2.7-r1 yasm=1.3.0-r1 --update
RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories
RUN apk add fdk-aac-dev --update
#   Get FFmpeg source.
RUN cd /tmp/ \
 && wget http://ffmpeg.org/releases/ffmpeg-${FFMPEG_VERSION}.tar.gz \
 && tar zxf ffmpeg-${FFMPEG_VERSION}.tar.gz \
 && rm ffmpeg-${FFMPEG_VERSION}.tar.gz
#   Compile ffmpeg.
RUN cd /tmp/ffmpeg-${FFMPEG_VERSION} \
 && ./configure --prefix=${PREFIX} --enable-version3 --enable-gpl --enable-nonfree --enable-small --enable-libmp3lame --enable-libx264 --enable-libx265 --enable-libvpx --enable-libtheora --enable-libvorbis --enable-libopus --enable-libfdk-aac --enable-libass --enable-libwebp --enable-librtmp --enable-postproc --enable-avresample --enable-libfreetype --enable-openssl --disable-debug --disable-doc --disable-ffplay --extra-libs="-lpthread -lm" \
 && make \
 && make install \
 && make distclean
#   Cleanup.
RUN rm -rf /var/cache/* /tmp/*
#  #########################
#   Build the release image.
FROM alpine:3.8
LABEL MAINTAINER="Alfred Gutierrez <alf.g.jr@gmail.com>"
RUN apk add ca-certificates=20191127-r2 openssl=1.0.2u-r0 pcre=8.42-r0 lame=3.100-r0 libogg=1.3.3-r2 libass=0.13.7-r1 libvpx=1.6.1-r0 libvorbis=1.3.6-r2 libwebp=1.0.0-r0 libtheora=1.1.1-r13 opus=1.2.1-r1 rtmpdump=2.4_git20160909-r5 x264-dev=20180304-r1 x265-dev=2.7-r1 --update
COPY --from=build-nginx /opt/nginx /opt/nginx
COPY --from=build-ffmpeg /usr/local /usr/local
COPY --from=build-ffmpeg /usr/lib/libfdk-aac.so.2 /usr/lib/libfdk-aac.so.2
#   Add NGINX config and static files.
COPY nginx.conf /opt/nginx/nginx.conf
RUN mkdir -p /opt/data \
 && mkdir /www
COPY static /www/static
EXPOSE 1935/tcp
EXPOSE 80/tcp
CMD ["/opt/nginx/sbin/nginx"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
