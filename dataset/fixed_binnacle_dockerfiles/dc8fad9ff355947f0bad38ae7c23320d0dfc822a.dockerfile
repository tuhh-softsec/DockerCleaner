FROM alpine:3.9.2
ENV WORKDIR="/mnt"
ARG M4B_TOOL_DOWNLOAD_LINK="https://github.com/sandreas/m4b-tool/releases/latest/download/m4b-tool.phar"
ARG FFMPEG_VERSION=4.1
ARG PREFIX=/ffmpeg_build
RUN echo "---- INSTALL BUILD DEPENDENCIES ----" \
 && apk add autoconf=2.69-r2 automake=1.16.1-r0 boost-dev=1.67.0-r2 build-base=0.5-r1 gcc=8.3.0-r0 lame-dev=3.100-r0 libogg-dev=1.3.3-r2 yasm=1.3.0-r1 nasm=2.13.03-r0 yasm-dev=1.3.0-r1 zlib-dev=1.2.11-r1 freetype-dev=2.9.1-r3 libogg-dev=1.3.3-r2 libtheora-dev=1.1.1-r13 libvorbis-dev=1.3.6-r2 openssl-dev=1.1.1k-r0 opus-dev=1.3-r0 git=2.20.4-r0 tar=1.32-r0 wget=1.20.3-r0 --no-cache --update --upgrade --virtual=build-dependencies \
 && echo "---- INSTALL RUNTIME PACKAGES ----" \
 && apk add bzip2=1.0.6-r7 ca-certificates=20191127-r2 coreutils=8.30-r0 curl=7.64.0-r5 file=5.36-r1 libtool=2.4.6-r5 freetype=2.9.1-r3 lame=3.100-r0 libogg=1.3.3-r2 libvpx=1.6.1-r1 libvorbis=1.3.6-r2 libtheora=1.1.1-r13 libvorbis=1.3.6-r2 openssl=1.1.1k-r0 opus=1.3-r0 pcre=8.42-r2 php7-cli php7-dom=7.2.33-r0 php7-intl=7.2.33-r0 php7-json=7.2.33-r0 php7-xml=7.2.33-r0 php7-curl=7.2.33-r0 php7-mbstring=7.2.33-r0 php7-simplexml=7.2.33-r0 php7-phar=7.2.33-r0 pkgconf=1.6.0-r0 pkgconfig --no-cache --update --upgrade \
 && echo http://dl-cdn.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories \
 && apk add fdk-aac-dev --update \
 && echo "date.timezone = UTC" >> /etc/php7/php.ini \
 && echo "---- COMPILE FFMPEG ----" \
 && cd /tmp/ \
 && wget http://ffmpeg.org/releases/ffmpeg-${FFMPEG_VERSION}.tar.gz \
 && tar zxf ffmpeg-${FFMPEG_VERSION}.tar.gz \
 && rm ffmpeg-${FFMPEG_VERSION}.tar.gz \
 && cd /tmp/ffmpeg-${FFMPEG_VERSION} \
 && ./configure --enable-version3 --enable-gpl --enable-nonfree --enable-small --enable-libmp3lame --enable-libtheora --enable-libvorbis --enable-libopus --enable-libfdk_aac --enable-avresample --enable-libfreetype --enable-openssl --disable-debug --disable-doc --disable-ffplay --prefix="/tmp${PREFIX}" --extra-cflags="-I/tmp${PREFIX}/include" --extra-ldflags="-L/tmp${PREFIX}/lib" --extra-libs="-lpthread -lm" --bindir="/usr/local/bin/" \
 && make \
 && make install \
 && make distclean \
 && hash -r \
 && rm -rf /tmp/* \
 && echo "---- COMPILE SANDREAS MP4V2 ----" \
 && cd /tmp/ \
 && wget https://github.com/sandreas/mp4v2/archive/master.zip \
 && unzip master.zip \
 && rm master.zip \
 && cd mp4v2-master \
 && ./configure \
 && make \
 && make install \
 && make distclean \
 && rm -rf /tmp/* \
 && echo "---- COMPILE FDKAAC ----" \
 && cd /tmp/ \
 && wget https://github.com/nu774/fdkaac/archive/1.0.0.tar.gz \
 && tar xzf 1.0.0.tar.gz \
 && rm 1.0.0.tar.gz \
 && cd fdkaac-1.0.0 \
 && autoreconf -i \
 && ./configure \
 && make \
 && make install \
 && rm -rf /tmp/* \
 && echo "---- REMOVE BUILD DEPENDENCIES ----" \
 && apk del --purge build-dependencies
#   workaround to copy a local m4b-tool.phar IF it exists
COPY ./Dockerfile ./dist/m4b-tool.phar* /tmp/
RUN echo "---- INSTALL M4B-TOOL ----" \
 && if [ ! -f /tmp/m4b-tool.phar ] ; then wget "${M4B_TOOL_DOWNLOAD_LINK}" -O /tmp/m4b-tool.phar ; fi \
 && mv /tmp/m4b-tool.phar /usr/local/bin/m4b-tool \
 && chmod +x /usr/local/bin/m4b-tool
WORKDIR ${WORKDIR}
CMD ["list"]
ENTRYPOINT ["m4b-tool"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
