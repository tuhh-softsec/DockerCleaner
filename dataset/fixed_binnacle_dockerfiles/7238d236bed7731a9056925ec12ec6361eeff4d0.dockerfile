FROM alpine:3.6
ARG RTORRENT_VER=0.9.6
ARG LIBTORRENT_VER=0.13.6
ARG MEDIAINFO_VER=0.7.99
ARG FILEBOT_VER=4.7.9
ARG CHROMAPRINT_VER=1.4.2
ARG LIBZEN_VER=0.4.37
ARG FLOOD_VER=1.0.0
ARG BUILD_CORES
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV UID="991" \
    GID="991" \
    CONTEXT_PATH="/" \
    RTORRENT_SCGI="0" \
    PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"
ENV FILEBOT_RENAME_METHOD="symlink" \
    FILEBOT_RENAME_MOVIES="{n} ({y})" \
    FILEBOT_RENAME_SERIES="{n}/Season {s.pad(2)}/{s00e00} - {t}" \
    FILEBOT_RENAME_ANIMES="{n}/{e.pad(3)} - {t}" \
    FILEBOT_RENAME_MUSICS="{artist}/{album}/{fn}"
RUN NB_CORES=${BUILD_CORES-`getconf _NPROCESSORS_CONF `} \
 && apk -U upgrade \
 && apk add build-base=0.5-r0 git=2.13.7-r2 libtool=2.4.6-r1 automake=1.15.1-r0 autoconf=2.69-r0 wget=1.20.3-r0 tar=1.32-r0 xz=5.2.3-r0 zlib-dev=1.2.11-r0 cppunit-dev=1.13.2-r1 libressl-dev=2.5.5-r2 ncurses-dev=6.0_p20171125-r1 curl-dev=7.61.1-r2 binutils=2.30-r1 -t build-dependencies \
 && apk add ca-certificates=20161130-r3 curl=7.61.1-r2 ncurses=6.0_p20171125-r1 libressl=2.5.5-r2 gzip=1.8-r0 zip=3.0-r4 zlib=1.2.11-r0 unrar=5.4.5-r0 s6=2.5.1.0-r0 su-exec=0.2-r0 python nodejs=6.10.3-r2 nodejs-npm=6.10.3-r2 openjdk8-jre=8.212.04-r0 java-jna-native=4.3.0-r0 \
 && cd /tmp \
 && mkdir libtorrent rtorrent \
 && cd libtorrent \
 && wget -qO- https://github.com/rakshasa/libtorrent/archive/${LIBTORRENT_VER}.tar.gz | tar xz --strip 1 \
 && cd ../rtorrent \
 && wget -qO- https://github.com/rakshasa/rtorrent/archive/${RTORRENT_VER}.tar.gz | tar xz --strip 1 \
 && cd /tmp \
 && git clone https://github.com/mirror/xmlrpc-c.git \
 && git clone https://github.com/Rudde/mktorrent.git \
 && git clone https://github.com/acoustid/chromaprint.git \
 && cd /tmp/mktorrent \
 && make -j ${NB_CORES} \
 && make install \
 && cd /tmp/xmlrpc-c/stable \
 && ./configure \
 && make -j ${NB_CORES} \
 && make install \
 && cd /tmp/libtorrent \
 && ./autogen.sh \
 && ./configure \
 && make -j ${NB_CORES} \
 && make install \
 && cd /tmp/rtorrent \
 && ./autogen.sh \
 && ./configure --with-xmlrpc-c \
 && make -j ${NB_CORES} \
 && make install \
 && cd /tmp \
 && wget -q http://mediaarea.net/download/binary/mediainfo/${MEDIAINFO_VER}/MediaInfo_CLI_${MEDIAINFO_VER}_GNU_FromSource.tar.gz \
 && wget -q http://mediaarea.net/download/binary/libmediainfo0/${MEDIAINFO_VER}/MediaInfo_DLL_${MEDIAINFO_VER}_GNU_FromSource.tar.gz \
 && wget -q https://github.com/MediaArea/ZenLib/archive/v${LIBZEN_VER}.tar.gz -O libzen.tar.gz \
 && tar xzf MediaInfo_DLL_${MEDIAINFO_VER}_GNU_FromSource.tar.gz \
 && tar xzf MediaInfo_CLI_${MEDIAINFO_VER}_GNU_FromSource.tar.gz \
 && tar xzf libzen.tar.gz \
 && cd /tmp/ZenLib-${LIBZEN_VER}/Project/GNU/Library \
 && ./autogen.sh \
 && ./configure --prefix=/usr --enable-shared --disable-static \
 && make \
 && make install \
 && cd /tmp/MediaInfo_DLL_GNU_FromSource \
 && ./SO_Compile.sh \
 && cd /tmp/MediaInfo_DLL_GNU_FromSource/ZenLib/Project/GNU/Library \
 && make install \
 && cd /tmp/MediaInfo_DLL_GNU_FromSource/MediaInfoLib/Project/GNU/Library \
 && make install \
 && cd /tmp/MediaInfo_CLI_GNU_FromSource \
 && ./CLI_Compile.sh \
 && cd /tmp/MediaInfo_CLI_GNU_FromSource/MediaInfo/Project/GNU/CLI \
 && make install \
 && strip -s /usr/local/bin/rtorrent \
 && strip -s /usr/local/bin/mktorrent \
 && strip -s /usr/local/bin/mediainfo \
 && mkdir /filebot \
 && cd /filebot \
 && wget https://github.com/acoustid/chromaprint/releases/download/v${CHROMAPRINT_VER}/chromaprint-fpcalc-${CHROMAPRINT_VER}-linux-x86_64.tar.gz \
 && tar xvf chromaprint-fpcalc-${CHROMAPRINT_VER}-linux-x86_64.tar.gz \
 && mv chromaprint-fpcalc-${CHROMAPRINT_VER}-linux-x86_64/fpcalc /usr/local/bin \
 && strip -s /usr/local/bin/fpcalc \
 && wget -q https://netcologne.dl.sourceforge.net/project/filebot/filebot/FileBot_${FILEBOT_VER}/FileBot_${FILEBOT_VER}-portable.tar.xz \
 && tar xJf FileBot_${FILEBOT_VER}-portable.tar.xz \
 && rm FileBot_${FILEBOT_VER}-portable.tar.xz \
 && mkdir /usr/flood \
 && cd /usr/flood \
 && wget -qO- https://github.com/jfurrow/flood/archive/v${FLOOD_VER}.tar.gz | tar xz --strip 1 \
 && npm install --production \
 && ln -sf /usr/local/lib/libmediainfo.so.0.0.0 /filebot/lib/x86_64/libmediainfo.so \
 && ln -sf /usr/local/lib/libzen.la /filebot/lib/x86_64/libzen.so \
 && ln -sf /usr/local/bin/mediainfo /usr/bin/mediainfo \
 && apk del build-dependencies \
 && rm -rf /var/cache/apk/* /tmp/*
COPY config.js /usr/flood/
COPY s6.d /etc/s6.d
COPY run.sh /usr/bin/
COPY postdl /usr/bin/
COPY postrm /usr/bin/
COPY config.js /usr/flood/
COPY rtorrent.rc /home/torrent/.rtorrent.rc
RUN chmod +x /usr/bin/* /etc/s6.d/*/* /etc/s6.d/.s6-svscan/*
VOLUME /data /flood-db
EXPOSE 3000/tcp 49184/tcp 49184/udp
LABEL description="BitTorrent client with WebUI front-end" \
      rtorrent="rTorrent BiTorrent client v$RTORRENT_VER" \
      libtorrent="libtorrent v$LIBTORRENT_VER" \
      filebot="Filebot v$FILEBOT_VER" \
      maintainer="Wonderfall <wonderfall@targaryen.house>"
CMD ["run.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
