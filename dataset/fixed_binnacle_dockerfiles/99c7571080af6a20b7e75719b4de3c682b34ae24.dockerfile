FROM alpine:3.8
ARG BUILD_CORES
ARG MEDIAINFO_VER=0.7.99
ARG RTORRENT_VER=v0.9.7
ARG LIBTORRENT_VER=v0.13.7
ARG LIBZEN_VER=0.4.31
ARG GEOIP_VER=1.1.1
ENV UID="991" \
    GID="991" \
    WEBROOT="/" \
    PORT_RTORRENT="45000" \
    DHT_RTORRENT="off" \
    DISABLE_PERM_DATA="false" \
    PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"
LABEL Description="rutorrent based on alpine" \
      tags="latest" \
      maintainer="xataz <https://github.com/xataz>" \
      mediainfo_version="${MEDIAINFO_VER}" \
      libtorrent_version="${LIBTORRENT_VER}" \
      rtorrent_version="${RTORRENT_VER}" \
      libzen_version="${LIBZEN_VER}" \
      build_ver="201904081410"
RUN export BUILD_DEPS="build-base libtool automake autoconf wget libressl-dev ncurses-dev curl-dev zlib-dev libnl3-dev libsigc++-dev linux-headers py-pip" \
 && if [ "$RTORRENT_VER" == "0.9.6" ] ; then CPPUNIT_VER="==1.13.2-r1" ; fi \
 && apk upgrade --no-cache \
 && apk add ffmpeg=3.4.6-r0 libnl3=3.2.28-r1 ca-certificates=20191127-r2 gzip=1.9-r0 zip=3.0-r6 unrar=5.6.4-r0 curl=7.61.1-r3 c-ares=1.14.0-r0 s6=2.7.1.1-r1 geoip=1.6.12-r1 geoip-dev=1.6.12-r1 su-exec=0.2-r0 nginx=1.14.2-r2 php7=7.2.26-r0 php7-fpm=7.2.26-r0 php7-json=7.2.26-r0 php7-opcache=7.2.26-r0 php7-apcu=5.1.11-r2 php7-mbstring=7.2.26-r0 php7-ctype=7.2.26-r0 php7-pear=7.2.26-r0 php7-dev=7.2.26-r0 php7-sockets=7.2.26-r0 php7-phar=7.2.26-r0 libressl=2.7.5-r0 file=5.32-r2 findutils=4.6.0-r1 tar=1.32-r0 xz=5.2.4-r0 screen=4.6.2-r1 findutils=4.6.0-r1 bzip2=1.0.6-r7 bash=4.4.19-r1 git=2.18.4-r0 sox=14.4.2-r1 python ${BUILD_DEPS} cppunit-dev${CPPUNIT_VER} cppunit${CPPUNIT_VER} -X http://dl-cdn.alpinelinux.org/alpine/v3.6/main --no-cache \
 && git clone https://github.com/esmil/mktorrent /tmp/mktorrent \
 && git clone https://github.com/mirror/xmlrpc-c.git /tmp/xmlrpc-c \
 && git clone -b ${LIBTORRENT_VER} https://github.com/rakshasa/libtorrent.git /tmp/libtorrent \
 && git clone -b ${RTORRENT_VER} https://github.com/rakshasa/rtorrent.git /tmp/rtorrent \
 && wget http://mediaarea.net/download/binary/mediainfo/${MEDIAINFO_VER}/MediaInfo_CLI_${MEDIAINFO_VER}_GNU_FromSource.tar.gz -O /tmp/MediaInfo_CLI_${MEDIAINFO_VER}_GNU_FromSource.tar.gz \
 && wget http://mediaarea.net/download/binary/libmediainfo0/${MEDIAINFO_VER}/MediaInfo_DLL_${MEDIAINFO_VER}_GNU_FromSource.tar.gz -O /tmp/MediaInfo_DLL_${MEDIAINFO_VER}_GNU_FromSource.tar.gz \
 && wget http://downloads.sourceforge.net/zenlib/libzen_${LIBZEN_VER}.tar.gz -O /tmp/libzen_${LIBZEN_VER}.tar.gz \
 && cd /tmp \
 && tar xzf libzen_${LIBZEN_VER}.tar.gz \
 && tar xzf MediaInfo_DLL_${MEDIAINFO_VER}_GNU_FromSource.tar.gz \
 && tar xzf MediaInfo_CLI_${MEDIAINFO_VER}_GNU_FromSource.tar.gz \
 && cd /tmp/ZenLib/Project/GNU/Library \
 && ./autogen \
 && ./configure --prefix=/usr/local --enable-shared --disable-static \
 && make \
 && make install \
 && cd /tmp/mktorrent \
 && make -j ${BUILD_CORES-$( grep -c "processor" /proc/cpuinfo ;)} \
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
 && cd /tmp/xmlrpc-c/stable \
 && ./configure \
 && make -j ${NB_CORES} \
 && make install \
 && cd /tmp/libtorrent \
 && ./autogen.sh \
 && ./configure --disable-debug --disable-instrumentation \
 && make -j ${BUILD_CORES-$( grep -c "processor" /proc/cpuinfo ;)} \
 && make install \
 && cd /tmp/rtorrent \
 && ./autogen.sh \
 && ./configure --enable-ipv6 --disable-debug --with-xmlrpc-c \
 && make -j ${BUILD_CORES-$( grep -c "processor" /proc/cpuinfo ;)} \
 && make install \
 && mkdir -p /var/www \
 && git clone https://github.com/Novik/ruTorrent.git /var/www/html/rutorrent \
 && git clone https://github.com/nelu/rutorrent-thirdparty-plugins /tmp/rutorrent-thirdparty-plugins \
 && git clone https://github.com/mcrapet/plowshare /tmp/plowshare \
 && git clone https://github.com/xombiemp/rutorrentMobile.git /var/www/html/rutorrent/plugins/mobile \
 && git clone https://github.com/Phlooo/ruTorrent-MaterialDesign.git /var/www/html/rutorrent/plugins/theme/themes/materialdesign \
 && git clone https://github.com/Micdu70/geoip2-rutorrent /var/www/html/rutorrent/plugins/geoip2 \
 && rm -rf /var/www/html/rutorrent/plugins/geoip \
 && sed -i "s/'mkdir'.*$/'mkdir',/" /tmp/rutorrent-thirdparty-plugins/filemanager/flm.class.php \
 && sed -i 's#.*/usr/bin/rar.*##' /tmp/rutorrent-thirdparty-plugins/filemanager/conf.php \
 && mv /tmp/rutorrent-thirdparty-plugins/* /var/www/html/rutorrent/plugins/ \
 && mv /var/www/html/rutorrent /var/www/html/torrent \
 && cd /tmp/plowshare \
 && make \
 && mkdir -p /usr/share/GeoIP \
 && cd /usr/share/GeoIP \
 && wget https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz \
 && wget https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz \
 && tar xzf GeoLite2-City.tar.gz \
 && tar xzf GeoLite2-Country.tar.gz \
 && rm -f *.tar.gz \
 && mv GeoLite2-*/*.mmdb . \
 && cp *.mmdb /var/www/html/torrent/plugins/geoip2/database/ \
 && pecl install geoip-${GEOIP_VER} \
 && chmod +x /usr/lib/php7/modules/geoip.so \
 && pip install cfscrape==2.1.1 \
 && strip -s /usr/local/bin/rtorrent \
 && strip -s /usr/local/bin/mktorrent \
 && strip -s /usr/local/bin/mediainfo \
 && apk del -X http://dl-cdn.alpinelinux.org/alpine/v3.6/main --no-cache ${BUILD_DEPS} cppunit-dev \
 && rm -rf /tmp/*
ARG WITH_FILEBOT=NO
ARG FILEBOT_VER=4.7.9
ARG CHROMAPRINT_VER=1.4.3
ENV filebot_version="${FILEBOT_VER}" \
    chromaprint_ver="${CHROMAPRINT_VER}"
ENV FILEBOT_RENAME_METHOD="symlink" \
    FILEBOT_RENAME_MOVIES="{n} ({y})" \
    FILEBOT_RENAME_SERIES="{n}/Season {s.pad(2)}/{s00e00} - {t}" \
    FILEBOT_RENAME_ANIMES="{n}/{e.pad(3)} - {t}" \
    FILEBOT_RENAME_MUSICS="{n}/{fn}" \
    FILEBOT_LANG="fr" \
    FILEBOT_CONFLICT="skip"
RUN if [ "${WITH_FILEBOT}" == "YES" ] ; then apk add openjdk8-jre=8.275.01-r0 java-jna-native=4.5.1-r0 binutils=2.30-r6 wget=1.20.3-r0 nss=3.36.1-r1 --no-cache \
 && mkdir /filebot \
 && cd /filebot \
 && wget http://downloads.sourceforge.net/project/filebot/filebot/FileBot_${FILEBOT_VER}/FileBot_${FILEBOT_VER}-portable.tar.xz -O /filebot/filebot.tar.xz \
 && tar xJf filebot.tar.xz \
 && ln -sf /usr/local/lib/libzen.so.0.0.0 /filebot/lib/x86_64/libzen.so \
 && ln -sf /usr/local/lib/libmediainfo.so.0.0.0 /filebot/lib/x86_64/libmediainfo.so \
 && wget https://github.com/acoustid/chromaprint/releases/download/v${CHROMAPRINT_VER}/chromaprint-fpcalc-${CHROMAPRINT_VER}-linux-x86_64.tar.gz \
 && tar xvf chromaprint-fpcalc-${CHROMAPRINT_VER}-linux-x86_64.tar.gz \
 && mv chromaprint-fpcalc-${CHROMAPRINT_VER}-linux-x86_64/fpcalc /usr/local/bin \
 && strip -s /usr/local/bin/fpcalc \
 && apk del --no-cache binutils wget \
 && rm -rf /tmp/* /filebot/FileBot_${FILEBOT_VER}-portable.tar.xz /filebot/chromaprint-fpcalc-${CHROMAPRINT_VER}-linux-x86_64.tar.gz /filebot/chromaprint-fpcalc-${CHROMAPRINT_VER}-linux-x86_64 ; fi
COPY rootfs /
VOLUME /data /config
EXPOSE 8080/tcp
RUN chmod +x /usr/local/bin/startup
ENTRYPOINT ["/usr/local/bin/startup"]
CMD ["/bin/s6-svscan", "/etc/s6.d"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
