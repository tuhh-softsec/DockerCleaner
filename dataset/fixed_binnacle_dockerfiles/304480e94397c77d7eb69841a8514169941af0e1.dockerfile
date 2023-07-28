FROM swerpbox/alpine-base
LABEL org.label-schema.vendor="SwerpBox: rTorrent/ruTorrent" \
      org.label-schema.build-date="2017-02-12T00:12:00+00:00" \
      org.label-schema.name="rTorrent and ruTorrent running on Alpine 3.5" \
      org.label-schema.vcs-type="git" \
      org.label-schema.vcs-url="https://github.com/strues/swerpbox" \
      maintainer="Steven Truesdell <steven@strues.io>"
ARG BUILD_CORES
ARG MEDIAINF_VER="0.7.90"
ARG RTORRENT_VER="0.9.6"
ARG LIBTORRENT_VER="0.13.6"
ARG RUTORRENT_ADDR="https://github.com/Novik/ruTorrent/archive/master.tar.gz"
#   install runtime packages
RUN apk add ca-certificates=20220614-r4 curl=7.88.1-r1 fcgi=2.4.2-r2 ffmpeg=5.1.3-r0 geoip=1.6.12-r3 gzip=1.12-r0 dtach=0.9-r3 tar=1.34-r2 unrar unzip=6.0-r13 wget=1.21.3-r2 irssi=1.4.4-r0 irssi-perl=1.4.4-r0 libressl-dev=3.6.2-r0 zlib=1.2.13-r0 zlib-dev=1.2.13-r0 libxml2-dev=2.10.4-r0 perl-archive-zip=1.68-r2 perl-net-ssleay=1.92-r2 perl-digest-sha1=2.13-r15 git=2.38.4-r1 zip=3.0-r10 --no-cache \
 && apk add php7 php7-cgi php7-fpm php7-json php7-mbstring php7-sockets php7-opcache php7-mcrypt php7-xmlrpc php7-pear --no-cache --repository http://nl.alpinelinux.org/alpine/edge/community \
 && apk add autoconf=2.71-r1 automake=1.16.5-r1 build-base=0.5-r3 subversion=1.14.2-r4 cppunit-dev=1.15.1-r1 curl-dev=7.88.1-r1 perl-dev=5.36.0-r0 file=5.43-r0 g++=12.2.1_git20220924-r4 gcc=12.2.1_git20220924-r4 libtool=2.4.7-r1 make=4.3-r1 ncurses-dev=6.3_p20221119-r0 --no-cache --virtual=build-deps \
 && cd /tmp \
 && git clone https://github.com/esmil/mktorrent \
 && svn checkout http://svn.code.sf.net/p/xmlrpc-c/code/stable xmlrpc-c \
 && git clone -b ${LIBTORRENT_VER} https://github.com/rakshasa/libtorrent.git \
 && git clone -b ${RTORRENT_VER} https://github.com/rakshasa/rtorrent.git \
 && cd /tmp/mktorrent \
 && make -j ${BUILD_CORES-$( grep -c "processor" /proc/cpuinfo ;)} \
 && make install \
 && mkdir -p /var/www/public/rutorrent /defaults/rutorrent-conf \
 && perl -MCPAN -e 'my $c = "CPAN::HandleConfig"; $c->load(doit => 1, autoconfig => 1); $c->edit(prerequisites_policy => "follow"); $c->edit(build_requires_install_policy => "yes"); $c->commit' \
 && curl -L http://cpanmin.us | perl - App::cpanminus \
 && cpanm HTML::Entities XML::LibXML JSON JSON::XS \
 && curl -o /tmp/libmediainfo.tar.gz -L "http://mediaarea.net/download/binary/libmediainfo0/${MEDIAINF_VER}/MediaInfo_DLL_${MEDIAINF_VER}_GNU_FromSource.tar.gz" \
 && curl -o /tmp/mediainfo.tar.gz -L "http://mediaarea.net/download/binary/mediainfo/${MEDIAINF_VER}/MediaInfo_CLI_${MEDIAINF_VER}_GNU_FromSource.tar.gz" \
 && mkdir -p /tmp/libmediainfo /tmp/mediainfo \
 && tar xf /tmp/libmediainfo.tar.gz -C /tmp/libmediainfo --strip-components=1 \
 && tar xf /tmp/mediainfo.tar.gz -C /tmp/mediainfo --strip-components=1 \
 && cd /tmp/libmediainfo \
 && ./SO_Compile.sh \
 && cd /tmp/libmediainfo/ZenLib/Project/GNU/Library \
 && make -j $( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && cd /tmp/libmediainfo/MediaInfoLib/Project/GNU/Library \
 && make -j $( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && cd /tmp/mediainfo \
 && ./CLI_Compile.sh \
 && cd /tmp/mediainfo/MediaInfo/Project/GNU/CLI \
 && make -j $( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && cd /tmp/xmlrpc-c \
 && ./configure \
 && make -j ${NB_CORES} \
 && make install \
 && cd /tmp/libtorrent \
 && ./autogen.sh \
 && ./configure \
 && make -j ${BUILD_CORES-$( grep -c "processor" /proc/cpuinfo ;)} \
 && make install \
 && cd /tmp/rtorrent \
 && ./autogen.sh \
 && ./configure --with-xmlrpc-c \
 && make -j ${BUILD_CORES-$( grep -c "processor" /proc/cpuinfo ;)} \
 && make install \
 && apk del --purge build-deps \
 && rm -rf /tmp/*
#   add local files
COPY rootfs/ /
#   ports and volumes
EXPOSE 49123/tcp 5000/tcp
VOLUME ["/config", "/data", "/logs"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
