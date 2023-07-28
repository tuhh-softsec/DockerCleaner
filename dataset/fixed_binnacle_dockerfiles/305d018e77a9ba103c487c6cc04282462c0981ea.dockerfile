FROM lsiobase/ubuntu:bionic AS buildstage
#  ############# build stage ##############
#   package versions
ARG KODI_NAME="Leia"
ARG KODI_VER="18.3"
#   defines which addons to build
ARG KODI_ADDONS="vfs.libarchive vfs.rar"
#   environment settings
ARG DEBIAN_FRONTEND="noninteractive"
#   copy patches and excludes
COPY patches/ /patches/
#   install build packages
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-11 automake=1:1.15.1-3ubuntu2 autopoint=0.19.8.1-6ubuntu0.3 binutils=2.30-21ubuntu1~18.04.8 cmake=3.10.2-1ubuntu2.18.04.2 curl=7.58.0-2ubuntu3.24 default-jre=2:1.11-68ubuntu1~18.04.1 g++=4:7.4.0-1ubuntu2.3 gawk=1:4.1.4+dfsg-1build1 gcc=4:7.4.0-1ubuntu2.3 git=1:2.17.1-1ubuntu0.17 gperf=3.1-1 libass-dev=1:0.14.0-1 libavahi-client-dev=0.7-3.1ubuntu1.3 libavahi-common-dev=0.7-3.1ubuntu1.3 libbluray-dev=1:1.0.2-3 libbz2-dev=1.0.6-8.1ubuntu0.2 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libegl1-mesa-dev=20.0.8-0ubuntu1~18.04.1 libflac-dev=1.3.2-1ubuntu0.1 libfmt-dev=4.0.0+ds-2 libfreetype6-dev=2.8.1-2ubuntu2.2 libfstrcmp-dev=0.7.D001-1.1build1 libgif-dev=5.1.4-2ubuntu0.1 libglew-dev=2.0.0-5 libiso9660-dev=1.0.0-2ubuntu2 libjpeg-dev=8c-2ubuntu8 liblcms2-dev=2.9-1ubuntu0.1 liblzo2-dev=2.08-1.2 libmicrohttpd-dev=0.9.59-1 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 libnfs-dev=2.0.0-1~exp1 libpcre3-dev=2:8.39-9ubuntu0.1 libplist-dev=2.0.0-2ubuntu1 libsmbclient-dev=2:4.7.6+dfsg~ubuntu-0ubuntu2.29 libsqlite3-dev=3.22.0-1ubuntu0.7 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libtag1-dev=1.11.1+dfsg.1-0.2build2 libtiff5-dev=4.0.9-5ubuntu0.10 libtinyxml-dev=2.6.2-4 libtool=2.4.6-2 libvorbis-dev=1.3.5-4.2 libxrandr-dev=2:1.5.1-1 libxslt-dev make=4.1-9.1ubuntu1 nasm=2.13.02-0.1 python-dev=2.7.15~rc1-1 rapidjson-dev=1.1.0+dfsg2-3 swig=3.0.12-1 uuid-dev=2.31.1-0.4ubuntu3.7 yasm=1.3.0-2build1 zip=3.0-11build1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y
#   fetch source and apply any required patches
RUN set -ex \
 && mkdir -p /tmp/kodi-source/build \
 && curl -o /tmp/kodi.tar.gz -L "https://github.com/xbmc/xbmc/archive/${KODI_VER}-${KODI_NAME}.tar.gz" \
 && tar xf /tmp/kodi.tar.gz -C /tmp/kodi-source --strip-components=1 \
 && cd /tmp/kodi-source \
 && git apply /patches/"${KODI_NAME}"/headless.patch
#   build package
RUN cd /tmp/kodi-source/build \
 && cmake ../. -DCMAKE_INSTALL_LIBDIR=/usr/lib -DCMAKE_INSTALL_PREFIX=/usr -DENABLE_AIRTUNES=OFF -DENABLE_ALSA=OFF -DENABLE_AVAHI=OFF -DENABLE_BLUETOOTH=OFF -DENABLE_BLURAY=ON -DENABLE_CAP=OFF -DENABLE_CEC=OFF -DENABLE_DBUS=OFF -DENABLE_DVDCSS=OFF -DENABLE_GLX=OFF -DENABLE_INTERNAL_FLATBUFFERS=ON -DENABLE_LIBUSB=OFF -DENABLE_NFS=ON -DENABLE_OPENGL=OFF -DENABLE_OPTICAL=OFF -DENABLE_PULSEAUDIO=OFF -DENABLE_SNDIO=OFF -DENABLE_UDEV=OFF -DENABLE_UPNP=ON -DENABLE_VAAPI=OFF -DENABLE_VDPAU=OFF \
 && make -j3 \
 && make DESTDIR=/tmp/kodi-build install
#   build kodi addons
RUN set -ex \
 && cd /tmp/kodi-source \
 && make -j3 -C tools/depends/target/binary-addons ADDONS="$KODI_ADDONS" PREFIX=/tmp/kodi-build/usr
#   install kodi send
RUN install -Dm755 /tmp/kodi-source/tools/EventClients/Clients/KodiSend/kodi-send.py /tmp/kodi-build/usr/bin/kodi-send \
 && install -Dm644 /tmp/kodi-source/tools/EventClients/lib/python/xbmcclient.py /tmp/kodi-build/usr/lib/python2.7/xbmcclient.py
FROM lsiobase/ubuntu:bionic
#  ############# runtime stage ##############
#   set version label
ARG BUILD_DATE
ARG VERSION
LABEL build_version="Linuxserver.io version:- ${VERSION} Build-date:- ${BUILD_DATE}"
LABEL maintainer="sparklyballs"
#   environment settings
ARG DEBIAN_FRONTEND="noninteractive"
ENV HOME="/config"
#   install runtime packages
RUN apt-get update \
 && apt-get install --no-install-recommends libass9=1:0.14.0-1 libbluray2=1:1.0.2-3 libegl1=1.0.0-2ubuntu2.3 libfstrcmp0=0.7.D001-1.1build1 libgl1=1.0.0-2ubuntu2.3 liblcms2-2=2.9-1ubuntu0.1 liblzo2-2=2.08-1.2 libmicrohttpd12=0.9.59-1 libmysqlclient20=5.7.41-0ubuntu0.18.04.1 libnfs11=2.0.0-1~exp1 libpcrecpp0v5=2:8.39-9ubuntu0.1 libpython2.7=2.7.17-1~18.04ubuntu1.11 libsmbclient=2:4.7.6+dfsg~ubuntu-0ubuntu2.29 libtag1v5=1.11.1+dfsg.1-0.2build2 libtinyxml2.6.2v5=2.6.2-4 libxrandr2=2:1.5.1-1 libxslt1.1=1.1.29-5ubuntu0.3 -y \
 && rm -rf /tmp/* /var/lib/apt/lists/* /var/tmp/*
#   copy local files and artifacts of build stages.
COPY root/ /
COPY --from=buildstage /tmp/kodi-build/usr/ /usr/
#   ports and volumes
VOLUME /config/.kodi
EXPOSE 8080/tcp 9090/tcp 9777/udp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
