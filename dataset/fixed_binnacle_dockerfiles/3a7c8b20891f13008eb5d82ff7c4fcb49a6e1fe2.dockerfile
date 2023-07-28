FROM debian:stretch AS buildstage
ARG KODI_NAME="Leia"
ARG KODI_VERSION="18.2"
ARG DEBIAN_FRONTEND="noninteractive"
COPY dpkg_excludes /etc/dpkg/dpkg.cfg.d/excludes
RUN apt-get update \
 && apt-get install --no-install-recommends ant=1.9.9-1+deb9u1 git-core=1:2.11.0-3+deb9u7 build-essential=12.3 autoconf=2.69-10 automake=1:1.15-6 cmake=3.7.2-1 pkg-config=0.29-4+b1 autopoint=0.19.8.1-2+deb9u1 libtool=2.4.6-2 swig=3.0.10-1.1 doxygen=1.8.13-4+b1 default-jdk-headless=2:1.8-58+deb9u1 libbz2-dev=1.0.6-8.1 liblzo2-dev=2.08-1.2+b2 libtinyxml-dev=2.6.2-4+deb9u1 libmariadbclient-dev-compat=10.1.48-0+deb9u2 libcurl4-openssl-dev=7.52.1-5+deb9u16 libssl-dev=1.1.0l-1~deb9u6 libyajl-dev=2.1.0-2+b3 libxml2-dev=2.9.4+dfsg1-2.2+deb9u7 libxslt-dev libsqlite3-dev=3.16.2-5+deb9u3 libnfs-dev=1.11.0-2 libpcre3-dev=2:8.39-3 libtag1-dev=1.11.1+dfsg.1-0.3+deb9u1 libsmbclient-dev=2:4.5.16+dfsg-1+deb9u4 libmicrohttpd-dev=0.9.51-1 libgnutls28-dev=3.5.8-5+deb9u6 libass-dev=1:0.13.4-2 libxrandr-dev=2:1.5.1-1 libegl1-mesa-dev=13.0.6-1+b2 libgif-dev=5.1.4-0.4 libjpeg-dev=1:1.5.1-2+deb9u2 libglu1-mesa-dev=9.0.0-2.1 gawk=1:4.1.4+dfsg-1 gperf=3.0.4-2+b1 curl=7.52.1-5+deb9u16 m4=1.4.18-1 python-dev=2.7.13-2 uuid-dev=2.29.2-1+deb9u1 yasm=1.3.0-2+b1 unzip=6.0-21+deb9u2 libiso9660-dev=0.83-4.3+b1 libfstrcmp-dev=0.7.D001-1.1+b2 zip=3.0-11+b1 -y
COPY kodi-headless.patch /tmp/kodi-headless.patch
RUN mkdir -p /tmp/kodi_src \
 && curl -o /tmp/kodi.tar.gz -L "https://github.com/xbmc/xbmc/archive/${KODI_VERSION}-${KODI_NAME}.tar.gz" \
 && tar xf /tmp/kodi.tar.gz -C /tmp/kodi_src --strip-components=1 \
 && cd /tmp/kodi_src \
 && git apply /tmp/kodi-headless.patch
RUN mkdir /tmp/kodi_src/build \
 && cd /tmp/kodi_src/build \
 && cmake ../ -DCMAKE_INSTALL_LIBDIR=/usr/lib -DCMAKE_INSTALL_PREFIX=/usr -DENABLE_INTERNAL_FLATBUFFERS=ON -DENABLE_INTERNAL_FMT=ON -DENABLE_INTERNAL_RapidJSON=ON -DENABLE_SMBCLIENT=ON -DENABLE_MYSQLCLIENT=ON -DENABLE_NFS=ON -DENABLE_UPNP=ON -DENABLE_LCMS2=OFF -DENABLE_AIRTUNES=OFF -DENABLE_CAP=OFF -DENABLE_DVDCSS=OFF -DENABLE_LIBUSB=OFF -DENABLE_EVENTCLIENTS=OFF -DENABLE_OPTICAL=OFF -DENABLE_CEC=OFF -DENABLE_BLURAY=OFF -DENABLE_BLUETOOTH=OFF -DENABLE_PULSEAUDIO=OFF -DENABLE_AVAHI=OFF -DENABLE_ALSA=OFF -DENABLE_DBUS=OFF -DENABLE_UDEV=OFF -DENABLE_VAAPI=OFF -DENABLE_VDPAU=OFF -DENABLE_GLX=OFF -DENABLE_SNDIO=OFF -DENABLE_LIRCCLIENT=OFF \
 && make -j$( nproc --all ;) \
 && make DESTDIR=/tmp/kodi_build install
RUN cp /tmp/kodi_src/tools/EventClients/Clients/KodiSend/kodi-send.py /tmp/kodi_build/usr/bin/kodi-send \
 && mkdir -p /tmp/kodi_build/usr/lib/python2.7/ \
 && cp /tmp/kodi_src/tools/EventClients/lib/python/xbmcclient.py /tmp/kodi_build/usr/lib/python2.7/xbmcclient.py
FROM debian:stretch
MAINTAINER milaq
LABEL build_version="Build-date:- ${BUILD_DATE}"
COPY --from=buildstage /tmp/kodi_build/usr/ /usr/
ARG DEBIAN_FRONTEND="noninteractive"
COPY dpkg_excludes /etc/dpkg/dpkg.cfg.d/excludes
RUN apt-get update \
 && apt-get install --no-install-recommends libcurl3=7.52.1-5+deb9u16 libegl1-mesa=13.0.6-1+b2 libglu1-mesa=9.0.0-2.1 libfreetype6=2.6.3-3.2+deb9u2 libfribidi0=0.19.7-1+deb9u2 libglew2.0=2.0.0-3+b1 liblzo2-2=2.08-1.2+b2 libmicrohttpd12=0.9.51-1 libmariadbclient18=10.1.48-0+deb9u2 libnfs8=1.11.0-2 libpcrecpp0v5=2:8.39-3 libpython2.7=2.7.13-2+deb9u6 libsmbclient=2:4.5.16+dfsg-1+deb9u4 libtag1v5=1.11.1+dfsg.1-0.3+deb9u1 libtinyxml2.6.2v5=2.6.2-4+deb9u1 libxml2=2.9.4+dfsg1-2.2+deb9u7 libcdio13=0.83-4.3+b1 libxcb-shape0=1.12-1 libxrandr2=2:1.5.1-1 libxslt1.1=1.1.29-2.1+deb9u2 libyajl2=2.1.0-2+b3 libass5=1:0.13.4-2 libiso9660-8=0.83-4.3+b1 libfstrcmp0=0.7.D001-1.1+b2 ca-certificates=20200601~deb9u2 -y \
 && apt-get clean
RUN mkdir /var/cache/samba
RUN mkdir -p /config/userdata
COPY advancedsettings.xml.default /usr/local/share/kodi/advancedsettings.xml.default
COPY smb.conf /config/.smb/user.conf
COPY kodi_init /sbin/kodi_init
RUN useradd -m -u 10000 kodi \
 && chown kodi. -R /config \
 && ln -s /config /usr/share/kodi/portable_data
VOLUME /config
WORKDIR /config
EXPOSE 8080/tcp 9090/tcp 9777/udp
CMD ["/sbin/kodi_init"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
