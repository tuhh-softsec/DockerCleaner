#   docker-xbmc-server
#
#   Setup: Clone repo then checkout appropriate version
#     For stable (Helix)
#       $ git checkout master
#     For experimental (master development)
#       $ git checkout experimental
#
#   Create your own Build:
#   	$ docker build --rm=true -t $(whoami)/docker-xbmc-server .
#
#   Run your build:
#   There are two choices   
#     - UPnP server and webserver in the background: (replace ip and xbmc data location)
#  	  $ docker run -d --net=host --privileged -v /directory/with/xbmcdata:/opt/xbmc-server/portable_data $(whoami)/docker-xbmc-server
#
#     - Run only the libraryscan and quit: 
#  	  $ docker run -v /directory/with/xbmcdata:/opt/xbmc-server/portable_data --entrypoint=/opt/xbmc-server/xbmcVideoLibraryScan $(whoami)/docker-xbmc-server --no-test --nolirc -p
#
#   See README.md.
#   Source: https://github.com/wernerb/docker-xbmc-server
FROM ubuntu:14.04
MAINTAINER Werner Buck "email@wernerbuck.nl"
#   Set locale to UTF8
RUN locale-gen --no-purge en_US.UTF-8
RUN update-locale LANG=en_US.UTF-8
RUN dpkg-reconfigure locales
ENV LANGUAGE="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
#   Set Terminal to non interactive
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
#   Install java, git wget and supervisor
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 openjdk-7-jre-headless=7u211-2.6.17-0ubuntu0.1 supervisor=3.0b2-1ubuntu0.1 -y
#   Download XBMC, pick version from github
RUN git clone --depth 1 --branch "14.0-Helix" https://github.com/xbmc/xbmc.git
#   Add patches and xbmc-server files
COPY src/fixcrash.diff xbmc/fixcrash.diff
COPY src/5071.patch xbmc/5071.patch
COPY src/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#   Apply patches:
#  	fixrash.diff : Fixes crashing in UPnP 
RUN cd xbmc \
 && git apply fixcrash.diff \
 && git apply 5071.patch
#   Installs xbmc dependencies, configure, make, clean.
#   Taken out of the list of dependencies: libbluetooth3. Put in the list: libssh-4 libtag1c2a libcurl3-gnutls libnfs1
RUN apt-get install --no-install-recommends build-essential=11.6ubuntu6 gawk=1:4.0.1+dfsg-2.1ubuntu2 pmount=0.9.23-2 libtool=2.4.2-1.7ubuntu1 nasm=2.10.09-1ubuntu0.1 yasm=1.2.0-1ubuntu1 automake=1:1.14.1-2ubuntu1 cmake=2.8.12.2-0ubuntu3 gperf=3.0.4-1 zip=3.0-8 unzip=6.0-9ubuntu1.5 bison=2:3.0.2.dfsg-2 libsdl-dev libsdl-image1.2-dev=1.2.12-5+deb9u1build0.14.04.1 libsdl-gfx1.2-dev=2.0.23-3 libsdl-mixer1.2-dev=1.2.12-10 libfribidi-dev=0.19.6-1 liblzo2-dev=2.06-1.2ubuntu1.1 libfreetype6-dev=2.5.2-1ubuntu2.8 libsqlite3-dev=3.8.2-1ubuntu2.2 libogg-dev=1.3.1-1ubuntu1 libasound2-dev=1.0.27.2-3ubuntu7 python-sqlite=1.0.1-11 libglew-dev=1.10.0-3 libcurl3=7.35.0-1ubuntu2.20 libcurl4-gnutls-dev=7.35.0-1ubuntu2.20 libxrandr-dev=2:1.5.0-1~trusty1 libxrender-dev=1:0.9.8-1build0.14.04.1 libmad0-dev=0.15.1b-9ubuntu14.04.1 libogg-dev=1.3.1-1ubuntu1 libvorbisenc2=1.3.2-1.3ubuntu1.2 libsmbclient-dev=2:4.3.11+dfsg-0ubuntu0.14.04.20 libmysqlclient-dev=5.5.62-0ubuntu0.14.04.1 libpcre3-dev=1:8.31-2ubuntu2.3 libdbus-1-dev=1.6.18-0ubuntu4.5 libjasper-dev=1.900.1-14ubuntu3.5 libfontconfig-dev libbz2-dev=1.0.6-5 libboost-dev=1.54.0.1ubuntu1 libenca-dev=1.15-2 libxt-dev=1:1.1.4-1 libxmu-dev=2:1.1.1-1 libpng-dev libjpeg-dev=8c-2ubuntu8 libpulse-dev=1:4.0-0ubuntu11.1 mesa-utils=8.1.0-2 libcdio-dev=0.83-4.1ubuntu1 libsamplerate-dev libmpeg3-dev=1.5.4-5ubuntu1 libflac-dev=1.3.0-2ubuntu0.14.04.1 libiso9660-dev=0.83-4.1ubuntu1 libass-dev=0.10.1-3ubuntu1 libssl-dev=1.0.1f-1ubuntu2.27 fp-compiler=2.6.2-8 gdc=4.8.2-1ubuntu6 libmpeg2-4-dev=0.5.1-5ubuntu1 libmicrohttpd-dev=0.9.33-1 libmodplug-dev=1:0.8.8.4-4.1 libssh-dev=0.6.1-0ubuntu3.5 gettext=0.18.3.1-1ubuntu3.1 cvs=2:1.12.13+real-12ubuntu0.1 python-dev=2.7.5-5ubuntu3 libyajl-dev=2.0.4-4 libboost-thread-dev=1.54.0.1ubuntu1 libplist-dev=1.10-1ubuntu0.1 libusb-dev=2:0.1.12-23.3ubuntu1 libudev-dev=204-5ubuntu20.31 libtinyxml-dev=2.6.2-2 libcap-dev=1:2.24-0ubuntu2 autopoint=0.18.3.1-1ubuntu3.1 libltdl-dev=2.4.2-1.7ubuntu1 swig=2.0.11-1ubuntu2 libgtk2.0-bin=2.24.23-0ubuntu1.4 libtag1-dev=1.9.1-2 libtiff-dev libnfs1=1.3.0-2ubuntu1 libnfs-dev=1.3.0-2ubuntu1 libxslt-dev libbluray-dev=1:0.5.0-1 -y \
 && cd xbmc \
 && ./bootstrap \
 && ./configure --enable-nfs --enable-upnp --enable-ssh --disable-libbluray --disable-debug --disable-vdpau --disable-vaapi --disable-crystalhd --disable-vdadecoder --disable-vtbdecoder --disable-openmax --disable-joystick --disable-rsxs --disable-projectm --disable-rtmp --disable-airplay --disable-airtunes --disable-dvdcss --disable-optical-drive --disable-libusb --disable-libcec --disable-libmp3lame --disable-libcap --disable-udev --disable-libvorbisenc --disable-asap-codec --disable-afpclient --disable-goom --disable-fishbmc --disable-spectrum --disable-waveform --disable-avahi --disable-non-free --disable-texturepacker --disable-pulse --disable-dbus --disable-alsa --disable-hal --prefix=/opt/kodi-server \
 && make -j2 \
 && make install \
 && mkdir -p /opt/kodi-server/share/kodi/portable_data/ \
 && cd / \
 && rm -rf /xbmc \
 && apt-get purge -y --auto-remove git openjdk* build-essential gcc gawk pmount libtool nasm yasm automake cmake gperf zip unzip bison libsdl-dev libsdl-image1.2-dev libsdl-gfx1.2-dev libsdl-mixer1.2-dev libfribidi-dev liblzo2-dev libfreetype6-dev libsqlite3-dev libogg-dev libasound2-dev python-sqlite libglew-dev libcurl3 libcurl4-gnutls-dev libxrandr-dev libxrender-dev libmad0-dev libogg-dev libvorbisenc2 libsmbclient-dev libmysqlclient-dev libpcre3-dev libdbus-1-dev libjasper-dev libfontconfig-dev libbz2-dev libboost-dev libenca-dev libxt-dev libxmu-dev libpng-dev libjpeg-dev libpulse-dev mesa-utils libcdio-dev libsamplerate-dev libmpeg3-dev libflac-dev libiso9660-dev libass-dev libssl-dev fp-compiler gdc libmpeg2-4-dev libmicrohttpd-dev libmodplug-dev libssh-dev gettext cvs python-dev libyajl-dev libboost-thread-dev libplist-dev libusb-dev libudev-dev libtinyxml-dev libcap-dev autopoint libltdl-dev swig libgtk2.0-bin libtag1-dev libtiff-dev libnfs-dev libbluray-dev \
 && apt-get -y autoremove \
 && apt-get install --no-install-recommends fonts-liberation=1.07.3-3 libaacs0=0.7.0-1 libbluray1=1:0.5.0-1 libasound2=1.0.27.2-3ubuntu7 libass4=0.10.1-3ubuntu1 libasyncns0=0.8-4ubuntu2 libavcodec54=6:9.20-0ubuntu0.14.04.1 libavfilter3=6:9.20-0ubuntu0.14.04.1 libavformat54=6:9.20-0ubuntu0.14.04.1 libavutil52=6:9.20-0ubuntu0.14.04.1 libcaca0=0.99.beta18-1ubuntu5.1 libcap2=1:2.24-0ubuntu2 libcdio13=0.83-4.1ubuntu1 libcec2=2.1.4-1ubuntu2 libcrystalhd3=1:0.0~git20110715.fdd2f19-9ubuntu1 libdrm-nouveau2=2.4.67-1ubuntu0.14.04.2 libenca0=1.15-2 libflac8=1.3.0-2ubuntu0.14.04.1 libfontenc1=1:1.1.2-1 libgl1-mesa-dri=10.1.3-0ubuntu0.6 libgl1-mesa-glx=10.1.3-0ubuntu0.6 libglapi-mesa=10.1.3-0ubuntu0.6 libglew1.10=1.10.0-3 libglu1-mesa=9.0.0-2 libgsm1=1.0.13-4 libice6=2:1.0.8-2 libjson0=0.11-3ubuntu1.2 liblcms1=1.19.dfsg-1.2ubuntu5.1 libllvm3.5=1:3.5-4ubuntu2~trusty2 liblzo2-2=2.06-1.2ubuntu1.1 libmad0=0.15.1b-9ubuntu14.04.1 libmicrohttpd10=0.9.33-1 libmikmod2=3.1.16-1 libmodplug1=1:0.8.8.4-4.1 libmp3lame0=3.99.5+repack1-3ubuntu1 libmpeg2-4=0.5.1-5ubuntu1 libmysqlclient18=5.5.62-0ubuntu0.14.04.1 liborc-0.4-0=1:0.4.18-1ubuntu1 libpcrecpp0=1:8.31-2ubuntu2.3 libplist1=1.10-1ubuntu0.1 libpostproc52=6:0.git20120821-4 libpulse0=1:4.0-0ubuntu11.1 libpython2.7=2.7.6-8ubuntu0.5 libschroedinger-1.0-0=1.0.11-2ubuntu1 libsdl-mixer1.2=1.2.12-10 libsdl1.2debian=1.2.15-8ubuntu1.1 libshairport1=1.2.1~git20120110.aeb4987-2ubuntu1 libsm6=2:1.2.1-2 libsmbclient=2:4.3.11+dfsg-0ubuntu0.14.04.20 libsndfile1=1.0.25-7ubuntu2.2 libspeex1=1.2~rc1.1-1ubuntu1 libswscale2=6:9.20-0ubuntu0.14.04.1 libtalloc2=2.1.5-0ubuntu0.14.04.1 libtdb1=1.3.8-0ubuntu0.14.04.1 libtheora0=1.1.1+dfsg.1-3.2 libtinyxml2.6.2=2.6.2-2 libtxc-dxtn-s2tc0=0~git20131104-1.1 libva-glx1=1.3.0-2 libva-x11-1=1.3.0-2 libva1=1.3.0-2 libvdpau1=0.7-1ubuntu0.1 libvorbisfile3=1.3.2-1.3ubuntu1.2 libvpx1=1.3.0-2 libwbclient0=2:4.3.11+dfsg-0ubuntu0.14.04.20 libwrap0=7.6.q-25 libx11-xcb1=2:1.6.2-1ubuntu2.1 libxaw7=2:1.0.12-1 libxcb-glx0=1.10-2ubuntu1 libxcb-shape0=1.10-2ubuntu1 libxmu6=2:1.1.1-1 libxpm4=1:3.5.10-1ubuntu0.1 libxt6=1:1.1.4-1 libxtst6=2:1.2.2-1 libxv1=2:1.0.10-1 libxxf86dga1=2:1.1.4-1 libxxf86vm1=1:1.1.3-1 libyajl2=2.0.4-4 mesa-utils=8.1.0-2 mysql-common=5.5.62-0ubuntu0.14.04.1 python-cairo=1.8.8-1ubuntu5 python-gobject-2=2.28.6-12build1 python-gtk2=2.24.0-3ubuntu3 python-imaging=2.3.0-1ubuntu3.4 python-support=1.0.15 tcpd=7.6.q-25 ttf-liberation=1.07.3-3 libssh-4=0.6.1-0ubuntu3.5 libtag1c2a=1.9.1-2 libcurl3-gnutls=7.35.0-1ubuntu2.20 libnfs1=1.3.0-2ubuntu1 -y \
 && apt-get -y autoremove \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists /usr/share/man /usr/share/doc
#  Eventserver and webserver respectively.
EXPOSE 9777/udp 8089/tcp
ENTRYPOINT ["/usr/bin/supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
