FROM ubuntu:14.04
#   python && pip
RUN apt-get update \
 && apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 python-dev=2.7.5-5ubuntu3 build-essential=11.6ubuntu6 git=1:1.9.1-1ubuntu0.10 telnet=0.17-36build2 amqp-tools=0.4.1-1 wget=1.15-1ubuntu1.14.04.5 axel=2.4-1 -y \
 && pip install pip==23.1 --upgrade \
 && pip install virtualenv==20.21.0 --upgrade
#   GNURadio
WORKDIR /projects
RUN apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 python-pip=1.5.4-1ubuntu4
RUN pip install PyBOMBS==2.3.5
RUN pybombs prefix init /usr/local -a default_prx
RUN pybombs config default_prefix default_prx
RUN pybombs recipes add gr-recipes git+https://github.com/gnuradio/gr-recipes.git
RUN pybombs recipes add gr-etcetera git+https://github.com/gnuradio/gr-etcetera.git
RUN apt-get install --no-install-recommends autoconf=2.69-6 automake=1:1.14.1-2ubuntu1 libfftw3-3=3.3.3-7ubuntu3 libasound2=1.0.27.2-3ubuntu7 libasound2-data=1.0.27.2-3ubuntu7 libcppunit-dev=1.13.1-2ubuntu1 libgsl0ldbl=1.16+dfsg-1ubuntu1 libgsl0-dev=1.16+dfsg-1ubuntu1 libbz2-dev=1.0.6-5 libboost-dev=1.54.0.1ubuntu1 libboost-date-time-dev=1.54.0.1ubuntu1 libboost-serialization-dev=1.54.0.1ubuntu1 libboost-filesystem-dev=1.54.0.1ubuntu1 libboost-system-dev=1.54.0.1ubuntu1 libboost-program-options-dev=1.54.0.1ubuntu1 libboost-regex-dev=1.54.0.1ubuntu1 libboost-atomic-dev=1.54.0.1ubuntu1 libboost-chrono-dev=1.54.0.1ubuntu1 libboost-thread-dev=1.54.0.1ubuntu1 libboost-test-dev=1.54.0.1ubuntu1 cmake=2.8.12.2-0ubuntu3 cmake-data=2.8.12.2-0ubuntu3 pkg-config=0.26-1ubuntu4 libbison-dev=2:3.0.2.dfsg-2 libssl-dev=1.0.1f-1ubuntu2.27 libssl-doc=1.0.1f-1ubuntu2.27 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libevent-dev=2.0.21-stable-1ubuntu1.14.04.2 libtool=2.4.2-1.7ubuntu1 python-crypto=2.6.1-4ubuntu0.3 python-openssl=0.13-2ubuntu6 python-pam=0.4.2-13.1ubuntu3 python-pyasn1=0.1.7-1ubuntu2.1 python-serial=2.6-1build1 python-twisted=13.2.0-1ubuntu1.2 python-twisted-bin=13.2.0-1ubuntu1.2 python-twisted-conch=1:13.2.0-1ubuntu1.2 python-twisted-core=13.2.0-1ubuntu1.2 python-twisted-lore=13.2.0-1ubuntu1.2 python-twisted-mail=13.2.0-1ubuntu1.2 python-twisted-names=13.2.0-1ubuntu1.2 python-twisted-news=13.2.0-1ubuntu1.2 python-twisted-runner=13.2.0-1ubuntu1.2 python-twisted-web=13.2.0-1ubuntu1.2 python-twisted-words=13.2.0-1ubuntu1.2 python-zope.interface=4.0.5-1ubuntu4 flex=2.5.35-10.1ubuntu2 python-cheetah=2.4.4-3.fakesyncbuild1 wget=1.15-1ubuntu1.14.04.5 liblog4cpp5-dev=1.0-4 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 python-sip-dev=4.15.5-1build1 fontconfig=2.11.0-0ubuntu4.2 fontconfig-config=2.11.0-0ubuntu4.2 fonts-dejavu-core=2.34-1ubuntu1 iso-codes=3.52-1 libaudio2=1.9.4-1 libavahi-client3=0.6.31-4ubuntu1.3 libavahi-common-data=0.6.31-4ubuntu1.3 libavahi-common3=0.6.31-4ubuntu1.3 libcups2=1.7.2-0ubuntu1.11 libdrm-amdgpu1=2.4.67-1ubuntu0.14.04.2 libdrm-dev=2.4.67-1ubuntu0.14.04.2 libdrm-intel1=2.4.67-1ubuntu0.14.04.2 libdrm-nouveau2=2.4.67-1ubuntu0.14.04.2 libdrm-radeon1=2.4.67-1ubuntu0.14.04.2 libelf1=0.158-0ubuntu5.3 libfontconfig1=2.11.0-0ubuntu4.2 libfreetype6=2.5.2-1ubuntu2.8 libgl1-mesa-dev=10.1.3-0ubuntu0.6 libgl1-mesa-dri=10.1.3-0ubuntu0.6 libgl1-mesa-glx=10.1.3-0ubuntu0.6 libglapi-mesa=10.1.3-0ubuntu0.6 libglu1-mesa=9.0.0-2 libglu1-mesa-dev=9.0.0-2 libgstreamer-plugins-base1.0-0=1.2.4-1~ubuntu2.1 libgstreamer1.0-0=1.2.4-0ubuntu1.1 libice6=2:1.0.8-2 libjbig0=2.0-2ubuntu4.1 libjpeg-turbo8=1.3.0-0ubuntu2.1 libjpeg8=8c-2ubuntu8 libllvm3.4=1:3.4-1ubuntu3 libmysqlclient18=5.5.62-0ubuntu0.14.04.1 liborc-0.4-0=1:0.4.18-1ubuntu1 libpciaccess0=0.13.2-1 libpthread-stubs0-dev=0.3-4 libqt4-dbus=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-declarative=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-designer=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-dev=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-dev-bin=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-help=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-network=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-opengl=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-opengl-dev=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-qt3support=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-script=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-scripttools=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-sql=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-sql-mysql=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-svg=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-test=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-xml=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqt4-xmlpatterns=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqtcore4=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqtdbus4=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqtgui4=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libqtwebkit-dev=2.3.2-0ubuntu7 libqtwebkit4=2.3.2-0ubuntu7 libsm6=2:1.2.1-2 libtiff5=4.0.3-7ubuntu0.11 libtxc-dxtn-s2tc0=0~git20131104-1.1 libx11-dev=2:1.6.2-1ubuntu2.1 libx11-doc=2:1.6.2-1ubuntu2.1 libx11-xcb-dev=2:1.6.2-1ubuntu2.1 libx11-xcb1=2:1.6.2-1ubuntu2.1 libxau-dev=1:1.0.8-1 libxcb-dri2-0=1.10-2ubuntu1 libxcb-dri2-0-dev=1.10-2ubuntu1 libxcb-dri3-0=1.10-2ubuntu1 libxcb-dri3-dev=1.10-2ubuntu1 libxcb-glx0=1.10-2ubuntu1 libxcb-glx0-dev=1.10-2ubuntu1 libxcb-present-dev=1.10-2ubuntu1 libxcb-present0=1.10-2ubuntu1 libxcb-randr0=1.10-2ubuntu1 libxcb-randr0-dev=1.10-2ubuntu1 libxcb-render0=1.10-2ubuntu1 libxcb-render0-dev=1.10-2ubuntu1 libxcb-shape0=1.10-2ubuntu1 libxcb-shape0-dev=1.10-2ubuntu1 libxcb-sync-dev=1.10-2ubuntu1 libxcb-sync1=1.10-2ubuntu1 libxcb-xfixes0=1.10-2ubuntu1 libxcb-xfixes0-dev=1.10-2ubuntu1 libxcb1-dev=1.10-2ubuntu1 libxdamage-dev=1:1.1.4-1ubuntu1 libxdamage1=1:1.1.4-1ubuntu1 libxdmcp-dev=1:1.1.1-1 libxext-dev=2:1.3.2-1ubuntu0.0.14.04.1 libxi6=2:1.7.1.901-1ubuntu1.1 libxrender1=1:0.9.8-1build0.14.04.1 libxshmfence-dev=1.1-2 libxshmfence1=1.1-2 libxslt1.1=1.1.28-2ubuntu0.2 libxt6=1:1.1.4-1 libxxf86vm-dev=1:1.1.3-1 libxxf86vm1=1:1.1.3-1 mesa-common-dev=10.1.3-0ubuntu0.6 mysql-common=5.5.62-0ubuntu0.14.04.1 qdbus=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 qt4-linguist-tools=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 qt4-qmake=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 qtchooser=39-g4717841-3 qtcore4-l10n=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 x11-common=1:7.7+1ubuntu8.1 x11proto-core-dev=7.0.26-1~ubuntu2 x11proto-damage-dev=1:1.2.1-2 x11proto-dri2-dev=2.8-2 x11proto-fixes-dev=1:5.0-2ubuntu2 x11proto-gl-dev=1.4.17-1 x11proto-input-dev=2.3-1 x11proto-kb-dev=1.0.6-2 x11proto-xext-dev=7.3.0-1 x11proto-xf86vidmode-dev=2.3.1-2 xorg-sgml-doctools=1:1.11-1 xtrans-dev=1.3.5-1~ubuntu14.04.2 python-qt4=4.10.4+dfsg-1ubuntu1 pyqt4-dev-tools=4.10.4+dfsg-1ubuntu1 libqwt5-qt4=5.2.3-1 libqwt-dev=6.0.0-1.2 libqwt6=6.0.0-1.2 python-numpy=1:1.8.2-0ubuntu0.1 python-qwt5-qt4=5.2.1~cvs20091107+dfsg-7build1 libfreetype6-dev=2.5.2-1ubuntu2.8 libpng12-dev=1.2.50-1ubuntu2.14.04.3 libfontconfig1-dev=2.11.0-0ubuntu4.2 libpixman-1-0=0.30.2-2ubuntu1.2 libxrender-dev=1:0.9.8-1build0.14.04.1 x11proto-render-dev=2:0.11.1-2 libcairo2=1.13.0~20140204-0ubuntu1.1 libdatrie1=0.2.8-1 libgraphite2-3=1.3.10-0ubuntu0.14.04.1 libharfbuzz0b=0.9.27-1ubuntu1.1 libpango-1.0-0=1.36.3-1ubuntu1.1 libpango1.0-0=1.36.3-1ubuntu1.1 libpangocairo-1.0-0=1.36.3-1ubuntu1.1 libpangoft2-1.0-0=1.36.3-1ubuntu1.1 libpangox-1.0-0=0.0.2-4ubuntu1 libpangoxft-1.0-0=1.36.3-1ubuntu1.1 libthai-data=0.1.20-3 libthai0=0.1.20-3 libxft2=2.3.1-2 libglib2.0-dev=2.40.2-0ubuntu1.1 libjpeg-turbo8-dev=1.3.0-0ubuntu2.1 libtiff5-dev=4.0.3-7ubuntu0.11 libgdk-pixbuf2.0-dev=2.30.7-0ubuntu1.8 libatk1.0-0=2.10.0-2ubuntu2 bsdmainutils=9.0.5ubuntu1 debhelper=9.20131227ubuntu1 dh-apparmor=2.10.95-0ubuntu2.6~14.04.4 gettext=0.18.3.1-1ubuntu3.1 gettext-base=0.18.3.1-1ubuntu3.1 gir1.2-atk-1.0=2.10.0-2ubuntu2 gir1.2-freedesktop=1.40.0-1ubuntu0.2 gir1.2-gtk-2.0=2.24.23-0ubuntu1.4 gir1.2-pango-1.0=1.36.3-1ubuntu1.1 groff-base=1.22.2-5 hicolor-icon-theme=0.13-1 intltool-debian=0.35.0+20060710.1 libasprintf-dev=0.18.3.1-1ubuntu3.1 libasprintf0c2=0.18.3.1-1ubuntu3.1 libatk1.0-dev=2.10.0-2ubuntu2 libcairo-gobject2=1.13.0~20140204-0ubuntu1.1 libcairo-script-interpreter2=1.13.0~20140204-0ubuntu1.1 libcairo2-dev=1.13.0~20140204-0ubuntu1.1 libcroco3=0.6.8-2ubuntu1 libgettextpo-dev=0.18.3.1-1ubuntu3.1 libgettextpo0=0.18.3.1-1ubuntu3.1 libgtk2.0-0=2.24.23-0ubuntu1.4 libgtk2.0-bin=2.24.23-0ubuntu1.4 libgtk2.0-common=2.24.23-0ubuntu1.4 libgtk2.0-dev=2.24.23-0ubuntu1.4 libharfbuzz-dev=0.9.27-1ubuntu1.1 libharfbuzz-gobject0=0.9.27-1ubuntu1.1 libharfbuzz-icu0=0.9.27-1ubuntu1.1 libice-dev=2:1.0.8-2 libmail-sendmail-perl=0.79.16-1 libpango1.0-dev=1.36.3-1ubuntu1.1 libpipeline1=1.3.0-1 libpixman-1-dev=0.30.2-2ubuntu1.2 libsm-dev=2:1.2.1-2 libsys-hostname-long-perl=1.4-3 libunistring0=0.9.3-5ubuntu3 libxcb-shm0-dev=1.10-2ubuntu1 libxcomposite-dev=1:0.4.4-1 libxcomposite1=1:0.4.4-1 libxcursor-dev=1:1.1.14-1ubuntu0.14.04.2 libxcursor1=1:1.1.14-1ubuntu0.14.04.2 libxft-dev=2.3.1-2 libxi-dev=2:1.7.1.901-1ubuntu1.1 libxinerama-dev=2:1.1.3-1 libxinerama1=2:1.1.3-1 libxml2-utils=2.9.1+dfsg1-3ubuntu4.13 libxrandr-dev=2:1.5.0-1~trusty1 libxrandr2=2:1.5.0-1~trusty1 man-db=2.6.7.1-1ubuntu1 po-debconf=1.0.16+nmu2ubuntu1 x11proto-composite-dev=1:0.4.2-2 x11proto-randr-dev=1.5.0-1~trusty2 x11proto-xinerama-dev=1.2.1-2 swig2.0=2.0.11-1ubuntu2 python-wxgtk2.8=2.8.12.1+dfsg-2ubuntu2 python-cairo-dev=1.8.8-1ubuntu5 gobject-introspection=1.40.0-1ubuntu0.2 python-gobject-2-dev=2.28.6-12build1 python-gtk2=2.24.0-3ubuntu3 libfftw3-dev=3.3.3-7ubuntu3 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt1-dev=1.1.28-2ubuntu0.2 python-lxml=3.3.3-1ubuntu0.2 libusb-1.0-0-dev=2:1.0.17-1ubuntu2 amqp-tools=0.4.1-1 wget=1.15-1ubuntu1.14.04.5 axel=2.4-1 telnet=0.17-36build2 nodejs=0.10.25~dfsg2-2ubuntu1.2 npm=1.3.10~dfsg-1 -y
RUN npm install amqp-ts@1.8.0 -g
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!