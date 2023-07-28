#   Copyright 2018 Shift Devices AG
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM ubuntu:16.04 AS qt5base
ENV DEBIAN_FRONTEND="noninteractive"
RUN :
#   qt5 build deps and convenience tools.
RUN (apt-get update ;apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 bash-completion=1:2.1-4.2ubuntu1.1 git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 build-essential=12.1ubuntu2 ccache=3.2.4-1 python=2.7.12-1~16.04 libfontconfig1-dev=2.11.94-0ubuntu1.1 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libx11-dev=2:1.6.3-1ubuntu2.2 libxext-dev=2:1.3.3-1 libxfixes-dev=1:5.0.1-2 libxi-dev=2:1.7.6-1 libxrender-dev=1:0.9.9-0ubuntu1 libx11-xcb-dev=2:1.6.3-1ubuntu2.2 libx11-xcb-dev=2:1.6.3-1ubuntu2.2 libglu1-mesa-dev=9.0.0-2.1 libxrender-dev=1:0.9.9-0ubuntu1 libxi-dev=2:1.7.6-1 libatspi2.0-dev=2.18.3-4ubuntu1 libdbus-1-dev=1.10.6-1ubuntu3.6 flex=2.6.0-11 bison=2:3.0.4.dfsg-1 gperf=3.0.4-2 libicu-dev=55.1-7ubuntu0.5 libxslt-dev ruby=1:2.3.0+1 libssl-dev=1.0.2g-1ubuntu4.20 libxcursor-dev=1:1.1.14-1ubuntu0.16.04.2 libxcomposite-dev=1:0.4.4-1 libxdamage-dev=1:1.1.4-2 libxrandr-dev=2:1.5.0-1 libfontconfig1-dev=2.11.94-0ubuntu1.1 libcap-dev=1:2.24-12 libbz2-dev=1.0.6-8ubuntu0.2 libgcrypt11-dev=1.5.4-3+really1.6.5-2ubuntu0.6 libpci-dev=1:3.3.1-1.1ubuntu1.3 libnss3-dev=2:3.28.4-0ubuntu0.16.04.14 libxcursor-dev=1:1.1.14-1ubuntu0.16.04.2 libxcomposite-dev=1:0.4.4-1 libxdamage-dev=1:1.1.4-2 libxrandr-dev=2:1.5.0-1 libdrm-dev=2.4.91-2~16.04.1 libfontconfig1-dev=2.11.94-0ubuntu1.1 libxtst-dev=2:1.2.2-1 libasound2-dev=1.1.0-0ubuntu1 libcups2-dev=2.1.3-4ubuntu0.11 libpulse-dev=1:8.0-0ubuntu3.15 libudev-dev=229-4ubuntu21.31 libssl-dev=1.0.2g-1ubuntu4.20 libegl1-mesa-dev=18.0.5-0ubuntu0~16.04.1 ninja-build=1.5.1-0.1ubuntu1 gyp=0.1+20150913git1f374df9-1ubuntu1 libxss-dev=1:1.2.2-1 libasound2-dev=1.1.0-0ubuntu1 libgstreamer0.10-dev=0.10.36-1.5ubuntu1 libgstreamer-plugins-base0.10-dev=0.10.36-2ubuntu0.2 '^libxcb.*' -y )
#   Get the source code
RUN cd /tmp \
 && wget https://download.qt.io/archive/qt/5.11/5.11.2/single/qt-everywhere-src-5.11.2.tar.xz \
 && tar -xf qt-everywhere-src-5.11.2.tar.xz \
 && mv qt-everywhere-src-5.11.2 qt5
RUN cd /tmp/qt5 \
 && ./configure -prefix /opt/qt5 -opensource -confirm-license -nomake tests -nomake examples -dbus -xcb -system-xcb -qpa xcb -release -reduce-relocations -optimized-qmake
RUN cd /tmp/qt5 \
 && make -j1
RUN cd /tmp/qt5 \
 && make install
FROM ubuntu:16.04
COPY --from=qt5base /opt/qt5 /opt/qt5
RUN :
#   This is needed for compiling apps depending on the qt5 libs.
RUN (apt-get update ;apt-get install --no-install-recommends libxcb-xinerama0=1.11.1-1ubuntu1 libxcb-xkb-dev=1.11.1-1ubuntu1 libxcb-render-util0=0.3.9-1 libxcb-image0=0.4.0-1build1 libxcb-keysyms1=0.4.0-1 libxcb-icccm4=0.4.1-1ubuntu1 libcups2=2.1.3-4ubuntu0.11 libgl1-mesa-dev=18.0.5-0ubuntu0~16.04.1 libegl1-mesa-dev=18.0.5-0ubuntu0~16.04.1 libfontconfig1-dev=2.11.94-0ubuntu1.1 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libxi-dev=2:1.7.6-1 libxcursor-dev=1:1.1.14-1ubuntu0.16.04.2 libxrender-dev=1:0.9.9-0ubuntu1 libxss-dev=1:1.2.2-1 libxcomposite-dev=1:0.4.4-1 libasound2-dev=1.1.0-0ubuntu1 libxtst-dev=2:1.2.2-1 libxslt-dev libnss3-dev=2:3.28.4-0ubuntu0.16.04.14 libicu-dev=55.1-7ubuntu0.5 -y )
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
