FROM ubuntu:trusty
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.92.37.8 python-software-properties=0.92.37.8 -y \
 && add-apt-repository ppa:beineri/opt-qt571-trusty
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 qt57base qt57svg build-essential=11.6ubuntu6 mesa-common-dev=10.1.3-0ubuntu0.6 libglu1-mesa-dev=9.0.0-2 -y \
 && rm -rf /var/lib/apt/lists/*
ENV QT_BASE_DIR="/opt/qt57"
ENV QTDIR="$QT_BASE_DIR"
ENV PATH="$QT_BASE_DIR/bin:$PATH"
ENV LD_LIBRARY_PATH="$QT_BASE_DIR/lib/x86_64-linux-gnu:$QT_BASE_DIR/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="$QT_BASE_DIR/lib/pkgconfig:$PKG_CONFIG_PATH"
#   Install dependencies and build tools
RUN apt-get update \
 && apt-get install --no-install-recommends ruby-dev=1:1.9.3.4 rpm=4.11.1-3ubuntu0.1 wget=1.15-1ubuntu1.14.04.5 bsdmainutils=9.0.5ubuntu1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 -y \
 && gem install fpm --version 1.15.1 \
 && wget http://mdocml.bsd.lv/snapshots/mandoc.tar.gz \
 && tar fzx mandoc.tar.gz \
 && cd mandoc-* \
 && ./configure \
 && make \
 && make install
#   Download and build cutechess
RUN cd / \
 && git clone https://github.com/cutechess/cutechess.git \
 && cd /cutechess \
 && qmake \
 && make -j4 \
 && make doc-txt \
 && make doc-html
#   Create .deb and .rpm packages
RUN mkdir -p /cutechess_pkg \
 && cd /cutechess_pkg \
 && mkdir -p usr/games \
 && mkdir -p usr/share/menu \
 && mkdir -p usr/share/pixmaps \
 && mkdir -p usr/share/applications \
 && mkdir -p usr/share/man/man6 \
 && mkdir -p usr/share/man/man5 \
 && mkdir -p usr/share/doc/cutechess \
 && cp /cutechess/projects/cli/cutechess-cli usr/games \
 && cp /cutechess/projects/gui/cutechess usr/games \
 && cp /cutechess/dist/linux/menu/cutechess usr/share/menu \
 && cp /cutechess/projects/gui/res/icons/cutechess_128x128.png usr/share/pixmaps/cutechess.png \
 && cp /cutechess/projects/gui/res/icons/cutechess_32x32.xpm usr/share/pixmaps/cutechess.xpm \
 && cp /cutechess/dist/linux/cutechess.desktop usr/share/applications \
 && cp /cutechess/docs/cutechess-cli.6 usr/share/man/man6 \
 && gzip usr/share/man/man6/cutechess-cli.6 \
 && cp /cutechess/docs/engines.json.5 usr/share/man/man5 \
 && gzip usr/share/man/man5/engines.json.5 \
 && cp /cutechess/COPYING usr/share/doc/cutechess/copyright \
 && cp /cutechess/README.md usr/share/doc/cutechess/README \
 && mkdir /finished_pkg \
 && export CUTECHESS_CLI_VERSION=$( grep "^CUTECHESS_CLI_VERSION" /cutechess/projects/cli/cli.pro | awk '{print $NF}' ;) \
 && export CUTECHESS_GUI_VERSION=$( grep "^CUTECHESS_VERSION" /cutechess/projects/gui/gui.pro | awk '{print $NF}' ;) \
 && export TODAY=$( date +%Y%m%d ;) \
 && fpm -s dir -t deb -C /cutechess_pkg -a "amd64" --license "GPLv3" --url "https://github.com/cutechess/cutechess" -n "cutechess" -v "$TODAY+$CUTECHESS_CLI_VERSION+$CUTECHESS_GUI_VERSION" --iteration 1 --category "games" -m "Ilari Pihlajisto <ilari.pihlajisto@mbnet.fi>" --description "Commandline and graphical interface for playing chess" -d "libc6 (>= 2.19)" -d "libgcc1 (>= 1:4.9)" -d "libqt5svg5 (>= 5.7.0)" -d "libqt5core5a (>= 5.7.0)" -d "libqt5gui5 (>= 5.7.0)" -d "libqt5widgets5 (>= 5.7.0)" -d "libqt5printsupport5 (>= 5.7.0)" -d "libqt5concurrent5 (>= 5.7.0)" -d "libstdc++6 (>= 4.8.4)" \
 && mv /cutechess_pkg/*.deb /finished_pkg/ \
 && fpm -s dir -t rpm -C /cutechess_pkg -a "x86_64" --license "GPLv3" --url "https://github.com/cutechess/cutechess" -n "cutechess" -v "$TODAY+$CUTECHESS_CLI_VERSION+$CUTECHESS_GUI_VERSION" --iteration 1 --category "Amusements/Games/Board/Chess" -m "Ilari Pihlajisto <ilari.pihlajisto@mbnet.fi>" --description "Commandline and graphical interface for playing chess" -d "qt5-qtbase >= 5.7.0" -d "qt5-qtsvg >= 5.7.0" \
 && mv /cutechess_pkg/*.rpm /finished_pkg/
#   Create .tar.gz package for cutechess-cli
RUN cd /cutechess \
 && mkdir -p /cutechess_pkg/cutechess-cli \
 && cd /cutechess_pkg \
 && mkdir -p ./cutechess-cli/lib \
 && cp $QT_BASE_DIR/lib/libQt5Core.so.5 cutechess-cli/lib/ \
 && cp /cutechess/projects/cli/cutechess-cli cutechess-cli/ \
 && cp /cutechess/COPYING cutechess-cli/ \
 && cp /cutechess/docs/man-style.css cutechess-cli/ \
 && cp /cutechess/tools/clop-cutechess-cli.py cutechess-cli/ \
 && cp /cutechess/dist/linux/cutechess-cli.sh cutechess-cli/ \
 && cp /cutechess/docs/cutechess-cli.6.html cutechess-cli/ \
 && cp /cutechess/docs/cutechess-cli.6.txt cutechess-cli/ \
 && cp /cutechess/docs/engines.json.5.html cutechess-cli/ \
 && cp /cutechess/docs/engines.json.5.txt cutechess-cli/ \
 && tar -zcvf cutechess-cli-linux64.tar.gz cutechess-cli \
 && export CUTECHESS_CLI_VERSION=$( grep "^CUTECHESS_CLI_VERSION" /cutechess/projects/cli/cli.pro | awk '{print $NF}' ;) \
 && mv cutechess-cli-linux64.tar.gz /finished_pkg/cutechess-cli-$CUTECHESS_CLI_VERSION-linux64.tar.gz
#   Copy the .deb package to the host
CMD cp /finished_pkg/cutechess*.* /package
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
