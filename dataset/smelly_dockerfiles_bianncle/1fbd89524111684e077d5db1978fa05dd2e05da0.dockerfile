#  this file is used to build the image gpsbabel_build_environment used by travis.
FROM ubuntu:bionic AS qt_install
WORKDIR /app
#  update environment.
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils -y \
 && apt-get upgrade -y \
 && rm -rf /var/lib/apt/lists/*
#  install packages needed to install Qt
RUN apt-get update \
 && apt-get install --no-install-recommends libdbus-1-3 libfreetype6 libfontconfig1 libx11-6 libx11-xcb1 ca-cacert wget -y \
 && rm -rf /var/lib/apt/lists/*
#  basic build and test tools
COPY ./qtci/install-qt ./qtci/extract-qt-installer /app/
RUN QT_CI_PACKAGES=qt.qt5.5124.gcc_64,qt.qt5.5124.qtwebengine QT_CI_DOWNLOADER="wget -nv -c" PATH=/app:${PATH} ./install-qt 5.12.4 /opt
FROM ubuntu:bionic
LABEL maintainer="https://github.com/tsteven4"
WORKDIR /app
#  update environment.
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils -y \
 && apt-get upgrade -y \
 && rm -rf /var/lib/apt/lists/*
#  install packages needed for gpsbabel build
#  split into multiple commands to limit layer size
#  basic build and test tools
RUN apt-get update \
 && apt-get install --no-install-recommends g++ make autoconf gperf git valgrind expat libxml2-utils bear -y \
 && rm -rf /var/lib/apt/lists/*
#  alternative compiler
RUN apt-get update \
 && apt-get install --no-install-recommends clang -y \
 && rm -rf /var/lib/apt/lists/*
#  pkgs needed to build document
RUN apt-get update \
 && apt-get install --no-install-recommends fop xsltproc docbook-xml docbook-xsl -y \
 && rm -rf /var/lib/apt/lists/*
#  pkgs with libraries needed by gpsbabel
RUN apt-get update \
 && apt-get install --no-install-recommends libusb-dev pkg-config libudev-dev -y \
 && rm -rf /var/lib/apt/lists/*
#  dependencies of Qt
RUN apt-get update \
 && apt-get install --no-install-recommends libglib2.0-0 libx11-xcb-dev libglu1-mesa-dev libasound2 libxcomposite1 libxcursor1 -y \
 && rm -rf /var/lib/apt/lists/*
#  Qt
COPY --from=qt_install /opt/qt-5.12.4.env /opt/qtio.env
COPY --from=qt_install /opt/Qt/5.12.4 /opt/Qt/5.12.4
COPY --from=qt_install /opt/Qt/Licenses /opt/Qt/Licenses
#  pkgs needed to generate coverage report:
RUN apt-get update \
 && apt-get install --no-install-recommends gcovr -y \
 && rm -rf /var/lib/apt/lists/*
#  install environment for locale test
RUN apt-get update \
 && apt-get install --no-install-recommends locales -y \
 && rm -rf /var/lib/apt/lists/* \
 && sed -i 's/^# *\(en_US ISO-8859-1\)/\1/' /etc/locale.gen \
 && locale-gen \
 && locale -a
