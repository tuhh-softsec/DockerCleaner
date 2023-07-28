#   this file is used to build the image gpsbabel_build_environment used by travis.
FROM ubuntu:bionic AS qt_install
WORKDIR /app
#   update environment.
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.6.14 -y \
 && apt-get upgrade -y \
 && rm -rf /var/lib/apt/lists/*
#   install packages needed to install Qt
RUN apt-get update \
 && apt-get install --no-install-recommends libdbus-1-3=1.12.2-1ubuntu1.4 libfreetype6=2.8.1-2ubuntu2.2 libfontconfig1=2.12.6-0ubuntu2 libx11-6=2:1.6.4-3ubuntu0.4 libx11-xcb1=2:1.6.4-3ubuntu0.4 ca-cacert=2011.0523-2 wget=1.19.4-1ubuntu2.2 -y \
 && rm -rf /var/lib/apt/lists/*
#   basic build and test tools
COPY ./qtci/install-qt ./qtci/extract-qt-installer /app/
RUN QT_CI_PACKAGES=qt.qt5.5124.gcc_64,qt.qt5.5124.qtwebengine QT_CI_DOWNLOADER="wget -nv -c" PATH=/app:${PATH} ./install-qt 5.12.4 /opt
FROM ubuntu:bionic
LABEL maintainer="https://github.com/tsteven4"
WORKDIR /app
#   update environment.
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.6.14 -y \
 && apt-get upgrade -y \
 && rm -rf /var/lib/apt/lists/*
#   install packages needed for gpsbabel build
#   split into multiple commands to limit layer size
#   basic build and test tools
RUN apt-get update \
 && apt-get install --no-install-recommends g++=4:7.4.0-1ubuntu2.3 make=4.1-9.1ubuntu1 autoconf=2.69-11 gperf=3.1-1 git=1:2.17.1-1ubuntu0.17 valgrind=1:3.13.0-2ubuntu2.3 expat=2.2.5-3ubuntu0.9 libxml2-utils=2.9.4+dfsg1-6.1ubuntu1.8 bear=2.3.11-1 -y \
 && rm -rf /var/lib/apt/lists/*
#   alternative compiler
RUN apt-get update \
 && apt-get install --no-install-recommends clang=1:6.0-41~exp5~ubuntu1 -y \
 && rm -rf /var/lib/apt/lists/*
#   pkgs needed to build document
RUN apt-get update \
 && apt-get install --no-install-recommends fop=1:2.1-7 xsltproc=1.1.29-5ubuntu0.3 docbook-xml=4.5-8 docbook-xsl=1.79.1+dfsg-2 -y \
 && rm -rf /var/lib/apt/lists/*
#   pkgs with libraries needed by gpsbabel
RUN apt-get update \
 && apt-get install --no-install-recommends libusb-dev=2:0.1.12-31 pkg-config=0.29.1-0ubuntu2 libudev-dev=237-3ubuntu10.57 -y \
 && rm -rf /var/lib/apt/lists/*
#   dependencies of Qt
RUN apt-get update \
 && apt-get install --no-install-recommends libglib2.0-0=2.56.4-0ubuntu0.18.04.9 libx11-xcb-dev=2:1.6.4-3ubuntu0.4 libglu1-mesa-dev=9.0.0-2.1build1 libasound2=1.1.3-5ubuntu0.6 libxcomposite1=1:0.4.4-2 libxcursor1=1:1.1.15-1 -y \
 && rm -rf /var/lib/apt/lists/*
#   Qt
COPY --from=qt_install /opt/qt-5.12.4.env /opt/qtio.env
COPY --from=qt_install /opt/Qt/5.12.4 /opt/Qt/5.12.4
COPY --from=qt_install /opt/Qt/Licenses /opt/Qt/Licenses
#   pkgs needed to generate coverage report:
RUN apt-get update \
 && apt-get install --no-install-recommends gcovr=3.4-1 -y \
 && rm -rf /var/lib/apt/lists/*
#   install environment for locale test
RUN apt-get update \
 && apt-get install --no-install-recommends locales=2.27-3ubuntu1.6 -y \
 && rm -rf /var/lib/apt/lists/* \
 && sed -i 's/^# *\(en_US ISO-8859-1\)/\1/' /etc/locale.gen \
 && locale-gen \
 && locale -a
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
