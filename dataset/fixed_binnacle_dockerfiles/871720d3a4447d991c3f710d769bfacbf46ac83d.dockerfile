#   Copyright 2018 The Chromium Authors. All rights reserved.
#   Use of this source code is governed by a BSD-style license that can be
#   found in the LICENSE file.
#
#   Defines a docker image that can build sound open firmware
#
#   Usage:
#   check out sof
#   build docker image:
#   docker build --build-arg UID=$(id -u) -t sof .
#   docker run -it  -v <insert sof dir here>:/home/sof/workdir --user `id -u` sof
#
#   For incremental builds:
#   docker run -it  -v <insert sof dir here>:/home/sof/work/sof.git --user `id -u` sof ./incremental.sh
#
FROM ubuntu:18.04
ARG UID=1000
#   Set up proxy from host
COPY apt.conf /etc/apt/
ARG host_http_proxy
ARG host_https_proxy
ENV http_proxy="$host_http_proxy"
ENV https_proxy="$host_https_proxy"
RUN apt-get update -y \
 && apt-get install --no-install-recommends autoconf=2.69-11 bison=2:3.0.4.dfsg-1build1 build-essential=12.4ubuntu1 flex=2.6.4-6 gawk=1:4.1.4+dfsg-1build1 gettext=0.19.8.1-6ubuntu0.3 git=1:2.17.1-1ubuntu0.17 gperf=3.1-1 help2man=1.47.6 libncurses5-dev=6.1-1ubuntu1.18.04 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libtool=2.4.6-2 libtool-bin=2.4.6-2 pkg-config=0.29.1-0ubuntu2 software-properties-common=0.96.24.32.20 sudo=1.8.21p2-3ubuntu1.5 texinfo=6.5.0.dfsg.1-2 udev=237-3ubuntu10.57 wget=1.19.4-1ubuntu2.2 unzip=6.0-21ubuntu1.2 cmake=3.10.2-1ubuntu2.18.04.2 python3=3.6.7-1~18.04 -y
#   Use ToT alsa utils for the latest topology patches.
RUN mkdir -p /root/alsa-build \
 && cd /root/alsa-build \
 && git clone https://github.com/thesofproject/alsa-lib.git \
 && git clone https://github.com/thesofproject/alsa-utils.git \
 && cd /root/alsa-build/alsa-lib \
 && ./gitcompile \
 && make install \
 && cd /root/alsa-build/alsa-utils \
 && ./gitcompile \
 && make install \
 && cd /root/ \
 && rm -rf alsa-build
#   Set up sof user
RUN useradd --create-home -d /home/sof -u $UID -G sudo sof \
 && echo "sof:test0000" | chpasswd \
 && adduser sof sudo
ENV HOME="/home/sof"
#   build cross compiler
USER sof
RUN cd /home/sof \
 && git clone https://github.com/thesofproject/xtensa-overlay.git \
 && cd xtensa-overlay \
 && git checkout sof-gcc8.1 \
 && cd ../ \
 && git clone https://github.com/thesofproject/crosstool-ng.git \
 && mkdir -p /home/sof/work/ \
 && cd crosstool-ng \
 && git checkout sof-gcc8.1 \
 && ./bootstrap \
 && ./configure --prefix=`pwd ` \
 && make \
 && make install \
 && for arch in byt hsw apl cnl imx; do cp config-${arch}-gcc8.1-gdb8.1 .config \
 && sed -i 's#${CT_TOP_DIR}\/builds#\/home\/sof\/work#g' .config \
 && gl_cv_func_getcwd_path_max=yes ./ct-ng build \
 && ./ct-ng distclean ; done \
 && cd /home/sof/ \
 && rm -rf xtensa-overlay \
 && rm -rf crosstool-ng
ENV PATH="/home/sof/work/xtensa-byt-elf/bin:${PATH}"
ENV PATH="/home/sof/work/xtensa-hsw-elf/bin:${PATH}"
ENV PATH="/home/sof/work/xtensa-apl-elf/bin:${PATH}"
ENV PATH="/home/sof/work/xtensa-cnl-elf/bin:${PATH}"
ENV PATH="/home/sof/work/xtensa-imx-elf/bin:${PATH}"
RUN cd /home/sof \
 && git clone https://github.com/jcmvbkbc/newlib-xtensa.git newlib-xtensa.git \
 && cd newlib-xtensa.git \
 && git checkout -b xtensa origin/xtensa \
 && for arch in byt hsw apl cnl imx; do ./configure --target=xtensa-${arch}-elf --prefix=/home/sof/work/xtensa-root \
 && make \
 && make install \
 && rm -rf etc/config.cache ; done \
 && cd /home/sof/ \
 && rm -rf newlib-xtensa.git
#   Create direcroties for the host machines sof directories to be mounted.
RUN mkdir -p /home/sof/work/sof.git
USER sof
WORKDIR /home/sof/work/sof.git/
# Please add your HEALTHCHECK here!!!
