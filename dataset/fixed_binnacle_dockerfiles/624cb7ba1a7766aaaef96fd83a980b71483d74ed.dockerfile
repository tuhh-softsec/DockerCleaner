#
#   uIota Dockerfile
#
#   The resulting image will contain everything needed to build uIota FW.
#
#   Setup: (only needed once per Dockerfile change)
#   1. install docker, add yourself to docker group, enable docker, relogin
#   2. # docker build -t uiota-build .
#
#   Usage:
#   3. cd to riot root
#   4. # docker run -i -t -u $UID -v $(pwd):/data/riotbuild uiota-build ./dist/tools/compile_test/compile_test.py
FROM debian:stretch
MAINTAINER Kristoffer Ek <stoffer@skulp.net>
#   unrar is non-free
RUN "echo" "deb http://http.us.debian.org/debian stretch non-free" >> /etc/apt/sources.list
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends aptitude=0.8.7-1 autoconf=2.69-10 automake=1:1.15-6 bash=4.4-5 bison=2:3.0.4.dfsg-1+b1 bzip2=1.0.6-8.1 flex=2.6.1-1.3 g++=4:6.3.0-4 gawk=1:4.1.4+dfsg-1 gcc=4:6.3.0-4 git=1:2.11.0-3+deb9u7 gnupg=2.1.18-8~deb9u4 gperf=3.0.4-2+b1 help2man=1.47.4 joe=4.4-1 libexpat-dev libtool=2.4.6-2 libtool-bin=2.4.6-2 make=4.1-9.1 ncurses-dev nano=2.7.4-1 python=2.7.13-2 python-dev=2.7.13-2 python-serial=3.2.1-1 sed=4.4-1 texinfo=6.3.0.dfsg.1-1+b2 unrar unzip=6.0-21+deb9u2 vim=2:8.0.0197-4+deb9u7 wget=1.18-5+deb9u3 splint=3.1.2.dfsg1-4 sudo=1.8.19p1-2.1+deb9u3 screen=4.5.0-6+deb9u1 software-properties-common=0.96.20.2-1+deb9u1 -y )
#   Java
RUN echo "deb http://ppa.launchpad.net/linuxuprising/java/ubuntu bionic main" > /etc/apt/sources.list.d/linuxuprising-java.list \
 && :
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 73C3DB2A
RUN echo oracle-java11-installer shared/accepted-oracle-license-v1-2 select true | sudo /usr/bin/debconf-set-selections
RUN echo oracle-java11-installer shared/accepted-oracle-licence-v1-2 boolean true | sudo /usr/bin/debconf-set-selections
RUN (apt-get update ;apt-get install --no-install-recommends oracle-java11-set-default -y --allow-unauthenticated )
#   Adduser `meterlogger`
RUN perl -pi -e 's/^#?\%sudo\W+ALL=\(ALL\:ALL\)\W+ALL/\%sudo\tALL=\(ALL\:ALL\) NOPASSWD\: ALL/' /etc/sudoers
RUN adduser --disabled-password --gecos "" meterlogger \
 && usermod -a -G dialout meterlogger
RUN usermod -a -G sudo meterlogger
#   Create our main work directory
RUN mkdir /meterlogger
RUN chown -R meterlogger:meterlogger /meterlogger
#   Crosstool demands non-root user for compilation
USER meterlogger
#   esp-open-sdk
RUN cd /meterlogger \
 && git clone --recursive https://github.com/nabovarme/esp-open-sdk.git \
 && cd /meterlogger/esp-open-sdk \
 && git checkout sdk-v2.2.x
RUN rm -fr /meterlogger/esp-open-sdk/esp-open-lwip
RUN cd /meterlogger/esp-open-sdk \
 && git clone https://github.com/nabovarme/esp-open-lwip.git \
 && cd /meterlogger/esp-open-sdk/esp-open-lwip \
 && git checkout dns_cache
RUN cd /meterlogger/esp-open-sdk \
 && make STANDALONE=y
#   EspStackTraceDecoder.jar
RUN cd /meterlogger \
 && wget https://github.com/littleyoda/EspStackTraceDecoder/releases/download/untagged-59a763238a6cedfe0362/EspStackTraceDecoder.jar
#   meterlogger
RUN cd /meterlogger \
 && git clone --recursive https://github.com/nabovarme/MeterLogger.git \
 && cd /meterlogger/MeterLogger \
 && git checkout dns_cache
USER root
#   Export ENV
ENV PATH="/meterlogger/esp-open-sdk/xtensa-lx106-elf/bin:$PATH"
ENV XTENSA_TOOLS_ROOT="/meterlogger/esp-open-sdk/xtensa-lx106-elf/bin"
ENV SDK_BASE="/meterlogger/esp-open-sdk/sdk"
WORKDIR /meterlogger/MeterLogger
CMD cp /meterlogger/esp-open-sdk/xtensa-lx106-elf/bin/esptool.py /meterlogger/MeterLogger/tools/ \
 && cd /meterlogger/MeterLogger \
 && eval $BUILD_ENV make clean all
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
