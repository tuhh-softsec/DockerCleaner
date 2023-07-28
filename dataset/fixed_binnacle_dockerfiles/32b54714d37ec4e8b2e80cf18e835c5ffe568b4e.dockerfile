#   Name: webrtcDocker
#   Description: installs webrtc in ubuntu trusty 14.04 environment
#
#   VERSION       1.0
#
#   Use the ubuntu base image
FROM ubuntu:trusty
MAINTAINER Oleg Blinnikov, osblinnikov@gmail.com
#   make sure the package repository is up to date
RUN :
RUN apt-get -y upgrade
#   Adding vnc server
#   no Upstart or DBus
#   https://github.com/dotcloud/docker/issues/1724#issuecomment-26294856
RUN apt-mark hold initscripts udev plymouth mountall
RUN dpkg-divert --local --rename --add /sbin/initctl \
 && ln -sf /bin/true /sbin/initctl
RUN (apt-get update ;apt-get install --no-install-recommends supervisor=3.0b2-1ubuntu0.1 openssh-server=1:6.6p1-2ubuntu2.13 pwgen=2.06-1ubuntu4 sudo=1.8.9p5-1ubuntu1.4 vim-tiny=2:7.4.052-1ubuntu3.1 net-tools=1.60-25ubuntu2.1 lxde=0.5.0-4ubuntu4 x11vnc=0.9.13-1.1 xvfb=2:1.15.1-0ubuntu2.11 gtk2-engines-murrine=0.98.2-0ubuntu2 ttf-ubuntu-font-family=0.80-0ubuntu6 nodejs=0.10.25~dfsg2-2ubuntu1.2 wget=1.15-1ubuntu1.14.04.5 curl=7.35.0-1ubuntu2.20 -y --force-yes )
RUN (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 git-svn=1:1.9.1-1ubuntu0.10 subversion=1.8.8-1ubuntu3.3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-7-jdk=7u211-2.6.17-0ubuntu0.1 -y )
#   Now create the webrtc user itself
RUN adduser --gecos "webRTC User" --disabled-password webrtc
RUN usermod -a -G dialout webrtc
COPY 99_aptget /etc/sudoers.d/99_aptget
RUN chmod 0440 /etc/sudoers.d/99_aptget \
 && chown root:root /etc/sudoers.d/99_aptget
RUN echo " ForwardX11Trusted yes\n" >> /etc/ssh/ssh_config
RUN mkdir -p /home/webrtc \
 && chown webrtc:webrtc /home/webrtc
#   And, as that user...
USER webrtc
#   clone depot tools from chromium project
RUN cd /home/webrtc \
 && git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git
RUN mkdir -p /home/webrtc/webrtc.googlecode.com
RUN cd /home/webrtc/webrtc.googlecode.com \
 && wget https://src.chromium.org/svn/trunk/src/build/install-build-deps.sh \
 && wget https://src.chromium.org/svn/trunk/src/build/install-build-deps-android.sh \
 && chmod 755 install-build-deps.sh \
 && chmod 755 install-build-deps-android.sh
RUN mkdir -p /home/webrtc/webrtc.googlecode.com/linux \
 && cd /home/webrtc/webrtc.googlecode.com/linux \
 && wget https://src.chromium.org/svn/trunk/src/build/linux/install-chromeos-fonts.py \
 && chmod u+rx install-chromeos-fonts.py
#   as root...
USER root
RUN dpkg --add-architecture i386 \
 && echo "deb http://us.archive.ubuntu.com/ubuntu/ trusty multiverse\n deb-src http://us.archive.ubuntu.com/ubuntu/ trusty multiverse\n deb http://us.archive.ubuntu.com/ubuntu/ trusty-updates multiverse\n deb-src http://us.archive.ubuntu.com/ubuntu/ trusty-updates multiverse\n deb http://archive.canonical.com/ trusty partner" >> /etc/apt/sources.list \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends lsb-release=4.1+Debian11ubuntu6.2 file=1:5.14-2ubuntu3.4 gcc-multilib=4:4.8.2-1ubuntu6 -y )
RUN /home/webrtc/webrtc.googlecode.com/install-build-deps.sh --no-prompt --syms \
 && /home/webrtc/webrtc.googlecode.com/install-build-deps-android.sh --no-prompt --syms
USER webrtc
RUN cd /home/webrtc/webrtc.googlecode.com \
 && JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64 PATH=$PATH:/home/webrtc/depot_tools GYP_DEFINES="enable_tracing=1 build_with_libjingle=1 build_with_chromium=0 libjingle_java=1 OS=android" GYP_GENERATOR_FLAGS="output_dir=out_android" fetch webrtc_android
#   RUN cd /home/webrtc/webrtc.googlecode.com && \
#       PATH=$PATH:/home/webrtc/depot_tools gclient config http://webrtc.googlecode.com/svn/trunk && \
#       echo "target_os = ['android', 'unix']" >> .gclient && \
#       JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/ PATH=$PATH:/home/webrtc/depot_tools gclient sync --force --jobs 100 && \
#  RUN cd /home/webrtc/webrtc.googlecode.com && chown -R webrtc:webrtc *
RUN cd /home/webrtc/webrtc.googlecode.com/src \
 && GYP_DEFINES="enable_tracing=1 build_with_libjingle=1 build_with_chromium=0 libjingle_java=1 OS=android" GYP_GENERATOR_FLAGS="output_dir=out_android" JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/ PATH=$PATH:/home/webrtc/depot_tools gclient runhooks \
 && JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/ PATH=$PATH:/home/webrtc/depot_tools ninja -C out_android/Debug AppRTCDemo \
 && JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/ PATH=$PATH:/home/webrtc/depot_tools ninja -C out_android/Release AppRTCDemo
RUN cd /home/webrtc/webrtc.googlecode.com/src \
 && GYP_DEFINES="enable_tracing=1 build_with_libjingle=1 build_with_chromium=0 libjingle_java=1" GYP_GENERATOR_FLAGS="output_dir=out" JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/ PATH=$PATH:/home/webrtc/depot_tools gclient runhooks \
 && JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/ PATH=$PATH:/home/webrtc/depot_tools ninja -C out/Debug \
 && JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/ PATH=$PATH:/home/webrtc/depot_tools ninja -C out/Release
USER root
#   Launch bash when launching the container
COPY startcontainer /usr/local/bin/startcontainer
RUN chmod 755 /usr/local/bin/startcontainer
#   as a user...
USER webrtc
#   ADD build.bash /home/webrtc/webrtc.googlecode.com/src/build.bash
#   RUN sudo chmod 755 /home/webrtc/webrtc.googlecode.com/src/build.bash
#   RUN /home/webrtc/webrtc.googlecode.com/src/build.bash
COPY noVNC /noVNC/
COPY supervisord.conf /
COPY bashrc /.bashrc
COPY bashrc /home/webrtc/.bashrc
RUN mkdir -p /home/webrtc/Desktop
COPY xterm /home/webrtc/Desktop/
CMD ["/bin/bash"]
ENTRYPOINT ["/usr/local/bin/startcontainer"]
# Please add your HEALTHCHECK here!!!
