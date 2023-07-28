FROM openjdk:8
LABEL maintainer="jfloff@inesc-id.pt"
#  ##################
#   This Dockerfile was based on the following Dockerfiles
#   - docker-lineageos: existing unoptimized image
#      https://github.com/AnthoDingo/docker-lineageos/blob/autobuild/Dockerfile
#
#   default user
ENV USER="lineageos"
ENV BASE_DIR="/home/$USER" \
    DEVICE_CONFIGS_DIR="/home/device-config"
#   install packages
RUN set -ex ; apt-get update \
 && apt-get install --no-install-recommends android-sdk-platform-tools-common=28.0.2+3 android-tools-adb android-tools-fastboot bc=1.07.1-2+b2 bison=2:3.7.5+dfsg-1 build-essential=12.9 flex=2.6.4-8 g++-multilib=4:10.2.1-1 gcc-multilib=4:10.2.1-1 git=1:2.30.2-1+deb11u2 gnupg=2.2.27-2+deb11u2 gperf=3.1-1 imagemagick=8:6.9.11.60+dfsg-1.3+deb11u1 lib32ncurses5-dev lib32readline-dev=8.1-1 lib32z1-dev=1:1.2.11.dfsg-2+deb11u2 libesd0-dev liblz4-tool=1.9.3-2 libncurses5-dev=6.2+20201114-2 libsdl1.2-dev=1.2.15+dfsg2-6 libssl-dev=1.1.1n-0+deb11u4 libwxgtk3.0-dev libxml2=2.9.10+dfsg-6.7+deb11u3 libxml2-utils=2.9.10+dfsg-6.7+deb11u3 lzop=1.04-2 pngcrush=1.8.13-0.1 rsync=3.2.3-4+deb11u1 schedtool=1.3.0-4 squashfs-tools=1:4.4-2+deb11u2 xsltproc=1.1.34-4+deb11u1 zip=3.0-12 zlib1g-dev=1:1.2.11.dfsg-2+deb11u2 python procps=2:3.3.17-5 less=551-2 vim=2:8.2.2434-3+deb11u1 bsdmainutils=12.1.7+nmu3 fakeroot=1.25.3-1.1 sudo=1.9.5p2-3+deb11u1 -y ; rm -rf /var/lib/apt/lists/*
#   run config in a seperate layer so we cache it
RUN set -ex ; groupadd -r lineageos \
 && useradd -r -g lineageos lineageos \
 && usermod -u 1000 lineageos ; echo "lineageos ALL=NOPASSWD: ALL" >> /etc/sudoers; curl https://storage.googleapis.com/git-repo-downloads/repo > /usr/bin/repo; chmod a+x /usr/bin/repo ; git config --global color.ui true ; echo "source /etc/profile.d/init.sh" >> /etc/bash.bashrc
#   copy default configuration into container
COPY default.env init.sh /etc/profile.d/
#   copy script and config vars
COPY lineageos /bin
#   copy dir with several PRed device configurations
COPY device-config $DEVICE_CONFIGS_DIR
#   set volume and user home folder
USER $USER
WORKDIR $BASE_DIR
CMD /bin/bash
# Please add your HEALTHCHECK here!!!
