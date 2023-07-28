FROM openjdk:8
LABEL maintainer="jfloff@inesc-id.pt"
# ##################
#  This Dockerfile was based on the following Dockerfiles
#  - docker-lineageos: existing unoptimized image
#     https://github.com/AnthoDingo/docker-lineageos/blob/autobuild/Dockerfile
#
#  default user
ENV USER="lineageos"
ENV BASE_DIR="/home/$USER" \
    DEVICE_CONFIGS_DIR="/home/device-config"
#  install packages
RUN set -ex ; apt-get update \
 && apt-get install --no-install-recommends android-sdk-platform-tools-common android-tools-adb android-tools-fastboot bc bison build-essential flex g++-multilib gcc-multilib git gnupg gperf imagemagick lib32ncurses5-dev lib32readline-dev lib32z1-dev libesd0-dev liblz4-tool libncurses5-dev libsdl1.2-dev libssl-dev libwxgtk3.0-dev libxml2 libxml2-utils lzop pngcrush rsync schedtool squashfs-tools xsltproc zip zlib1g-dev python procps less vim bsdmainutils fakeroot sudo -y ; rm -rf /var/lib/apt/lists/*
#  run config in a seperate layer so we cache it
RUN set -ex ; groupadd -r lineageos \
 && useradd -r -g lineageos lineageos \
 && usermod -u 1000 lineageos ; echo "lineageos ALL=NOPASSWD: ALL" >> /etc/sudoers; curl https://storage.googleapis.com/git-repo-downloads/repo > /usr/bin/repo; chmod a+x /usr/bin/repo ; git config --global color.ui true ; echo "source /etc/profile.d/init.sh" >> /etc/bash.bashrc
#  copy default configuration into container
COPY default.env init.sh /etc/profile.d/
#  copy script and config vars
COPY lineageos /bin
#  copy dir with several PRed device configurations
COPY device-config $DEVICE_CONFIGS_DIR
#  set volume and user home folder
USER $USER
WORKDIR $BASE_DIR
CMD /bin/bash
