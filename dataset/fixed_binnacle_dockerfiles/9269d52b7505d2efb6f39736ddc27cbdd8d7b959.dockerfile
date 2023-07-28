FROM ubuntu:16.04
MAINTAINER Stefano Pacifici <stefano@cliqz.com>
ENV DEBIAN_FRONTEND="noninteractive"
#  Install the required packages. 1st Set is for Browser Project and the 2nd for Ruby and NodeJS.
RUN dpkg --add-architecture i386 \
 && apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 gnupg2=2.1.11-6ubuntu2.1 language-pack-en=1:16.04+20161009 lib32z1=1:1.2.8.dfsg-2ubuntu4.3 libc6:i386 libncurses5:i386 libstdc++6:i386 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python-virtualenv=15.0.1+ds-3ubuntu1.1 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 xz-utils=5.1.1alpha+20120614-2ubuntu2 -y \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 autoconf=2.69-9 automake=1:1.15-4ubuntu1 bison=2:3.0.4.dfsg-1 build-essential=12.1ubuntu2 ca-certificates=20210119~16.04.1 gawk=1:4.1.3+dfsg-0.1 libffi-dev=3.2.1-4 libgdbm-dev=1.8.3-13.1 libgmp-dev=2:6.1.0+dfsg-2 libgmp-dev=2:6.1.0+dfsg-2 libncurses5-dev=6.0+20160213-1ubuntu1 libreadline6-dev=6.3-8ubuntu2 libsqlite3-dev=3.11.0-1ubuntu1.5 libssl-dev=1.0.2g-1ubuntu4.20 libtool=2.4.6-0.1 libyaml-dev=0.1.6-3 pkg-config=0.29.1-0ubuntu1 sqlite3=3.11.0-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y \
 && apt-get clean -y \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Set the locale
RUN locale-gen en_US en_US.UTF-8
RUN dpkg-reconfigure locales
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8"
#   Add jenkins to the user group
ARG UID
ARG GID
RUN getent group $GID || groupadd jenkins --gid $GID \
 && useradd --create-home --shell /bin/bash jenkins --uid $UID --gid $GID
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"
ENV ANDROID_HOME="/home/jenkins/android_home"
ENV GRADLE_USER_HOME="/home/jenkins/gradle_home"
ENV NVM_DIR="/home/jenkins/nvm "
ENV NODE_VERSION="8.9.3"
USER jenkins
#  Install Android SDK and the Required SDKs
RUN mkdir -p $ANDROID_HOME ; mkdir -p $GRADLE_USER_HOME ; cd $ANDROID_HOME ; wget --output-document=sdktools.zip --quiet 'https://dl.google.com/android/repository/sdk-tools-linux-3859397.zip' ; unzip sdktools.zip ; rm -r sdktools.zip ; (while (true ) ; do echo y ;sleep 2 ; done ) | tools/bin/sdkmanager "build-tools;26.0.2" "platforms;android-23" "platforms;android-27" "platform-tools" "tools" "platforms;android-25" "extras;google;m2repository" "extras;android;m2repository" "extras;google;google_play_services"
ENV LD_LIBRARY_PATH="\"/home/jenkins/android_home/emulator/lib64/qt/lib\""
#   Install Node.JS
SHELL ["/bin/bash", "-l", "-c"]
RUN curl https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash \
 && . $NVM_DIR/nvm.sh
ENV PATH="\"$NVM_DIR/versions/node/v$NODE_VERSION/bin:$PATH\""
#  Installation of 'yarn'; 'appium' & 'wd' for Integration Tests
RUN npm install yarn@1.22.19 appium@1.22.3 wd@1.14.0 --global
#  Install Ruby and Fastlane
RUN gpg2 --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 \
 && curl -sSL https://get.rvm.io | bash -s stable --ruby=2.4.1 --autolibs=read-fail
RUN gem install fastlane --version 2.212.1
# Please add your HEALTHCHECK here!!!
