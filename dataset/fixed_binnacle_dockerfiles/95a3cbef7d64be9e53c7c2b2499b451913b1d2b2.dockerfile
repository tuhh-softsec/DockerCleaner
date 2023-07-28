FROM ubuntu:18.04
LABEL maintainer="\"Srinivasan Sekar <srinivasan.sekar1990@gmail.com>\""
ENV DEBIAN_FRONTEND="noninteractive"
#  =============
#   Set WORKDIR
#  =============
WORKDIR /root
#  ==================
#   General Packages
#  ------------------
#   openjdk-8-jdk
#     Java
#   ca-certificates
#     SSL client
#   tzdata
#     Timezone
#   zip
#     Make a zip file
#   unzip
#     Unzip zip file
#   curl
#     Transfer data from or to a server
#   wget
#     Network downloader
#   libqt5webkit5
#     Web content engine (Fix issue in Android)
#   libgconf-2-4
#     Required package for chrome and chromedriver to run on Linux
#   xvfb
#     X virtual framebuffer
#   gnupg
#     Encryption software. It is needed for nodejs
#   salt-minion
#     Infrastructure management (client-side)
#  ==================
RUN apt-get update -qqy \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 ca-certificates=20211016ubuntu0.18.04.1 tzdata=2022g-0ubuntu0.18.04 zip=3.0-11build1 unzip=6.0-21ubuntu1.2 curl=7.58.0-2ubuntu3.24 wget=1.19.4-1ubuntu2.2 libqt5webkit5=5.212.0~alpha2-7ubuntu1 libgconf-2-4=3.2.6-4ubuntu1 xvfb=2:1.19.6-1ubuntu4.14 gnupg=2.2.4-1ubuntu1.6 salt-minion=2017.7.4+dfsg1-1ubuntu18.04.2 -qqy \
 && rm -rf /var/lib/apt/lists/*
#  ===============
#   Set JAVA_HOME
#  ===============
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/jre" \
    PATH="$PATH:$JAVA_HOME/bin"
#  =====================
#   Install Android SDK
#  =====================
ARG SDK_VERSION=sdk-tools-linux-3859397
ARG ANDROID_BUILD_TOOLS_VERSION=26.0.0
ARG ANDROID_PLATFORM_VERSION="android-25"
ENV SDK_VERSION="$SDK_VERSION" \
    ANDROID_BUILD_TOOLS_VERSION="$ANDROID_BUILD_TOOLS_VERSION" \
    ANDROID_HOME="/root"
RUN wget -O tools.zip https://dl.google.com/android/repository/${SDK_VERSION}.zip \
 && unzip tools.zip \
 && rm tools.zip \
 && chmod a+x -R $ANDROID_HOME \
 && chown -R root:root $ANDROID_HOME
ENV PATH="$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin"
#   https://askubuntu.com/questions/885658/android-sdk-repositories-cfg-could-not-be-loaded
RUN mkdir -p ~/.android \
 && touch ~/.android/repositories.cfg \
 && echo y | sdkmanager "platform-tools" \
 && echo y | sdkmanager "build-tools;$ANDROID_BUILD_TOOLS_VERSION" \
 && echo y | sdkmanager "platforms;$ANDROID_PLATFORM_VERSION"
ENV PATH="$PATH:$ANDROID_HOME/platform-tools:$ANDROID_HOME/build-tools"
#  ====================================
#   Install latest nodejs, npm, appium
#   Using this workaround to install Appium -> https://github.com/appium/appium/issues/10020 -> Please remove this workaround asap
#  ====================================
ARG APPIUM_VERSION=1.7.0-beta
ENV APPIUM_VERSION="$APPIUM_VERSION"
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -qqy \
 && npm install appium@${APPIUM_VERSION} -g --unsafe-perm=true --allow-root \
 && exit 0 \
 && npm cache clean \
 && apt-get remove --purge -y npm \
 && apt-get autoremove --purge -y \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && apt-get clean
#  ==================================
#   Fix Issue with timezone mismatch
#  ==================================
ENV TZ="US/Pacific"
RUN echo "${TZ}" > /etc/timezone
#  ===============
#   Expose Ports
#  ---------------
#   4723
#     Appium port
#  ===============
EXPOSE 4723/tcp
#  ====================================================
#   Scripts to run appium and connect to Selenium Grid
#  ====================================================
COPY entry_point.sh generate_config.sh wireless_connect.sh wireless_autoconnect.sh /root/
RUN chmod +x /root/entry_point.sh \
 && chmod +x /root/generate_config.sh \
 && chmod +x /root/wireless_connect.sh \
 && chmod +x /root/wireless_autoconnect.sh
#  ========================================
#   Run xvfb and appium server
#  ========================================
CMD /root/wireless_autoconnect.sh \
 && /root/entry_point.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
