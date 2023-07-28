FROM ubuntu:14.04
MAINTAINER Alejandro Gomez <agommor@gmail.com>
#  ==========================
#   Build arguments
#  ==========================
ARG ANDROID_SDK_VERSION=23
ARG JAVA_VERSION=8
ARG APPIUM_VERSION=1.5.2
ARG CHROMEDRIVER_VERSION=2.21.0
ARG VNC_PASSWD=1234
#  ==========================
#   Env variables
#  ==========================
ENV SHELL="\"/bin/bash\""
ENV X11_RESOLUTION="\"480x600x24\""
ENV DISPLAY=":1"
ENV VNC_PASSWD="${VNC_PASSWD}"
ENV DEBIAN_FRONTEND="noninteractive"
ENV ANDROID_SDK_VERSION="${ANDROID_SDK_VERSION}"
ENV ANDROID_SDKTOOLS_VERSION="24.4.1"
ENV JAVA_VERSION="${JAVA_VERSION}"
ENV APPIUM_VERSION="${APPIUM_VERSION}"
ENV CHROMEDRIVER_VERSION="${CHROMEDRIVER_VERSION}"
ENV ANDROID_HOME="/opt/android-sdk-linux"
ENV APPIUM_HOME="/opt/appium"
ENV PATH="$PATH:$ANDROID_HOME/platform-tools:$ANDROID_HOME/tools"
ENV SDK_PACKAGES="platform-tools,build-tools-23.0.3,build-tools-23.0.2,build-tools-23.0.1,build-tools-22.0.1,android-23,android-22,sys-img-armeabi-v7a-android-$ANDROID_SDK_VERSION,sys-img-x86_64-android-$ANDROID_SDK_VERSION,sys-img-x86-android-$ANDROID_SDK_VERSION,sys-img-armeabi-v7a-google_apis-$ANDROID_SDK_VERSION,sys-img-x86_64-google_apis-$ANDROID_SDK_VERSION,sys-img-x86-google_apis-$ANDROID_SDK_VERSION,addon-google_apis-google-$ANDROID_SDK_VERSION,source-$ANDROID_SDK_VERSION,extra-android-m2repository,extra-android-support,extra-google-google_play_services,extra-google-m2repository"
#  ==========================
#   Add external files
#  ==========================
COPY assets/etc/apt/apt.conf.d/99norecommends /etc/apt/apt.conf.d/99norecommends
COPY assets/etc/apt/sources.list /etc/apt/sources.list
#  ==========================
#   Install necessary packages, Appium and NPM
#  ==========================
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EB9B1D8886F44E2A \
 && apt-get update -y \
 && apt-get install --no-install-recommends apt-transport-https=1.0.1ubuntu2.24 build-essential=11.6ubuntu6 curl=7.35.0-1ubuntu2.20 g++=4:4.8.2-1ubuntu6 lib32gcc1=1:4.9.3-0ubuntu4 lib32ncurses5=5.9+20140118-1ubuntu1 lib32stdc++6=4.8.4-2ubuntu1~14.04.4 lib32z1=1:1.2.8.dfsg-1ubuntu1.1 libc6-i386=2.19-0ubuntu6.15 libgconf-2-4=3.2.6-0ubuntu2 libvirt-bin=1.2.2-0ubuntu13.1.28 libxi6=2:1.7.1.901-1ubuntu1.1 make=3.81-8.2ubuntu3 maven=3.0.5-1 psmisc=22.20-1ubuntu2 python=2.7.5-5ubuntu3 python-software-properties=0.92.37.8 qemu-kvm=2.0.0+dfsg-2ubuntu1.46 software-properties-common=0.92.37.8 unzip=6.0-9ubuntu1.5 wget=1.15-1ubuntu1.14.04.5 x11vnc=0.9.13-1.1 xvfb=2:1.15.1-0ubuntu2.11 openjdk-${JAVA_VERSION}-jdk -y \
 && curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash - \
 && apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 -y \
 && apt-get -qqy clean \
 && rm -rf /var/cache/apt/* \
 && mkdir $APPIUM_HOME \
 && cd $APPIUM_HOME \
 && npm install appium@$APPIUM_VERSION chromedriver@$CHROMEDRIVER_VERSION \
 && ln -s $APPIUM_HOME/node_modules/.bin/appium /usr/bin/appium \
 && ln -s $ANDROID_HOME/platform-tools/adb /usr/local/sbin/adb
#  ==========================
#   Install Android SDK's and Platform tools
#  ==========================
RUN wget --progress=dot:giga -O /opt/android-sdk-linux.tgz https://dl.google.com/android/android-sdk_r$ANDROID_SDKTOOLS_VERSION-linux.tgz \
 && tar xzf /opt/android-sdk-linux.tgz -C /tmp \
 && rm /opt/android-sdk-linux.tgz \
 && mv /tmp/android-sdk-linux $ANDROID_HOME \
 && echo 'y' | $ANDROID_HOME/tools/android update sdk -s -u -a -t ${SDK_PACKAGES} \
 && echo 'y' | $ANDROID_HOME/tools/android update sdk -s -u -a -t tools \
 && if [ -f $ANDROID_HOME/temp/tools_*.zip ] ; then mv $ANDROID_HOME/temp/tools_*.zip $ANDROID_HOME/tools.zip \
 && unzip $ANDROID_HOME/tools.zip -d $ANDROID_HOME/ ; fi
#  ==========================
#   Final steps
#  ==========================
EXPOSE 5900/tcp
COPY assets/bin/entrypoint /entrypoint
RUN chmod +x /entrypoint \
 && cat /entrypoint
ENTRYPOINT ["/bin/bash", "/entrypoint"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
