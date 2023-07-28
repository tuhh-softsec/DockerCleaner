FROM ubuntu:18.04
MAINTAINER Ming Chen
ENV ANDROID_HOME="/opt/android-sdk" \
    ANDROID_NDK="/opt/android-ndk" \
    FLUTTER_HOME="/opt/flutter" \
    JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"
#   Get the latest version from https://developer.android.com/studio/index.html
ENV ANDROID_SDK_TOOLS_VERSION="4333796"
#   Get the latest version from https://developer.android.com/ndk/downloads/index.html
ENV ANDROID_NDK_VERSION="19"
#   nodejs version
ENV NODE_VERSION="10.x"
#   Set locale
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8"
RUN apt-get clean \
 && apt-get update -qq \
 && apt-get install --no-install-recommends apt-utils=1.6.14 locales=2.27-3ubuntu1.6 -qq -y \
 && locale-gen $LANG
ENV DEBIAN_FRONTEND="noninteractive" \
    TERM="dumb" \
    DEBIAN_FRONTEND="noninteractive"
#   Variables must be references after they are created
ENV ANDROID_SDK_HOME="$ANDROID_HOME"
ENV ANDROID_NDK_HOME="$ANDROID_NDK/android-ndk-r$ANDROID_NDK_VERSION"
ENV PATH="$PATH:$ANDROID_SDK_HOME/emulator:$ANDROID_SDK_HOME/tools/bin:$ANDROID_SDK_HOME/tools:$ANDROID_SDK_HOME/platform-tools:$ANDROID_NDK:$FLUTTER_HOME/bin:$FLUTTER_HOME/bin/cache/dart-sdk/bin"
COPY README.md /README.md
WORKDIR /tmp
#   Installing packages
RUN apt-get update -qq > /dev/null \
 && apt-get install --no-install-recommends locales=2.27-3ubuntu1.6 -qq > /dev/null \
 && locale-gen "$LANG" > /dev/null \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 autoconf=2.69-11 curl=7.58.0-2ubuntu3.24 git=1:2.17.1-1ubuntu0.17 gpg-agent=2.2.4-1ubuntu1.6 lib32stdc++6=8.4.0-1ubuntu1~18.04 lib32z1=1:1.2.11.dfsg-0ubuntu2.2 lib32z1-dev=1:1.2.11.dfsg-0ubuntu2.2 lib32ncurses5=6.1-1ubuntu1.18.04 libc6-dev=2.27-3ubuntu1.6 libgmp-dev=2:6.1.2+dfsg-2ubuntu0.1 libmpc-dev=1.1.0-1 libmpfr-dev=4.0.1-1 libxslt-dev libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 m4=1.4.18-1 ncurses-dev ocaml=4.05.0-10ubuntu1 openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 openssh-client=1:7.6p1-4ubuntu0.7 pkg-config=0.29.1-0ubuntu2 software-properties-common=0.96.24.32.20 ruby-full=1:2.5.1 software-properties-common=0.96.24.32.20 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 zip=3.0-11build1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -qq > /dev/null \
 && echo "Installing nodejs, npm, cordova, ionic, react-native" \
 && curl -sL -k https://deb.nodesource.com/setup_${NODE_VERSION} | bash - > /dev/null \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -qq > /dev/null \
 && apt-get clean > /dev/null \
 && rm -rf /var/lib/apt/lists/ \
 && npm install npm@9.6.4 --quiet -g > /dev/null \
 && npm install bower@1.8.14 cordova@11.1.0 eslint@8.38.0 gulp@4.0.2 ionic@5.4.16 jshint@2.13.6 karma-cli@2.0.0 mocha@10.2.0 node-gyp@9.3.1 npm-check-updates@16.10.8 react-native-cli@2.0.1 --quiet -g > /dev/null \
 && npm cache clean --force > /dev/null \
 && rm -rf /tmp/* /var/tmp/* \
 && echo "Installing fastlane" \
 && gem install fastlane --version 2.212.1 --quiet --no-document > /dev/null
#   Install Android SDK
RUN echo "Installing sdk tools ${ANDROID_SDK_TOOLS_VERSION}" \
 && wget --quiet --output-document=sdk-tools.zip "https://dl.google.com/android/repository/sdk-tools-linux-${ANDROID_SDK_TOOLS_VERSION}.zip" \
 && mkdir --parents "$ANDROID_HOME" \
 && unzip -q sdk-tools.zip -d "$ANDROID_HOME" \
 && rm --force sdk-tools.zip \
 && echo "Installing ndk r${ANDROID_NDK_VERSION}" \
 && wget --quiet --output-document=android-ndk.zip "http://dl.google.com/android/repository/android-ndk-r${ANDROID_NDK_VERSION}-linux-x86_64.zip" \
 && mkdir --parents "$ANDROID_NDK_HOME" \
 && unzip -q android-ndk.zip -d "$ANDROID_NDK" \
 && rm --force android-ndk.zip \
 && mkdir --parents "$ANDROID_HOME/.android/" \
 && echo '### User Sources for Android SDK Manager' > "$ANDROID_HOME/.android/repositories.cfg" \
 && yes | "$ANDROID_HOME"/tools/bin/sdkmanager --licenses > /dev/null \
 && echo "Installing platforms" \
 && yes | "$ANDROID_HOME"/tools/bin/sdkmanager "platforms;android-28" "platforms;android-27" "platforms;android-26" "platforms;android-25" "platforms;android-24" "platforms;android-23" "platforms;android-22" "platforms;android-21" "platforms;android-20" "platforms;android-19" "platforms;android-18" "platforms;android-17" "platforms;android-16" > /dev/null \
 && echo "Installing platform tools " \
 && yes | "$ANDROID_HOME"/tools/bin/sdkmanager "platform-tools" > /dev/null \
 && echo "Installing build tools " \
 && yes | "$ANDROID_HOME"/tools/bin/sdkmanager "build-tools;28.0.3" "build-tools;28.0.2" "build-tools;27.0.3" "build-tools;27.0.2" "build-tools;27.0.1" "build-tools;26.0.2" "build-tools;26.0.1" "build-tools;26.0.0" "build-tools;25.0.3" "build-tools;25.0.2" "build-tools;25.0.1" "build-tools;25.0.0" "build-tools;24.0.3" "build-tools;24.0.2" "build-tools;24.0.1" "build-tools;24.0.0" > /dev/null \
 && echo "Installing build tools " \
 && yes | "$ANDROID_HOME"/tools/bin/sdkmanager "build-tools;23.0.3" "build-tools;23.0.2" "build-tools;23.0.1" "build-tools;22.0.1" "build-tools;21.1.2" "build-tools;20.0.0" "build-tools;19.1.0" "build-tools;18.1.1" "build-tools;17.0.0" > /dev/null \
 && echo "Installing extras " \
 && yes | "$ANDROID_HOME"/tools/bin/sdkmanager "extras;android;m2repository" "extras;google;m2repository" > /dev/null \
 && echo "Installing play services " \
 && yes | "$ANDROID_HOME"/tools/bin/sdkmanager "extras;google;google_play_services" "extras;m2repository;com;android;support;constraint;constraint-layout;1.0.2" "extras;m2repository;com;android;support;constraint;constraint-layout;1.0.1" > /dev/null \
 && echo "Installing Google APIs" \
 && yes | "$ANDROID_HOME"/tools/bin/sdkmanager "add-ons;addon-google_apis-google-24" "add-ons;addon-google_apis-google-23" "add-ons;addon-google_apis-google-22" "add-ons;addon-google_apis-google-21" "add-ons;addon-google_apis-google-19" "add-ons;addon-google_apis-google-18" "add-ons;addon-google_apis-google-17" "add-ons;addon-google_apis-google-16" > /dev/null \
 && echo "Installing emulator " \
 && yes | "$ANDROID_HOME"/tools/bin/sdkmanager "emulator" > /dev/null \
 && echo "Installing kotlin" \
 && wget --quiet -O sdk.install.sh "https://get.sdkman.io" \
 && bash -c "bash ./sdk.install.sh > /dev/null \
 && source ~/.sdkman/bin/sdkman-init.sh \
 && sdk install kotlin" \
 && rm -f sdk.install.sh \
 && cd /opt \
 && wget --quiet https://storage.googleapis.com/flutter_infra/releases/stable/linux/flutter_linux_v1.5.4-hotfix.2-stable.tar.xz -O flutter.tar.xz \
 && tar xf flutter.tar.xz \
 && rm -f flutter.tar.xz \
 && flutter config --no-analytics
#   Copy sdk license agreement files.
RUN mkdir -p $ANDROID_HOME/licenses
COPY sdk/licenses/* $ANDROID_HOME/licenses/
#   Create some jenkins required directory to allow this image run with Jenkins
RUN mkdir -p /var/lib/jenkins/workspace
RUN mkdir -p /home/jenkins
RUN chmod 777 /home/jenkins
RUN chmod 777 /var/lib/jenkins/workspace
RUN chmod 777 $ANDROID_HOME/.android
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
