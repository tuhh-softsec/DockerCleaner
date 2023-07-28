#   Flutter (https://flutter.dev) Developement Environment for Linux
#   ===============================================================
#
#   This environment passes all Linux Flutter Doctor checks and is sufficient
#   for building Android applications and running Flutter tests.
#
#   To build iOS applications, a Mac development environment is necessary.
#
#   This includes applications and sdks that are needed only by the CI system
#   for performing pushes to production, and so this image is quite a bit larger
#   than strictly needed for just building Flutter apps.
FROM debian:stretch
MAINTAINER Flutter Developers <flutter-dev@googlegroups.com>
RUN :
RUN apt-get upgrade -y
#   Install basics
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.11.0-3+deb9u7 wget=1.18-5+deb9u3 curl=7.52.1-5+deb9u16 zip=3.0-11+b1 unzip=6.0-21+deb9u2 ca-certificates=20200601~deb9u2 gnupg=2.1.18-8~deb9u4 -y )
#   Add nodejs repository to apt sources and install it.
ENV NODEJS_INSTALL="/opt/nodejs_install"
RUN mkdir -p "${NODEJS_INSTALL}"
RUN wget -q https://deb.nodesource.com/setup_10.x -O "${NODEJS_INSTALL}/nodejs_install.sh"
RUN bash "${NODEJS_INSTALL}/nodejs_install.sh"
#   Install the rest of the dependencies.
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.24-11+deb9u4 golang=2:1.7~5 ruby=1:2.3.3 ruby-dev=1:2.3.3 nodejs=4.8.2~dfsg-1 lib32stdc++6=6.3.0-18+deb9u1 libstdc++6=6.3.0-18+deb9u1 libglu1-mesa=9.0.0-2.1 build-essential=12.3 default-jdk-headless=2:1.8-58+deb9u1 -y )
#   Install the Android SDK Dependency.
ENV ANDROID_SDK_URL="https://dl.google.com/android/repository/sdk-tools-linux-4333796.zip"
ENV ANDROID_TOOLS_ROOT="/opt/android_sdk"
RUN mkdir -p "${ANDROID_TOOLS_ROOT}"
RUN mkdir -p ~/.android
#   Silence warning.
RUN touch ~/.android/repositories.cfg
ENV ANDROID_SDK_ARCHIVE="${ANDROID_TOOLS_ROOT}/archive"
RUN wget --progress=dot:giga "${ANDROID_SDK_URL}" -O "${ANDROID_SDK_ARCHIVE}"
RUN unzip -q -d "${ANDROID_TOOLS_ROOT}" "${ANDROID_SDK_ARCHIVE}"
#   Suppressing output of sdkmanager to keep log size down
#   (it prints install progress WAY too often).
RUN yes "y" | "${ANDROID_TOOLS_ROOT}/tools/bin/sdkmanager" "tools" > /dev/null
RUN yes "y" | "${ANDROID_TOOLS_ROOT}/tools/bin/sdkmanager" "build-tools;28.0.3" > /dev/null
RUN yes "y" | "${ANDROID_TOOLS_ROOT}/tools/bin/sdkmanager" "platforms;android-28" > /dev/null
RUN yes "y" | "${ANDROID_TOOLS_ROOT}/tools/bin/sdkmanager" "platform-tools" > /dev/null
RUN yes "y" | "${ANDROID_TOOLS_ROOT}/tools/bin/sdkmanager" "extras;android;m2repository" > /dev/null
RUN yes "y" | "${ANDROID_TOOLS_ROOT}/tools/bin/sdkmanager" "extras;google;m2repository" > /dev/null
RUN yes "y" | "${ANDROID_TOOLS_ROOT}/tools/bin/sdkmanager" "patcher;v4" > /dev/null
RUN rm "${ANDROID_SDK_ARCHIVE}"
ENV PATH="${ANDROID_TOOLS_ROOT}/tools:${PATH}"
ENV PATH="${ANDROID_TOOLS_ROOT}/tools/bin:${PATH}"
#   Silence warnings when accepting android licenses.
RUN mkdir -p ~/.android
RUN touch ~/.android/repositories.cfg
#   Setup gradle
ENV GRADLE_ROOT="/opt/gradle"
RUN mkdir -p "${GRADLE_ROOT}"
ENV GRADLE_ARCHIVE="${GRADLE_ROOT}/gradle.zip"
ENV GRADLE_URL="http://services.gradle.org/distributions/gradle-4.4-bin.zip"
RUN wget --progress=dot:giga "$GRADLE_URL" -O "${GRADLE_ARCHIVE}"
RUN unzip -q -d "${GRADLE_ROOT}" "${GRADLE_ARCHIVE}"
ENV PATH="$GRADLE_ROOT/bin:$PATH"
#   Add npm to path.
ENV PATH="/usr/bin:${PATH}"
RUN dpkg-query -L nodejs
#   Install Firebase
#   This is why we need nodejs installed.
RUN /usr/bin/npm --verbose install -g firebase-tools
#   Install dashing
#   This is why we need golang installed.
RUN mkdir -p /opt/gopath/bin
ENV GOPATH="/opt/gopath"
ENV PATH="${GOPATH}/bin:${PATH}"
RUN go get -u github.com/technosophos/dashing
#   Set locale to en_US
RUN locale-gen en_US "en_US.UTF-8" \
 && dpkg-reconfigure locales
ENV LANG="en_US.UTF-8"
#   Install coveralls and Firebase
#   This is why we need ruby installed.
#   Skip all the documentation (-N) since it's just on CI.
RUN gem install coveralls --version 0.8.23 -N
RUN gem install bundler --version 2.4.12 -N
#   Install fastlane which is used on Linux to build and deploy Android
#   builds to the Play Store.
RUN gem install fastlane --version 2.212.1 -N
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
