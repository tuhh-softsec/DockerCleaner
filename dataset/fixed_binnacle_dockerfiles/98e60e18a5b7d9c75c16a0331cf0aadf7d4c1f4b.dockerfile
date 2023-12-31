#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.
FROM ubuntu:18.04
MAINTAINER Sebastian Kaspari "skaspari@mozilla.com"
#  ----------------------------------------------------------------------------------------------------------------------
#  -- Configuration -----------------------------------------------------------------------------------------------------
#  ----------------------------------------------------------------------------------------------------------------------
ENV ANDROID_BUILD_TOOLS="\"28.0.3\""
ENV ANDROID_SDK_VERSION="\"3859397\""
ENV ANDROID_PLATFORM_VERSION="\"28\""
ENV PROJECT_REPOSITORY="\"https://github.com/mozilla-mobile/android-components.git\""
ENV GOOGLE_SDK_VERSION="233"
ENV LANG="en_US.UTF-8"
#   Do not use fancy output on taskcluster
ENV TERM="dumb"
ENV GRADLE_OPTS="-Xmx4096m -Dorg.gradle.daemon=false"
#   Used to detect in scripts whether we are running on taskcluster
ENV CI_TASKCLUSTER="true"
#  ----------------------------------------------------------------------------------------------------------------------
#  -- System ------------------------------------------------------------------------------------------------------------
#  ----------------------------------------------------------------------------------------------------------------------
RUN apt-get update -qq \
 && apt-get install --no-install-recommends tzdata=2022g-0ubuntu0.18.04 -y \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 wget=1.19.4-1ubuntu2.2 expect=5.45.4-1 git=1:2.17.1-1ubuntu0.17 curl=7.58.0-2ubuntu3.24 python=2.7.15~rc1-1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 locales=2.27-3ubuntu1.6 unzip=6.0-21ubuntu1.2 -y \
 && apt-get clean
RUN pip install pip==23.1 --upgrade
RUN pip install 'taskcluster>=4,<5'
RUN pip install arrow==1.2.3
RUN pip install pyyaml==6.0
RUN locale-gen en_US.UTF-8
#  ----------------------------------------------------------------------------------------------------------------------
#  -- Android -----------------------------------------------------------------------------------------------------------
#  ----------------------------------------------------------------------------------------------------------------------
RUN mkdir -p /build/android-sdk
WORKDIR /build
ENV ANDROID_HOME="/build/android-sdk"
ENV ANDROID_SDK_HOME="/build/android-sdk"
ENV PATH="${PATH}:${ANDROID_SDK_HOME}/tools:${ANDROID_SDK_HOME}/tools/bin:${ANDROID_SDK_HOME}/platform-tools:/opt/tools:${ANDROID_SDK_HOME}/build-tools/${ANDROID_BUILD_TOOLS}"
RUN curl -L https://dl.google.com/android/repository/sdk-tools-linux-${ANDROID_SDK_VERSION}.zip > sdk.zip \
 && unzip sdk.zip -d ${ANDROID_SDK_HOME} \
 && rm sdk.zip \
 && mkdir -p /build/android-sdk/.android/ \
 && touch /build/android-sdk/.android/repositories.cfg \
 && yes | sdkmanager --licenses
#  ----------------------------------------------------------------------------------------------------------------------
#  -- Test tools --------------------------------------------------------------------------------------------------------
#  ----------------------------------------------------------------------------------------------------------------------
RUN mkdir -p /build/test-tools
WORKDIR /build
ENV TEST_TOOLS="/build/test-tools"
ENV GOOGLE_SDK_DOWNLOAD="./gcloud.tar.gz"
ENV PATH="${PATH}:${TEST_TOOLS}:${TEST_TOOLS}/google-cloud-sdk/bin"
RUN curl https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-${GOOGLE_SDK_VERSION}.0.0-linux-x86_64.tar.gz --output ${GOOGLE_SDK_DOWNLOAD} \
 && tar -xvf ${GOOGLE_SDK_DOWNLOAD} -C ${TEST_TOOLS} \
 && rm -f ${GOOGLE_SDK_DOWNLOAD} \
 && ${TEST_TOOLS}/google-cloud-sdk/install.sh --quiet \
 && ${TEST_TOOLS}/google-cloud-sdk/bin/gcloud --quiet components update
RUN URL_FLANK_BIN=$( curl -s "https://api.github.com/repos/TestArmada/flank/releases/latest" | grep "browser_download_url*" | sed -r "s/\"//g" | cut -d ":" -f3 ;) \
 && wget "https:${URL_FLANK_BIN}" -O ${TEST_TOOLS}/flank.jar \
 && chmod +x ${TEST_TOOLS}/flank.jar
#  ----------------------------------------------------------------------------------------------------------------------
#  -- Project -----------------------------------------------------------------------------------------------------------
#  ----------------------------------------------------------------------------------------------------------------------
RUN git clone --depth=1 $PROJECT_REPOSITORY
WORKDIR /build/android-components
RUN ./gradlew clean \
 && ./gradlew dependencies \
 && ./gradlew androidDependencies \
 && ./gradlew --stop \
 && ./gradlew --no-daemon assemble \
 && ./gradlew --no-daemon -Pcoverage test \
 && ./gradlew --no-daemon detekt \
 && ./gradlew --no-daemon ktlint \
 && ./gradlew --no-daemon docs \
 && ./gradlew clean
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
