#   based on https://github.com/kivy/python-for-android/blob/master/Dockerfile
FROM ubuntu:18.04
ENV ANDROID_HOME="/opt/android"
#   configure locale
RUN apt-get update -qq > /dev/null \
 && apt-get install --no-install-recommends locales=2.27-3ubuntu1.6 -qq --yes \
 && locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8"
RUN apt-get update -y -qq \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 unzip=6.0-21ubuntu1.2 ca-certificates=20211016ubuntu0.18.04.1 -y -qq \
 && apt-get -y autoremove
ENV ANDROID_NDK_HOME="${ANDROID_HOME}/android-ndk"
ENV ANDROID_NDK_VERSION="17c"
ENV ANDROID_NDK_HOME_V="${ANDROID_NDK_HOME}-r${ANDROID_NDK_VERSION}"
#   get the latest version from https://developer.android.com/ndk/downloads/index.html
ENV ANDROID_NDK_ARCHIVE="android-ndk-r${ANDROID_NDK_VERSION}-linux-x86_64.zip"
ENV ANDROID_NDK_DL_URL="https://dl.google.com/android/repository/${ANDROID_NDK_ARCHIVE}"
#   download and install Android NDK
RUN curl --location --progress-bar "${ANDROID_NDK_DL_URL}" --output "${ANDROID_NDK_ARCHIVE}" \
 && mkdir --parents "${ANDROID_NDK_HOME_V}" \
 && unzip -q "${ANDROID_NDK_ARCHIVE}" -d "${ANDROID_HOME}" \
 && ln -sfn "${ANDROID_NDK_HOME_V}" "${ANDROID_NDK_HOME}" \
 && rm -rf "${ANDROID_NDK_ARCHIVE}"
ENV ANDROID_SDK_HOME="${ANDROID_HOME}/android-sdk"
#   get the latest version from https://developer.android.com/studio/index.html
ENV ANDROID_SDK_TOOLS_VERSION="4333796"
ENV ANDROID_SDK_BUILD_TOOLS_VERSION="28.0.3"
ENV ANDROID_SDK_TOOLS_ARCHIVE="sdk-tools-linux-${ANDROID_SDK_TOOLS_VERSION}.zip"
ENV ANDROID_SDK_TOOLS_DL_URL="https://dl.google.com/android/repository/${ANDROID_SDK_TOOLS_ARCHIVE}"
#   download and install Android SDK
RUN curl --location --progress-bar "${ANDROID_SDK_TOOLS_DL_URL}" --output "${ANDROID_SDK_TOOLS_ARCHIVE}" \
 && mkdir --parents "${ANDROID_SDK_HOME}" \
 && unzip -q "${ANDROID_SDK_TOOLS_ARCHIVE}" -d "${ANDROID_SDK_HOME}" \
 && rm -rf "${ANDROID_SDK_TOOLS_ARCHIVE}"
#   update Android SDK, install Android API, Build Tools...
RUN mkdir --parents "${ANDROID_SDK_HOME}/.android/" \
 && echo '### User Sources for Android SDK Manager' > "${ANDROID_SDK_HOME}/.android/repositories.cfg"
#   accept Android licenses (JDK necessary!)
RUN apt-get update -y -qq \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 -y -qq \
 && apt-get -y autoremove
RUN yes | "${ANDROID_SDK_HOME}/tools/bin/sdkmanager" "build-tools;${ANDROID_SDK_BUILD_TOOLS_VERSION}" > /dev/null
#   download platforms, API, build tools
RUN "${ANDROID_SDK_HOME}/tools/bin/sdkmanager" "platforms;android-24" > /dev/null \
 && "${ANDROID_SDK_HOME}/tools/bin/sdkmanager" "platforms;android-28" > /dev/null \
 && "${ANDROID_SDK_HOME}/tools/bin/sdkmanager" "build-tools;${ANDROID_SDK_BUILD_TOOLS_VERSION}" > /dev/null \
 && "${ANDROID_SDK_HOME}/tools/bin/sdkmanager" "extras;android;m2repository" > /dev/null \
 && chmod +x "${ANDROID_SDK_HOME}/tools/bin/avdmanager"
ENV USER="user"
ENV HOME_DIR="/home/${USER}"
ENV WORK_DIR="${HOME_DIR}/wspace" \
    PATH="${HOME_DIR}/.local/bin:${PATH}"
#   install system dependencies
RUN apt-get update -y -qq \
 && apt-get install --no-install-recommends python3=3.6.7-1~18.04 virtualenv=15.1.0+ds-1.1 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-setuptools=39.0.1-2ubuntu0.1 git=1:2.17.1-1ubuntu0.17 wget=1.19.4-1ubuntu2.2 lbzip2=2.5-2 patch=2.7.6-2ubuntu1.1 sudo=1.8.21p2-3ubuntu1.5 software-properties-common=0.96.24.32.20 -y -qq \
 && apt-get -y autoremove
#   install kivy
RUN add-apt-repository ppa:kivy-team/kivy \
 && apt-get update -y -qq \
 && apt-get install --no-install-recommends python3-kivy=1.9.1-1build3 -y -qq \
 && apt-get -y autoremove \
 && apt-get -y clean
RUN python3 -m pip install image
#   build dependencies
#   https://buildozer.readthedocs.io/en/latest/installation.html#android-on-ubuntu-16-04-64bit
RUN dpkg --add-architecture i386 \
 && apt-get update -y -qq \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 ccache=3.4.1-1 git=1:2.17.1-1ubuntu0.17 python3=3.6.7-1~18.04 python3-dev=3.6.7-1~18.04 libncurses5:i386 libstdc++6:i386 libgtk2.0-0:i386 libpangox-1.0-0:i386 libpangoxft-1.0-0:i386 libidn11:i386 zip=3.0-11build1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 zlib1g:i386 -y -qq \
 && apt-get -y autoremove \
 && apt-get -y clean
#   specific recipes dependencies (e.g. libffi requires autoreconf binary)
RUN apt-get update -y -qq \
 && apt-get install --no-install-recommends libffi-dev=3.2.1-8 autoconf=2.69-11 automake=1:1.15.1-3ubuntu2 cmake=3.10.2-1ubuntu2.18.04.2 gettext=0.19.8.1-6ubuntu0.3 libltdl-dev=2.4.6-2 libtool=2.4.6-2 pkg-config=0.29.1-0ubuntu2 -y -qq \
 && apt-get -y autoremove \
 && apt-get -y clean
#   prepare non root env
RUN useradd --create-home --shell /bin/bash ${USER}
#   with sudo access and no password
RUN usermod -append --groups sudo ${USER}
RUN echo "%sudo ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
WORKDIR ${WORK_DIR}
#   user needs ownership/write access to these directories
RUN chown --recursive ${USER} ${WORK_DIR} ${ANDROID_SDK_HOME}
RUN chown ${USER} /opt
USER ${USER}
RUN python3 -m pip install --upgrade cython==0.28.6
RUN python3 -m pip install --upgrade pip
RUN python3 -m pip install --user wheel
#   prepare git
RUN git config --global user.name "John Doe" \
 && git config --global user.email johndoe@example.com
#   install buildozer
RUN cd /opt \
 && git clone https://github.com/kivy/buildozer \
 && cd buildozer \
 && git checkout 678b1bf52cf63daa51b06e86a43ea4e2ea8a0b24 \
 && python3 -m pip install --user -e .
#   install python-for-android
RUN cd /opt \
 && git clone https://github.com/kivy/python-for-android \
 && cd python-for-android \
 && git remote add sombernight https://github.com/SomberNight/python-for-android \
 && git fetch --all \
 && git checkout ccb0f8e1bab36f1b7d1508216b4b4afb076e614f \
 && git cherry-pick d7f722e4e5d4b3e6f5b1733c95e6a433f78ee570 \
 && git cherry-pick ed20e196fbcdce718a180f88f23bb2d165c4c5d8 \
 && python3 -m pip install --user -e .
#   build env vars
ENV USE_SDK_WRAPPER="1"
ENV GRADLE_OPTS="-Xmx1536M -Dorg.gradle.jvmargs='-Xmx1536M'"
# Please add your HEALTHCHECK here!!!
