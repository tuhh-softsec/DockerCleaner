FROM ubuntu:18.04
LABEL maintainer="\"Andriy Khavryuchenko <akhavr@khavr.com>\""
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8"
ENV OPTDIR="/opt"
ENV ANDROID_NDK_HOME="${OPTDIR}/android-ndk"
ENV ANDROID_NDK_VERSION="17c"
ENV ANDROID_NDK_HOME_V="${ANDROID_NDK_HOME}-r${ANDROID_NDK_VERSION}"
ENV ANDROID_NDK_ARCHIVE="android-ndk-r${ANDROID_NDK_VERSION}-linux-x86_64.zip"
ENV ANDROID_NDK_DL_URL="https://dl.google.com/android/repository/${ANDROID_NDK_ARCHIVE}"
ENV ANDROID_SDK_HOME="${OPTDIR}/android-sdk"
ENV ANDROID_SDK_TOOLS_VERSION="4333796"
ENV ANDROID_SDK_BUILD_TOOLS_VERSION="28.0.3"
ENV ANDROID_SDK_TOOLS_ARCHIVE="sdk-tools-linux-${ANDROID_SDK_TOOLS_VERSION}.zip"
ENV ANDROID_SDK_TOOLS_DL_URL="https://dl.google.com/android/repository/${ANDROID_SDK_TOOLS_ARCHIVE}"
ENV USE_SDK_WRAPPER="1"
ENV GRADLE_OPTS="-Xmx1536M -Dorg.gradle.jvmargs='-Xmx1536M'"
ENV USER="buildozer"
ENV HOMEDIR="/home/${USER}"
ENV WORKDIR="${HOMEDIR}/build" \
    PATH="${HOMEDIR}/.local/bin:${PATH}"
RUN dpkg --add-architecture i386 \
 && apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y \
 && add-apt-repository ppa:kivy-team/kivy \
 && apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 language-pack-en=1:18.04+20190718 build-essential=12.4ubuntu1 ccache=3.4.1-1 git=1:2.17.1-1ubuntu0.17 libncurses5:i386 libstdc++6:i386 libgtk2.0-0:i386 libpangox-1.0-0:i386 libpangoxft-1.0-0:i386 libidn11:i386 openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 unzip=6.0-21ubuntu1.2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 zlib1g:i386 zip=3.0-11build1 sudo=1.8.21p2-3ubuntu1.5 python3-dev=3.6.7-1~18.04 python3-kivy=1.9.1-1build3 lbzip2=2.5-2 wget=1.19.4-1ubuntu2.2 curl=7.58.0-2ubuntu3.24 ca-certificates=20211016ubuntu0.18.04.1 patch=2.7.6-2ubuntu1.1 vim=2:8.0.1453-1ubuntu1.11 less=487-0.1 autoconf=2.69-11 automake=1:1.15.1-3ubuntu2 libtool=2.4.6-2 libffi-dev=3.2.1-8 cmake=3.10.2-1ubuntu2.18.04.2 gettext=0.19.8.1-6ubuntu0.3 libltdl-dev=2.4.6-2 pkg-config=0.29.1-0ubuntu2 locales=2.27-3ubuntu1.6 ffmpeg=7:3.4.11-0ubuntu0.1 libsdl2-dev=2.0.8+dfsg1-1ubuntu1.18.04.4 libsdl2-image-dev=2.0.3+dfsg1-1 libsdl2-mixer-dev=2.0.2+dfsg1-2 libsdl2-ttf-dev=2.0.14+dfsg1-2 libportmidi-dev=1:217-6 libswscale-dev=7:3.4.11-0ubuntu0.1 libavformat-dev=7:3.4.11-0ubuntu0.1 libavcodec-dev=7:3.4.11-0ubuntu0.1 -y \
 && locale-gen en_US.UTF-8 \
 && rm -rf /var/lib/apt/lists/*
RUN wget https://bootstrap.pypa.io/get-pip.py \
 && python3 get-pip.py \
 && pip3 install --upgrade setuptools cython==0.28.6 image
RUN git config --global user.email "buildozer@example.com" \
 && git config --global user.name "Buildozer"
RUN adduser --disabled-password --gecos '' ${USER} \
 && mkdir /home/buildozer/build \
 && chown ${USER}.${USER} ${WORKDIR}
RUN usermod -append --groups sudo ${USER}
RUN echo "%sudo ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN cd ${OPTDIR} \
 && git clone https://github.com/kivy/python-for-android \
 && cd python-for-android \
 && git remote add sombernight https://github.com/SomberNight/python-for-android \
 && git fetch --all \
 && git checkout dec1badc3bd134a9a1c69275339423a95d63413e \
 && git cherry-pick d7f722e4e5d4b3e6f5b1733c95e6a433f78ee570 \
 && git cherry-pick a607f4a446773ac0b0a5150171092b0617fbe670 \
 && python3 -m pip install -e . \
 && cd ${OPTDIR} \
 && git clone https://github.com/kivy/buildozer \
 && cd buildozer \
 && git checkout 6142acfa40cf9a225644470e5410333c35a3bc4a \
 && python3 -m pip install -e .
RUN cd ${OPTDIR} \
 && curl -# "${ANDROID_SDK_TOOLS_DL_URL}" --output "${ANDROID_SDK_TOOLS_ARCHIVE}" \
 && mkdir --parents "${ANDROID_SDK_HOME}" \
 && unzip -q "${ANDROID_SDK_TOOLS_ARCHIVE}" -d "${ANDROID_SDK_HOME}" \
 && rm -rf "${ANDROID_SDK_TOOLS_ARCHIVE}" \
 && mkdir --parents "${ANDROID_SDK_HOME}/.android/" \
 && echo '### User Sources for Android SDK Manager' > "${ANDROID_SDK_HOME}/.android/repositories.cfg" \
 && yes | "${ANDROID_SDK_HOME}/tools/bin/sdkmanager" "build-tools;${ANDROID_SDK_BUILD_TOOLS_VERSION}" > /dev/null \
 && "${ANDROID_SDK_HOME}/tools/bin/sdkmanager" "platforms;android-24" > /dev/null \
 && "${ANDROID_SDK_HOME}/tools/bin/sdkmanager" "platforms;android-28" > /dev/null \
 && "${ANDROID_SDK_HOME}/tools/bin/sdkmanager" "build-tools;${ANDROID_SDK_BUILD_TOOLS_VERSION}" > /dev/null \
 && "${ANDROID_SDK_HOME}/tools/bin/sdkmanager" "extras;android;m2repository" > /dev/null \
 && chmod +x "${ANDROID_SDK_HOME}/tools/bin/avdmanager"
RUN cd ${OPTDIR} \
 && curl -# "${ANDROID_NDK_DL_URL}" --output "${ANDROID_NDK_ARCHIVE}" \
 && mkdir --parents "${ANDROID_NDK_HOME_V}" \
 && unzip -q "${ANDROID_NDK_ARCHIVE}" -d "${OPTDIR}" \
 && chown -R ${USER}.${USER} ${OPTDIR} \
 && ln -sfn "${ANDROID_NDK_HOME_V}" "${ANDROID_NDK_HOME}" \
 && rm -rf "${ANDROID_NDK_ARCHIVE}"
USER ${USER}
WORKDIR ${WORKDIR}
# Please add your HEALTHCHECK here!!!
