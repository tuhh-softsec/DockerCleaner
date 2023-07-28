#   Go cross compiler (xgo): Base cross-compilation layer
#   Copyright (c) 2014 Péter Szilágyi. All rights reserved.
#
#   Released under the MIT license.
FROM ubuntu:16.04
MAINTAINER Péter Szilágyi <peterke@gmail.com>
#   Mark the image as xgo enabled to support xgo-in-xgo
ENV XGO_IN_XGO="1"
#   Configure the Go environment, since it's not going to change
ENV PATH="/usr/local/go/bin:$PATH"
ENV GOPATH="/go"
#   Inject the remote file fetcher and checksum verifier
COPY fetch.sh /fetch.sh
ENV FETCH="/fetch.sh"
RUN chmod +x $FETCH
#   Make sure apt-get is up to date and dependent packages are installed
RUN apt-get update \
 && apt-get install --no-install-recommends automake=1:1.15-4ubuntu1 autogen=1:5.18.7-3 build-essential=12.1ubuntu2 ca-certificates=20210119~16.04.1 gcc-5-arm-linux-gnueabi=5.4.0-6ubuntu1~16.04.9cross1 g++-5-arm-linux-gnueabi=5.4.0-6ubuntu1~16.04.9cross1 libc6-dev-armel-cross=2.23-0ubuntu3cross1 gcc-5-arm-linux-gnueabihf=5.4.0-6ubuntu1~16.04.9cross1 g++-5-arm-linux-gnueabihf=5.4.0-6ubuntu1~16.04.9cross1 libc6-dev-armhf-cross=2.23-0ubuntu3cross1 gcc-5-aarch64-linux-gnu=5.4.0-6ubuntu1~16.04.9cross1 g++-5-aarch64-linux-gnu=5.4.0-6ubuntu1~16.04.9cross1 libc6-dev-arm64-cross=2.23-0ubuntu3cross1 gcc-5-mips-linux-gnu=5.4.0-6ubuntu1~16.04.9cross1 g++-5-mips-linux-gnu=5.4.0-6ubuntu1~16.04.9cross1 libc6-dev-mips-cross=2.23-0ubuntu3cross1 gcc-5-mipsel-linux-gnu=5.4.0-6ubuntu1~16.04.9cross1 g++-5-mipsel-linux-gnu=5.4.0-6ubuntu1~16.04.9cross1 libc6-dev-mipsel-cross=2.23-0ubuntu3cross1 gcc-5-mips64-linux-gnuabi64=5.4.0-6ubuntu1~16.04.9cross1 g++-5-mips64-linux-gnuabi64=5.4.0-6ubuntu1~16.04.9cross1 libc6-dev-mips64-cross=2.23-0ubuntu3cross1 gcc-5-mips64el-linux-gnuabi64=5.4.0-6ubuntu1~16.04.9cross1 g++-5-mips64el-linux-gnuabi64=5.4.0-6ubuntu1~16.04.9cross1 libc6-dev-mips64el-cross=2.23-0ubuntu3cross1 gcc-5-multilib=5.4.0-6ubuntu1~16.04.12 g++-5-multilib=5.4.0-6ubuntu1~16.04.12 gcc-mingw-w64=5.3.1-8ubuntu3+17 g++-mingw-w64=5.3.1-8ubuntu3+17 clang=1:3.8-33ubuntu3.1 llvm-dev=1:3.8-33ubuntu3.1 libtool=2.4.6-0.1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 uuid-dev=2.27.1-6ubuntu3.10 libssl-dev=1.0.2g-1ubuntu4.20 swig=3.0.8-0ubuntu3 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 pkg-config=0.29.1-0ubuntu1 patch=2.7.5-1ubuntu0.16.04.2 make=4.1-6 xz-utils=5.1.1alpha+20120614-2ubuntu2 cpio=2.11+dfsg-5ubuntu1.1 wget=1.17.1-1ubuntu1.5 zip=3.0-11 unzip=6.0-20ubuntu1.1 p7zip=9.20.1~dfsg.1-4.2ubuntu0.1 git=1:2.7.4-0ubuntu1.10 mercurial=3.7.3-1ubuntu1.2 bzr=2.7.0-2ubuntu3.1 texinfo=6.1.0.dfsg.1-5 help2man=1.47.3 -y
#   Fix any stock package issues
RUN ln -s /usr/include/asm-generic /usr/include/asm
#   Configure the container for OSX cross compilation
ENV OSX_SDK="MacOSX10.11.sdk"
ENV OSX_NDK_X86="/usr/local/osx-ndk-x86"
RUN OSX_SDK_PATH=https://s3.dockerproject.org/darwin/v2/$OSX_SDK.tar.xz \
 && $FETCH $OSX_SDK_PATH dd228a335194e3392f1904ce49aff1b1da26ca62 \
 && git clone https://github.com/tpoechtrager/osxcross.git \
 && mv `basename $OSX_SDK_PATH ` /osxcross/tarballs/ \
 && sed -i -e 's|-march=native||g' /osxcross/build_clang.sh /osxcross/wrapper/build.sh \
 && UNATTENDED=yes OSX_VERSION_MIN=10.6 /osxcross/build.sh \
 && mv /osxcross/target $OSX_NDK_X86 \
 && rm -rf /osxcross
COPY patch.tar.xz $OSX_NDK_X86/SDK/$OSX_SDK/usr/include/c++
ENV PATH="$OSX_NDK_X86/bin:$PATH"
#   Configure the container for iOS cross compilation
ENV IOS_NDK_ARM_7="/usr/local/ios-ndk-arm-7"
ENV IOS_NDK_ARM64="/usr/local/ios-ndk-arm64"
ENV IOS_SIM_NDK_AMD64="/usr/local/ios-sim-ndk-amd64"
COPY update_ios.sh /update_ios.sh
ENV UPDATE_IOS="/update_ios.sh"
RUN chmod +x $UPDATE_IOS
RUN IOS_SDK_PATH=https://sdks.website/dl/iPhoneOS9.3.sdk.tbz2 \
 && $FETCH $IOS_SDK_PATH db5ecf91617abf26d3db99e769bd655b943905e5 \
 && mv `basename $IOS_SDK_PATH ` iPhoneOS9.3.sdk.tar.bz2 \
 && $UPDATE_IOS /iPhoneOS9.3.sdk.tar.bz2 \
 && rm -rf /iPhoneOS9.3.sdk.tar.bz2
#   Configure the container for Android cross compilation
ENV ANDROID_NDK="android-ndk-r11c"
ENV ANDROID_NDK_PATH="http://dl.google.com/android/repository/$ANDROID_NDK-linux-x86_64.zip"
ENV ANDROID_NDK_ROOT="/usr/local/$ANDROID_NDK"
ENV ANDROID_NDK_LIBC="$ANDROID_NDK_ROOT/sources/cxx-stl/gnu-libstdc++/4.9"
ENV ANDROID_PLATFORM="21"
ENV ANDROID_CHAIN_ARM="arm-linux-androideabi-4.9"
ENV ANDROID_CHAIN_ARM64="aarch64-linux-android-4.9"
ENV ANDROID_CHAIN_386="x86-4.9"
RUN $FETCH $ANDROID_NDK_PATH de5ce9bddeee16fb6af2b9117e9566352aa7e279 \
 && unzip `basename $ANDROID_NDK_PATH ` "$ANDROID_NDK/build/*" "$ANDROID_NDK/sources/cxx-stl/gnu-libstdc++/4.9/include/*" "$ANDROID_NDK/sources/cxx-stl/gnu-libstdc++/4.9/libs/armeabi*/*" "$ANDROID_NDK/sources/cxx-stl/gnu-libstdc++/4.9/libs/arm64*/*" "$ANDROID_NDK/sources/cxx-stl/gnu-libstdc++/4.9/libs/x86/*" "$ANDROID_NDK/prebuilt/linux-x86_64/*" "$ANDROID_NDK/platforms/*/arch-arm/*" "$ANDROID_NDK/platforms/*/arch-arm64/*" "$ANDROID_NDK/platforms/*/arch-x86/*" "$ANDROID_NDK/toolchains/$ANDROID_CHAIN_ARM/*" "$ANDROID_NDK/toolchains/$ANDROID_CHAIN_ARM64/*" "$ANDROID_NDK/toolchains/$ANDROID_CHAIN_386/*" -d /usr/local > /dev/null \
 && rm -f `basename $ANDROID_NDK_PATH `
ENV PATH="/usr/$ANDROID_CHAIN_ARM/bin:$PATH"
ENV PATH="/usr/$ANDROID_CHAIN_ARM64/bin:$PATH"
ENV PATH="/usr/$ANDROID_CHAIN_386/bin:$PATH"
#   Inject the old Go package downloader and tool-chain bootstrapper
COPY bootstrap.sh /bootstrap.sh
ENV BOOTSTRAP="/bootstrap.sh"
RUN chmod +x $BOOTSTRAP
#   Inject the new Go root distribution downloader and bootstrapper
COPY bootstrap_pure.sh /bootstrap_pure.sh
ENV BOOTSTRAP_PURE="/bootstrap_pure.sh"
RUN chmod +x $BOOTSTRAP_PURE
#   Inject the Go source distribution downloader and bootstrapper
COPY bootstrap_repo.sh /bootstrap_repo.sh
ENV BOOTSTRAP_REPO="/bootstrap_repo.sh"
RUN chmod +x $BOOTSTRAP_REPO
#   Inject the C dependency cross compiler
COPY build_deps.sh /build_deps.sh
ENV BUILD_DEPS="/build_deps.sh"
RUN chmod +x $BUILD_DEPS
#   Inject the container entry point, the build script
COPY build.sh /build.sh
ENV BUILD="/build.sh"
RUN chmod +x $BUILD
ENTRYPOINT ["/build.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
