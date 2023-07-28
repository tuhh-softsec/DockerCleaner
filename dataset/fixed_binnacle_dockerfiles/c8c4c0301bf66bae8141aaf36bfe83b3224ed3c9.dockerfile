#  #
#  # Copyright (c) 2014-present, Facebook, Inc.
#  # All rights reserved.
#  #
#  # This source code is licensed under the University of Illinois/NCSA Open
#  # Source License found in the LICENSE file in the root directory of this
#  # source tree. An additional grant of patent rights can be found in the
#  # PATENTS file in the same directory.
#  #
#   BEFORE PUSHING A NEW DOCKER IMAGE, PLEASE READ ALL OF THESE INSTRUCTIONS!
#
#   To build a docker image using this file, run the following command:
#       docker build --file Support/Testing/CircleCI/Dockerfile .
#
#   After building the image, you will see a line like this at the end of the
#   build process:
#       Successfully built 360abf8e6246
#
#   This hash identifies the image you just built. You can then run it locally
#   with the following command:
#       docker run --privileged --security-opt seccomp:unconfined --rm -it --name ds2-testing 360abf8e6246
#   We run in privileged mode so that we may use ptrace to its full
#   extent as needed.
#
#   When building a new docker image, it's important to tag it by the date and
#   time you built the image so that we can revert to an older image if something
#   goes wrong when we build the new one.
#   For example, if you built the image on November 1st, 2018 at 4:20:00 pm, your tag
#   would be something like: 2018-11-01_16-20-00.
#
#   Publishing the image for use by CircleCI is done with:
#       docker tag 360abf8e6246 sas42/ds2-build-env:<tag>
#       docker push sas42/ds2-build-env:<tag>
#
#   Alternatively, you can tag immediately after the build is finished via:
#       docker build --file Support/Testing/CircleCI/Dockerfile --tag 2018-09-07_12-34-56
FROM ubuntu:16.04
MAINTAINER Stephane Sezer <sas@fb.com>
#   Install apt tools
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 wget=1.17.1-1ubuntu1.5 -y )
#   In case lldb needs built
RUN (apt-get update ;apt-get install --no-install-recommends libz-dev swig=3.0.8-0ubuntu3 ncurses-dev -y )
#   Make the developer's life not suck
RUN apt-add-repository -y ppa:neovim-ppa/stable
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 zsh=5.1.1-1ubuntu2.3 tmux=2.1-3build1 curl=7.47.0-1ubuntu2.19 vim=2:7.4.1689-3ubuntu1.5 neovim -y )
#   Debugging tools
RUN (apt-get update ;apt-get install --no-install-recommends strace=4.11-1ubuntu3 htop=2.0.1-1ubuntu1 psmisc=22.21-2.1ubuntu0.1 -y )
#   Python 3.7 and misc
RUN add-apt-repository -y ppa:deadsnakes/ppa
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends python3.7 -y )
RUN curl https://bootstrap.pypa.io/get-pip.py | python3.7
RUN python3.7 -m pip install --upgrade pip
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=8.1.1-2ubuntu0.6 -y )
RUN python -m pip install --upgrade pip
#   Add LLVM apt repos
RUN wget -O - "http://llvm.org/apt/llvm-snapshot.gpg.key" | apt-key add -
RUN add-apt-repository -y "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-7 main"
RUN :
#   Install build dependencies
RUN (apt-get update ;apt-get install --no-install-recommends ninja-build=1.5.1-0.1ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends flex=2.6.0-11 bison=2:3.0.4.dfsg-1 -y )
#   Install x86 compilers
RUN (apt-get update ;apt-get install --no-install-recommends g++-multilib=4:5.3.1-1ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends clang-7 -y )
RUN ln -s /usr/bin/clang-7 /usr/local/bin/clang
RUN ln -s /usr/bin/clang++-7 /usr/local/bin/clang++
#   Install arm compilers
RUN (apt-get update ;apt-get install --no-install-recommends g++-multilib-arm-linux-gnueabi=4:5.3.1-1ubuntu1 -y )
#   Install mingw compilers
RUN (apt-get update ;apt-get install --no-install-recommends g++-mingw-w64-x86-64=5.3.1-8ubuntu3+17 -y )
RUN (apt-get update ;apt-get install --no-install-recommends g++-mingw-w64-i686=5.3.1-8ubuntu3+17 -y )
RUN update-alternatives --set i686-w64-mingw32-gcc /usr/bin/i686-w64-mingw32-gcc-posix
RUN update-alternatives --set i686-w64-mingw32-g++ /usr/bin/i686-w64-mingw32-g++-posix
RUN update-alternatives --set x86_64-w64-mingw32-gcc /usr/bin/x86_64-w64-mingw32-gcc-posix
RUN update-alternatives --set x86_64-w64-mingw32-g++ /usr/bin/x86_64-w64-mingw32-g++-posix
#   Install test dependencies
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 lldb-7 python-lldb-7 gdb=7.11.1-0ubuntu1~16.5 clang-format-7 make=4.1-6 dejagnu=1.5.3-2 -y )
RUN ln -s /usr/bin/lldb-7 /usr/local/bin/lldb
RUN ln -s /usr/bin/clang-format-7 /usr/local/bin/clang-format
#   Install documentation dependencies
RUN (apt-get update ;apt-get install --no-install-recommends doxygen=1.8.11-1ubuntu0.1 graphviz=2.38.0-12ubuntu2.1 -y )
#   Create multilib symlink for gcc 4.9
RUN ln -s /usr/include/x86_64-linux-gnu/asm /usr/include/asm
#   Install Android toolchains with our local script
COPY Support/Scripts/common.sh /tmp
COPY Support/Scripts/prepare-android-ndk.py /tmp
RUN (apt-get update ;apt-get install --no-install-recommends unzip=6.0-20ubuntu1.1 -y )
RUN /tmp/prepare-android-ndk.py
RUN ln -s /tmp/android-sdk-linux/platform-tools/adb /usr/local/bin/adb
#   Install a version of cmake that is at least the minimum version we support.
COPY Support/Testing/CircleCI/install-cmake.sh /tmp
RUN /tmp/install-cmake.sh
#   Install Android emulators
RUN (apt-get update ;apt-get install --no-install-recommends default-jdk=2:1.8-56ubuntu2 -y )
COPY Support/Scripts/install-android-emulator.sh /tmp
RUN /tmp/install-android-emulator.sh arm
RUN /tmp/install-android-emulator.sh arm64
RUN /tmp/install-android-emulator.sh x86
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
