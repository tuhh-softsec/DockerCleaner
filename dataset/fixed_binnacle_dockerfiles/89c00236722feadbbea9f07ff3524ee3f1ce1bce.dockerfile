FROM debian:stretch
MAINTAINER Charles Hubain <chubain@quarkslab.com>
ARG QBDI_BUILD_TYPE=Release
ENV USER="docker"
ENV HOME="/home/$USER"
ENV PREFIX="/usr"
ENV QBDI_PLATFORM="android-ARM"
ENV LLVM_VERSION="5.0"
ENV CMAKE_MAJOR="3.11"
ENV CMAKE_VERSION="3.11.4"
ENV NDK_VERSION="r13b"
#   Install base tools
RUN apt-get update \
 && apt-get install --no-install-recommends bash=4.4-5 gpg wget=1.18-5+deb9u3 -y
#   Add LLVM debian repository
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
 && echo "deb http://apt.llvm.org/stretch/ llvm-toolchain-stretch main" >> /etc/apt/sources.list \
 && echo "deb-src http://apt.llvm.org/stretch/ llvm-toolchain-stretch main" >> /etc/apt/sources.list \
 && echo "deb http://apt.llvm.org/stretch/ llvm-toolchain-stretch-4.0 main" >> /etc/apt/sources.list \
 && echo "deb-src http://apt.llvm.org/stretch/ llvm-toolchain-stretch-4.0 main" >> /etc/apt/sources.list \
 && echo "deb http://apt.llvm.org/stretch/ llvm-toolchain-stretch-5.0 main" >> /etc/apt/sources.list \
 && echo "deb-src http://apt.llvm.org/stretch/ llvm-toolchain-stretch-5.0 main" >> /etc/apt/sources.list
#   Get latest package list, upgrade packages, install required packages 
#   and cleanup to keep container as small as possible
RUN apt-get update \
 && apt-get install --no-install-recommends bash=4.4-5 sudo=1.8.19p1-2.1+deb9u3 unzip=6.0-21+deb9u2 build-essential=12.3 cmake=3.7.2-1 ninja-build=1.7.2-1 g++=4:6.3.0-4 gcc-arm-linux-gnueabi=4:6.3.0-4 g++-arm-linux-gnueabi=4:6.3.0-4 libncurses5-dev=6.0+20161126-1+deb9u2 libstdc++-6-dev=6.3.0-18+deb9u1 make=4.1-9.1 pkg-config=0.29-4+b1 python=2.7.13-2 python-dev=2.7.13-2 wget=1.18-5+deb9u3 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 qemu-user=1:2.8+dfsg-6+deb9u17 vim=2:8.0.0197-4+deb9u7 llvm-$LLVM_VERSION -y \
 && ln -s /usr/bin/llvm-tblgen-$LLVM_VERSION /usr/bin/llvm-tblgen \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN wget "https://cmake.org/files/v$CMAKE_MAJOR/cmake-$CMAKE_VERSION.tar.gz" \
 && tar xf cmake-$CMAKE_VERSION.tar.gz \
 && cd cmake-$CMAKE_VERSION/ \
 && ./configure --prefix=$PREFIX \
 && make \
 && make install \
 && cd .. \
 && rm -rf cmake-*
#   create a user
RUN adduser --disabled-password --gecos '' $USER \
 && adduser $USER sudo \
 && echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
#   switch to new user
USER $USER
WORKDIR $HOME
#   Install NDK
RUN sudo chown -R $USER:$USER . \
 && wget https://dl.google.com/android/repository/android-ndk-$NDK_VERSION-linux-x86_64.zip \
 && unzip android-ndk-$NDK_VERSION-linux-x86_64.zip \
 && rm android-ndk-$NDK_VERSION-linux-x86_64.zip
#   install QBDI compilation dependencies
#   git archive -o qbdi-deps.tar.gz --prefix=qbdi-deps/ HEAD deps
COPY qbdi-deps.tar.gz $HOME/
RUN sudo chown -R $USER:$USER qbdi-deps
WORKDIR $HOME/qbdi-deps/deps/gtest/$QBDI_PLATFORM
RUN sh build.sh prepare \
 && sh build.sh build \
 && sh build.sh package \
 && sh build.sh clean
WORKDIR $HOME/qbdi-deps/deps/llvm/$QBDI_PLATFORM
RUN sh build.sh prepare \
 && sh build.sh build \
 && sh build.sh package \
 && sh build.sh clean
#   build / test / install QBDI
#   git archive -o qbdi.tar.gz --prefix=qbdi/ HEAD .
COPY qbdi.tar.gz $HOME/
WORKDIR $HOME/qbdi
RUN sudo chown -R $USER:$USER . \
 && rm -rf deps \
 && ln -s $HOME/qbdi-deps/deps deps \
 && mkdir build \
 && cd build \
 && sh ../cmake/config-$QBDI_PLATFORM.sh \
 && make -j2 \
 && rm -f QBDI-*-$QBDI_PLATFORM.tar.gz \
 && cpack
WORKDIR "$HOME/"
CMD ["/bin/bash"]
# Please add your HEALTHCHECK here!!!
