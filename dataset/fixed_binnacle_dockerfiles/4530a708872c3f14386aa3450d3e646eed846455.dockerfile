#  ===- libcxx/utils/docker/debian9/Dockerfile --------------------------------------------------===//
#
#   Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
#   See https://llvm.org/LICENSE.txt for license information.
#   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#  ===-------------------------------------------------------------------------------------------===//
#   Setup the base builder image with the packages we'll need to build GCC and Clang from source.
FROM launcher.gcr.io/google/debian9:latest AS builder-base
LABEL maintainer="\"libc++ Developers\""
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20230311 gnupg=2.2.40-1ubuntu2 build-essential=12.9ubuntu3 wget=1.21.3-1ubuntu1 subversion=1.14.2-4build2 unzip=6.0-27ubuntu1 automake=1:1.16.5-1.3 python cmake=3.25.1-1 ninja-build=1.11.1-1 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 gcc-multilib=4:12.2.0-3ubuntu1 g++-multilib=4:12.2.0-3ubuntu1 libc6-dev=2.37-0ubuntu2 bison=2:3.8.2+dfsg-1build1 flex=2.6.4-8.1 libtool=2.4.7-5 autoconf=2.71-3 binutils-dev=2.40-2ubuntu3 binutils-gold software-properties-common=0.99.35 gnupg=2.2.40-1ubuntu2 apt-transport-https=2.6.0 sudo=1.9.13p1-1ubuntu2 systemd=252.5-2ubuntu3 sysvinit-utils=3.06-2ubuntu1 systemd-sysv=252.5-2ubuntu3 -y \
 && update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20 \
 && update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10 \
 && rm -rf /var/lib/apt/lists/*
#   Build GCC versions
FROM builder-base AS gcc-48-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_gcc_version.sh /tmp/build_gcc_version.sh
RUN /tmp/build_gcc_version.sh --install /opt/gcc-4.8.5 --branch gcc-4_8_5-release --cherry-pick ec1cc0263f156f70693a62cf17b254a0029f4852
FROM builder-base AS gcc-49-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_gcc_version.sh /tmp/build_gcc_version.sh
RUN /tmp/build_gcc_version.sh --install /opt/gcc-4.9.4 --branch gcc-4_9_4-release
FROM builder-base AS gcc-5-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_gcc_version.sh /tmp/build_gcc_version.sh
RUN /tmp/build_gcc_version.sh --install /opt/gcc-5 --branch gcc-5_5_0-release
FROM builder-base AS gcc-6-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_gcc_version.sh /tmp/build_gcc_version.sh
RUN /tmp/build_gcc_version.sh --install /opt/gcc-6 --branch gcc-6_5_0-release
FROM builder-base AS gcc-7-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_gcc_version.sh /tmp/build_gcc_version.sh
RUN /tmp/build_gcc_version.sh --install /opt/gcc-7 --branch gcc-7_4_0-release
FROM builder-base AS gcc-8-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_gcc_version.sh /tmp/build_gcc_version.sh
RUN /tmp/build_gcc_version.sh --install /opt/gcc-8 --branch gcc-8_2_0-release
FROM builder-base AS gcc-tot-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_gcc_version.sh /tmp/build_gcc_version.sh
RUN /tmp/build_gcc_version.sh --install /opt/gcc-tot --branch master
#   Build additional LLVM versions
FROM builder-base AS llvm-36-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_llvm_version.sh /tmp/build_llvm_version.sh
RUN /tmp/build_llvm_version.sh --install /opt/llvm-3.6 --branch release/3.6.x
FROM builder-base AS llvm-37-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_llvm_version.sh /tmp/build_llvm_version.sh
RUN /tmp/build_llvm_version.sh --install /opt/llvm-3.7 --branch release/3.7.x
FROM builder-base AS llvm-38-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_llvm_version.sh /tmp/build_llvm_version.sh
RUN /tmp/build_llvm_version.sh --install /opt/llvm-3.8 --branch release/3.8.x
FROM builder-base AS llvm-39-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_llvm_version.sh /tmp/build_llvm_version.sh
RUN /tmp/build_llvm_version.sh --install /opt/llvm-3.9 --branch release/3.9.x
FROM builder-base AS llvm-4-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_llvm_version.sh /tmp/build_llvm_version.sh
RUN /tmp/build_llvm_version.sh --install /opt/llvm-4.0 --branch release/4.x
FROM builder-base AS llvm-5-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_llvm_version.sh /tmp/build_llvm_version.sh
RUN /tmp/build_llvm_version.sh --install /opt/llvm-5.0 --branch release/5.x
FROM builder-base AS llvm-6-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_llvm_version.sh /tmp/build_llvm_version.sh
RUN /tmp/build_llvm_version.sh --install /opt/llvm-6.0 --branch release/6.x
FROM builder-base AS llvm-7-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_llvm_version.sh /tmp/build_llvm_version.sh
RUN /tmp/build_llvm_version.sh --install /opt/llvm-7.0 --branch release/7.x
FROM builder-base AS llvm-8-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_llvm_version.sh /tmp/build_llvm_version.sh
RUN /tmp/build_llvm_version.sh --install /opt/llvm-8.0 --branch release/8.x
FROM builder-base AS llvm-tot-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_llvm_version.sh /tmp/build_llvm_version.sh
RUN /tmp/build_llvm_version.sh --install /opt/llvm-tot --branch master
#  ===-------------------------------------------------------------------------------------------===//
#   buildslave
#  ===-------------------------------------------------------------------------------------------===//
FROM builder-base AS buildbot
#   Copy over the GCC and Clang installations
COPY --from=gcc-5-builder /opt/gcc-5 /opt/gcc-5
COPY --from=gcc-tot-builder /opt/gcc-tot /opt/gcc-tot
COPY --from=llvm-4-builder /opt/llvm-4.0 /opt/llvm-4.0
RUN ln -s /opt/gcc-5/bin/gcc /usr/local/bin/gcc-4.9 \
 && ln -s /opt/gcc-5/bin/g++ /usr/local/bin/g++-4.9
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion=1:2.11-6ubuntu1 vim=2:9.0.1000-4ubuntu2 buildbot-slave=3.7.0-2 -y \
 && rm -rf /var/lib/apt/lists/*
COPY scripts/install_clang_packages.sh /tmp/install_clang_packages.sh
RUN /tmp/install_clang_packages.sh \
 && rm /tmp/install_clang_packages.sh
RUN git clone https://git.llvm.org/git/libcxx.git /libcxx
#  ===-------------------------------------------------------------------------------------------===//
#    compiler-zoo
#  ===-------------------------------------------------------------------------------------------===//
FROM buildbot AS compiler-zoo
LABEL maintainer="\"libc++ Developers\""
#   Copy over the GCC and Clang installations
COPY --from=gcc-48-builder /opt/gcc-4.8.5 /opt/gcc-4.8.5
COPY --from=gcc-49-builder /opt/gcc-4.9.4 /opt/gcc-4.9.4
COPY --from=gcc-5-builder /opt/gcc-5 /opt/gcc-5
COPY --from=gcc-6-builder /opt/gcc-6 /opt/gcc-6
COPY --from=gcc-7-builder /opt/gcc-7 /opt/gcc-7
COPY --from=gcc-8-builder /opt/gcc-8 /opt/gcc-8
COPY --from=gcc-tot-builder /opt/gcc-tot /opt/gcc-tot
COPY --from=llvm-36-builder /opt/llvm-3.6 /opt/llvm-3.6
COPY --from=llvm-37-builder /opt/llvm-3.7 /opt/llvm-3.7
COPY --from=llvm-38-builder /opt/llvm-3.8 /opt/llvm-3.8
COPY --from=llvm-39-builder /opt/llvm-3.9 /opt/llvm-3.9
COPY --from=llvm-4-builder /opt/llvm-4.0 /opt/llvm-4.0
COPY --from=llvm-5-builder /opt/llvm-5.0 /opt/llvm-5.0
COPY --from=llvm-6-builder /opt/llvm-6.0 /opt/llvm-6.0
COPY --from=llvm-7-builder /opt/llvm-7.0 /opt/llvm-7.0
COPY --from=llvm-8-builder /opt/llvm-8.0 /opt/llvm-8.0
COPY --from=llvm-tot-builder /opt/llvm-tot /opt/llvm-tot
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
