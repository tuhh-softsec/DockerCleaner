#  ===- libcxx/utils/docker/debian9/Dockerfile -------------------------===//
#
#                       The LLVM Compiler Infrastructure
#
#   This file is distributed under the University of Illinois Open Source
#   License. See LICENSE.TXT for details.
#
#  ===----------------------------------------------------------------------===//
#   Setup the base builder image with the packages we'll need to build GCC and Clang from source.
FROM launcher.gcr.io/google/debian9:latest AS builder-base
LABEL maintainer="\"libc++ Developers\""
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20230311 gnupg=2.2.40-1ubuntu2 build-essential=12.9ubuntu3 wget=1.21.3-1ubuntu1 subversion=1.14.2-4build2 unzip=6.0-27ubuntu1 automake=1:1.16.5-1.3 python cmake=3.25.1-1 ninja-build=1.11.1-1 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 gcc-multilib=4:12.2.0-3ubuntu1 g++-multilib=4:12.2.0-3ubuntu1 libc6-dev=2.37-0ubuntu2 bison=2:3.8.2+dfsg-1build1 flex=2.6.4-8.1 libtool=2.4.7-5 autoconf=2.71-3 binutils-dev=2.40-2ubuntu3 binutils-gold software-properties-common=0.99.35 -y \
 && update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20 \
 && update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10
#   Build GCC 4.9 for testing our C++11 against
FROM builder-base AS gcc-49-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_gcc.sh /tmp/build_gcc.sh
RUN git clone --depth=1 --branch gcc-4_9_4-release git://gcc.gnu.org/git/gcc.git /tmp/gcc-4.9.4
RUN cd /tmp/gcc-4.9.4/ \
 && ./contrib/download_prerequisites
RUN /tmp/build_gcc.sh --source /tmp/gcc-4.9.4 --to /opt/gcc-4.9.4
#   Build GCC ToT for testing in all dialects.
FROM builder-base AS gcc-tot-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/build_gcc.sh /tmp/build_gcc.sh
RUN git clone --depth=1 git://gcc.gnu.org/git/gcc.git /tmp/gcc-tot
RUN cd /tmp/gcc-tot \
 && ./contrib/download_prerequisites
RUN /tmp/build_gcc.sh --source /tmp/gcc-tot --to /opt/gcc-tot
#   Build LLVM 4.0 which is used to test against a "legacy" compiler.
FROM builder-base AS llvm-4-builder
LABEL maintainer="\"libc++ Developers\""
COPY scripts/checkout_git.sh /tmp/checkout_git.sh
COPY scripts/build_install_llvm.sh /tmp/build_install_llvm.sh
RUN /tmp/checkout_git.sh --to /tmp/llvm-4.0 -p clang -p compiler-rt --branch release_40
RUN /tmp/build_install_llvm.sh --install /opt/llvm-4.0 --source /tmp/llvm-4.0 --build /tmp/build-llvm-4.0 -i install-clang -i install-clang-headers -i install-compiler-rt -- -DCMAKE_BUILD_TYPE=RELEASE -DLLVM_ENABLE_ASSERTIONS=ON
#   Stage 2. Produce a minimal release image with build results.
FROM launcher.gcr.io/google/debian9:latest
LABEL maintainer="\"libc++ Developers\""
#   Copy over the GCC and Clang installations
COPY --from=gcc-49-builder /opt/gcc-4.9.4 /opt/gcc-4.9.4
COPY --from=gcc-tot-builder /opt/gcc-tot /opt/gcc-tot
COPY --from=llvm-4-builder /opt/llvm-4.0 /opt/llvm-4.0
RUN ln -s /opt/gcc-4.9.4/bin/gcc /usr/local/bin/gcc-4.9 \
 && ln -s /opt/gcc-4.9.4/bin/g++ /usr/local/bin/g++-4.9
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20230311 gnupg=2.2.40-1ubuntu2 build-essential=12.9ubuntu3 apt-transport-https=2.6.0 curl=7.88.1-7ubuntu1 software-properties-common=0.99.35 -y
RUN apt-get install --no-install-recommends systemd=252.5-2ubuntu3 sysvinit-utils=3.06-2ubuntu1 cmake=3.25.1-1 subversion=1.14.2-4build2 git=1:2.39.2-1ubuntu1 ninja-build=1.11.1-1 gcc-multilib=4:12.2.0-3ubuntu1 g++-multilib=4:12.2.0-3ubuntu1 python buildbot-slave=3.7.0-2 -y
COPY scripts/install_clang_packages.sh /tmp/install_clang_packages.sh
RUN /tmp/install_clang_packages.sh \
 && rm /tmp/install_clang_packages.sh
RUN git clone https://git.llvm.org/git/libcxx.git /libcxx
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
