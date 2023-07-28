#
#   Copyright 2016-2019, Intel Corporation
#
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions
#   are met:
#
#       * Redistributions of source code must retain the above copyright
#         notice, this list of conditions and the following disclaimer.
#
#       * Redistributions in binary form must reproduce the above copyright
#         notice, this list of conditions and the following disclaimer in
#         the documentation and/or other materials provided with the
#         distribution.
#
#       * Neither the name of the copyright holder nor the names of its
#         contributors may be used to endorse or promote products derived
#         from this software without specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#   Dockerfile - a 'recipe' for Docker to build an image of ubuntu-based
#                environment prepared for running libpmemobj-cpp tests.
#
#   Pull base image
FROM ubuntu:18.04
MAINTAINER marcin.slusarz@intel.com
ENV DEBIAN_FRONTEND="noninteractive"
#   Update the Apt cache and install basic tools
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 asciidoc=8.6.10-2 autoconf=2.69-11 clang=1:6.0-41~exp5~ubuntu1 clang-format=1:6.0-41~exp5~ubuntu1 cmake=3.10.2-1ubuntu2.18.04.2 curl=7.58.0-2ubuntu3.24 debhelper=11.1.6ubuntu2 devscripts=2.17.12ubuntu1.1 doxygen=1.8.13-10 gcc=4:7.4.0-1ubuntu2.3 gdb=8.1.1-0ubuntu1 git=1:2.17.1-1ubuntu0.17 graphviz=2.40.1-2 libjson-c-dev=0.12.1-1.3ubuntu0.3 libkmod-dev=24-1ubuntu3.5 libncurses5-dev=6.1-1ubuntu1.18.04 libsfml-dev=2.4.2+dfsg-4 libtext-diff-perl=1.45-1 libudev-dev=237-3ubuntu10.57 libunwind-dev=1.2.1-8ubuntu0.1 llvm=1:6.0-41~exp5~ubuntu1 pkg-config=0.29.1-0ubuntu2 ruby=1:2.5.1 sudo=1.8.21p2-3ubuntu1.5 tzdata=2022g-0ubuntu0.18.04 uuid-dev=2.31.1-0.4ubuntu3.7 wget=1.19.4-1ubuntu2.2 whois=5.3.0 -y \
 && rm -rf /var/lib/apt/lists/*
#   Install libndctl
COPY install-libndctl.sh install-libndctl.sh
RUN ./install-libndctl.sh
#   Install valgrind
COPY install-valgrind.sh install-valgrind.sh
RUN ./install-valgrind.sh
#   Install pmdk
COPY install-pmdk.sh install-pmdk.sh
RUN ./install-pmdk.sh dpkg
#   Install Intel TBB
COPY install-tbb.sh install-tbb.sh
RUN ./install-tbb.sh
#   Add user
ENV USER="user"
ENV USERPASS="pass"
RUN useradd -m $USER -g sudo -p `mkpasswd $USERPASS `
USER $USER
#   Set required environment variables
ENV OS="ubuntu"
ENV OS_VER="18.04"
ENV PACKAGE_MANAGER="deb"
ENV NOTTY="1"
# Please add your HEALTHCHECK here!!!
