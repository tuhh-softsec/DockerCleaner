#
#  Copyright 2016-2019, Intel Corporation
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#      * Redistributions of source code must retain the above copyright
#        notice, this list of conditions and the following disclaimer.
#
#      * Redistributions in binary form must reproduce the above copyright
#        notice, this list of conditions and the following disclaimer in
#        the documentation and/or other materials provided with the
#        distribution.
#
#      * Neither the name of the copyright holder nor the names of its
#        contributors may be used to endorse or promote products derived
#        from this software without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#  Dockerfile - a 'recipe' for Docker to build an image of ubuntu-based
#               environment prepared for running pmemkv build and tests.
#
#  Pull base image
FROM ubuntu:18.04
MAINTAINER lukasz.stolarczuk@intel.com
#  Update the Apt cache and install basic tools
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends autoconf automake build-essential clang clang-format cmake curl debhelper devscripts fakeroot git libc6-dbg libdaxctl-dev libndctl-dev libnuma-dev libtext-diff-perl libtool libunwind8-dev maven nodejs-dev npm numactl openjdk-8-jdk pkg-config rapidjson-dev ruby-dev sudo wget whois \
 && rm -rf /var/lib/apt/lists/*
#  Install Googletest
COPY install-googletest.sh install-googletest.sh
RUN ./install-googletest.sh
#  Install valgrind
COPY install-valgrind.sh install-valgrind.sh
RUN ./install-valgrind.sh
#  Install pmdk
COPY install-pmdk.sh install-pmdk.sh
RUN ./install-pmdk.sh dpkg
#  Install pmdk c++ bindings
COPY install-libpmemobj-cpp.sh install-libpmemobj-cpp.sh
RUN ./install-libpmemobj-cpp.sh DEB
#  Install memkind
COPY install-memkind.sh install-memkind.sh
RUN ./install-memkind.sh
#  Install Intel TBB
COPY install-tbb.sh install-tbb.sh
RUN ./install-tbb.sh
#  Add user
ENV USER="user"
ENV USERPASS="pass"
RUN useradd -m $USER -g sudo -p `mkpasswd $USERPASS `
USER $USER
#  Set required environment variables
ENV OS="ubuntu"
ENV OS_VER="18.04"
ENV PACKAGE_MANAGER="dpkg"
ENV NOTTY="1"
