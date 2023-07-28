#
#   Copyright 2016-2017, Intel Corporation
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
#                environment for building the PMEMFILE project.
#
#   Pull base image
FROM ubuntu:16.04
MAINTAINER marcin.slusarz@intel.com
#   Update the Apt cache and install basic tools
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends attr=1:2.4.47-2 autoconf=2.69-9 automake=1:1.15-4ubuntu1 bc=1.06.95-9build1 clang=1:3.8-33ubuntu3.1 clang-format-3.8=1:3.8-2ubuntu4 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 debhelper=9.20160115ubuntu3 devscripts=2.16.2ubuntu3 doxygen=1.8.11-1ubuntu0.1 git=1:2.7.4-0ubuntu1.10 libacl1-dev=2.2.52-3 libaio-dev=0.3.110-2 libattr1-dev=1:2.4.47-2 libblkid-dev=2.27.1-6ubuntu3.10 libcapstone-dev=3.0.4-0.2 libcap-dev=1:2.24-12 libc6-dbg=2.23-0ubuntu11.3 libclang-dev=1:3.8-33ubuntu3.1 libfuse-dev=2.9.4-1ubuntu3.1 libtext-diff-perl=1.43-1 libtool=2.4.6-0.1 libunwind-dev=1.1-4.1 pandoc=1.16.0.2~dfsg-1 pkg-config=0.29.1-0ubuntu1 python-pip=8.1.1-2ubuntu0.6 python3=3.5.1-3 python3-coverage=3.7.1+dfsg.1-1ubuntu7 ruby=1:2.3.0+1 sqlite3=3.11.0-1ubuntu1.5 sudo=1.8.16-0ubuntu1.10 tcl-dev=8.6.0+9 uuid-dev=2.27.1-6ubuntu3.10 wget=1.17.1-1ubuntu1.5 whois=5.2.11 xfsdump=3.1.6+nmu1 -y )
#   Upgrade pip
RUN pip install pip==23.1 --upgrade
#   Install codecov
RUN pip install codecov==null
#   Install valgrind
COPY install-valgrind.sh install-valgrind.sh
RUN ./install-valgrind.sh
#   Install nvml
COPY install-nvml.sh install-nvml.sh
RUN ./install-nvml.sh dpkg
#   Install syscall_intercept
COPY install-syscall_intercept.sh install-syscall_intercept.sh
RUN ./install-syscall_intercept.sh deb
RUN curl -L -o /googletest-1.8.0.zip https://github.com/google/googletest/archive/release-1.8.0.zip
#   Install pjdfstest
COPY 0001-disable-special-files-tests.patch 0001-disable-special-files-tests.patch
COPY install-pjdfstest.sh install-pjdfstest.sh
RUN ./install-pjdfstest.sh
#   Add user
ENV USER="user"
ENV USERPASS="pass"
RUN useradd -m $USER -g sudo -p `mkpasswd $USERPASS `
#   Install ltp
COPY install-ltp.sh install-ltp.sh
RUN ./install-ltp.sh
#   Install xfs
COPY 0001-enable-pmemfile.patch 0001-enable-pmemfile.patch
COPY install-xfs.sh install-xfs.sh
RUN ./install-xfs.sh
RUN apt-get remove -y autoconf automake doxygen whois
RUN apt-get autoremove -y
USER $USER
#   Install sqlite
COPY install-sqlite.sh install-sqlite.sh
RUN ./install-sqlite.sh
#   Set required environment variables
ENV OS="ubuntu"
ENV OS_VER="16.04"
ENV PACKAGE_MANAGER="deb"
ENV NOTTY="1"
# Please add your HEALTHCHECK here!!!
