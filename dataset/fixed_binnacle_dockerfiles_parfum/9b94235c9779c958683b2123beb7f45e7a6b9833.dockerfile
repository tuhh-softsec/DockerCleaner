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
#  Dockerfile - a 'recipe' for Docker to build an image of fedora-based
#               environment prepared for running libpmemobj-cpp tests.
#
#  Pull base image
FROM fedora:28
MAINTAINER marcin.slusarz@intel.com
#  Install basic tools
RUN dnf update -y \
 && dnf install -y asciidoc autoconf automake bash-completion clang cmake doxygen gcc gdb git hub json-c-devel kmod-devel libtool libudev-devel libunwind-devel libuuid-devel make man ncurses-devel open-sans-fonts passwd perl-Text-Diff rpm-build rpm-build-libs rpmdevtools SFML-devel sudo tar wget which xmlto \
 && dnf clean all
#  Install libndctl
COPY install-libndctl.sh install-libndctl.sh
RUN ./install-libndctl.sh fedora
#  Install valgrind
COPY install-valgrind.sh install-valgrind.sh
RUN ./install-valgrind.sh
#  Install pmdk
COPY install-pmdk.sh install-pmdk.sh
RUN ./install-pmdk.sh rpm
#  Install Intel TBB
COPY install-tbb.sh install-tbb.sh
RUN ./install-tbb.sh
#  Add user
ENV USER="user"
ENV USERPASS="pass"
RUN useradd -m $USER
RUN echo $USERPASS | passwd $USER --stdin
RUN gpasswd wheel -a $USER
USER $USER
#  Set required environment variables
ENV OS="fedora"
ENV OS_VER="28"
ENV PACKAGE_MANAGER="rpm"
ENV NOTTY="1"
