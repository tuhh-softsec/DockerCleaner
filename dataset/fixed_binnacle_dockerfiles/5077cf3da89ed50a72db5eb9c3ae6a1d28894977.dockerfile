#   Copyright (C) 2016-2019 Intel Corporation
#   All rights reserved.
#
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted for any purpose (including commercial purposes)
#   provided that the following conditions are met:
#
#   1. Redistributions of source code must retain the above copyright notice,
#      this list of conditions, and the following disclaimer.
#
#   2. Redistributions in binary form must reproduce the above copyright notice,
#      this list of conditions, and the following disclaimer in the
#      documentation and/or materials provided with the distribution.
#
#   3. In addition, redistributions of modified forms of the source or binary
#      code must carry prominent notices stating that the original code was
#      changed and the date of the change.
#
#    4. All publications or advertising materials mentioning features or use of
#       this software are asked, but not required, to acknowledge that it was
#       developed by Intel Corporation and credit the contributors.
#
#   5. Neither the name of Intel Corporation, nor the name of any Contributor
#      may be used to endorse or promote products derived from this software
#      without specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
#   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
#   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
#   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
#   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
#   THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#   'recipe' for Docker to build an image of Leap-based
#   environment for building the DAOS project.
#
#   Pull base image
FROM suse/sles:12.3
MAINTAINER Johann Lombardi <johann.lombardi@intel.com>
#   Build arguments can be set via -build-arg
#   use same UID as host and default value of 1000 if not specified
ARG UID=1000
#   Update distribution
#   It's better to put the zypper update in the same "cache layer" as the
#   zypper install command so that the database is updated if/when the
#   installed packages list below is updated
RUN zypper --non-interactive update
RUN zypper --non-interactive install boost-devel ca-certificates-mozilla cmake cunit-devel curl doxygen flex gcc gcc-c++ git go graphviz libaio-devel libevent-devel libiscsi-devel libltdl7 libtool libnuma-devel libopenssl-devel libuuid-devel libyaml-devel make man nasm ninja pandoc patch python python-xml python3 readline-devel rsync yasm valgrind
RUN curl -ksS "https://bootstrap.pypa.io/get-pip.py" -o "/tmp/git-pip.py" ; python /tmp/git-pip.py pip ; python3 /tmp/git-pip.py pip ; cp /usr/bin/pip2 /usr/bin/pip ; rm /tmp/git-pip.py
RUN pip install setuptools==67.6.1 -U
RUN pip install wheel==0.40.0 -U
RUN pip install scons==3.0.1 ; pip3 install -U "meson<0.45.0"
#   Add CaRT user
ENV USER="cart"
ENV PASSWD="cart"
RUN useradd -u $UID -ms /bin/bash $USER
#   Switch to new user
USER $USER
WORKDIR /home/$USER
#   set NOBUILD to disable git clone & build
ARG NOBUILD
#   Fetch CaRT code
RUN if [ "x$NOBUILD" = "x" ] ; then git clone https://github.com/daos-stack/cart.git ; fi
WORKDIR /home/$USER/cart
#   Build CaRT & dependencies
RUN if [ "x$NOBUILD" = "x" ] ; then git submodule init \
 && git submodule update ; fi
RUN if [ "x$NOBUILD" = "x" ] ; then scons --build-deps=yes USE_INSTALLED=all install ; fi
#   Set environment variables
ENV PATH="/home/cart/cart/install/bin:$PATH"
ENV LD_LIBRARY_PATH="/home/cart/cart/install/Linux/lib:$LD_LIBRARY_PATH"
ENV CPATH="/home/cart/cart/install/Linux/include:$CPATH"
ENV CRT_PHY_ADDR_STR="ofi+sockets"
ENV OFI_INTERFACE="eth0"
# Please add your HEALTHCHECK here!!!
