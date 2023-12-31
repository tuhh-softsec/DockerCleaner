#   Copyright (c) 2019, 2019 IBM Corp. and others
#
#   This program and the accompanying materials are made available under
#   the terms of the Eclipse Public License 2.0 which accompanies this
#   distribution and is available at https://www.eclipse.org/legal/epl-2.0/
#   or the Apache License, Version 2.0 which accompanies this distribution and
#   is available at https://www.apache.org/licenses/LICENSE-2.0.
#
#   This Source Code may also be made available under the following
#   Secondary Licenses when the conditions for such availability set
#   forth in the Eclipse Public License, v. 2.0 are satisfied: GNU
#   General Public License, version 2 with the GNU Classpath
#   Exception [1] and GNU General Public License, version 2 with the
#   OpenJDK Assembly Exception [2].
#
#   [1] https://www.gnu.org/software/classpath/license.html
#   [2] http://openjdk.java.net/legal/assembly-exception.html
#
#   SPDX-License-Identifier: EPL-2.0 OR Apache-2.0 OR GPL-2.0 WITH Classpath-exception-2.0 OR LicenseRef-GPL-2.0 WITH Assembly-exception
#   To use this docker file:
#     docker build -t=openj9aarch64 .
#     docker run -it openj9aarch64
FROM ubuntu:16.04
#   Install required OS tools
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 -qq -y \
 && add-apt-repository ppa:ubuntu-toolchain-r/test \
 && apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-9 ca-certificates=20210119~16.04.1 ccache=3.2.4-1 cmake=3.5.1-1ubuntu3 cpio=2.11+dfsg-5ubuntu1.1 file=1:5.25-2ubuntu1.4 g++-7 gcc-7 git=1:2.7.4-0ubuntu1.10 git-core=1:2.7.4-0ubuntu1.10 make=4.1-6 pkg-config=0.29.1-0ubuntu1 qemu=1:2.5+dfsg-5ubuntu10.51 realpath=8.25-2ubuntu3~16.04 ssh=1:7.2p2-4ubuntu2.10 unzip=6.0-20ubuntu1.1 vim-tiny=2:7.4.1689-3ubuntu1.5 wget=1.17.1-1ubuntu1.5 zip=3.0-11 -qq -y \
 && rm -rf /var/lib/apt/lists/*
#   Create links for c++,g++,cc,gcc
RUN ln -s g++ /usr/bin/c++ \
 && ln -s g++-7 /usr/bin/g++ \
 && ln -s gcc /usr/bin/cc \
 && ln -s gcc-7 /usr/bin/gcc
#   Download and setup freemarker.jar to /root/freemarker.jar
RUN cd /root \
 && wget https://sourceforge.net/projects/freemarker/files/freemarker/2.3.8/freemarker-2.3.8.tar.gz/download -O freemarker.tgz \
 && tar -xzf freemarker.tgz freemarker-2.3.8/lib/freemarker.jar --strip=2 \
 && rm -f freemarker.tgz
#   Download and install boot JDK from AdoptOpenJDK
RUN cd /root \
 && wget -O bootjdk11.tar.gz "https://api.adoptopenjdk.net/v2/binary/releases/openjdk11?openjdk_impl=openj9&os=linux&arch=x64&release=latest&heap_size=normal&type=jdk" \
 && tar -xzf bootjdk11.tar.gz \
 && rm -f bootjdk11.tar.gz \
 && mv $( ls | grep -i jdk ;) bootjdk11
#   get the toolchain
RUN cd /root \
 && wget https://releases.linaro.org/components/toolchain/binaries/7.3-2018.05/aarch64-linux-gnu/gcc-linaro-7.3.1-2018.05-x86_64_aarch64-linux-gnu.tar.xz \
 && tar xf gcc-linaro-7.3.1-2018.05-x86_64_aarch64-linux-gnu.tar.xz \
 && rm gcc-linaro-7.3.1-2018.05-x86_64_aarch64-linux-gnu.tar.xz
#   Get the debs for native libraries on the target platform
#   These were current releases in stretch as of Nov 2017
#   We might need older libraries for older OS releases, and presumably at
#   some point we might need to update to newer ones.
#   Libraries can be found through https://www.debian.org/distrib/packages
#   TODO Mirror site should be configurable.
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/a/alsa-lib/libasound2_1.1.3-5_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/a/alsa-lib/libasound2-dev_1.1.3-5_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/c/cups/libcups2-dev_2.2.1-8%2bdeb9u3_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/c/cups/libcupsimage2-dev_2.2.1-8%2bdeb9u3_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/d/dwarfutils/libdwarf-dev_20161124-1+deb9u1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/f/freetype/libfreetype6_2.6.3-3.2_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/f/freetype/libfreetype6-dev_2.6.3-3.2_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/f/fontconfig/libfontconfig1_2.13.1-2_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/f/fontconfig/libfontconfig1-dev_2.13.1-2_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libi/libice/libice-dev_1.0.9-2_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libp/libpng1.6/libpng16-16_1.6.28-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libp/libpng1.6/libpng-dev_1.6.28-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libs/libsm/libsm-dev_1.2.2-1+b3_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/o/openssl/libssl1.1_1.1.1b-2_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/o/openssl/libssl-dev_1.1.1b-2_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libx11/libx11-6_1.6.4-3%2bdeb9u1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libx11/libx11-dev_1.6.4-3%2bdeb9u1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxext/libxext6_1.3.3-1+b2_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxext/libxext-dev_1.3.3-1+b2_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxi/libxi6_1.7.9-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxi/libxi-dev_1.7.9-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxrandr/libxrandr-dev_1.5.1-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxrandr/libxrandr2_1.5.1-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxrender/libxrender1_0.9.10-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxrender/libxrender-dev_0.9.10-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxt/libxt-dev_1.1.5-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxtst/libxtst6_1.2.3-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/libx/libxtst/libxtst-dev_1.2.3-1_arm64.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/x/x11proto-core/x11proto-core-dev_7.0.31-1_all.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/x/x11proto-input/x11proto-input-dev_2.3.2-1_all.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/x/x11proto-kb/x11proto-kb-dev_1.0.7-1_all.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/x/x11proto-randr/x11proto-randr-dev_1.5.0-1_all.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/x/x11proto-render/x11proto-render-dev_0.11.1-2_all.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/x/x11proto-xext/x11proto-xext-dev_7.3.0-1_all.deb
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/debs/ http://ftp.us.debian.org/debian/pool/main/z/zlib/zlib1g_1.2.8.dfsg-5_arm64.deb
#   unpack debs into toolchain libc dir
RUN cd /root/debs \
 && for f in *.deb; do dpkg-deb -x $f . ; done \
 && cp -r usr/include/* ../gcc-linaro-7.3.1-2018.05-x86_64_aarch64-linux-gnu/aarch64-linux-gnu/libc/usr/include/ \
 && cp -r usr/lib/aarch64-linux-gnu/* ../gcc-linaro-7.3.1-2018.05-x86_64_aarch64-linux-gnu/aarch64-linux-gnu/libc/usr/lib/ \
 && cp lib/aarch64-linux-gnu/* ../gcc-linaro-7.3.1-2018.05-x86_64_aarch64-linux-gnu/aarch64-linux-gnu/libc/lib/ \
 && rm -rf /root/debs
#   Env vars set here will be visible in the running container, so can be
#   used to convey configuration information to the build scripts.
#   Set environment variable JAVA_HOME, and prepend $JAVA_HOME/bin to PATH
ENV JAVA_HOME="/root/bootjdk11"
ENV PATH="$JAVA_HOME/bin:$PATH"
#   Directory containing the cross compilation tool chain
ENV OPENJ9_CC_DIR="/root/gcc-linaro-7.3.1-2018.05-x86_64_aarch64-linux-gnu"
#   Prefix for the cross compilation tools (without the trailing '-')
ENV OPENJ9_CC_PREFIX="aarch64-linux-gnu"
#   Add the toolchain bin dir to the PATH for convenience
ENV PATH="$PATH:$OPENJ9_CC_DIR/bin"
WORKDIR /root
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
