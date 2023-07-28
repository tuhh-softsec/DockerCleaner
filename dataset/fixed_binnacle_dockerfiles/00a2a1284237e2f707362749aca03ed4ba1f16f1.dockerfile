#   ################################################################
#   Copyright (C) 2016-present Genome Research Ltd.
#   Author: Marcus D. R. Klarqvist <mk819@cam.ac.uk>
#   
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files (the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in
#   all copies or substantial portions of the Software.
#   
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
#   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
#   DEALINGS IN THE SOFTWARE.
#   ################################################################
#   Base image
FROM ubuntu:16.04
#  ################# METADATA ######################
LABEL base_image="ubuntu:16.04"
LABEL version="1.0"
LABEL software="Tomahawk"
LABEL software.version="0.7.0"
LABEL about.summary="Latest image for Tomahawk"
LABEL about.home="https://github.com/mklarqvist/tomahawk"
LABEL about.documentation="https://github.com/mklarqvist/tomahawk"
LABEL about.license_file="https://github.com/mklarqvist/tomahawk/blob/master/LICENSE"
LABEL about.license="MIT"
LABEL about.tags="Genomics,Linkage-disequilibrium,LD"
#  ################# MAINTAINER ######################
MAINTAINER Marcus D. R. Klarqvist <mk819@cam.ac.uk>
ENV DEBIAN_FRONTEND="noninteractive"
RUN mv /etc/apt/sources.list /etc/apt/sources.list.bkp \
 && bash -c 'echo -e "deb mirror://mirrors.ubuntu.com/mirrors.txt xenial main restricted universe multiverse\ndeb mirror://mirrors.ubuntu.com/mirrors.txt xenial-updates main restricted universe multiverse\ndeb mirror://mirrors.ubuntu.com/mirrors.txt xenial-backports main restricted universe multiverse\ndeb mirror://mirrors.ubuntu.com/mirrors.txt xenial-security main restricted universe multiverse\n\n" > /etc/apt/sources.list' \
 && cat /etc/apt/sources.list.bkp >> /etc/apt/sources.list \
 && cat /etc/apt/sources.list
RUN apt-get clean all \
 && apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends pkg-config=0.29.1-0ubuntu1 zip=3.0-11 g++=4:5.3.1-1ubuntu1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 unzip=6.0-20ubuntu1.1 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 lsb-release=9.20160110ubuntu0.2 liblz4-dev=0.0~r131-2ubuntu2 libssl-dev=1.0.2g-1ubuntu4.20 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 liblz-dev=1.7-1 libbz2-dev=1.0.6-8ubuntu0.2 liblzma-dev=5.1.1alpha+20120614-2ubuntu2 autotools-dev=20150820.1 automake=1:1.15-4ubuntu1 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 grep=2.25-1~16.04.1 sed=4.2.2-7 dpkg=1.18.4ubuntu1.7 fuse=2.9.4-1ubuntu3.1 git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 zip=3.0-11 build-essential=12.1ubuntu2 pkg-config=0.29.1-0ubuntu1 bzip2=1.0.6-8ubuntu0.2 git=1:2.7.4-0ubuntu1.10 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y \
 && apt-get clean \
 && apt-get purge \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN mkdir -p /opt
RUN cd /opt \
 && git clone https://github.com/samtools/htslib.git \
 && cd /opt/htslib \
 && make \
 && make lib-static \
 && make install
RUN cd /opt \
 && git clone https://github.com/facebook/zstd.git \
 && cd /opt/zstd/ \
 && make \
 && make install
RUN cd /opt \
 && git clone https://mklarqvist:CpgqYBz6-S@github.com/mklarqvist/tomahawk \
 && cd /opt/tomahawk/ \
 && make \
 && make install
ENV PATH="$PATH:/usr/local/bin"
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib/"
VOLUME ["/data"]
CMD ["/bin/bash"]
WORKDIR /data
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
