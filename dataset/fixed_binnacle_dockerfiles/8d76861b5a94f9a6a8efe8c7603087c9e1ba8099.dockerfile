#   Licensed to the Apache Software Foundation (ASF) under one
#   or more contributor license agreements.  See the NOTICE file
#   distributed with this work for additional information
#   regarding copyright ownership.  The ASF licenses this file
#   to you under the Apache License, Version 2.0 (the
#   "License"); you may not use this file except in compliance
#   with the License.  You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#   Dockerfile for installing the necessary dependencies for building Hadoop.
#   See BUILDING.txt.
FROM ubuntu:xenial
WORKDIR /root
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  ####
#   Disable suggests/recommends
#  ####
RUN echo APT::Install-Recommends "0" ; > /etc/apt/apt.conf.d/10disableextras
RUN echo APT::Install-Suggests "0" ; >> /etc/apt/apt.conf.d/10disableextras
ENV DEBIAN_FRONTEND="noninteractive"
ENV DEBCONF_TERSE="true"
#  #####
#   Install common dependencies from packages. Versions here are either
#   sufficient or irrelevant.
#
#   WARNING: DO NOT PUT JAVA APPS HERE! Otherwise they will install default
#   Ubuntu Java.  See Java section below!
#  #####
#   hadolint ignore=DL3008
RUN apt-get update -q \
 && apt-get install --no-install-recommends apt-utils=1.2.35 build-essential=12.1ubuntu2 bzip2=1.0.6-8ubuntu0.2 clang=1:3.8-33ubuntu3.1 curl=7.47.0-1ubuntu2.19 doxygen=1.8.11-1ubuntu0.1 fuse=2.9.4-1ubuntu3.1 g++=4:5.3.1-1ubuntu1 gcc=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 gnupg-agent=2.1.11-6ubuntu2.1 libbz2-dev=1.0.6-8ubuntu0.2 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libfuse-dev=2.9.4-1ubuntu3.1 libprotobuf-dev=2.6.1-1.3 libprotoc-dev=2.6.1-1.3 libsasl2-dev=2.1.26.dfsg1-14ubuntu0.2 libsnappy-dev=1.1.3-2 libssl-dev=1.0.2g-1ubuntu4.20 libtool=2.4.6-0.1 locales=2.23-0ubuntu11.3 make=4.1-6 pinentry-curses=0.9.7-3 pkg-config=0.29.1-0ubuntu1 python=2.7.12-1~16.04 python2.7=2.7.12-1ubuntu0~16.04.18 python-pip=8.1.1-2ubuntu0.6 python-pkg-resources=20.7.0-1 python-setuptools=20.7.0-1 python-wheel=0.29.0-1 rsync=3.1.1-3ubuntu1.3 software-properties-common=0.96.20.10 snappy=0.3.7-1.1 sudo=1.8.16-0ubuntu1.10 valgrind=1:3.11.0-1ubuntu4.2 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  ######
#   OpenJDK 8
#  ######
#   hadolint ignore=DL3008
RUN apt-get update -q \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 libbcprov-java=1.51-4ubuntu1 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  #####
#   Install cmake 3.1.0 (3.5.1 ships with Xenial)
#  #####
RUN mkdir -p /opt/cmake \
 && curl -L -s -S https://cmake.org/files/v3.1/cmake-3.1.0-Linux-x86_64.tar.gz -o /opt/cmake.tar.gz \
 && tar xzf /opt/cmake.tar.gz --strip-components 1 -C /opt/cmake
ENV CMAKE_HOME="/opt/cmake"
ENV PATH="\"${PATH}:/opt/cmake/bin\""
#  #####
#   Install Google Protobuf 2.5.0 (2.6.0 ships with Xenial)
#  #####
#   hadolint ignore=DL3003
RUN mkdir -p /opt/protobuf-src \
 && curl -L -s -S https://github.com/google/protobuf/releases/download/v2.5.0/protobuf-2.5.0.tar.gz -o /opt/protobuf.tar.gz \
 && tar xzf /opt/protobuf.tar.gz --strip-components 1 -C /opt/protobuf-src \
 && cd /opt/protobuf-src \
 && ./configure --prefix=/opt/protobuf \
 && make install \
 && cd /root \
 && rm -rf /opt/protobuf-src
ENV PROTOBUF_HOME="/opt/protobuf"
ENV PATH="\"${PATH}:/opt/protobuf/bin\""
#  #####
#   Install Apache Maven 3.3.9 (3.3.9 ships with Xenial)
#  #####
#   hadolint ignore=DL3008
RUN apt-get update -q \
 && apt-get install --no-install-recommends maven=3.3.9-3 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ENV MAVEN_HOME="/usr"
#  #####
#   Install findbugs 3.0.1 (3.0.1 ships with Xenial)
#   Ant is needed for findbugs
#  #####
#   hadolint ignore=DL3008
RUN apt-get update -q \
 && apt-get install --no-install-recommends findbugs=3.0.1-2 ant=1.9.6-1ubuntu1.1 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ENV FINDBUGS_HOME="/usr"
#  ###
#   Install shellcheck (0.4.6, the latest as of 2017-09-26)
#  ###
#   hadolint ignore=DL3008
RUN add-apt-repository -y ppa:jonathonf/ghc-8.0.2 \
 && apt-get update -q \
 && apt-get install --no-install-recommends shellcheck=0.3.7-5 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  ###
#   Install bats (0.4.0, the latest as of 2017-09-26, ships with Xenial)
#  ###
#   hadolint ignore=DL3008
RUN apt-get update -q \
 && apt-get install --no-install-recommends bats=0.4.0-1ubuntu4 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  ###
#   Install pylint at fixed version (2.0.0 removed python2 support)
#   https://github.com/PyCQA/pylint/issues/2294
#  ###
RUN pip2 install pylint==1.9.2
#  ###
#   Install dateutil.parser
#  ###
RUN pip2 install python-dateutil==2.7.3
#  ##
#   Install node.js for web UI framework (4.2.6 ships with Xenial)
#  ##
#   hadolint ignore=DL3008, DL3016
RUN apt-get update -q \
 && apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && ln -s /usr/bin/nodejs /usr/bin/node \
 && npm install npm@latest -g \
 && npm install jshint@2.13.6 -g
#  ##
#   Install hadolint
#  ###
RUN curl -L -s -S https://github.com/hadolint/hadolint/releases/download/v1.11.1/hadolint-Linux-x86_64 -o /bin/hadolint \
 && chmod a+rx /bin/hadolint \
 && shasum -a 512 /bin/hadolint | awk '$1!="734e37c1f6619cbbd86b9b249e69c9af8ee1ea87a2b1ff71dccda412e9dac35e63425225a95d71572091a3f0a11e9a04c2fc25d9e91b840530c26af32b9891ca" {exit(1)}'
#  ##
#   Avoid out of memory errors in builds
#  ##
ENV MAVEN_OPTS="-Xms256m -Xmx1536m"
#  ##
#   Everything past this point is either not needed for testing or breaks Yetus.
#   So tell Yetus not to read the rest of the file:
#   YETUS CUT HERE
#  ##
#  ###
#   Install svn & Forrest (for Apache Hadoop website)
#  ##
#   hadolint ignore=DL3008
RUN apt-get update -q \
 && apt-get install --no-install-recommends subversion=1.9.3-2ubuntu1.3 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /opt/apache-forrest \
 && curl -L -s -S https://archive.apache.org/dist/forrest/0.8/apache-forrest-0.8.tar.gz -o /opt/forrest.tar.gz \
 && tar xzf /opt/forrest.tar.gz --strip-components 1 -C /opt/apache-forrest
RUN echo 'forrest.home=/opt/apache-forrest' > build.properties
ENV FORREST_HOME="/opt/apache-forrest"
#   Hugo static website generator (for new tez site and Ozone docs)
RUN curl -L -o hugo.deb https://github.com/gohugoio/hugo/releases/download/v0.30.2/hugo_0.30.2_Linux-64bit.deb \
 && dpkg --install hugo.deb \
 && rm hugo.deb
#   Add a welcome message and environment checks.
COPY tez_env_checks.sh /root/tez_env_checks.sh
RUN chmod 755 /root/tez_env_checks.sh
#   hadolint ignore=SC2016
RUN echo '${HOME}/tez_env_checks.sh' >> /root/.bashrc
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
