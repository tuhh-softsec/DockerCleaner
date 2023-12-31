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
#  ##############
#
#   Example Apache Yetus Dockerfile that includes all functionality supported
#   as well as enough bits to build and release Apache Yetus itself.
#
#  ##############
FROM ubuntu:xenial
#  # NOTE to committers: if this gets moved from Xenial to something else, be
#  # sure to also fix the gpg link in asf-site-src as appropriate
WORKDIR /root
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV DEBIAN_FRONTEND="noninteractive"
ENV DEBCONF_TERSE="true"
#  #####
#   Install some basic Apache Yetus requirements
#   some git repos need ssh-client so do it too
#   Adding libffi-dev for all the programming languages
#   that take advantage of it.
#  #####
RUN apt-get update -q \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 libffi-dev=3.2.1-4 locales=2.23-0ubuntu11.3 pkg-config=0.29.1-0ubuntu1 rsync=3.1.1-3ubuntu1.3 software-properties-common=0.96.20.10 ssh-client -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  ##
#   Set the locale
#  ##
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#  ###
#   Install java (first, since we want to dicate what form of Java)
#  ###
#  ###
#   OpenJDK 8
#  ###
RUN apt-get update -q \
 && apt-get install --no-install-recommends openjdk-8-jdk-headless=8u292-b10-0ubuntu1~16.04.1 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
#  ###
#   Install ant
#  ###
RUN apt-get update -q \
 && apt-get install --no-install-recommends ant=1.9.6-1ubuntu1.1 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  ###
#   Install GNU automake, GNU make, and related
#  ###
RUN apt-get update -q \
 && apt-get install --no-install-recommends autoconf=2.69-9 automake=1:1.15-4ubuntu1 libtool=2.4.6-0.1 make=4.1-6 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  ###
#   Install bats (TAP-capable unit testing for shell scripts)
#  ###
RUN git clone --branch v1.1.0 https://github.com/bats-core/bats-core.git /tmp/bats-core \
 && /tmp/bats-core/install.sh /usr/local \
 && rm -rf /tmp/bats-core
#  ###
#   Install cmake
#  ###
RUN apt-get update -q \
 && apt-get install --no-install-recommends cmake=3.5.1-1ubuntu3 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  ##
#   Install docker
#  ##
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
RUN add-apt-repository -y "deb [arch=amd64] https://download.docker.com/linux/ubuntu $( lsb_release -cs ;) stable"
RUN apt-get update -q \
 && apt-get install --no-install-recommends docker-ce -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  #####
#   Install findbugs
#  #####
RUN apt-get update -q \
 && apt-get install --no-install-recommends findbugs=3.0.1-2 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ENV FINDBUGS_HOME="/usr"
#  ####
#   Install SpotBugs
#  ####
RUN curl -fsSL https://repo.maven.apache.org/maven2/com/github/spotbugs/spotbugs/3.1.12/spotbugs-3.1.12.tgz -o spotbugs.tgz \
 && curl -fsSL https://repo.maven.apache.org/maven2/com/github/spotbugs/spotbugs/3.1.12/spotbugs-3.1.12.tgz.sha1 -o spotbugs.tgz.sha1 \
 && echo -n " spotbugs.tgz" >> spotbugs.tgz.sha1 \
 && shasum -c spotbugs.tgz.sha1 \
 && mkdir -p /opt/spotbugs \
 && tar -C /opt/spotbugs --strip-components 1 -xpf spotbugs.tgz \
 && rm spotbugs.tgz spotbugs.tgz.sha1
ENV SPOTBUGS_HOME="/opt/spotbugs"
#  ###
#   Install GNU C/C++
#  ###
RUN apt-get update -q \
 && apt-get install --no-install-recommends g++=4:5.3.1-1ubuntu1 gcc=4:5.3.1-1ubuntu1 libc-dev -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  #####
#   Install maven
#  #####
RUN apt-get update -q \
 && apt-get install --no-install-recommends maven=3.3.9-3 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  #####
#   Install perl
#  #####
RUN apt-get update -q \
 && apt-get install --no-install-recommends perl=5.22.1-9ubuntu0.9 libperl-critic-perl=1.126-1 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  #####
#   Install python3 and pylint3
#  #####
RUN add-apt-repository -y ppa:deadsnakes/ppa
#   hadolint ignore=DL3008
RUN apt-get update -q \
 && apt-get install --no-install-recommends python3.7 python3.7-dev -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py \
 && python3.7 get-pip.py \
 && rm /usr/local/bin/pip
#   astroid and pylint go hand-in-hand.  Upgrade both at the same time.
#   hadolint ignore=DL3013
RUN pip3 install -v astroid==2.2.4 pylint==2.3.1 docker-compose==1.24.0
RUN mv /usr/local/bin/pylint /usr/local/bin/pylint3
#  #####
#   Install python, pylint2, and yamllint
#  #####
RUN apt-get update -q \
 && apt-get install --no-install-recommends python=2.7.12-1~16.04 python2.7=2.7.12-1ubuntu0~16.04.18 python-pip=8.1.1-2ubuntu0.6 python-pkg-resources=20.7.0-1 python-setuptools=20.7.0-1 python-wheel=0.29.0-1 python-dev=2.7.12-1~16.04 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN pip2 install -v astroid==1.6.5 pylint==1.9.2 python-dateutil==2.7.3 yamllint==1.12.1
RUN mv /usr/local/bin/pylint /usr/local/bin/pylint2
#  ####
#   backward compatibility
#  ####
RUN ln -s /usr/local/bin/pylint2 /usr/local/bin/pylint
#  ###
#   Install ruby and associated bits
#  ##
RUN echo 'gem: --no-rdoc --no-ri' >> /root/.gemrc
RUN apt-get update -q \
 && apt-get install --no-install-recommends ruby=1:2.3.0+1 ruby-dev=1:2.3.0+1 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN gem install rake --version 12.3.2
RUN gem install rubocop --version 0.67.2
RUN gem install bundler --version 1.17.3
#   set some reasonable defaults for ruby
#   user's can always override these as needed
ENV PATH="${PATH}:/var/tmp/.bundler-gems/bin"
ENV BUNDLE_PATH="/var/tmp/.bundler-gems"
#  ###
#   Install shellcheck (shell script lint)
#   NOTE: A bunch of stuff is removed to shrink the size of the Docker image
#   Be very careful changing the code here, layer size may grow very large!
#  ###
RUN apt-get update -q \
 && apt-get install --no-install-recommends cabal-install=1.22.6.0-2 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && cabal update \
 && cabal install -j ShellCheck \
 && cp -p /root/.cabal/bin/shellcheck /usr/local/bin/shellcheck \
 && apt-get remove -y ghc cabal-install \
 && apt-get autoremove -y \
 && rm -rf /root/.cabal
#  ##
#   Install hadolint
#  ###
RUN curl -L -s -S https://github.com/hadolint/hadolint/releases/download/v1.16.2/hadolint-Linux-x86_64 -o /bin/hadolint \
 && chmod a+rx /bin/hadolint \
 && shasum -a 512 /bin/hadolint | awk '$1!="7044f2f5a8a9f2a52d9921f34a5cca5fee6a26c1de052f5348d832624976eb760195c65d79a69454f3056359e23b382353977340a7a1ca5c7805b164690c0485" {exit(1)}'
#  ##
#   Install npm and JSHint
#  ##
#   hadolint ignore=DL3008
RUN curl -sL https://deb.nodesource.com/setup_11.x | bash - \
 && apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && npm install jshint@2.10.2 markdownlint-cli@0.15.0 -g
#  ##
#   Install golang and supported helpers
#  ##
#   hadolint ignore=DL3008
RUN add-apt-repository -y ppa:longsleep/golang-backports \
 && apt-get update -q \
 && apt-get install --no-install-recommends golang-go=2:1.6-1ubuntu4 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN go get -u github.com/mgechev/revive \
 && go get -u github.com/mrtazz/checkmake \
 && (GO111MODULE=on go get github.com/golangci/golangci-lint/cmd/golangci-lint@v1.16.0 ) \
 && (GO111MODULE=on go get github.com/uber/prototool/cmd/prototool@6a473a4f1d86e7c8ff6a844d7dc4f7c3f6207a3f ) \
 && mv /root/go/bin/* /usr/local/bin \
 && rm -rf /root/go
#  ###
#   YETUS CUT HERE
#   Anthing after the above line is ignored by Yetus, so could
#   include other requirements not needed by your development
#   (but not build) environment
#  ##
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
