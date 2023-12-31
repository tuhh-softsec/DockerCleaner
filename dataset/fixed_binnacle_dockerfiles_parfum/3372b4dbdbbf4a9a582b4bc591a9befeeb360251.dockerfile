#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
#  Apache Thrift Docker build environment for Ubuntu Xenial
#  Using all stock Ubuntu Xenial packaging except for:
#  - d: does not come with Ubuntu so we're installing 2.075.1 for coverage
#  - dart: does not come with Ubuntu so we're installing 1.22.1 for coverage
#  - dotnet: does not come with Ubuntu
#  - go: Xenial comes with 1.6, but we need 1.7 or later
#  - nodejs: Xenial comes with 4.2.6 which exits LTS April 2018, so we're installing 6.x
#  - ocaml: causes stack overflow error, just started March 2018 not sure why
#
FROM buildpack-deps:xenial-scm
MAINTAINER Apache Thrift <dev@thrift.apache.org>
ENV DEBIAN_FRONTEND="noninteractive"
# ## Add apt repos
RUN apt-get update \
 && apt-get dist-upgrade -y \
 && apt-get install --no-install-recommends apt apt-transport-https apt-utils curl software-properties-common wget -y
#  csharp (mono)
#  RUN echo "deb http://download.mono-project.com/repo/debian xenial main" | tee /etc/apt/sources.list.d/mono.list && \
#      apt-key adv --keyserver keyserver.ubuntu.com --recv-keys A6A19B38D3D831EF
#  D
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EBCF975E5BA24D5E \
 && wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list \
 && wget -qO - https://dlang.org/d-keyring.gpg | apt-key add -
#  Dart
RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list
ENV DART_VERSION="1.22.1-1"
#  dotnet (core)
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/microsoft.gpg \
 && echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod xenial main" > /etc/apt/sources.list.d/dotnetdev.list
#  node.js
RUN curl -sL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - \
 && echo "deb https://deb.nodesource.com/node_6.x xenial main" | tee /etc/apt/sources.list.d/nodesource.list
# ## install general dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion bison build-essential clang cmake debhelper flex gdb llvm ninja-build pkg-config valgrind vim `` -y
ENV PATH="/usr/lib/llvm-3.8/bin:$PATH"
# ## languages
RUN apt-get install --no-install-recommends libboost-dev libboost-filesystem-dev libboost-program-options-dev libboost-system-dev libboost-test-dev libboost-thread-dev libevent-dev libssl-dev qt5-default qtbase5-dev qtbase5-dev-tools `` -y
RUN apt-get install --no-install-recommends mono-devel `` -y
ENV D_VERSION="2.075.1-0"
RUN apt-get install --no-install-recommends dub=1.6.0-0 dfmt dscanner libevent-dev libssl-dev xdg-utils `` dmd-bin=$D_VERSION libphobos2-dev=$D_VERSION -y --allow-unauthenticated
RUN mkdir -p /usr/include/dmd/druntime/import/deimos /usr/include/dmd/druntime/import/C \
 && curl -sSL https://github.com/D-Programming-Deimos/libevent/archive/master.tar.gz | tar xz \
 && mv libevent-master/deimos/* /usr/include/dmd/druntime/import/deimos/ \
 && mv libevent-master/C/* /usr/include/dmd/druntime/import/C/ \
 && rm -rf libevent-master
RUN curl -sSL https://github.com/D-Programming-Deimos/openssl/archive/v1.1.6+1.0.1g.tar.gz | tar xz \
 && mv openssl-1.1.6-1.0.1g/deimos/* /usr/include/dmd/druntime/import/deimos/ \
 && mv openssl-1.1.6-1.0.1g/C/* /usr/include/dmd/druntime/import/C/ \
 && rm -rf openssl-1.1.6-1.0.1g
RUN apt-get install --no-install-recommends `` dart=$DART_VERSION -y
ENV PATH="/usr/lib/dart/bin:$PATH"
RUN apt-get install --no-install-recommends dotnet-sdk-2.1.4 `` -y
RUN apt-get install --no-install-recommends erlang-base erlang-eunit erlang-dev erlang-tools rebar `` -y
RUN apt-get install --no-install-recommends libglib2.0-dev `` -y
#  golang
ENV GOLANG_VERSION="1.7.6"
ENV GOLANG_DOWNLOAD_URL="https://golang.org/dl/go$GOLANG_VERSION.linux-amd64.tar.gz"
ENV GOLANG_DOWNLOAD_SHA256="ad5808bf42b014c22dd7646458f631385003049ded0bb6af2efc7f1f79fa29ea"
RUN curl -fsSL "$GOLANG_DOWNLOAD_URL" -o golang.tar.gz \
 && echo "$GOLANG_DOWNLOAD_SHA256 golang.tar.gz" | sha256sum -c - \
 && tar -C /usr/local -xzf golang.tar.gz \
 && ln -s /usr/local/go/bin/go /usr/local/bin \
 && rm golang.tar.gz
#  due to a bug in cabal in xenial (cabal-install package) we pull in another:
RUN apt-get install --no-install-recommends ghc `` -y \
 && cd /tmp \
 && wget -q https://www.haskell.org/cabal/release/cabal-install-1.24.0.2/cabal-install-1.24.0.2-x86_64-unknown-linux.tar.gz \
 && tar xzf cabal-install-1.24.0.2-x86_64-unknown-linux.tar.gz \
 && find dist-newstyle/ -type f -name cabal -exec mv {} /usr/bin ; \
 && rm -rf /tmp/cabal* \
 && cabal --version
RUN apt-get install --no-install-recommends haxe neko neko-dev libneko0 `` -y \
 && haxelib setup --always /usr/share/haxe/lib \
 && haxelib install --always hxcpp 3.4.64 2>&1 > /dev/null
#  note: hxcpp 3.4.185 (latest) no longer ships static libraries, and caused a build failure
RUN apt-get install --no-install-recommends ant ant-optional openjdk-8-jdk maven `` -y
RUN apt-get install --no-install-recommends lua5.2 lua5.2-dev `` -y
#  https://bugs.launchpad.net/ubuntu/+source/lua5.3/+bug/1707212
#  lua5.3 does not install alternatives so stick with 5.2 here
RUN apt-get install --no-install-recommends nodejs `` -y
#  Test dependencies for running puppeteer
RUN apt-get install --no-install-recommends libxss1 libatk-bridge2.0-0 libgtk-3-0 `` -y
#  THRIFT-4517: causes stack overflows; version too old; skip ocaml in xenial
#  RUN apt-get install -y --no-install-recommends \
#  `# OCaml dependencies` \
#        ocaml \
#        opam && \
#      opam init --yes && \
#      opam install --yes oasis
RUN apt-get install --no-install-recommends libbit-vector-perl libclass-accessor-class-perl libcrypt-ssleay-perl libio-socket-ssl-perl libnet-ssleay-perl `` -y
RUN apt-get install --no-install-recommends php7.0 php7.0-cli php7.0-dev php-pear re2c composer `` -y
RUN apt-get install --no-install-recommends python-all python-all-dbg python-all-dev python-backports.ssl-match-hostname python-ipaddress python-pip python-setuptools python-six python-tornado python-twisted python-wheel python-zope.interface python3-all python3-all-dbg python3-all-dev python3-setuptools python3-six python3-tornado python3-twisted python3-wheel python3-zope.interface `` -y \
 && pip install backports.ssl_match_hostname --upgrade
RUN apt-get install --no-install-recommends ruby ruby-dev ruby-bundler `` -y
RUN apt-get install --no-install-recommends cargo rustc `` -y
#  Clean up
RUN rm -rf /var/cache/apt/* \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/* \
 && rm -rf /var/tmp/*
ENV DOTNET_CLI_TELEMETRY_OPTOUT="1"
ENV THRIFT_ROOT="/thrift"
RUN mkdir -p $THRIFT_ROOT/src
COPY Dockerfile $THRIFT_ROOT/
WORKDIR $THRIFT_ROOT/src
