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
#  - d: does not come with Ubuntu so we're installing 2.073.2 for coverage
#  - dart: does not come with Ubuntu so we're installing 1.22.1 for coverage
#
#
#  Known missing or disabled libraries:
#  - d: deimos for libevent and openssl omitted - not compatible / build errors
FROM buildpack-deps:xenial-scm
MAINTAINER Apache Thrift <dev@thrift.apache.org>
ENV DEBIAN_FRONTEND="noninteractive"
# ## Add apt repos
RUN apt-get update \
 && apt-get dist-upgrade -y \
 && apt-get install --no-install-recommends apt apt-transport-https curl wget apt-utils -y
#  csharp (mono)
#  RUN echo "deb http://download.mono-project.com/repo/debian xenial main" | tee /etc/apt/sources.list.d/mono.list && \
#      apt-key adv --keyserver keyserver.ubuntu.com --recv-keys A6A19B38D3D831EF
#  D
RUN wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list \
 && wget -qO - https://dlang.org/d-keyring.gpg | apt-key add -
ENV D_VERSION="2.073.2-0"
#  Dart
RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list \
 && sed -i /etc/apt/sources.list.d/dart_stable.list -e 's/https:/http:/g'
#  since ubuntu-artful can't run dart, we'll run 1.240 on xenial for now
ENV DART_VERSION="1.24.2-1"
#  dotnet (core)
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/microsoft.gpg \
 && echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod xenial main" > /etc/apt/sources.list.d/dotnetdev.list
#  node.js (this step runs apt-get update internally)
#  note: node 8.5 introduced some issues with directory handling / jsdoc / something... using 7.x for now
#  RUN curl -sL https://deb.nodesource.com/setup_7.x | bash
# ## install general dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion bison build-essential clang cmake debhelper flex gdb llvm ninja-build pkg-config valgrind vim `` -y
ENV PATH="/usr/lib/llvm-3.8/bin:$PATH"
# ## languages
RUN apt-get install --no-install-recommends libboost-dev libboost-filesystem-dev libboost-program-options-dev libboost-system-dev libboost-test-dev libboost-thread-dev libevent-dev libssl-dev qt5-default qtbase5-dev qtbase5-dev-tools `` -y
RUN apt-get install --no-install-recommends mono-devel `` -y
RUN apt-get install --no-install-recommends dub dfmt dscanner libevent-dev libssl-dev xdg-utils `` dmd-bin=$D_VERSION libphobos2-dev=$D_VERSION -y --allow-unauthenticated
#  libevent deimos doesn't seem to work so not enabling it:
#  RUN mkdir -p /usr/include/dmd/druntime/import/deimos /usr/include/dmd/druntime/import/C && \
#      curl -sSL https://github.com/D-Programming-Deimos/libevent/archive/master.tar.gz| tar xz && \
#      mv libevent-master/deimos/* /usr/include/dmd/druntime/import/deimos/ && \
#      mv libevent-master/C/* /usr/include/dmd/druntime/import/C/ && \
#      rm -rf libevent-master
#  openssl deimos doesn't work with openssl-1.0.2 so not enabling it:
#  RUN curl -sSL https://github.com/D-Programming-Deimos/openssl/archive/master.tar.gz| tar xz && \
#      mv openssl-master/deimos/* /usr/include/dmd/druntime/import/deimos/ && \
#      mv openssl-master/C/* /usr/include/dmd/druntime/import/C/ && \
#      rm -rf openssl-master
RUN apt-get install --no-install-recommends `` dart=$DART_VERSION -y
ENV PATH="/usr/lib/dart/bin:$PATH"
RUN apt-get install --no-install-recommends dotnet-sdk-2.0.3 `` -y
RUN apt-get install --no-install-recommends erlang-base erlang-eunit erlang-dev erlang-tools rebar `` -y
RUN apt-get install --no-install-recommends libglib2.0-dev `` -y
RUN apt-get install --no-install-recommends golang-go golang-race-detector-runtime `` -y
RUN apt-get install --no-install-recommends ghc cabal-install `` -y
RUN apt-get install --no-install-recommends haxe neko neko-dev libneko0 `` -y
RUN haxelib setup --always /usr/share/haxe/lib \
 && haxelib install --always hxcpp 3.4.64
#  note: hxcpp 3.4.185 (latest) no longer ships static libraries, and caused a build failure
RUN apt-get install --no-install-recommends ant ant-optional openjdk-8-jdk maven `` -y
RUN apt-get install --no-install-recommends lua5.2 lua5.2-dev `` -y
#  https://bugs.launchpad.net/ubuntu/+source/lua5.3/+bug/1707212
#  lua5.3 does not install alternatives so stick with 5.2 here
RUN apt-get install --no-install-recommends nodejs npm `` -y \
 && ln -s /usr/bin/nodejs /usr/bin/node
RUN apt-get install --no-install-recommends ocaml opam `` -y \
 && opam init --yes \
 && opam install --yes oasis
RUN apt-get install --no-install-recommends libbit-vector-perl libclass-accessor-class-perl libcrypt-ssleay-perl libio-socket-ssl-perl libnet-ssleay-perl `` -y
RUN apt-get install --no-install-recommends php7.0 php7.0-cli php7.0-dev php-pear re2c phpunit `` -y
RUN apt-get install --no-install-recommends python-all python-all-dbg python-all-dev python-backports.ssl-match-hostname python-ipaddress python-pip python-setuptools python-six python-tornado python-twisted python-wheel python-zope.interface python3-all python3-all-dbg python3-all-dev python3-setuptools python3-six python3-tornado python3-twisted python3-wheel python3-zope.interface `` -y \
 && pip install backports.ssl_match_hostname --upgrade
RUN apt-get install --no-install-recommends ruby ruby-dev ruby-bundler `` -y
RUN gem install bundler --no-ri --no-rdoc
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
