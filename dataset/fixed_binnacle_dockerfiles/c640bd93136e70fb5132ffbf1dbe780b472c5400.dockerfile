#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#   Apache Thrift Docker build environment for Ubuntu Xenial
#   Using all stock Ubuntu Xenial packaging except for:
#   - d: does not come with Ubuntu so we're installing 2.073.2 for coverage
#   - dart: does not come with Ubuntu so we're installing 1.22.1 for coverage
#
#
#   Known missing or disabled libraries:
#   - d: deimos for libevent and openssl omitted - not compatible / build errors
FROM buildpack-deps:xenial-scm
MAINTAINER Apache Thrift <dev@thrift.apache.org>
ENV DEBIAN_FRONTEND="noninteractive"
#  ## Add apt repos
RUN apt-get update \
 && apt-get dist-upgrade -y \
 && apt-get install --no-install-recommends apt=1.2.35 apt-transport-https=1.2.35 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 apt-utils=1.2.35 -y
#   csharp (mono)
#   RUN echo "deb http://download.mono-project.com/repo/debian xenial main" | tee /etc/apt/sources.list.d/mono.list && \
#       apt-key adv --keyserver keyserver.ubuntu.com --recv-keys A6A19B38D3D831EF
#   D
RUN wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list \
 && wget -qO - https://dlang.org/d-keyring.gpg | apt-key add -
ENV D_VERSION="2.073.2-0"
#   Dart
RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list \
 && sed -i /etc/apt/sources.list.d/dart_stable.list -e 's/https:/http:/g'
#   since ubuntu-artful can't run dart, we'll run 1.240 on xenial for now
ENV DART_VERSION="1.24.2-1"
#   dotnet (core)
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/microsoft.gpg \
 && echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod xenial main" > /etc/apt/sources.list.d/dotnetdev.list
#   node.js (this step runs apt-get update internally)
#   note: node 8.5 introduced some issues with directory handling / jsdoc / something... using 7.x for now
#   RUN curl -sL https://deb.nodesource.com/setup_7.x | bash
#  ## install general dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion=1:2.1-4.2ubuntu1.1 bison=2:3.0.4.dfsg-1 build-essential=12.1ubuntu2 clang=1:3.8-33ubuntu3.1 cmake=3.5.1-1ubuntu3 debhelper=9.20160115ubuntu3 flex=2.6.0-11 gdb=7.11.1-0ubuntu1~16.5 llvm=1:3.8-33ubuntu3.1 ninja-build=1.5.1-0.1ubuntu1 pkg-config=0.29.1-0ubuntu1 valgrind=1:3.11.0-1ubuntu4.2 vim=2:7.4.1689-3ubuntu1.5 `` -y
ENV PATH="/usr/lib/llvm-3.8/bin:$PATH"
#  ## languages
RUN apt-get install --no-install-recommends libboost-dev=1.58.0.1ubuntu1 libboost-filesystem-dev=1.58.0.1ubuntu1 libboost-program-options-dev=1.58.0.1ubuntu1 libboost-system-dev=1.58.0.1ubuntu1 libboost-test-dev=1.58.0.1ubuntu1 libboost-thread-dev=1.58.0.1ubuntu1 libevent-dev=2.0.21-stable-2ubuntu0.16.04.1 libssl-dev=1.0.2g-1ubuntu4.20 qt5-default=5.5.1+dfsg-16ubuntu7.7 qtbase5-dev=5.5.1+dfsg-16ubuntu7.7 qtbase5-dev-tools=5.5.1+dfsg-16ubuntu7.7 `` -y
RUN apt-get install --no-install-recommends mono-devel=4.2.1.102+dfsg2-7ubuntu4 `` -y
RUN apt-get install --no-install-recommends dub=0.9.24-2 dfmt dscanner libevent-dev=2.0.21-stable-2ubuntu0.16.04.1 libssl-dev=1.0.2g-1ubuntu4.20 xdg-utils=1.1.1-1ubuntu1.16.04.5 `` dmd-bin=$D_VERSION libphobos2-dev=$D_VERSION -y --allow-unauthenticated
#   libevent deimos doesn't seem to work so not enabling it:
#   RUN mkdir -p /usr/include/dmd/druntime/import/deimos /usr/include/dmd/druntime/import/C && \
#       curl -sSL https://github.com/D-Programming-Deimos/libevent/archive/master.tar.gz| tar xz && \
#       mv libevent-master/deimos/* /usr/include/dmd/druntime/import/deimos/ && \
#       mv libevent-master/C/* /usr/include/dmd/druntime/import/C/ && \
#       rm -rf libevent-master
#   openssl deimos doesn't work with openssl-1.0.2 so not enabling it:
#   RUN curl -sSL https://github.com/D-Programming-Deimos/openssl/archive/master.tar.gz| tar xz && \
#       mv openssl-master/deimos/* /usr/include/dmd/druntime/import/deimos/ && \
#       mv openssl-master/C/* /usr/include/dmd/druntime/import/C/ && \
#       rm -rf openssl-master
RUN apt-get install --no-install-recommends `` dart=$DART_VERSION -y
ENV PATH="/usr/lib/dart/bin:$PATH"
RUN apt-get install --no-install-recommends dotnet-sdk-2.0.3 `` -y
RUN apt-get install --no-install-recommends erlang-base=1:18.3-dfsg-1ubuntu3.1 erlang-eunit=1:18.3-dfsg-1ubuntu3.1 erlang-dev=1:18.3-dfsg-1ubuntu3.1 erlang-tools=1:18.3-dfsg-1ubuntu3.1 rebar=2.6.0-2 `` -y
RUN apt-get install --no-install-recommends libglib2.0-dev=2.48.2-0ubuntu4.8 `` -y
RUN apt-get install --no-install-recommends golang-go=2:1.6-1ubuntu4 golang-race-detector-runtime=2:1.6-1ubuntu4 `` -y
RUN apt-get install --no-install-recommends ghc=7.10.3-7 cabal-install=1.22.6.0-2 `` -y
RUN apt-get install --no-install-recommends haxe=1:3.2.1+dfsg-1build1 neko=2.0.0-4build1 neko-dev=2.0.0-4build1 libneko0=2.0.0-4build1 `` -y
RUN haxelib setup --always /usr/share/haxe/lib \
 && haxelib install --always hxcpp 3.4.64
#   note: hxcpp 3.4.185 (latest) no longer ships static libraries, and caused a build failure
RUN apt-get install --no-install-recommends ant=1.9.6-1ubuntu1.1 ant-optional=1.9.6-1ubuntu1.1 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 maven=3.3.9-3 `` -y
RUN apt-get install --no-install-recommends lua5.2=5.2.4-1ubuntu1 lua5.2-dev `` -y
#   https://bugs.launchpad.net/ubuntu/+source/lua5.3/+bug/1707212
#   lua5.3 does not install alternatives so stick with 5.2 here
RUN apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 `` -y \
 && ln -s /usr/bin/nodejs /usr/bin/node
RUN apt-get install --no-install-recommends ocaml=4.02.3-5ubuntu2 opam=1.2.2-4 `` -y \
 && opam init --yes \
 && opam install --yes oasis
RUN apt-get install --no-install-recommends libbit-vector-perl=7.4-1build1 libclass-accessor-class-perl=0.503-2 libcrypt-ssleay-perl=0.73.04-1build1 libio-socket-ssl-perl=2.024-1 libnet-ssleay-perl=1.72-1build1 `` -y
RUN apt-get install --no-install-recommends php7.0=7.0.33-0ubuntu0.16.04.16 php7.0-cli=7.0.33-0ubuntu0.16.04.16 php7.0-dev=7.0.33-0ubuntu0.16.04.16 php-pear=1:1.10.1+submodules+notgz-6ubuntu0.3 re2c=0.16-1 phpunit=5.1.3-1+ubuntu3 `` -y
RUN apt-get install --no-install-recommends python-all=2.7.12-1~16.04 python-all-dbg=2.7.12-1~16.04 python-all-dev=2.7.12-1~16.04 python-backports.ssl-match-hostname=3.4.0.2-1 python-ipaddress=1.0.16-1 python-pip=8.1.1-2ubuntu0.6 python-setuptools=20.7.0-1 python-six=1.10.0-3 python-tornado=4.2.1-1ubuntu3.1 python-twisted=16.0.0-1ubuntu0.4 python-wheel=0.29.0-1 python-zope.interface=4.1.3-1build1 python3-all=3.5.1-3 python3-all-dbg=3.5.1-3 python3-all-dev=3.5.1-3 python3-setuptools=20.7.0-1 python3-six=1.10.0-3 python3-tornado=4.2.1-1ubuntu3.1 python3-twisted=16.0.0-1ubuntu0.4 python3-wheel=0.29.0-1 python3-zope.interface=4.1.3-1build1 `` -y \
 && pip install backports.ssl_match_hostname==3.7.0.1 --upgrade
RUN apt-get install --no-install-recommends ruby=1:2.3.0+1 ruby-dev=1:2.3.0+1 ruby-bundler=1.11.2-1 `` -y
RUN gem install bundler --version 2.4.12 --no-ri --no-rdoc
RUN apt-get install --no-install-recommends cargo=0.47.0-1~exp1ubuntu1~16.04.1 rustc=1.47.0+dfsg1+llvm-1ubuntu1~16.04.1 `` -y
#   Clean up
RUN rm -rf /var/cache/apt/* \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/* \
 && rm -rf /var/tmp/*
ENV DOTNET_CLI_TELEMETRY_OPTOUT="1"
ENV THRIFT_ROOT="/thrift"
RUN mkdir -p $THRIFT_ROOT/src
COPY Dockerfile $THRIFT_ROOT/
WORKDIR $THRIFT_ROOT/src
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
