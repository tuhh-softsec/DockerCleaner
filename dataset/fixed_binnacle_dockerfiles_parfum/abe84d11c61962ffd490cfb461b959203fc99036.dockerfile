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
#  Apache Thrift Docker build environment for Ubuntu Artful
#  Using all stock Ubuntu Artful packaging except for:
#  - cpp: stock boost 1.62 in artful has a nasty bug so we use stock boost 1.63
#  - d: dmd does not come with Ubuntu
#  - dart: does not come with Ubuntu. Pinned to last 1.x release
#  - dotnet: does not come with Ubuntu
#  - haxe: version 3.4.2 that comes with Ubuntu cores in our CI build
#  - go: artful comes with 1.9, we want the latest (supported)
#  - nodejs: want v8, artful comes with v6
#
FROM buildpack-deps:artful-scm
MAINTAINER Apache Thrift <dev@thrift.apache.org>
ENV DEBIAN_FRONTEND="noninteractive"
# ## Add apt repos
RUN apt-get update \
 && apt-get dist-upgrade -y \
 && apt-get install --no-install-recommends apt apt-transport-https apt-utils curl dirmngr software-properties-common wget -y
#  csharp (mono) - if we ever want a later version
#  RUN echo "deb http://download.mono-project.com/repo/debian xenial main" | tee /etc/apt/sources.list.d/mono.list && \
#      apt-key adv --keyserver keyserver.ubuntu.com --recv-keys A6A19B38D3D831EF
#  Dart
RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list
ENV DART_VERSION="1.24.3-1"
#  dotnet (netcore)
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/microsoft.gpg \
 && echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-artful-prod artful main" > /etc/apt/sources.list.d/dotnetdev.list
#  haxe (https://haxe.org/download/linux/)
RUN add-apt-repository ppa:haxe/releases -y
#  node.js
RUN curl -sL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - \
 && echo "deb https://deb.nodesource.com/node_8.x artful main" | tee /etc/apt/sources.list.d/nodesource.list
# ## install general dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion bison build-essential clang cmake debhelper flex gdb llvm ninja-build pkg-config valgrind vim `` -y
ENV PATH="/usr/lib/llvm-3.8/bin:$PATH"
#  boost-1.62 has a terrible bug in boost::test, see https://svn.boost.org/trac10/ticket/12507
RUN apt-get install --no-install-recommends libboost1.63-all-dev libevent-dev libssl-dev qt5-default qtbase5-dev qtbase5-dev-tools `` -y
RUN apt-get install --no-install-recommends mono-devel `` -y
ENV SBCL_VERSION="1.4.5"
RUN `` curl --version \
 && curl -O -J -L https://kent.dl.sourceforge.net/project/sbcl/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2 \
 && tar xjf sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2 \
 && cd sbcl-${SBCL_VERSION}-x86-64-linux \
 && ./install.sh \
 && sbcl --version \
 && rm -rf sbcl*
ENV D_VERSION="2.080.0"
ENV DMD_DEB="dmd_2.080.0-0_amd64.deb"
RUN `` wget -q http://downloads.dlang.org/releases/2.x/${D_VERSION}/${DMD_DEB} \
 && dpkg --install ${DMD_DEB} \
 && rm -f ${DMD_DEB} \
 && mkdir -p /usr/include/dmd/druntime/import/deimos /usr/include/dmd/druntime/import/C \
 && curl -sSL https://github.com/D-Programming-Deimos/libevent/archive/master.tar.gz | tar xz \
 && mv libevent-master/deimos/* /usr/include/dmd/druntime/import/deimos/ \
 && mv libevent-master/C/* /usr/include/dmd/druntime/import/C/ \
 && rm -rf libevent-master \
 && curl -sSL https://github.com/D-Programming-Deimos/openssl/archive/master.tar.gz | tar xz \
 && mv openssl-master/deimos/* /usr/include/dmd/druntime/import/deimos/ \
 && mv openssl-master/C/* /usr/include/dmd/druntime/import/C/ \
 && rm -rf openssl-master
RUN apt-get install --no-install-recommends `` dart=$DART_VERSION -y
ENV PATH="/usr/lib/dart/bin:$PATH"
RUN apt-get install --no-install-recommends dotnet-sdk-2.1.4 `` -y
RUN apt-get install --no-install-recommends erlang-base erlang-eunit erlang-dev erlang-tools rebar `` -y
RUN apt-get install --no-install-recommends libglib2.0-dev `` -y
#  golang
ENV GOLANG_VERSION="1.10"
ENV GOLANG_DOWNLOAD_URL="https://golang.org/dl/go$GOLANG_VERSION.linux-amd64.tar.gz"
ENV GOLANG_DOWNLOAD_SHA256="b5a64335f1490277b585832d1f6c7f8c6c11206cba5cd3f771dcb87b98ad1a33"
RUN curl -fsSL "$GOLANG_DOWNLOAD_URL" -o golang.tar.gz \
 && echo "$GOLANG_DOWNLOAD_SHA256 golang.tar.gz" | sha256sum -c - \
 && tar -C /usr/local -xzf golang.tar.gz \
 && ln -s /usr/local/go/bin/go /usr/local/bin \
 && rm golang.tar.gz
RUN apt-get install --no-install-recommends ghc cabal-install `` -y
RUN apt-get install --no-install-recommends haxe neko neko-dev `` -y \
 && haxelib setup --always /usr/share/haxe/lib \
 && haxelib install --always hxcpp 2>&1 > /dev/null
RUN apt-get install --no-install-recommends ant ant-optional openjdk-8-jdk maven `` -y
RUN apt-get install --no-install-recommends lua5.2 lua5.2-dev `` -y
#  https://bugs.launchpad.net/ubuntu/+source/lua5.3/+bug/1707212
#  lua5.3 does not install alternatives!
#  need to update our luasocket code, lua doesn't have luaL_openlib any more
RUN apt-get install --no-install-recommends nodejs `` -y
#  Test dependencies for running puppeteer
RUN apt-get install --no-install-recommends libxss1 `` -y
RUN apt-get install --no-install-recommends ocaml opam `` -y \
 && opam init --yes \
 && opam install --yes oasis
RUN apt-get install --no-install-recommends libbit-vector-perl libclass-accessor-class-perl libcrypt-ssleay-perl libio-socket-ssl-perl libnet-ssleay-perl `` -y
RUN apt-get install --no-install-recommends php php-cli php-dev php-pear re2c composer `` -y
RUN apt-get install --no-install-recommends python-all python-all-dbg python-all-dev python-ipaddress python-pip python-setuptools python-six python-tornado python-twisted python-wheel python-zope.interface `` -y \
 && pip install backports.ssl_match_hostname --upgrade
RUN apt-get install --no-install-recommends python3-all python3-all-dbg python3-all-dev python3-pip python3-setuptools python3-six python3-tornado python3-twisted python3-wheel python3-zope.interface `` -y
RUN apt-get install --no-install-recommends ruby ruby-dev ruby-bundler `` -y
RUN apt-get install --no-install-recommends cargo rustc `` -y
RUN apt-get install --no-install-recommends cppcheck sloccount `` -y \
 && pip install flake8
#  Clean up
RUN rm -rf /var/cache/apt/* \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/* \
 && rm -rf /var/tmp/*
ENV THRIFT_ROOT="/thrift"
RUN mkdir -p $THRIFT_ROOT/src
COPY Dockerfile $THRIFT_ROOT/
WORKDIR $THRIFT_ROOT/src
