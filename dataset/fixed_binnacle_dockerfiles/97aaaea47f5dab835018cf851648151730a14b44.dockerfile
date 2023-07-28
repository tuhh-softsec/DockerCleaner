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
#   - d: does not come with Ubuntu so we're installing 2.075.1 for coverage
#   - dart: does not come with Ubuntu so we're installing 1.24.3 for coverage
#   - dotnet: does not come with Ubuntu
#   - go: Xenial comes with 1.6, but we need 1.10 or later
#   - nodejs: Xenial comes with 4.2.6 which exits LTS April 2018, so we're installing 6.x
#   - ocaml: causes stack overflow error, just started March 2018 not sure why
#
FROM buildpack-deps:xenial-scm
MAINTAINER Apache Thrift <dev@thrift.apache.org>
ENV DEBIAN_FRONTEND="noninteractive"
#  ## Add apt repos
RUN apt-get update \
 && apt-get dist-upgrade -y \
 && apt-get install --no-install-recommends apt=1.2.35 apt-transport-https=1.2.35 apt-utils=1.2.35 curl=7.47.0-1ubuntu2.19 software-properties-common=0.96.20.10 wget=1.17.1-1ubuntu1.5 -y
#   csharp (mono)
#   RUN echo "deb http://download.mono-project.com/repo/debian xenial main" | tee /etc/apt/sources.list.d/mono.list && \
#       apt-key adv --keyserver keyserver.ubuntu.com --recv-keys A6A19B38D3D831EF
#   D
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EBCF975E5BA24D5E \
 && wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list \
 && wget -qO - https://dlang.org/d-keyring.gpg | apt-key add -
#   Dart
RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list
#   dotnet (core)
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/microsoft.gpg \
 && echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod xenial main" > /etc/apt/sources.list.d/dotnetdev.list
#   node.js
RUN curl -sL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - \
 && echo "deb https://deb.nodesource.com/node_6.x xenial main" | tee /etc/apt/sources.list.d/nodesource.list
#  ## install general dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion=1:2.1-4.2ubuntu1.1 bison=2:3.0.4.dfsg-1 build-essential=12.1ubuntu2 clang=1:3.8-33ubuntu3.1 cmake=3.5.1-1ubuntu3 debhelper=9.20160115ubuntu3 flex=2.6.0-11 gdb=7.11.1-0ubuntu1~16.5 llvm=1:3.8-33ubuntu3.1 ninja-build=1.5.1-0.1ubuntu1 pkg-config=0.29.1-0ubuntu1 valgrind=1:3.11.0-1ubuntu4.2 vim=2:7.4.1689-3ubuntu1.5 `` -y
ENV PATH="/usr/lib/llvm-3.8/bin:$PATH"
#  ## languages
RUN apt-get install --no-install-recommends libboost-dev=1.58.0.1ubuntu1 libboost-filesystem-dev=1.58.0.1ubuntu1 libboost-program-options-dev=1.58.0.1ubuntu1 libboost-system-dev=1.58.0.1ubuntu1 libboost-test-dev=1.58.0.1ubuntu1 libboost-thread-dev=1.58.0.1ubuntu1 libevent-dev=2.0.21-stable-2ubuntu0.16.04.1 libssl-dev=1.0.2g-1ubuntu4.20 qt5-default=5.5.1+dfsg-16ubuntu7.7 qtbase5-dev=5.5.1+dfsg-16ubuntu7.7 qtbase5-dev-tools=5.5.1+dfsg-16ubuntu7.7 `` -y
RUN apt-get install --no-install-recommends mono-devel=4.2.1.102+dfsg2-7ubuntu4 `` -y
ENV D_VERSION="2.075.1-0"
RUN apt-get install --no-install-recommends dub=1.6.0-0 dfmt dscanner libevent-dev=2.0.21-stable-2ubuntu0.16.04.1 libssl-dev=1.0.2g-1ubuntu4.20 xdg-utils=1.1.1-1ubuntu1.16.04.5 `` dmd-bin=$D_VERSION libphobos2-dev=$D_VERSION -y --allow-unauthenticated
RUN mkdir -p /usr/include/dmd/druntime/import/deimos /usr/include/dmd/druntime/import/C \
 && curl -sSL https://github.com/D-Programming-Deimos/libevent/archive/master.tar.gz | tar xz \
 && mv libevent-master/deimos/* /usr/include/dmd/druntime/import/deimos/ \
 && mv libevent-master/C/* /usr/include/dmd/druntime/import/C/ \
 && rm -rf libevent-master
RUN curl -sSL https://github.com/D-Programming-Deimos/openssl/archive/v1.1.6+1.0.1g.tar.gz | tar xz \
 && mv openssl-1.1.6-1.0.1g/deimos/* /usr/include/dmd/druntime/import/deimos/ \
 && mv openssl-1.1.6-1.0.1g/C/* /usr/include/dmd/druntime/import/C/ \
 && rm -rf openssl-1.1.6-1.0.1g
ENV DART_VERSION="1.24.3-1"
RUN apt-get install --no-install-recommends `` dart=$DART_VERSION -y
ENV PATH="/usr/lib/dart/bin:$PATH"
RUN apt-get install --no-install-recommends dotnet-sdk-2.1 `` -y
RUN apt-get install --no-install-recommends erlang-base=1:18.3-dfsg-1ubuntu3.1 erlang-eunit=1:18.3-dfsg-1ubuntu3.1 erlang-dev=1:18.3-dfsg-1ubuntu3.1 erlang-tools=1:18.3-dfsg-1ubuntu3.1 rebar=2.6.0-2 `` -y
RUN apt-get install --no-install-recommends libglib2.0-dev=2.48.2-0ubuntu4.8 `` -y
#   golang
ENV GOLANG_VERSION="1.10.8"
ENV GOLANG_DOWNLOAD_URL="https://golang.org/dl/go$GOLANG_VERSION.linux-amd64.tar.gz"
ENV GOLANG_DOWNLOAD_SHA256="d8626fb6f9a3ab397d88c483b576be41fa81eefcec2fd18562c87626dbb3c39e"
RUN curl -fsSL "$GOLANG_DOWNLOAD_URL" -o golang.tar.gz \
 && echo "$GOLANG_DOWNLOAD_SHA256 golang.tar.gz" | sha256sum -c - \
 && tar -C /usr/local -xzf golang.tar.gz \
 && ln -s /usr/local/go/bin/go /usr/local/bin \
 && rm golang.tar.gz
#   due to a bug in cabal in xenial (cabal-install package) we pull in another:
RUN apt-get install --no-install-recommends ghc=7.10.3-7 `` -y \
 && cd /tmp \
 && wget -q https://www.haskell.org/cabal/release/cabal-install-1.24.0.2/cabal-install-1.24.0.2-x86_64-unknown-linux.tar.gz \
 && tar xzf cabal-install-1.24.0.2-x86_64-unknown-linux.tar.gz \
 && find dist-newstyle/ -type f -name cabal -exec mv {} /usr/bin
RUN apt-get install --no-install-recommends haxe=1:3.2.1+dfsg-1build1 neko=2.0.0-4build1 neko-dev=2.0.0-4build1 libneko0=2.0.0-4build1 `` -y \
 && haxelib setup --always /usr/share/haxe/lib \
 && haxelib install --always hxcpp 3.4.64 2>&1 > /dev/null
#   note: hxcpp 3.4.185 (latest) no longer ships static libraries, and caused a build failure
RUN apt-get install --no-install-recommends ant=1.9.6-1ubuntu1.1 ant-optional=1.9.6-1ubuntu1.1 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 maven=3.3.9-3 `` -y
RUN apt-get install --no-install-recommends lua5.2=5.2.4-1ubuntu1 lua5.2-dev `` -y
#   https://bugs.launchpad.net/ubuntu/+source/lua5.3/+bug/1707212
#   lua5.3 does not install alternatives so stick with 5.2 here
RUN apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 `` -y
#   Test dependencies for running puppeteer
RUN apt-get install --no-install-recommends libxss1=1:1.2.2-1 libatk-bridge2.0-0=2.18.1-2ubuntu1 libgtk-3-0=3.18.9-1ubuntu3.3 `` -y
#   THRIFT-4517: causes stack overflows; version too old; skip ocaml in xenial
#   RUN apt-get install -y --no-install-recommends \
#   `# OCaml dependencies` \
#         ocaml \
#         opam && \
#       opam init --yes && \
#       opam install --yes oasis
RUN apt-get install --no-install-recommends libbit-vector-perl=7.4-1build1 libclass-accessor-class-perl=0.503-2 libcrypt-ssleay-perl=0.73.04-1build1 libio-socket-ssl-perl=2.024-1 libnet-ssleay-perl=1.72-1build1 `` -y
RUN apt-get install --no-install-recommends php7.0=7.0.33-0ubuntu0.16.04.16 php7.0-cli=7.0.33-0ubuntu0.16.04.16 php7.0-dev=7.0.33-0ubuntu0.16.04.16 php-json=1:7.0+35ubuntu6.1 php-pear=1:1.10.1+submodules+notgz-6ubuntu0.3 re2c=0.16-1 composer=1.0.0~beta2-1 `` -y
RUN apt-get install --no-install-recommends python-all=2.7.12-1~16.04 python-all-dbg=2.7.12-1~16.04 python-all-dev=2.7.12-1~16.04 python-backports.ssl-match-hostname=3.4.0.2-1 python-ipaddress=1.0.16-1 python-pip=8.1.1-2ubuntu0.6 python-setuptools=20.7.0-1 python-six=1.10.0-3 python-tornado=4.2.1-1ubuntu3.1 python-twisted=16.0.0-1ubuntu0.4 python-wheel=0.29.0-1 python-zope.interface=4.1.3-1build1 python3-all=3.5.1-3 python3-all-dbg=3.5.1-3 python3-all-dev=3.5.1-3 python3-setuptools=20.7.0-1 python3-six=1.10.0-3 python3-tornado=4.2.1-1ubuntu3.1 python3-twisted=16.0.0-1ubuntu0.4 python3-wheel=0.29.0-1 python3-zope.interface=4.1.3-1build1 `` -y \
 && pip install backports.ssl_match_hostname==3.7.0.1 --upgrade
RUN apt-get install --no-install-recommends ruby=1:2.3.0+1 ruby-dev=1:2.3.0+1 ruby-bundler=1.11.2-1 `` -y
#   Rust dependencies
RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain stable -y
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
