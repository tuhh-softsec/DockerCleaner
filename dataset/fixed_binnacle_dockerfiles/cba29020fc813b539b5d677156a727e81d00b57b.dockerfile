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
#   Apache Thrift Docker build environment for Ubuntu Artful
#   Using all stock Ubuntu Artful packaging except for:
#   - cpp: stock boost 1.62 in artful has a nasty bug so we use stock boost 1.63
#   - d: does not come with Ubuntu so we're installing the latest
#   - d: deimos for libevent and openssl omitted - not compatible / build errors
#   - haxe: see THRIFT-4352, but test/haxe cores during testing
#           and hxcpp 3.4.64 is not compatible with artful
#
FROM buildpack-deps:artful-scm
MAINTAINER Apache Thrift <dev@thrift.apache.org>
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get dist-upgrade -y \
 && apt-get install --no-install-recommends apt apt-transport-https apt-utils curl dirmngr software-properties-common wget -y
#   csharp (mono) - if we ever want a later version
#   RUN echo "deb http://download.mono-project.com/repo/debian xenial main" | tee /etc/apt/sources.list.d/mono.list && \
#       apt-key adv --keyserver keyserver.ubuntu.com --recv-keys A6A19B38D3D831EF
#   dotnet (core)
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/microsoft.gpg \
 && echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-artful-prod artful main" > /etc/apt/sources.list.d/dotnetdev.list
#   node.js (this step runs apt-get update internally) - if we ever want a later version
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
#  ## install general dependencies
RUN apt-get install --no-install-recommends bash-completion bison build-essential clang cmake debhelper flex gdb llvm ninja-build pkg-config valgrind vim `` -y
ENV PATH="/usr/lib/llvm-3.8/bin:$PATH"
#   boost-1.62 has a terrible bug in boost::test, see https://svn.boost.org/trac10/ticket/12507
RUN apt-get install --no-install-recommends libboost1.63-all-dev libevent-dev libssl-dev qt5-default qtbase5-dev qtbase5-dev-tools `` -y
RUN apt-get install --no-install-recommends mono-devel `` -y
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EBCF975E5BA24D5E \
 && wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list \
 && wget -qO - https://dlang.org/d-keyring.gpg | apt-key add - \
 && apt-get update \
 && apt-get install --no-install-recommends dmd-bin libphobos2-dev dub dfmt dscanner libevent-dev libssl-dev xdg-utils `` -y
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
#   dart cannot be downloaded by aptitude because of
#   https://github.com/dart-lang/sdk/issues/30512
#   RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - && \
#       curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list && \
#       apt-get update && \
#       apt-get install -y --no-install-recommends \
#         `# Dart dependencies` \
#         dart
#   so instead we do:
RUN wget https://storage.googleapis.com/dart-archive/channels/stable/release/latest/linux_packages/dart_1.24.2-1_amd64.deb \
 && dpkg -i dart_1.24.2-1_amd64.deb \
 && rm dart_1.24.2-1_amd64.deb
ENV PATH="/usr/lib/dart/bin:$PATH"
RUN apt-get install --no-install-recommends dotnet-sdk-2.0.3 `` -y
RUN apt-get install --no-install-recommends erlang-base erlang-eunit erlang-dev erlang-tools rebar `` -y
RUN apt-get install --no-install-recommends libglib2.0-dev `` -y
RUN apt-get install --no-install-recommends golang-go golang-race-detector-runtime `` -y
RUN apt-get install --no-install-recommends ghc cabal-install `` -y
#   see THRIFT-4352, test/haxe cores on artful
#   RUN apt-get install -y --no-install-recommends \
#   `# Haxe dependencies` \
#         haxe \
#         neko \
#         neko-dev
#   RUN haxelib setup --always /usr/share/haxe/lib && \
#       haxelib install --always hxcpp
RUN apt-get install --no-install-recommends ant ant-optional openjdk-8-jdk maven `` -y
RUN apt-get install --no-install-recommends lua5.2 lua5.2-dev `` -y
#   https://bugs.launchpad.net/ubuntu/+source/lua5.3/+bug/1707212
#   lua5.3 does not install alternatives!
#   need to update our luasocket code, lua doesn't have luaL_openlib any more
RUN apt-get install --no-install-recommends nodejs `` -y
RUN apt-get install --no-install-recommends ocaml opam `` -y \
 && opam init --yes \
 && opam install --yes oasis
RUN apt-get install --no-install-recommends libbit-vector-perl libclass-accessor-class-perl libcrypt-ssleay-perl libio-socket-ssl-perl libnet-ssleay-perl `` -y
RUN apt-get install --no-install-recommends php php-cli php-dev php-pear re2c phpunit `` -y
RUN apt-get install --no-install-recommends python-all python-all-dbg python-all-dev python-ipaddress python-pip python-setuptools python-six python-tornado python-twisted python-wheel python-zope.interface `` -y \
 && pip install backports.ssl_match_hostname==3.7.0.1 --upgrade
RUN apt-get install --no-install-recommends python3-all python3-all-dbg python3-all-dev python3-pip python3-setuptools python3-six python3-tornado python3-twisted python3-wheel python3-zope.interface `` -y
RUN apt-get install --no-install-recommends ruby ruby-dev ruby-bundler `` -y
RUN gem install bundler --version 2.4.12 --no-ri --no-rdoc
RUN apt-get install --no-install-recommends cargo rustc `` -y
RUN apt-get install --no-install-recommends cppcheck sloccount `` -y \
 && pip install flake8==6.0.0
#   Clean up
RUN rm -rf /var/cache/apt/* \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/* \
 && rm -rf /var/tmp/*
ENV THRIFT_ROOT="/thrift"
RUN mkdir -p $THRIFT_ROOT/src
COPY Dockerfile $THRIFT_ROOT/
WORKDIR $THRIFT_ROOT/src
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
