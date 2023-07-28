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
#   Apache Thrift Docker build environment for Debian Stretch
#
#   Known issues:
#   - d: deimos for libevent and openssl disabled - build errors
#   - dotnetcore, because netcore is for 1.0.0-preview and 2.0.0 is out
#   - rust: cargo not in debian repo - perhaps not needed?
FROM buildpack-deps:stretch-scm
MAINTAINER Apache Thrift <dev@thrift.apache.org>
ENV DEBIAN_FRONTEND="noninteractive"
#  ## Add apt repos
RUN apt-get update \
 && apt-get install --no-install-recommends apt apt-transport-https curl wget apt-utils -y
#   D
RUN wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list \
 && apt-get update \
 && apt-get install --no-install-recommends d-apt-keyring -y --allow-unauthenticated --reinstall
#   Dart
RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list \
 && sed -i /etc/apt/sources.list.d/dart_stable.list -e 's/https:/http:/g'
#   dotnet (core) 2.0.0 - project isn't ready for this yet:
#   RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/microsoft.gpg && \
#       echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod xenial main" > /etc/apt/sources.list.d/dotnetdev.list
#   node.js (this step runs apt-get update internally)
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash
#  ## install general dependencies
RUN apt-get install --no-install-recommends bash-completion bison build-essential clang cmake debhelper flex gdb ninja-build pkg-config valgrind vim `` -y
#  ## languages
RUN apt-get install --no-install-recommends libboost-dev libboost-filesystem-dev libboost-program-options-dev libboost-system-dev libboost-test-dev libboost-thread-dev libevent-dev libssl-dev qt5-default qtbase5-dev qtbase5-dev-tools `` -y
RUN apt-get install --no-install-recommends mono-devel `` -y
RUN apt-get install --no-install-recommends dmd-bin libevent-dev libssl-dev xdg-utils `` -y
#   libevent deimos disabled - build errors
#   RUN mkdir -p /usr/include/dmd/druntime/import/deimos /usr/include/dmd/druntime/import/C && \
#       curl -sSL https://github.com/D-Programming-Deimos/libevent/archive/master.tar.gz| tar xz && \
#       mv libevent-master/deimos/* /usr/include/dmd/druntime/import/deimos/ && \
#       mv libevent-master/C/* /usr/include/dmd/druntime/import/C/ && \
#       rm -rf libevent-master
#   openssl deimos doesn't work with openssl-1.1.0 - disabling it for now:
#   RUN curl -sSL https://github.com/D-Programming-Deimos/openssl/archive/master.tar.gz| tar xz && \
#       mv openssl-master/deimos/* /usr/include/dmd/druntime/import/deimos/ && \
#       mv openssl-master/C/* /usr/include/dmd/druntime/import/C/ && \
#       rm -rf openssl-master
RUN apt-get install --no-install-recommends dart `` -y
ENV PATH="/usr/lib/dart/bin:$PATH"
#   project isn't ready for this quite yet:
#   RUN apt-get install -y --no-install-recommends \
#   `# dotnet core dependencies` \
#         dotnet-sdk-2.0.0
RUN apt-get install --no-install-recommends erlang-base erlang-eunit erlang-dev erlang-tools rebar `` -y
RUN apt-get install --no-install-recommends libglib2.0-dev `` -y
RUN apt-get install --no-install-recommends golang-go `` -y
RUN apt-get install --no-install-recommends ghc cabal-install `` -y
RUN apt-get install --no-install-recommends haxe neko neko-dev `` -y
RUN haxelib setup --always /usr/share/haxe/lib \
 && haxelib install --always hxcpp
RUN apt-get install --no-install-recommends ant ant-optional openjdk-8-jdk maven `` -y
RUN apt-get install --no-install-recommends lua5.2 lua5.2-dev `` -y
#   https://bugs.launchpad.net/ubuntu/+source/lua5.3/+bug/1707212
#   same for debian stretch
#   lua5.3 does not install alternatives so stick with 5.2 here
RUN apt-get install --no-install-recommends nodejs `` -y
RUN apt-get install --no-install-recommends ocaml opam `` -y \
 && opam init --yes \
 && opam install --yes oasis
RUN apt-get install --no-install-recommends libbit-vector-perl libclass-accessor-class-perl libcrypt-ssleay-perl libio-socket-ssl-perl libnet-ssleay-perl `` -y
RUN apt-get install --no-install-recommends php7.0 php7.0-cli php7.0-dev php-pear re2c phpunit `` -y
RUN apt-get install --no-install-recommends python-all python-all-dbg python-all-dev python-backports.ssl-match-hostname python-ipaddress python-pip python-setuptools python-six python-tornado python-twisted python-wheel python-zope.interface python3-all python3-all-dbg python3-all-dev python3-setuptools python3-six python3-tornado python3-twisted python3-wheel python3-zope.interface `` -y \
 && pip install backports.ssl_match_hostname==3.7.0.1 --upgrade
RUN apt-get install --no-install-recommends ruby ruby-dev ruby-bundler `` -y
RUN gem install bundler --version 2.4.12 --no-ri --no-rdoc
RUN apt-get install --no-install-recommends rustc `` -y
#   Update anything else left hanging
RUN apt-get dist-upgrade -y
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
