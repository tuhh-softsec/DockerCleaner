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
#   Apache Thrift Docker build environment for Ubuntu Trusty
#   Using all stock Ubuntu Trusty packaging except for:
#   - d: does not come with Ubuntu so we're installing 2.070.0
#   - dart: does not come with Ubuntu so we're installing 1.20.1
#   - dotnetcore, disabled because netcore is for 1.0.0-preview and 2.0.0 is out
#   - haxe, disabled because the distro comes with 3.0.0 and it cores while installing
#   - node.js, disabled because it is at 0.10.0 in the distro which is too old (need 4+)
#   - ocaml, disabled because it fails to install properly
#
FROM buildpack-deps:trusty-scm
MAINTAINER Apache Thrift <dev@thrift.apache.org>
ENV DEBIAN_FRONTEND="noninteractive"
#  ## Add apt repos
RUN apt-get update \
 && apt-get dist-upgrade -y \
 && apt-get install --no-install-recommends apt apt-transport-https apt-utils curl dirmngr software-properties-common wget -y
#   D
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EBCF975E5BA24D5E \
 && wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list \
 && wget -qO - https://dlang.org/d-keyring.gpg | apt-key add -
#   Dart
RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion bison build-essential clang cmake debhelper flex gdb llvm ninja-build pkg-config valgrind vim `` -y
ENV PATH="/usr/lib/llvm-3.8/bin:$PATH"
RUN apt-get install --no-install-recommends libboost-all-dev libevent-dev libssl-dev qt5-default qtbase5-dev qtbase5-dev-tools `` -y
RUN apt-get install --no-install-recommends mono-devel `` -y
RUN apt-get install --no-install-recommends dmd-bin=2.070.2-0 libphobos2-dev=2.070.2-0 dub dfmt dscanner xdg-utils `` -y
RUN mkdir -p /usr/include/dmd/druntime/import/deimos /usr/include/dmd/druntime/import/C \
 && curl -sSL https://github.com/D-Programming-Deimos/libevent/archive/master.tar.gz | tar xz \
 && mv libevent-master/deimos/* /usr/include/dmd/druntime/import/deimos/ \
 && mv libevent-master/C/* /usr/include/dmd/druntime/import/C/ \
 && rm -rf libevent-master
RUN curl -sSL https://github.com/D-Programming-Deimos/openssl/archive/master.tar.gz | tar xz \
 && mv openssl-master/deimos/* /usr/include/dmd/druntime/import/deimos/ \
 && mv openssl-master/C/* /usr/include/dmd/druntime/import/C/ \
 && rm -rf openssl-master
RUN apt-get install --no-install-recommends dart=1.20.1-1 `` -y
ENV PATH="/usr/lib/dart/bin:$PATH"
RUN apt-get install --no-install-recommends erlang-base erlang-eunit erlang-dev erlang-tools rebar `` -y
RUN apt-get install --no-install-recommends libglib2.0-dev `` -y
#   golang
ENV GOLANG_VERSION="1.7.6"
ENV GOLANG_DOWNLOAD_URL="https://golang.org/dl/go$GOLANG_VERSION.linux-amd64.tar.gz"
ENV GOLANG_DOWNLOAD_SHA256="ad5808bf42b014c22dd7646458f631385003049ded0bb6af2efc7f1f79fa29ea"
RUN curl -fsSL "$GOLANG_DOWNLOAD_URL" -o golang.tar.gz \
 && echo "$GOLANG_DOWNLOAD_SHA256 golang.tar.gz" | sha256sum -c - \
 && tar -C /usr/local -xzf golang.tar.gz \
 && ln -s /usr/local/go/bin/go /usr/local/bin \
 && rm golang.tar.gz
RUN apt-get install --no-install-recommends ghc cabal-install `` -y
#   disabled because it cores while installing
#   RUN apt-get install -y --no-install-recommends \
#   `# Haxe dependencies` \
#         haxe \
#         neko \
#         neko-dev && \
#       haxelib setup /usr/share/haxe/lib && \
#       haxelib install hxcpp 3.2.102
RUN apt-get install --no-install-recommends ant ant-optional openjdk-7-jdk maven `` -y
RUN apt-get install --no-install-recommends lua5.1 lua5.1-dev `` -y
#   disabled because it is too old
#   RUN apt-get install -y --no-install-recommends \
#   `# Node.js dependencies` \
#         nodejs \
#         npm
#   disabled because it fails to install properly
#   RUN apt-get install -y --no-install-recommends \
#   `# OCaml dependencies` \
#         ocaml \
#         opam && \
#       opam init --yes && \
#       opam install --yes oasis
RUN apt-get install --no-install-recommends libbit-vector-perl libclass-accessor-class-perl libcrypt-ssleay-perl libio-socket-ssl-perl libnet-ssleay-perl `` -y
RUN apt-get install --no-install-recommends php5 php5-cli php5-dev php-pear re2c `` -y \
 && wget https://getcomposer.org/installer -O - -q | php -- --quiet --install-dir=/usr/local/bin/ --filename=composer
RUN apt-get install --no-install-recommends python-all python-all-dbg python-all-dev python-pip python-setuptools python-six python-twisted python-wheel python-zope.interface python3-all python3-all-dbg python3-all-dev python3-pip python3-setuptools python3-six python3-wheel python3-zope.interface `` -y \
 && pip install ipaddress==1.0.23 backports.ssl_match_hostname==3.7.0.1 tornado==6.2 -U \
 && pip3 install -U backports.ssl_match_hostname tornado
#   installing tornado by pip/pip3 instead of debian package
#   if we install the debian package, the build fails in py2
RUN apt-get install --no-install-recommends ruby ruby-dev ruby-bundler `` -y
RUN gem install bundler --version 2.4.12 --no-ri --no-rdoc
RUN apt-get install --no-install-recommends cargo rustc `` -y
RUN apt-get install --no-install-recommends cppcheck sloccount `` -y \
 && pip install flake8==6.0.0
#   Install BouncyCastle provider to fix Java builds issues with JDK 7
#   Builds accessing repote repositories fail as seen here: https://github.com/travis-ci/travis-ci/issues/8503
RUN apt-get install --no-install-recommends libbcprov-java `` -y \
 && ln -s /usr/share/java/bcprov.jar /usr/lib/jvm/java-7-openjdk-amd64/jre/lib/ext/bcprov.jar \
 && awk -F . -v OFS=. 'BEGIN{n=2}/^security\.provider/ {split($3, posAndEquals, "=");$3=n++"="posAndEquals[2];print;next} 1' /etc/java-7-openjdk/security/java.security > /tmp/java.security \
 && echo "security.provider.1=org.bouncycastle.jce.provider.BouncyCastleProvider" >> /tmp/java.security \
 && mv /tmp/java.security /etc/java-7-openjdk/security/java.security
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
