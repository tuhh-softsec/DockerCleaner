#   Copyright 2018 Intel Corporation
#
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
#   ------------------------------------------------------------------------------
#   -------------=== seth cli-go build ===-------------
FROM ubuntu:bionic
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg=2.2.4-1ubuntu1.6 -y
ENV GOPATH="/project/sawtooth-seth/cli-go"
ENV PATH="$PATH:/project/sawtooth-seth/cli-go/bin:/project/sawtooth-seth/bin:/usr/lib/go-1.11/bin"
RUN (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 44FC67F19B2466EA || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 44FC67F19B2466EA ) \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 308C15A29AD198E9 || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 308C15A29AD198E9 ) \
 && echo 'deb http://repo.sawtooth.me/ubuntu/nightly bionic universe' >> /etc/apt/sources.list \
 && echo 'deb http://ppa.launchpad.net/gophers/archive/ubuntu bionic main' >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 git=1:2.17.1-1ubuntu0.17 golang-1.11-go libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libzmq3-dev=4.2.5-1ubuntu0.2 openssl=1.1.1-1ubuntu2.1~18.04.21 python3=3.6.7-1~18.04 python3-grpcio-tools python3-sawtooth-cli software-properties-common=0.96.24.32.20 -y -q \
 && add-apt-repository -k hkp://p80.pool.sks-keyservers.net:80 ppa:ethereum/ethereum \
 && apt-get update \
 && apt-get install --no-install-recommends solc -y -q \
 && curl -s -S -o /tmp/setup-node.sh https://deb.nodesource.com/setup_6.x \
 && chmod 755 /tmp/setup-node.sh \
 && /tmp/setup-node.sh \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -y -q \
 && rm /tmp/setup-node.sh \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN go get -u github.com/btcsuite/btcd/btcec github.com/golang/mock/gomock github.com/golang/mock/mockgen github.com/golang/protobuf/proto github.com/golang/protobuf/protoc-gen-go github.com/jessevdk/go-flags github.com/pebbe/zmq4 github.com/pelletier/go-toml github.com/satori/go.uuid golang.org/x/crypto/ssh/terminal
RUN git clone https://github.com/knkski/burrow.git $GOPATH/src/github.com/hyperledger/burrow
RUN go get github.com/hyperledger/sawtooth-sdk-go \
 && cd $GOPATH/src/github.com/hyperledger/sawtooth-sdk-go \
 && go generate
COPY . /project/sawtooth-seth
RUN seth-protogen go
WORKDIR $GOPATH/src/seth_cli/cli
ENV GOPATH="$GOPATH:/project/sawtooth-seth/burrow:/project/sawtooth-seth/common"
RUN go build -o /project/sawtooth-seth/cli-go/bin/seth
WORKDIR /project/sawtooth-seth/cli
RUN pkg_dir=/project/build/debs/ \
 && CHANGELOG_DIR="debian/usr/share/doc/sawtooth-seth" \
 && if [ -d "debian" ] ; then rm -rf debian ; fi \
 && mkdir -p $pkg_dir \
 && mkdir -p debian/DEBIAN \
 && mkdir -p $CHANGELOG_DIR \
 && cp packaging/ubuntu/* debian \
 && cp debian/changelog $CHANGELOG_DIR \
 && mv debian/changelog $CHANGELOG_DIR/changelog.Debian \
 && gzip --best $CHANGELOG_DIR/changelog \
 && gzip --best $CHANGELOG_DIR/changelog.Debian \
 && mv debian/control debian/DEBIAN \
 && PACKAGENAME=$( awk '/^Package:/ { print $2 }' debian/DEBIAN/control ;) \
 && PACKAGEVERSION=$( dpkg-parsechangelog -S version -l $CHANGELOG_DIR/changelog.gz ;) \
 && PACKAGEARCH=$( dpkg-architecture -qDEB_BUILD_ARCH ;) \
 && mkdir debian/usr/bin \
 && cp -R /project/sawtooth-seth/cli-go/bin/seth debian/usr/bin \
 && fakeroot dpkg-deb --build debian \
 && mv debian.deb $pkg_dir/"${PACKAGENAME}_${PACKAGEVERSION}_${PACKAGEARCH}.deb"
#   -------------=== seth cli-go docker build ===-------------
FROM ubuntu:bionic
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg=2.2.4-1ubuntu1.6 -y
RUN mkdir /debs
COPY --from=0 /project/build/debs/sawtooth-seth-cli_*amd64.deb /debs
RUN (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 44FC67F19B2466EA || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 44FC67F19B2466EA ) \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 308C15A29AD198E9 || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 308C15A29AD198E9 ) \
 && echo 'deb http://repo.sawtooth.me/ubuntu/nightly bionic universe' >> /etc/apt/sources.list \
 && echo 'deb http://ppa.launchpad.net/gophers/archive/ubuntu bionic main' >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 dpkg-dev=1.19.0.5ubuntu2.4 git=1:2.17.1-1ubuntu0.17 golang-1.11-go openssl=1.1.1-1ubuntu2.1~18.04.21 python3-sawtooth-cli software-properties-common=0.96.24.32.20 -y -q \
 && add-apt-repository -k hkp://p80.pool.sks-keyservers.net:80 ppa:ethereum/ethereum \
 && apt-get update \
 && apt-get install --no-install-recommends solc -y -q \
 && curl -s -S -o /tmp/setup-node.sh https://deb.nodesource.com/setup_6.x \
 && chmod 755 /tmp/setup-node.sh \
 && /tmp/setup-node.sh \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 npm=3.5.2-0ubuntu4 -y -q \
 && rm /tmp/setup-node.sh \
 && npm install ethereumjs-abi@0.6.8 web3@1.9.0
RUN apt-get update \
 && dpkg -i /debs/sawtooth-seth-cli_*amd64.deb || true \
 && apt-get install --no-install-recommends -f -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
