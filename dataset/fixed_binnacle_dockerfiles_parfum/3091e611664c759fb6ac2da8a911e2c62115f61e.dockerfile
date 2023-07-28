#  Copyright 2018 Cargill Incorporated
#
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
#  docker build -f examples/smallbank/smallbank_go/Dockerfile-installed-bionic -t sawtooth-smallbank-tp-go .
#  -------------=== smallbank-tp-go build ===-------------
FROM ubuntu:bionic AS smallbank-tp-go-builder
ENV VERSION="AUTO_STRICT"
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg -y
RUN echo "deb [arch=amd64] http://repo.sawtooth.me/ubuntu/ci bionic universe" >> /etc/apt/sources.list \
 && echo "deb http://archive.ubuntu.com/ubuntu bionic-backports universe" >> /etc/apt/sources.list \
 && echo 'deb http://ppa.launchpad.net/gophers/archive/ubuntu bionic main' >> /etc/apt/sources.list \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 8AA7AF1F1091A5FD || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 8AA7AF1F1091A5FD ) \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 308C15A29AD198E9 || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 308C15A29AD198E9 ) \
 && apt-get update \
 && apt-get install --no-install-recommends build-essential golang-1.11-go git libssl-dev libzmq3-dev openssl python3-grpcio-tools -y -q \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ENV GOPATH="/go:/project/:/go/src/github.com/hyperledger/sawtooth-sdk-go:/go/src/github.com/hyperledger/sawtooth-sdk-go/examples/smallbank/smallbank_go/"
ENV PATH="$PATH:/project/bin:/go/bin:/usr/lib/go-1.11/bin"
RUN mkdir /go
RUN go get -u github.com/golang/protobuf/proto github.com/golang/protobuf/protoc-gen-go github.com/pebbe/zmq4 github.com/brianolson/cbor_go github.com/satori/go.uuid github.com/btcsuite/btcd/btcec github.com/jessevdk/go-flags github.com/pelletier/go-toml github.com/golang/mock/gomock github.com/golang/mock/mockgen golang.org/x/crypto/ripemd160 golang.org/x/crypto/ssh
COPY . /go/src/github.com/hyperledger/sawtooth-sdk-go
WORKDIR /go/src/github.com/hyperledger/sawtooth-sdk-go
RUN go generate \
 && cd examples/smallbank/smallbank_go \
 && if [ -d "bin" ] ; then rm -rf bin ; fi \
 && mkdir bin \
 && cd src/sawtooth_smallbank \
 && go build -o /go/src/github.com/hyperledger/sawtooth-sdk-go/examples/smallbank/smallbank_go/bin/smallbank-tp-go
RUN pkg=smallbank \
 && GO_TP_DASH=$( echo $pkg | sed s/_/-/ ;) \
 && CHANGELOG_DIR="debian/usr/share/doc/sawtooth-tp-$GO_TP_DASH" \
 && ST_VERSION=$( bin/get_version ;) \
 && cd examples/smallbank/smallbank_go \
 && if [ -d "debian" ] ; then rm -rf debian ; fi \
 && mkdir -p debian/DEBIAN \
 && mkdir -p $CHANGELOG_DIR \
 && cp packaging/ubuntu/* debian \
 && sed -i -e"s/@VERSION@/$ST_VERSION/" debian/control \
 && sed -i -e"s/@VERSION@/$ST_VERSION/" debian/changelog \
 && cp debian/changelog $CHANGELOG_DIR \
 && mv debian/changelog $CHANGELOG_DIR/changelog.Debian \
 && gzip --best $CHANGELOG_DIR/changelog \
 && gzip --best $CHANGELOG_DIR/changelog.Debian \
 && mv debian/control debian/DEBIAN \
 && mv debian/postinst debian/DEBIAN \
 && PACKAGENAME=$( awk '/^Package:/ { print $2 }' debian/DEBIAN/control ;) \
 && PACKAGEVERSION=$( dpkg-parsechangelog -S version -l $CHANGELOG_DIR/changelog.gz ;) \
 && PACKAGEARCH=$( dpkg-architecture -qDEB_BUILD_ARCH ;) \
 && mkdir debian/usr/bin \
 && cp -R bin/ debian/usr/ \
 && cp -R packaging/systemd/* debian/ \
 && fakeroot dpkg-deb --build debian \
 && echo -- \
 && echo "${PACKAGENAME}_${PACKAGEVERSION}_${PACKAGEARCH}.deb" \
 && echo -- \
 && mv debian.deb "${PACKAGENAME}_${PACKAGEVERSION}_${PACKAGEARCH}.deb"
#  -------------=== sawtooth-smallbank-tp-go build ===-------------
FROM ubuntu:bionic
RUN apt-get update \
 && apt-get install --no-install-recommends systemd -y
COPY --from=smallbank-tp-go-builder /go/src/github.com/hyperledger/sawtooth-sdk-go/examples/smallbank/smallbank_go/sawtooth-smallbank-tp-go_*.deb /tmp
RUN apt-get update \
 && dpkg -i /tmp/sawtooth-*.deb || true \
 && apt-get install -f -y
CMD ["smallbank-tp-go", "-vv"]
