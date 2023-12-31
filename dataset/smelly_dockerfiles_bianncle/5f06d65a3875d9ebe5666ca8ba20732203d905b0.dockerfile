#  Copyright 2018 Intel Corporation
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
#  ------------------------------------------------------------------------------
#  -------------=== seth rpc build ===-------------
FROM ubuntu:bionic
RUN apt-get update \
 && apt-get install gnupg -y
RUN (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 44FC67F19B2466EA || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 44FC67F19B2466EA ) \
 && echo 'deb http://repo.sawtooth.me/ubuntu/nightly bionic universe' >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get install curl gcc git libssl-dev libzmq3-dev openssl pkg-config python3 python3-grpcio-tools unzip -y -q \
 && curl -s -S -o /tmp/setup-node.sh https://deb.nodesource.com/setup_6.x \
 && chmod 755 /tmp/setup-node.sh \
 && /tmp/setup-node.sh \
 && apt-get install nodejs -y -q \
 && rm /tmp/setup-node.sh \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -OLsS https://github.com/google/protobuf/releases/download/v3.5.1/protoc-3.5.1-linux-x86_64.zip \
 && unzip protoc-3.5.1-linux-x86_64.zip -d protoc3 \
 && rm protoc-3.5.1-linux-x86_64.zip
RUN curl https://sh.rustup.rs -sSf > /usr/bin/rustup-init \
 && chmod +x /usr/bin/rustup-init \
 && rustup-init -y
ENV PATH="$PATH:/project/sawtooth-seth/bin:/root/.cargo/bin:/protoc3/bin:/project/sawtooth-seth/rpc/bin" \
    CARGO_INCREMENTAL="0"
WORKDIR /project/sawtooth-seth/rpc
COPY bin/ /project/sawtooth-seth/bin
COPY protos/ /project/sawtooth-seth/protos
COPY rpc/ /project/sawtooth-seth/rpc
COPY tests/ /project/sawtooth-seth/tests
RUN mkdir /project/sawtooth-seth/rpc/bin \
 && cargo build \
 && cp ./target/debug/rpc /project/sawtooth-seth/rpc/bin/seth-rpc
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
 && cp -R bin/seth-rpc debian/usr/bin \
 && fakeroot dpkg-deb --build debian \
 && mv debian.deb $pkg_dir/"${PACKAGENAME}_${PACKAGEVERSION}_${PACKAGEARCH}.deb"
#  -------------=== seth rpc docker build ===-------------
FROM ubuntu:bionic
RUN mkdir /debs
COPY --from=0 /project/build/debs/sawtooth-seth-rpc_*amd64.deb /debs
RUN apt-get update \
 && dpkg -i /debs/sawtooth-seth-rpc_*amd64.deb || true \
 && apt-get install -f -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
