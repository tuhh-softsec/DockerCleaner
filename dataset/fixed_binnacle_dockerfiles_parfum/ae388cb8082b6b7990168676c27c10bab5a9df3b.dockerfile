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
#  docker build -f validator/Dockerfile-installed-bionic -t sawtooth-validator .
#  -------------=== validator build ===-------------
FROM ubuntu:bionic AS sawtooth-validator-builder
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg -y
ENV VERSION="AUTO_STRICT"
RUN echo "deb http://repo.sawtooth.me/ubuntu/nightly bionic universe" >> /etc/apt/sources.list \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 44FC67F19B2466EA || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 44FC67F19B2466EA ) \
 && apt-get update \
 && apt-get install --no-install-recommends git libssl-dev openssl pkg-config python3 python3-dev python3-grpcio python3-grpcio-tools python3-protobuf python3-pyformance python3-sawtooth-sdk python3-secp256k1 python3-stdeb python3-toml unzip -y -q
RUN curl -OLsS https://github.com/google/protobuf/releases/download/v3.5.1/protoc-3.5.1-linux-x86_64.zip \
 && unzip protoc-3.5.1-linux-x86_64.zip -d protoc3 \
 && rm protoc-3.5.1-linux-x86_64.zip
RUN curl https://sh.rustup.rs -sSf > /usr/bin/rustup-init \
 && chmod +x /usr/bin/rustup-init \
 && rustup-init -y
ENV PATH="$PATH:/project/sawtooth-core/bin:/protoc3/bin:/project/sawtooth-core/bin:/root/.cargo/bin" \
    CARGO_INCREMENTAL="0"
RUN ln -s /usr/bin/python3 /usr/bin/python
COPY . /project
RUN /project/bin/protogen \
 && cd /project/validator \
 && if [ -d "debian" ] ; then rm -rf debian ; fi \
 && python3 setup.py clean --all \
 && python3 setup.py --command-packages=stdeb.command debianize \
 && if [ -d "packaging/ubuntu" ] ; then cp -R packaging/ubuntu/* debian/ ; fi \
 && if [ -d "bin" ] ; then rm -rf bin ; fi \
 && mkdir bin \
 && if [ -d "lib" ] ; then rm -rf lib ; fi \
 && mkdir lib \
 && sed -i -e "0,/version.*$/ s/version.*$/version\ =\ \"$( ../bin/get_version ;)\"/" Cargo.toml \
 && cargo build --release \
 && cp ./target/release/sawtooth-validator bin/sawtooth-validator \
 && cp ./target/release/libsawtooth_validator.so lib/libsawtooth_validator.so \
 && dpkg-buildpackage -b -rfakeroot -us -uc
#  -------------=== cli build ===-------------
FROM ubuntu:bionic AS sawtooth-cli-builder
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg -y
ENV VERSION="AUTO_STRICT"
RUN echo "deb http://repo.sawtooth.me/ubuntu/nightly bionic universe" >> /etc/apt/sources.list \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 44FC67F19B2466EA || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 44FC67F19B2466EA ) \
 && apt-get update \
 && apt-get install --no-install-recommends git python3 python3-protobuf python3-stdeb python3-toml python3-grpcio-tools python3-grpcio python3-sawtooth-sdk -y -q
COPY . /project
RUN /project/bin/protogen \
 && cd /project/cli \
 && if [ -d "debian" ] ; then rm -rf debian ; fi \
 && python3 setup.py clean --all \
 && python3 setup.py --command-packages=stdeb.command debianize \
 && if [ -d "packaging/ubuntu" ] ; then cp -R packaging/ubuntu/* debian/ ; fi \
 && dpkg-buildpackage -b -rfakeroot -us -uc
#  -------------=== sawtooth-validator docker build ===-------------
FROM ubuntu:bionic
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg systemd -y
COPY --from=sawtooth-cli-builder /project/python3-sawtooth-cli*.deb /tmp
COPY --from=sawtooth-validator-builder /project/python3-sawtooth-validator*.deb /tmp
RUN echo "deb http://repo.sawtooth.me/ubuntu/nightly bionic universe" >> /etc/apt/sources.list \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 44FC67F19B2466EA || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 44FC67F19B2466EA ) \
 && apt-get update \
 && dpkg -i /tmp/python3-sawtooth-*.deb || true \
 && apt-get install -f -y
