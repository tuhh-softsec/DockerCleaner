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
#  docker build -f perf/smallbank_workload/Dockerfile-installed-bionic -t sawtooth-smallbank-workload .
#  -------------=== cli build ===-------------
FROM ubuntu:bionic AS cli-builder
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg -y
ENV VERSION="AUTO_STRICT"
RUN echo "deb http://repo.sawtooth.me/ubuntu/ci bionic universe" >> /etc/apt/sources.list \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 8AA7AF1F1091A5FD || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 8AA7AF1F1091A5FD ) \
 && apt-get update \
 && apt-get install --no-install-recommends git python3 python3-protobuf python3-stdeb python3-toml python3-grpcio-tools python3-grpcio -y -q
COPY . /project
RUN echo "deb http://repo.sawtooth.me/ubuntu/ci bionic universe" >> /etc/apt/sources.list \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 8AA7AF1F1091A5FD || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 8AA7AF1F1091A5FD ) \
 && apt-get update \
 && dpkg -i /tmp/python3-sawtooth-*.deb || true \
 && apt-get install -f -y
RUN /project/bin/protogen \
 && cd /project/cli \
 && if [ -d "debian" ] ; then rm -rf debian ; fi \
 && python3 setup.py clean --all \
 && python3 setup.py --command-packages=stdeb.command debianize \
 && if [ -d "packaging/ubuntu" ] ; then cp -R packaging/ubuntu/* debian/ ; fi \
 && dpkg-buildpackage -b -rfakeroot -us -uc
#  -------------=== smallbank workload build ===-------------
FROM ubuntu:bionic AS smallbank-workload-builder
ENV VERSION="AUTO_STRICT"
RUN apt-get update \
 && apt-get install --no-install-recommends curl gcc git libssl-dev libzmq3-dev pkg-config python3 unzip -y
#  For Building Protobufs
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y \
 && curl -OLsS https://github.com/google/protobuf/releases/download/v3.5.1/protoc-3.5.1-linux-x86_64.zip \
 && unzip protoc-3.5.1-linux-x86_64.zip -d protoc3 \
 && rm protoc-3.5.1-linux-x86_64.zip
ENV PATH="$PATH:/protoc3/bin"
RUN /root/.cargo/bin/cargo install cargo-deb
COPY . /project
WORKDIR /project/perf/smallbank_workload
RUN sed -i -e s/version.*$/version = \"$( ../../bin/get_version ;)\"/ Cargo.toml
RUN /root/.cargo/bin/cargo deb
#  -------------=== smallbank workload docker build ===-------------
FROM ubuntu:bionic
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg -y
COPY --from=cli-builder /project/python3-sawtooth-cli* /tmp
COPY --from=smallbank-workload-builder /project/perf/smallbank_workload/target/debian/sawtooth-smallbank-workload*.deb /tmp
RUN echo "deb http://repo.sawtooth.me/ubuntu/ci bionic universe" >> /etc/apt/sources.list \
 && echo "deb [arch=amd64] http://repo.sawtooth.me/ubuntu/nightly bionic universe" >> /etc/apt/sources.list \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 8AA7AF1F1091A5FD 44FC67F19B2466EA || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 8AA7AF1F1091A5FD 44FC67F19B2466EA ) \
 && apt-get update \
 && apt-get install --no-install-recommends python3-sawtooth-sdk -y -q \
 && dpkg -i /tmp/*sawtooth*.deb || true \
 && apt-get install -f -y
