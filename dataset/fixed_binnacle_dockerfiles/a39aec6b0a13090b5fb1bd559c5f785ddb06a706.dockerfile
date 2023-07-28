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
#   Description:
#     Builds the environment needed to build the Sawtooth Seth docs
#     Running the image will put the Sawtooth Seth docs in
#     sawtooth-seth/docs/build on your local machine.
#
#   Build:
#     $ cd sawtooth-seth
#     $ docker build . -f docs/Dockerfile -t seth-build-docs
#
#   Run:
#     $ cd sawtooth-seth
#     $ docker run -v $(pwd)/docs:/project/sawtooth-seth/docs seth-build-docs
FROM ubuntu:bionic
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg=2.2.4-1ubuntu1.6 -y
ENV DEBIAN_FRONTEND="noninteractive"
RUN (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 44FC67F19B2466EA || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 44FC67F19B2466EA ) \
 && (apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 308C15A29AD198E9 || apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 308C15A29AD198E9 ) \
 && echo 'deb http://repo.sawtooth.me/ubuntu/nightly bionic universe' >> /etc/apt/sources.list \
 && echo 'deb http://ppa.launchpad.net/gophers/archive/ubuntu bionic main' >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 dvipng=1.15-1 gcc=4:7.4.0-1ubuntu2.3 git=1:2.17.1-1ubuntu0.17 golang-1.11-go libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libzmq3-dev=4.2.5-1ubuntu0.2 make=4.1-9.1ubuntu1 openssl=1.1.1-1ubuntu2.1~18.04.21 python3=3.6.7-1~18.04 python3-grpcio-tools python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-sawtooth-cli software-properties-common=0.96.24.32.20 sudo=1.8.21p2-3ubuntu1.5 texlive-fonts-recommended=2017.20180305-1 texlive-latex-base=2017.20180305-1 texlive-latex-extra=2017.20180305-2 texlive-latex-recommended=2017.20180305-1 unzip=6.0-21ubuntu1.2 zip=3.0-11build1 -y -q \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN pip3 install sphinx==1.5.6 sphinx_rtd_theme
ENV PATH="$PATH:/go/bin:/project/sawtooth-seth/bin:/project/sawtooth-seth/cli-go/bin:/project/sawtooth-seth/common/bin:/project/sawtooth-seth/processor/bin:/project/sawtooth-seth/rpc/bin:/protoc3/bin:/root/.cargo/bin:/usr/lib/go-1.11/bin"
ENV GOPATH="/go:/project/sawtooth-seth/common:/project/sawtooth-seth/burrow:/project/sawtooth-seth/cli-go:/project/sawtooth-seth/processor"
RUN go get -u github.com/btcsuite/btcd/btcec github.com/golang/mock/gomock github.com/golang/mock/mockgen github.com/golang/protobuf/proto github.com/golang/protobuf/protoc-gen-go github.com/jessevdk/go-flags github.com/pebbe/zmq4 github.com/pelletier/go-toml github.com/satori/go.uuid golang.org/x/crypto/ripemd160 golang.org/x/crypto/ssh/terminal gopkg.in/fatih/set.v0
RUN git clone https://github.com/knkski/burrow.git /go/src/github.com/hyperledger/burrow
RUN go get github.com/hyperledger/sawtooth-sdk-go \
 && cd /go/src/github.com/hyperledger/sawtooth-sdk-go \
 && go generate
RUN curl -OLsS https://github.com/google/protobuf/releases/download/v3.5.1/protoc-3.5.1-linux-x86_64.zip \
 && unzip protoc-3.5.1-linux-x86_64.zip -d protoc3 \
 && rm protoc-3.5.1-linux-x86_64.zip
ENV CARGO_INCREMENTAL="0"
RUN curl https://sh.rustup.rs -sSf > /usr/bin/rustup-init \
 && chmod +x /usr/bin/rustup-init \
 && rustup-init -y
COPY . /project/sawtooth-seth
RUN seth-protogen go
WORKDIR /project/sawtooth-seth/cli-go/src/seth_cli/cli
RUN go build -o /project/sawtooth-seth/cli-go/bin/seth
WORKDIR /project/sawtooth-seth/processor/src/seth_tp
RUN go build -o /project/sawtooth-seth/processor/bin/seth-tp
WORKDIR /project/sawtooth-seth
RUN cargo build \
 && cp ./target/debug/rpc /project/sawtooth-seth/bin/seth-rpc \
 && cargo doc --no-deps
WORKDIR /project/sawtooth-seth/docs
CMD make html latexpdf
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
