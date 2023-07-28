ARG BASE_IMG
FROM golang AS verify-stage
RUN apt-get update \
 && apt-get install --no-install-recommends patch=2.7.6-7 python3-pip=20.3.4-4+deb11u1 python3=3.9.2-3 -y \
 && rm -rf /var/lib/apt/lists/* \
 && pip3 install ply
ARG VPP_REPO_URL
ARG VPP_COMMIT
RUN git clone ${VPP_REPO_URL} /opt/vpp \
 && cd /opt/vpp \
 && git checkout ${VPP_COMMIT}
COPY scripts/genjsonapi.sh /opt/genjsonapi.sh
RUN /opt/genjsonapi.sh
WORKDIR /go/src/github.com/ligato/vpp-agent
COPY plugins/vpp/binapi plugins/vpp/binapi
COPY vendor vendor
COPY Makefile vpp.env ./
RUN cp -r plugins/vpp/binapi /tmp/orig_binapi \
 && make generate-binapi \
 && diff -r plugins/vpp/binapi /tmp/orig_binapi
FROM ${BASE_IMG} AS dev-stage
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-14 automake=1:1.16.3-2 build-essential=12.9 ca-certificates=20210119 curl=7.74.0-1.3+deb11u7 gdb=10.1-1.7 git=1:2.30.2-1+deb11u2 graphviz=2.42.2-5 inetutils-traceroute=2:2.0-1+deb11u1 iproute2=5.10.0-4 ipsec-tools iputils-ping=3:20210202-1 libapr1=1.7.0-6+deb11u2 libmbedcrypto1 libmbedtls10 libmbedx509-0=2.16.9-0.1 libtool=2.4.6-15 make=4.3-4.1 mc=3:4.8.26-1.1 nano=5.4-2+deb11u2 netcat=1.10-46 python software-properties-common=0.96.20.2-2.1 sudo=1.9.5p2-3+deb11u1 supervisor=4.2.2-2 telnet=0.17-42 unzip=6.0-26+deb11u1 wget=1.21-1+deb11u1 python-cffi python3-cffi=1.14.5-1 gcc-8 g++-8 -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm /usr/bin/gcc \
 && rm /usr/bin/g++ \
 && ln -s /usr/bin/gcc-8 /usr/bin/gcc \
 && ln -s /usr/bin/g++-8 /usr/bin/g++
#   install Protobuf
ARG PROTOC_VERSION=3.6.1
ARG PROTOC_OS_ARCH=linux_x86_64
RUN wget -q https://github.com/google/protobuf/releases/download/v${PROTOC_VERSION}/protoc-${PROTOC_VERSION}-${PROTOC_OS_ARCH}.zip \
 && unzip protoc-${PROTOC_VERSION}-${PROTOC_OS_ARCH}.zip -d protoc3 \
 && mv protoc3/bin/protoc /usr/local/bin \
 && mv protoc3/include/google /usr/local/include \
 && rm -rf protoc-${PROTOC_VERSION}-${PROTOC_OS_ARCH}.zip protoc3
RUN mkdir -p /opt/vpp-agent/dev /opt/vpp-agent/plugin
WORKDIR /opt/vpp-agent/dev
ARG VPP_REPO_URL
ARG VPP_COMMIT
ARG VPP_DEBUG_DEB
RUN set -eux ; git clone "${VPP_REPO_URL}" vpp ; cd vpp ; git checkout "${VPP_COMMIT}" ; sed -i -e 's/vpp vom/vpp/g' build-data/platforms/vpp.mk ; export UNATTENDED=y ; make install-dep dpdk-install-dev ; if [ -n "${VPP_DEBUG_DEB}" ] ; then make build ;make -C build-root PLATFORM=vpp TAG=vpp_debug install-deb ; else make build-release pkg-deb ; fi ; cd build-root ; dpkg -i *.deb ; rm -rf .ccache /var/lib/apt/lists/* ; find . -type f -name '*.o' -exec rm -rf '{}'
#   install Go
ENV GOLANG_VERSION="1.11.8"
ARG GOLANG_OS_ARCH=linux-amd64
RUN wget -O go.tgz "https://golang.org/dl/go${GOLANG_VERSION}.${GOLANG_OS_ARCH}.tar.gz" \
 && tar -C /usr/local -xzf go.tgz \
 && rm go.tgz
ENV GOPATH="/go"
ENV PATH="$GOPATH/bin:/usr/local/go/bin:$PATH"
RUN mkdir -p "$GOPATH/src" "$GOPATH/bin" \
 && chmod -R 777 "$GOPATH"
#   install debugger
RUN go get -u github.com/go-delve/delve/cmd/dlv \
 && dlv version ; go get -u github.com/golang/dep/cmd/dep \
 && dep version
#   copy configs
COPY docker/dev/etcd.conf docker/dev/vpp-ifplugin.conf docker/dev/linux-ifplugin.conf docker/dev/logs.conf ./
COPY docker/dev/vpp.conf /etc/vpp/vpp.conf
COPY docker/dev/supervisord.conf /etc/supervisord/supervisord.conf
#   copy scripts
COPY docker/dev/exec_vpp.sh docker/dev/exec_agent.sh docker/dev/supervisord_kill.py /usr/bin/
ARG VERSION
ARG COMMIT
ARG DATE
#   copy & build agent
COPY . $GOPATH/src/github.com/ligato/vpp-agent
RUN cd $GOPATH/src/github.com/ligato/vpp-agent \
 && VERSION=$VERSION COMMIT=$COMMIT DATE=$DATE make install
WORKDIR /
#   run supervisor as the default executable
CMD rm -f /dev/shm/db /dev/shm/global_vm /dev/shm/vpe-api \
 && exec /usr/bin/supervisord -c /etc/supervisord/supervisord.conf
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
