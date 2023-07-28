#   -----------------------------------------------------------------------------
#   Development image for test, precommit, etc.
#   -----------------------------------------------------------------------------
FROM ubuntu:xenial AS base
#   Add the magma apt repo
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.2.35 software-properties-common=0.96.20.10 apt-transport-https=1.2.35 -y
COPY orc8r/tools/ansible/roles/pkgrepo/files/jfrog.pub /tmp/jfrog.pub
RUN apt-key add /tmp/jfrog.pub \
 && apt-add-repository "deb https://magma.jfrog.io/magma/list/dev/ xenial main"
#   Install the runtime deps.
RUN apt-get update \
 && apt-get install --no-install-recommends bzr=2.7.0-2ubuntu3.1 curl=7.47.0-1ubuntu2.19 daemontools=1:0.76-6ubuntu1 gcc=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 libc-ares-dev=1.10.0-3ubuntu0.2 libev-dev=1:4.22-1 libevent-dev=2.0.21-stable-2ubuntu0.16.04.1 libffi-dev=3.2.1-4 libjansson-dev=2.7-3ubuntu0.1 libjemalloc-dev=3.6.0-9ubuntu1 libssl-dev=1.0.2g-1ubuntu4.20 libsystemd-dev=229-4ubuntu21.31 magma-nghttpx=1.31.1-1 make=4.1-6 net-tools=1.60-26ubuntu1 pkg-config=0.29.1-0ubuntu1 python-cffi=1.5.2-1ubuntu1 python3-pip=8.1.1-2ubuntu0.6 redis-server=2:3.0.6-1ubuntu0.4 rsyslog=8.16.0-1ubuntu3.1 sudo=1.8.16-0ubuntu1.10 unzip=6.0-20ubuntu1.1 vim=2:7.4.1689-3ubuntu1.5 virtualenv=15.0.1+ds-3ubuntu1.1 -y
#   Install Golang 1.12.
WORKDIR /usr/local
RUN curl https://dl.google.com/go/go1.12.4.linux-amd64.tar.gz -O \
 && tar xvf go1.12.4.linux-amd64.tar.gz \
 && cp -r go/bin/* /usr/local/bin/
#   Install protobuf compiler.
RUN curl -Lfs https://github.com/google/protobuf/releases/download/v3.1.0/protoc-3.1.0-linux-x86_64.zip -o protoc3.zip \
 && unzip protoc3.zip -d protoc3 \
 && mv protoc3/bin/protoc /bin/protoc \
 && chmod a+rx /bin/protoc \
 && mv protoc3/include/google /usr/include/ \
 && chmod -R a+Xr /usr/include/google \
 && rm -rf protoc3.zip protoc3
ENV GOBIN="/var/opt/magma/bin"
ENV MAGMA_ROOT="/magma"
ENV PIP_CACHE_HOME="~/.pipcache"
ENV PYTHON_BUILD="/build/python"
ENV PATH="${PYTHON_BUILD}/bin:${PATH}:${GOBIN}"
ENV GO111MODULE="on"
#   Use public go modules proxy
ENV GOPROXY="https://proxy.golang.org"
RUN printenv > /etc/environment
#   Copy just the go.mod and go.sum files to download the golang deps.
#   This step allows us to cache the downloads, and prevents reaching out to
#   the internet unless any of the go.mod or go.sum files are changed.
COPY lte/cloud/go/go.* $MAGMA_ROOT/lte/cloud/go/
COPY feg/cloud/go/go.* $MAGMA_ROOT/feg/cloud/go/
COPY feg/cloud/go/protos/go.* $MAGMA_ROOT/feg/cloud/go/protos/
COPY feg/gateway/go.* $MAGMA_ROOT/feg/gateway/
COPY feg/gateway/third-party/go/src/github.com/fiorix/go-diameter/go.* $MAGMA_ROOT/feg/gateway/third-party/go/src/github.com/fiorix/go-diameter/
COPY orc8r/cloud/go/go.* $MAGMA_ROOT/orc8r/cloud/go/
COPY orc8r/gateway/go/go.* $MAGMA_ROOT/orc8r/gateway/go/
WORKDIR $MAGMA_ROOT/feg/gateway
RUN go mod download ; exit 0
#   Install protoc-gen-go
RUN go install github.com/golang/protobuf/protoc-gen-go ; exit 0
#   Copy the configs.
COPY orc8r/cloud/docker/proxy/magma_headers.rb /etc/nghttpx/magma_headers.rb
#   Symlink python scripts.
RUN ln -s /build/python/bin/generate_service_config.py /usr/local/bin/generate_service_config.py
RUN ln -s /build/python/bin/generate_nghttpx_config.py /usr/local/bin/generate_nghttpx_config.py
#   Build the code.
COPY feg $MAGMA_ROOT/feg
COPY lte/cloud $MAGMA_ROOT/lte/cloud
COPY orc8r/cloud $MAGMA_ROOT/orc8r/cloud
COPY orc8r/gateway/go $MAGMA_ROOT/orc8r/gateway/go
#   Enable make gen if proto gen is required
#   RUN make -C $MAGMA_ROOT/feg/gateway gen
RUN make -C $MAGMA_ROOT/feg/gateway build
#   -----------------------------------------------------------------------------
#   Production image
#   -----------------------------------------------------------------------------
FROM ubuntu:xenial AS gateway_go
#   Install envdir.
RUN apt-get update -y \
 && apt-get install --no-install-recommends daemontools=1:0.76-6ubuntu1 -y
#   Copy the build artifacts.
COPY --from=base /var/opt/magma/bin /var/opt/magma/bin
#   Copy the configs.
COPY feg/gateway/configs /etc/magma
#   Add docker config overrides
COPY feg/gateway/docker/configs /etc/magma
#   Create empty envdir directory
RUN mkdir -p /var/opt/magma/envdir
RUN mkdir -p /var/opt/magma/configs
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
