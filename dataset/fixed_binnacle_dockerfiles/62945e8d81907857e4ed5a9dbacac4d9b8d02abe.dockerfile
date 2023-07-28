#   -----------------------------------------------------------------------------
#   Builder image for C binaries and Magma proto files
#   -----------------------------------------------------------------------------
#   Stretch is required for c build
FROM debian:stretch AS builder
#   Add the magma apt repo
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.4.11 software-properties-common=0.96.20.2-1+deb9u1 apt-transport-https=1.4.11 gnupg=2.1.18-8~deb9u4 -y
COPY orc8r/tools/ansible/roles/pkgrepo/files/jfrog.pub /tmp/jfrog.pub
RUN apt-key add /tmp/jfrog.pub \
 && apt-add-repository "deb https://magma.jfrog.io/magma/list/dev/ stretch main"
#   Install dependencies required for building
RUN apt-get update -y \
 && apt-get install --no-install-recommends sudo=1.8.19p1-2.1+deb9u3 curl=7.52.1-5+deb9u16 wget=1.18-5+deb9u3 unzip=6.0-21+deb9u2 cmake=3.7.2-1 git=1:2.11.0-3+deb9u7 build-essential=12.3 autoconf=2.69-10 libtool=2.4.6-2 pkg-config=0.29-4+b1 libgflags-dev=2.1.2-4 libgtest-dev=1.8.0-6 clang-3.8=1:3.8.1-24 libc++-dev=3.5-2 protobuf-compiler=3.0.0-9 grpc-dev ninja-build=1.7.2-1 autogen=1:5.18.12-3 ccache=3.3.4-1 libprotoc-dev=3.0.0-9 libxml2-dev=2.9.4+dfsg1-2.2+deb9u7 libxslt-dev libyaml-cpp-dev=0.5.2-4 nlohmann-json-dev=2.0.6-1 magma-cpp-redis libgoogle-glog-dev=0.3.4-2 prometheus-cpp-dev libfolly-dev magma-libfluid libdouble-conversion-dev=2.0.1-4 libboost-chrono-dev=1.62.0.1 -y
ENV MAGMA_ROOT="/magma"
ENV C_BUILD="/build/c"
ENV OAI_BUILD="$C_BUILD/oai"
ENV CCACHE_DIR="$MAGMA_ROOT/.cache/gateway/ccache"
ENV MAGMA_DEV_MODE="1"
ENV XDG_CACHE_HOME="$MAGMA_ROOT/.cache"
#   Copy proto files
COPY feg/protos $MAGMA_ROOT/feg/protos
COPY lte/protos $MAGMA_ROOT/lte/protos
COPY orc8r/protos $MAGMA_ROOT/orc8r/protos
COPY protos $MAGMA_ROOT/protos
#   Build session_manager c code
COPY lte/gateway/Makefile $MAGMA_ROOT/lte/gateway/Makefile
COPY orc8r/gateway/c/common $MAGMA_ROOT/orc8r/gateway/c/common
COPY lte/gateway/c $MAGMA_ROOT/lte/gateway/c
RUN make -C $MAGMA_ROOT/lte/gateway/ build_session_manager
#   -----------------------------------------------------------------------------
#   Dev/Production image
#   -----------------------------------------------------------------------------
FROM debian:stretch AS gateway_c
#   Add the magma apt repo
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.4.11 software-properties-common=0.96.20.2-1+deb9u1 apt-transport-https=1.4.11 gnupg=2.1.18-8~deb9u4 -y
COPY orc8r/tools/ansible/roles/pkgrepo/files/jfrog.pub /tmp/jfrog.pub
RUN apt-key add /tmp/jfrog.pub \
 && apt-add-repository "deb https://magma.jfrog.io/magma/list/dev/ stretch main"
#   Install runtime dependencies
RUN apt-get update -y \
 && apt-get install --no-install-recommends curl=7.52.1-5+deb9u16 sudo=1.8.19p1-2.1+deb9u3 prometheus-cpp-dev magma-libfluid python3-lxml=3.7.1-1+deb9u5 bridge-utils=1.5-13+deb9u1 libyaml-cpp-dev=0.5.2-4 libgoogle-glog-dev=0.3.4-2 libfolly-dev libdouble-conversion-dev=2.0.1-4 libboost-chrono-dev=1.62.0.1 nlohmann-json-dev=2.0.6-1 redis-server=3:3.2.6-3+deb9u9 python-redis=2.10.5-2 magma-cpp-redis grpc-dev protobuf-compiler=3.0.0-9 libprotoc-dev=3.0.0-9 -y
#   Copy the build artifacts.
COPY --from=builder /build/c/session_manager/sessiond /usr/local/bin/sessiond
#   Copy the configs.
COPY lte/gateway/configs /etc/magma
COPY orc8r/gateway/configs/templates /etc/magma/templates
COPY orc8r/cloud/docker/proxy/magma_headers.rb /etc/nghttpx/magma_headers.rb
RUN mkdir -p /var/opt/magma/configs
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
