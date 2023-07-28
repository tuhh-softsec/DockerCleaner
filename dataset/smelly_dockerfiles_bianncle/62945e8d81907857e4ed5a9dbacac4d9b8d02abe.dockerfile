#  -----------------------------------------------------------------------------
#  Builder image for C binaries and Magma proto files
#  -----------------------------------------------------------------------------
#  Stretch is required for c build
FROM debian:stretch AS builder
#  Add the magma apt repo
RUN apt-get update \
 && apt-get install apt-utils software-properties-common apt-transport-https gnupg -y
COPY orc8r/tools/ansible/roles/pkgrepo/files/jfrog.pub /tmp/jfrog.pub
RUN apt-key add /tmp/jfrog.pub \
 && apt-add-repository "deb https://magma.jfrog.io/magma/list/dev/ stretch main"
#  Install dependencies required for building
RUN apt-get update -y \
 && apt-get install sudo curl wget unzip cmake git build-essential autoconf libtool pkg-config libgflags-dev libgtest-dev clang-3.8 libc++-dev protobuf-compiler grpc-dev ninja-build autogen ccache libprotoc-dev libxml2-dev libxslt-dev libyaml-cpp-dev nlohmann-json-dev magma-cpp-redis libgoogle-glog-dev prometheus-cpp-dev libfolly-dev magma-libfluid libdouble-conversion-dev libboost-chrono-dev -y
ENV MAGMA_ROOT="/magma"
ENV C_BUILD="/build/c"
ENV OAI_BUILD="$C_BUILD/oai"
ENV CCACHE_DIR="$MAGMA_ROOT/.cache/gateway/ccache"
ENV MAGMA_DEV_MODE="1"
ENV XDG_CACHE_HOME="$MAGMA_ROOT/.cache"
#  Copy proto files
COPY feg/protos $MAGMA_ROOT/feg/protos
COPY lte/protos $MAGMA_ROOT/lte/protos
COPY orc8r/protos $MAGMA_ROOT/orc8r/protos
COPY protos $MAGMA_ROOT/protos
#  Build session_manager c code
COPY lte/gateway/Makefile $MAGMA_ROOT/lte/gateway/Makefile
COPY orc8r/gateway/c/common $MAGMA_ROOT/orc8r/gateway/c/common
COPY lte/gateway/c $MAGMA_ROOT/lte/gateway/c
RUN make -C $MAGMA_ROOT/lte/gateway/ build_session_manager
#  -----------------------------------------------------------------------------
#  Dev/Production image
#  -----------------------------------------------------------------------------
FROM debian:stretch AS gateway_c
#  Add the magma apt repo
RUN apt-get update \
 && apt-get install apt-utils software-properties-common apt-transport-https gnupg -y
COPY orc8r/tools/ansible/roles/pkgrepo/files/jfrog.pub /tmp/jfrog.pub
RUN apt-key add /tmp/jfrog.pub \
 && apt-add-repository "deb https://magma.jfrog.io/magma/list/dev/ stretch main"
#  Install runtime dependencies
RUN apt-get update -y \
 && apt-get install curl sudo prometheus-cpp-dev magma-libfluid python3-lxml bridge-utils libyaml-cpp-dev libgoogle-glog-dev libfolly-dev libdouble-conversion-dev libboost-chrono-dev nlohmann-json-dev redis-server python-redis magma-cpp-redis grpc-dev protobuf-compiler libprotoc-dev -y
#  Copy the build artifacts.
COPY --from=builder /build/c/session_manager/sessiond /usr/local/bin/sessiond
#  Copy the configs.
COPY lte/gateway/configs /etc/magma
COPY orc8r/gateway/configs/templates /etc/magma/templates
COPY orc8r/cloud/docker/proxy/magma_headers.rb /etc/nghttpx/magma_headers.rb
RUN mkdir -p /var/opt/magma/configs
