#   -----------------------------------------------------------------------------
#   Builder image to generate OVS debian packages and Magma proto files
#   -----------------------------------------------------------------------------
FROM ubuntu:xenial AS builder
#   Add the magma apt repo
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.2.35 software-properties-common=0.96.20.10 apt-transport-https=1.2.35 -y
COPY orc8r/tools/ansible/roles/pkgrepo/files/jfrog.pub /tmp/jfrog.pub
RUN apt-key add /tmp/jfrog.pub \
 && apt-add-repository "deb https://magma.jfrog.io/magma/list/dev/ xenial main"
#   Install the runtime deps from apt.
RUN apt-get update -y \
 && apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 make=4.1-6 virtualenv=15.0.1+ds-3ubuntu1.1 zip=3.0-11 tar=1.28-2.1ubuntu0.2 -y
#   TODO: Fetch necessary OVS debian packages from pkgrepo
#   Fetch OVS v2.8.1 tarball
RUN curl -Lfs https://www.openvswitch.org/releases/openvswitch-2.8.1.tar.gz | tar xvz
#   Install OVS dependencies for building
RUN apt-get update -y \
 && apt-get install --no-install-recommends graphviz=2.38.0-12ubuntu2.1 autoconf=2.69-9 automake=1:1.15-4ubuntu1 bzip2=1.0.6-8ubuntu0.2 debhelper=9.20160115ubuntu3 dh-autoreconf=11 libssl-dev=1.0.2g-1ubuntu4.20 libtool=2.4.6-0.1 openssl=1.0.2g-1ubuntu4.20 procps=2:3.3.10-4ubuntu2.5 python-all=2.7.12-1~16.04 python-twisted-conch=1:16.0.0-1ubuntu0.4 python-zopeinterface python-six=1.10.0-3 build-essential=12.1ubuntu2 fakeroot=1.20.2-1ubuntu1 -y
#   Build OVS v2.8.1 debian packages
RUN cd openvswitch-2.8.1 \
 && DEB_BUILD_OPTIONS='parallel=8 nocheck' fakeroot debian/rules binary
#   Install protobuf compiler.
RUN curl -Lfs https://github.com/google/protobuf/releases/download/v3.1.0/protoc-3.1.0-linux-x86_64.zip -o protoc3.zip \
 && unzip protoc3.zip -d protoc3 \
 && mv protoc3/bin/protoc /usr/bin/protoc \
 && chmod a+rx /usr/bin/protoc \
 && cp -r protoc3/include/google /usr/include/ \
 && chmod -R a+Xr /usr/include/google \
 && rm -rf protoc3.zip protoc3
ENV MAGMA_ROOT="/magma"
ENV PYTHON_BUILD="/build/python"
ENV PIP_CACHE_HOME="~/.pipcache"
#   Generate python proto bindings.
COPY feg/protos $MAGMA_ROOT/feg/protos
COPY lte/gateway/python/defs.mk $MAGMA_ROOT/lte/gateway/python/defs.mk
COPY lte/gateway/python/Makefile $MAGMA_ROOT/lte/gateway/python/Makefile
COPY lte/protos $MAGMA_ROOT/lte/protos
COPY orc8r/gateway/python/python.mk $MAGMA_ROOT/orc8r/gateway/python/python.mk
COPY orc8r/protos $MAGMA_ROOT/orc8r/protos
COPY protos $MAGMA_ROOT/protos
RUN make -C $MAGMA_ROOT/lte/gateway/python protos
#   -----------------------------------------------------------------------------
#   Dev/Production image
#   -----------------------------------------------------------------------------
FROM ubuntu:xenial AS lte_gateway_python
#   Add the magma apt repo
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.2.35 software-properties-common=0.96.20.10 apt-transport-https=1.2.35 -y
COPY orc8r/tools/ansible/roles/pkgrepo/files/jfrog.pub /tmp/jfrog.pub
RUN apt-key add /tmp/jfrog.pub \
 && apt-add-repository "deb https://magma.jfrog.io/magma/list/dev/ xenial main"
#   Install the runtime deps from apt.
RUN apt-get update -y \
 && apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 libc-ares2=1.10.0-3ubuntu0.2 libev4=1:4.22-1 libevent-openssl-2.0-5=2.0.21-stable-2ubuntu0.16.04.1 libffi-dev=3.2.1-4 libjansson4=2.7-3ubuntu0.1 libjemalloc1=3.6.0-9ubuntu1 libssl-dev=1.0.2g-1ubuntu4.20 libsystemd-dev=229-4ubuntu21.31 magma-nghttpx=1.31.1-1 net-tools=1.60-26ubuntu1 openssl=1.0.2g-1ubuntu4.20 pkg-config=0.29.1-0ubuntu1 python-cffi=1.5.2-1ubuntu1 python3-aioeventlet=0.4-1 python3-pip=8.1.1-2ubuntu0.6 redis-server=2:3.0.6-1ubuntu0.4 -y
#   Copy OVS debian packages from builder
COPY --from=builder libopenvswitch_2.8.1-1_amd64.deb /tmp/ovs/
COPY --from=builder openvswitch-common_2.8.1-1_amd64.deb /tmp/ovs/
COPY --from=builder openvswitch-switch_2.8.1-1_amd64.deb /tmp/ovs/
COPY --from=builder python-openvswitch_2.8.1-1_all.deb /tmp/ovs/
#   Install OVS debian packages
RUN apt-get install --no-install-recommends /tmp/ovs/libopenvswitch_2.8.1-1_amd64.deb /tmp/ovs/openvswitch-common_2.8.1-1_amd64.deb /tmp/ovs/openvswitch-switch_2.8.1-1_amd64.deb /tmp/ovs/python-openvswitch_2.8.1-1_all.deb -y
#   Install orc8r python (magma.common required for lte python)
COPY orc8r/gateway/python /tmp/orc8r
RUN pip3 install /tmp/orc8r
#   Install lte python
COPY lte/gateway/python /tmp/lte
RUN pip3 install /tmp/lte
#   Copy the build artifacts.
COPY --from=builder /build/python/gen /usr/local/lib/python3.5/dist-packages/
#   Copy the configs.
COPY lte/gateway/configs /etc/magma
COPY orc8r/gateway/configs/templates /etc/magma/templates
COPY orc8r/cloud/docker/proxy/magma_headers.rb /etc/nghttpx/magma_headers.rb
RUN mkdir -p /var/opt/magma/configs
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
