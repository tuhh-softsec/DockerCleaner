#
#   Cilium runtime base image
#
FROM docker.io/library/ubuntu:18.04 AS runtime-base
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends gpg=2.2.4-1ubuntu1.6 gpg-agent=2.2.4-1ubuntu1.6 libelf-dev=0.170-0.4ubuntu0.1 libmnl-dev=1.0.4-2 libc6-dev-i386=2.27-3ubuntu1.6 iptables=1.6.1-2ubuntu2 libgcc-5-dev=5.5.0-12ubuntu1 bash-completion=1:2.8-1ubuntu1 binutils=2.30-21ubuntu1~18.04.8 binutils-dev=2.30-21ubuntu1~18.04.8 ca-certificates=20211016ubuntu0.18.04.1 clang-7=1:7-3~ubuntu0.18.04.1 llvm-7=1:7-3~ubuntu0.18.04.1 kmod=24-1ubuntu3.5 -y \
 && apt-get purge --auto-remove \
 && apt-get clean \
 && rm -fr /usr/lib/llvm-7/include/llvm-c \
 && rm -fr /usr/lib/llvm-7/include/clang-c \
 && rm -fr /usr/lib/llvm-7/include/c++ \
 && rm -fr /usr/lib/llvm-7/include/polly \
 && rm -fr /usr/lib/llvm-7/share \
 && ls -d /usr/lib/llvm-7/lib/* | grep -vE clang$ | xargs rm -r \
 && ls -d /usr/lib/llvm-7/bin/* | grep -vE "clang$|clang-7$|llc$" | xargs basename -a | awk '{ print "/usr/bin/"$1"-7" }' | xargs rm -r \
 && ls -d /usr/lib/llvm-7/bin/* | grep -vE "clang$|clang-7$|llc$" | xargs rm -r \
 && strip /usr/lib/llvm-7/bin/* \
 && update-alternatives --install /usr/bin/clang clang /usr/lib/llvm-7/bin/clang 1000 \
 && update-alternatives --install /usr/bin/llc llc /usr/lib/llvm-7/bin/llc 1000
#
#   Build Cilium runtime dependencies.
#
FROM runtime-base AS runtime-build
WORKDIR /tmp
RUN apt-get install --no-install-recommends make=4.1-9.1ubuntu1 git=1:2.17.1-1ubuntu0.17 curl=7.58.0-2ubuntu3.24 ca-certificates=20211016ubuntu0.18.04.1 xz-utils=5.2.2-1.3ubuntu0.1 gcc=4:7.4.0-1ubuntu2.3 git=1:2.17.1-1ubuntu0.17 pkg-config=0.29.1-0ubuntu2 bison=2:3.0.4.dfsg-1build1 flex=2.6.4-6 build-essential=12.4ubuntu1 -y \
 && git clone --depth 1 -b static-data https://github.com/cilium/iproute2.git iproute2 \
 && cd iproute2 \
 && ./configure \
 && make -j `getconf _NPROCESSORS_ONLN ` \
 && strip tc/tc \
 && strip ip/ip \
 && cd .. \
 && git clone --depth 1 -b master git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git \
 && cd linux/tools/bpf/bpftool/ \
 && make -j `getconf _NPROCESSORS_ONLN ` \
 && strip bpftool \
 && cd ../../../../ \
 && curl -SsL https://github.com/cilium/bpf-map/releases/download/v1.0/bpf-map -o bpf-map \
 && chmod +x bpf-map \
 && strip bpf-map \
 && curl -sS -L https://github.com/containernetworking/plugins/releases/download/v0.7.5/cni-plugins-amd64-v0.7.5.tgz -o cni.tar.gz \
 && tar -xvf cni.tar.gz ./loopback \
 && strip -s ./loopback \
 && apt-get purge --auto-remove -y gpg gpg-agent gcc make bison flex git curl xz-utils ca-certificates \
 && apt-get clean
#
#   Go-based tools we need at runtime
#
FROM docker.io/library/golang:1.12.5 AS runtime-gobuild
WORKDIR /tmp
RUN go get -d github.com/google/gops \
 && cd /go/src/github.com/google/gops \
 && git checkout -b v0.3.6 v0.3.6 \
 && go install \
 && strip /go/bin/gops
#
#   Stripped cilium runtime base image
#
FROM runtime-base
LABEL maintainer="maintainer@cilium.io"
WORKDIR /bin
COPY --from=runtime-build /tmp/iproute2/tc/tc /tmp/iproute2/ip/ip ./
COPY --from=runtime-build /tmp/linux/tools/bpf/bpftool/bpftool ./
COPY --from=runtime-build /tmp/bpf-map ./
COPY --from=runtime-gobuild /go/bin/gops ./
WORKDIR /cni
COPY --from=runtime-build /tmp/loopback ./
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
