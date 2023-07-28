#   This file describes the standard way to build Docker, using docker
#
#   Usage:
#
#   # Use make to build a development environment image and run it in a container.
#   # This is slow the first time.
#   make BIND_DIR=. shell
#
#   The following commands are executed inside the running container.
#   # Make a dockerd binary.
#   # hack/make.sh binary
#
#   # Install dockerd to /usr/local/bin
#   # make install
#
#   # Run unit tests
#   # hack/test/unit
#
#   # Run tests e.g. integration, py
#   # hack/make.sh binary test-integration test-docker-py
#
#   Note: AppArmor used to mess with privileged mode, but this is no longer
#   the case. Therefore, you don't have to disable it anymore.
#
ARG CROSS="false"
FROM golang:1.12.6 AS base
#   allow replacing httpredir or deb mirror
ARG APT_MIRROR=deb.debian.org
RUN sed -ri "s/(httpredir|deb).debian.org/$APT_MIRROR/g" /etc/apt/sources.list
FROM base AS criu
#   Install CRIU for checkpoint/restore support
ENV CRIU_VERSION="3.11"
#   Install dependency packages specific to criu
RUN apt-get update \
 && apt-get install --no-install-recommends libnet-dev libprotobuf-c0-dev libprotobuf-dev=3.0.0-9 libnl-3-dev=3.2.27-2 libcap-dev=1:2.25-1 protobuf-compiler=3.0.0-9 protobuf-c-compiler=1.2.1-2 python-protobuf=3.0.0-9 -y \
 && mkdir -p /usr/src/criu \
 && curl -sSL https://github.com/checkpoint-restore/criu/archive/v${CRIU_VERSION}.tar.gz | tar -C /usr/src/criu/ -xz --strip-components=1 \
 && cd /usr/src/criu \
 && make \
 && make PREFIX=/build/ install-criu
FROM base AS registry
#   Install two versions of the registry. The first is an older version that
#   only supports schema1 manifests. The second is a newer version that supports
#   both. This allows integration-cli tests to cover push/pull with both schema1
#   and schema2 manifests.
ENV REGISTRY_COMMIT_SCHEMA1="ec87e9b6971d831f0eff752ddb54fb64693e51cd"
ENV REGISTRY_COMMIT="47a064d4195a9b56133891bbb13620c3ac83a827"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/distribution.git "$GOPATH/src/github.com/docker/distribution" \
 && (cd "$GOPATH/src/github.com/docker/distribution" \
 && git checkout -q "$REGISTRY_COMMIT" ) \
 && GOPATH="$GOPATH/src/github.com/docker/distribution/Godeps/_workspace:$GOPATH" go build -buildmode=pie -o /build/registry-v2 github.com/docker/distribution/cmd/registry \
 && case $( dpkg --print-architecture ;) in (amd64|ppc64*|s390x) (cd "$GOPATH/src/github.com/docker/distribution" \
 && git checkout -q "$REGISTRY_COMMIT_SCHEMA1" ) ; GOPATH="$GOPATH/src/github.com/docker/distribution/Godeps/_workspace:$GOPATH" ; go build -buildmode=pie -o /build/registry-v2-schema1 github.com/docker/distribution/cmd/registry ;; esac \
 && rm -rf "$GOPATH"
FROM base AS docker-py
#   Get the "docker-py" source so we can run their integration tests
ENV DOCKER_PY_COMMIT="ac922192959870774ad8428344d9faa0555f7ba6"
RUN git clone https://github.com/docker/docker-py.git /build \
 && cd /build \
 && git checkout -q $DOCKER_PY_COMMIT
FROM base AS swagger
#   Install go-swagger for validating swagger.yaml
ENV GO_SWAGGER_COMMIT="c28258affb0b6251755d92489ef685af8d4ff3eb"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/go-swagger/go-swagger.git "$GOPATH/src/github.com/go-swagger/go-swagger" \
 && (cd "$GOPATH/src/github.com/go-swagger/go-swagger" \
 && git checkout -q "$GO_SWAGGER_COMMIT" ) \
 && go build -o /build/swagger github.com/go-swagger/go-swagger/cmd/swagger \
 && rm -rf "$GOPATH"
FROM base AS frozen-images
RUN apt-get update \
 && apt-get install --no-install-recommends jq=1.5+dfsg-1.3 ca-certificates=20200601~deb9u2 -y
#   Get useful and necessary Hub images so we can "docker load" locally instead of pulling
COPY contrib/download-frozen-image-v2.sh /
RUN /download-frozen-image-v2.sh /build buildpack-deps:jessie@sha256:dd86dced7c9cd2a724e779730f0a53f93b7ef42228d4344b25ce9a42a1486251 busybox:latest@sha256:bbc3a03235220b170ba48a157dd097dd1379299370e1ed99ce976df0355d24f0 busybox:glibc@sha256:0b55a30394294ab23b9afd58fab94e61a923f5834fba7ddbae7f8e0c11ba85e6 debian:jessie@sha256:287a20c5f73087ab406e6b364833e3fb7b3ae63ca0eb3486555dc27ed32c6e60 hello-world:latest@sha256:be0cd392e45be79ffeffa6b05338b98ebb16c87b255f48e297ec7f98e123905c
#   See also ensureFrozenImagesLinux() in "integration-cli/fixtures_linux_daemon_test.go" (which needs to be updated when adding images to this list)
FROM base AS cross-false
FROM base AS cross-true
RUN dpkg --add-architecture armhf
RUN dpkg --add-architecture arm64
RUN dpkg --add-architecture armel
RUN if [ "$( go env GOHOSTARCH ;)" = "amd64" ] ; then apt-get update \
 && apt-get install --no-install-recommends crossbuild-essential-armhf=12.3 crossbuild-essential-arm64=12.3 crossbuild-essential-armel=12.3 -y ; fi
FROM cross-${CROSS} AS dev-base
FROM dev-base AS runtime-dev-cross-false
RUN apt-get update \
 && apt-get install --no-install-recommends libapparmor-dev=2.11.0-3+deb9u2 libseccomp-dev=2.3.1-2.1+deb9u1 -y
FROM cross-true AS runtime-dev-cross-true
#   These crossbuild packages rely on gcc-<arch>, but this doesn't want to install
#   on non-amd64 systems.
#   Additionally, the crossbuild-amd64 is currently only on debian:buster, so
#   other architectures cannnot crossbuild amd64.
RUN if [ "$( go env GOHOSTARCH ;)" = "amd64" ] ; then apt-get update \
 && apt-get install --no-install-recommends libseccomp-dev:armhf libseccomp-dev:arm64 libseccomp-dev:armel libapparmor-dev:armhf libapparmor-dev:arm64 libapparmor-dev:armel libapparmor-dev=2.11.0-3+deb9u2 libseccomp-dev=2.3.1-2.1+deb9u1 -y ; fi
FROM runtime-dev-cross-${CROSS} AS runtime-dev
FROM base AS tomlv
ENV INSTALL_BINARY_NAME="tomlv"
COPY hack/dockerfile/install/install.sh ./install.sh
COPY hack/dockerfile/install/$INSTALL_BINARY_NAME.installer ./
RUN PREFIX=/build ./install.sh $INSTALL_BINARY_NAME
FROM base AS vndr
ENV INSTALL_BINARY_NAME="vndr"
COPY hack/dockerfile/install/install.sh ./install.sh
COPY hack/dockerfile/install/$INSTALL_BINARY_NAME.installer ./
RUN PREFIX=/build ./install.sh $INSTALL_BINARY_NAME
FROM dev-base AS containerd
RUN apt-get update \
 && apt-get install --no-install-recommends btrfs-tools=4.7.3-1 -y
ENV INSTALL_BINARY_NAME="containerd"
COPY hack/dockerfile/install/install.sh ./install.sh
COPY hack/dockerfile/install/$INSTALL_BINARY_NAME.installer ./
RUN PREFIX=/build ./install.sh $INSTALL_BINARY_NAME
FROM dev-base AS proxy
ENV INSTALL_BINARY_NAME="proxy"
COPY hack/dockerfile/install/install.sh ./install.sh
COPY hack/dockerfile/install/$INSTALL_BINARY_NAME.installer ./
RUN PREFIX=/build ./install.sh $INSTALL_BINARY_NAME
FROM base AS gometalinter
ENV INSTALL_BINARY_NAME="gometalinter"
COPY hack/dockerfile/install/install.sh ./install.sh
COPY hack/dockerfile/install/$INSTALL_BINARY_NAME.installer ./
RUN PREFIX=/build ./install.sh $INSTALL_BINARY_NAME
FROM dev-base AS dockercli
ENV INSTALL_BINARY_NAME="dockercli"
COPY hack/dockerfile/install/install.sh ./install.sh
COPY hack/dockerfile/install/$INSTALL_BINARY_NAME.installer ./
RUN PREFIX=/build ./install.sh $INSTALL_BINARY_NAME
FROM runtime-dev AS runc
ENV INSTALL_BINARY_NAME="runc"
COPY hack/dockerfile/install/install.sh ./install.sh
COPY hack/dockerfile/install/$INSTALL_BINARY_NAME.installer ./
RUN PREFIX=/build ./install.sh $INSTALL_BINARY_NAME
FROM dev-base AS tini
RUN apt-get update \
 && apt-get install --no-install-recommends cmake=3.7.2-1 vim-common=2:8.0.0197-4+deb9u7 -y
COPY hack/dockerfile/install/install.sh ./install.sh
ENV INSTALL_BINARY_NAME="tini"
COPY hack/dockerfile/install/$INSTALL_BINARY_NAME.installer ./
RUN PREFIX=/build ./install.sh $INSTALL_BINARY_NAME
FROM dev-base AS rootlesskit
ENV INSTALL_BINARY_NAME="rootlesskit"
COPY hack/dockerfile/install/install.sh ./install.sh
COPY hack/dockerfile/install/$INSTALL_BINARY_NAME.installer ./
RUN PREFIX=/build/ ./install.sh $INSTALL_BINARY_NAME
COPY ./contrib/dockerd-rootless.sh /build
#   TODO: Some of this is only really needed for testing, it would be nice to split this up
FROM runtime-dev AS dev
RUN groupadd -r docker
RUN useradd --create-home --gid docker unprivilegeduser
#   Let us use a .bashrc file
RUN ln -sfv /go/src/github.com/docker/docker/.bashrc ~/.bashrc
#   Activate bash completion and include Docker's completion if mounted with DOCKER_BASH_COMPLETION_PATH
RUN echo "source /usr/share/bash-completion/bash_completion" >> /etc/bash.bashrc
RUN ln -s /usr/local/completion/bash/docker /etc/bash_completion.d/docker
RUN ldconfig
#   This should only install packages that are specifically needed for the dev environment and nothing else
#   Do you really need to add another package here? Can it be done in a different build stage?
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor=2.11.0-3+deb9u2 aufs-tools=1:4.1+20161219-1 bash-completion=1:2.1-4.3 btrfs-tools=4.7.3-1 iptables=1.6.0+snapshot20161117-6 jq=1.5+dfsg-1.3 libcap2-bin=1:2.25-1 libdevmapper-dev=2:1.02.137-2 libffi-dev=3.2.1-6 libssl-dev=1.1.0l-1~deb9u6 libudev-dev=232-25+deb9u14 libsystemd-dev=232-25+deb9u14 binutils-mingw-w64=2.27.90.20161231-1+7.4 g++-mingw-w64-x86-64=6.3.0-18+19.3+b3 net-tools=1.60+git20161116.90da8a0-1 pigz=2.3.4-1 python-backports.ssl-match-hostname=3.5.0.1-1 python-dev=2.7.13-2 python-cffi=1.9.1-2 python-mock=2.0.0-3 python-pip=9.0.1-2+deb9u2 python-requests=2.12.4-1 python-setuptools=33.1.1-1 python-websocket=0.37.0-2 python-wheel=0.29.0-2 thin-provisioning-tools=0.6.1-4+b1 vim=2:8.0.0197-4+deb9u7 vim-common=2:8.0.0197-4+deb9u7 xfsprogs=4.9.0+nmu1 zip=3.0-11+b1 bzip2=1.0.6-8.1 xz-utils=5.2.2-1.2+deb9u1 libprotobuf-c1=1.2.1-2 libnet1=1.1.6+dfsg-3 libnl-3-200=3.2.27-2 -y
COPY --from=swagger /build/swagger* /usr/local/bin/
COPY --from=frozen-images /build/ /docker-frozen-images
COPY --from=gometalinter /build/ /usr/local/bin/
COPY --from=tomlv /build/ /usr/local/bin/
COPY --from=vndr /build/ /usr/local/bin/
COPY --from=tini /build/ /usr/local/bin/
COPY --from=runc /build/ /usr/local/bin/
COPY --from=containerd /build/ /usr/local/bin/
COPY --from=proxy /build/ /usr/local/bin/
COPY --from=dockercli /build/ /usr/local/cli
COPY --from=registry /build/registry* /usr/local/bin/
COPY --from=criu /build/ /usr/local/
COPY --from=docker-py /build/ /docker-py
#   TODO: This is for the docker-py tests, which shouldn't really be needed for
#   this image, but currently CI is expecting to run this image. This should be
#   split out into a separate image, including all the `python-*` deps installed
#   above.
RUN cd /docker-py \
 && pip install docker-pycreds==0.4.0 \
 && pip install paramiko==2.4.2 \
 && pip install yamllint==1.5.0 \
 && pip install -r test-requirements.txt
COPY --from=rootlesskit /build/ /usr/local/bin/
COPY --from=djs55/vpnkit@sha256:e508a17cfacc8fd39261d5b4e397df2b953690da577e2c987a47630cd0c42f8e /vpnkit /usr/local/bin/vpnkit.x86_64
ENV PATH="/usr/local/cli:$PATH"
ENV DOCKER_BUILDTAGS="apparmor seccomp selinux"
#   Options for hack/validate/gometalinter
ENV GOMETALINTER_OPTS="--deadline=2m"
WORKDIR /go/src/github.com/docker/docker
VOLUME /var/lib/docker
#   Wrap all commands in the "docker-in-docker" script to allow nested containers
ENTRYPOINT ["hack/dind"]
FROM dev AS final
#   Upload docker source
COPY . /go/src/github.com/docker/docker
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
