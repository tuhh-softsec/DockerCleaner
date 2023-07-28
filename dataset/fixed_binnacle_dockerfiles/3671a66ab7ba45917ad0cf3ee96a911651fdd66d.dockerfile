#   This file describes the standard way to build Docker on ARMv7, using docker
#
#   Usage:
#
#   # Assemble the full dev environment. This is slow the first time.
#   docker build -t docker -f Dockerfile.armhf .
#
#   # Mount your source in an interactive container for quick testing:
#   docker run -v `pwd`:/go/src/github.com/docker/docker --privileged -i -t docker bash
#
#   # Run the test suite:
#   docker run --privileged docker hack/make.sh test-unit test-integration test-docker-py
#
#   Note: AppArmor used to mess with privileged mode, but this is no longer
#   the case. Therefore, you don't have to disable it anymore.
#
FROM arm32v7/debian:stretch
#   allow replacing httpredir or deb mirror
ARG APT_MIRROR=deb.debian.org
RUN sed -ri "s/(httpredir|deb).debian.org/$APT_MIRROR/g" /etc/apt/sources.list
#   Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor=2.11.0-3+deb9u2 aufs-tools=1:4.1+20161219-1 automake=1:1.15-6 bash-completion=1:2.1-4.3 btrfs-tools=4.7.3-1 build-essential=12.3 createrepo=0.10.3-1 curl=7.52.1-5+deb9u16 cmake=3.7.2-1 dpkg-sig=0.13.1+nmu4 git=1:2.11.0-3+deb9u7 iptables=1.6.0+snapshot20161117-6 jq=1.5+dfsg-1.3 net-tools=1.60+git20161116.90da8a0-1 libapparmor-dev=2.11.0-3+deb9u2 libcap-dev=1:2.25-1 libdevmapper-dev=2:1.02.137-2 libseccomp-dev=2.3.1-2.1+deb9u1 libsystemd-dev=232-25+deb9u14 libtool=2.4.6-2 libudev-dev=232-25+deb9u14 mercurial=4.0-1+deb9u2 pkg-config=0.29-4+b1 python-backports.ssl-match-hostname=3.5.0.1-1 python-dev=2.7.13-2 python-mock=2.0.0-3 python-pip=9.0.1-2+deb9u2 python-requests=2.12.4-1 python-setuptools=33.1.1-1 python-websocket=0.37.0-2 python-wheel=0.29.0-2 xfsprogs=4.9.0+nmu1 tar=1.29b-1.1+deb9u1 thin-provisioning-tools=0.6.1-4+b1 vim-common=2:8.0.0197-4+deb9u7 -y \
 && pip install awscli==1.10.15
#   Install Go
#   IMPORTANT: When updating this please note that stdlib archive/tar pkg is vendored
ENV GO_VERSION="1.8.4"
RUN curl -fsSL "https://golang.org/dl/go${GO_VERSION}.linux-armv6l.tar.gz" | tar -xzC /usr/local
ENV PATH="/go/bin:/usr/local/go/bin:$PATH"
ENV GOPATH="/go"
#   We're building for armhf, which is ARMv7, so let's be explicit about that
ENV GOARCH="arm"
ENV GOARM="7"
#   Install two versions of the registry. The first is an older version that
#   only supports schema1 manifests. The second is a newer version that supports
#   both. This allows integration-cli tests to cover push/pull with both schema1
#   and schema2 manifests.
ENV REGISTRY_COMMIT_SCHEMA1="ec87e9b6971d831f0eff752ddb54fb64693e51cd"
ENV REGISTRY_COMMIT="cb08de17d74bef86ce6c5abe8b240e282f5750be"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/distribution.git "$GOPATH/src/github.com/docker/distribution" \
 && (cd "$GOPATH/src/github.com/docker/distribution" \
 && git checkout -q "$REGISTRY_COMMIT" ) \
 && GOPATH="$GOPATH/src/github.com/docker/distribution/Godeps/_workspace:$GOPATH" go build -o /usr/local/bin/registry-v2 github.com/docker/distribution/cmd/registry \
 && (cd "$GOPATH/src/github.com/docker/distribution" \
 && git checkout -q "$REGISTRY_COMMIT_SCHEMA1" ) \
 && GOPATH="$GOPATH/src/github.com/docker/distribution/Godeps/_workspace:$GOPATH" go build -o /usr/local/bin/registry-v2-schema1 github.com/docker/distribution/cmd/registry \
 && rm -rf "$GOPATH"
#   Install notary and notary-server
ENV NOTARY_VERSION="v0.5.0"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/notary.git "$GOPATH/src/github.com/docker/notary" \
 && (cd "$GOPATH/src/github.com/docker/notary" \
 && git checkout -q "$NOTARY_VERSION" ) \
 && GOPATH="$GOPATH/src/github.com/docker/notary/vendor:$GOPATH" go build -o /usr/local/bin/notary-server github.com/docker/notary/cmd/notary-server \
 && GOPATH="$GOPATH/src/github.com/docker/notary/vendor:$GOPATH" go build -o /usr/local/bin/notary github.com/docker/notary/cmd/notary \
 && rm -rf "$GOPATH"
#   Get the "docker-py" source so we can run their integration tests
ENV DOCKER_PY_COMMIT="a962578e515185cf06506050b2200c0b81aa84ef"
#   To run integration tests docker-pycreds is required.
RUN git clone https://github.com/docker/docker-py.git /docker-py \
 && cd /docker-py \
 && git checkout -q $DOCKER_PY_COMMIT \
 && pip install docker-pycreds==0.2.1 \
 && pip install -r test-requirements.txt
#   Set user.email so crosbymichael's in-container merge commits go smoothly
RUN git config --global user.email 'docker-dummy@example.com'
#   Add an unprivileged user to be used for tests which need it
RUN groupadd -r docker
RUN useradd --create-home --gid docker unprivilegeduser
VOLUME /var/lib/docker
WORKDIR /go/src/github.com/docker/docker
ENV DOCKER_BUILDTAGS="apparmor seccomp selinux"
#   Let us use a .bashrc file
RUN ln -sfv $PWD/.bashrc ~/.bashrc
#   Register Docker's bash completion.
RUN ln -sv $PWD/contrib/completion/bash/docker /etc/bash_completion.d/docker
#   Get useful and necessary Hub images so we can "docker load" locally instead of pulling
COPY contrib/download-frozen-image-v2.sh /go/src/github.com/docker/docker/contrib/
RUN ./contrib/download-frozen-image-v2.sh /docker-frozen-images armhf/buildpack-deps:jessie@sha256:eb2dad77ef53e88d94c3c83862d315c806ea1ca49b6e74f4db362381365ce489 armhf/busybox:latest@sha256:016a1e149d2acc2a3789a160dfa60ce870794eea27ad5e96f7a101970e5e1689 armhf/debian:jessie@sha256:ac59fa18b28d0ef751eabb5ba4c4b5a9063f99398bae2f70495aa8ed6139b577 armhf/hello-world:latest@sha256:9701edc932223a66e49dd6c894a11db8c2cf4eccd1414f1ec105a623bf16b426
#   See also ensureFrozenImagesLinux() in "integration-cli/fixtures_linux_daemon_test.go" (which needs to be updated when adding images to this list)
#   Install tomlv, vndr, runc, containerd, tini, docker-proxy
#   Please edit hack/dockerfile/install-binaries.sh to update them.
COPY hack/dockerfile/binaries-commits /tmp/binaries-commits
COPY hack/dockerfile/install-binaries.sh /tmp/install-binaries.sh
RUN /tmp/install-binaries.sh tomlv vndr runc containerd tini proxy dockercli gometalinter
ENV PATH="/usr/local/cli:$PATH"
ENTRYPOINT ["hack/dind"]
#   Upload docker source
COPY . /go/src/github.com/docker/docker
#   Options for hack/validate/gometalinter
ENV GOMETALINTER_OPTS="--deadline 10m -j2"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
