#   This file describes the standard way to build Docker on aarch64, using docker
#
#   Usage:
#
#   # Assemble the full dev environment. This is slow the first time.
#   docker build -t docker -f Dockerfile.aarch64 .
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
FROM arm64v8/debian:stretch
#   allow replacing httpredir or deb mirror
ARG APT_MIRROR=deb.debian.org
RUN sed -ri "s/(httpredir|deb).debian.org/$APT_MIRROR/g" /etc/apt/sources.list
#   Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor=2.11.0-3+deb9u2 apt-utils=1.4.11 aufs-tools=1:4.1+20161219-1 automake=1:1.15-6 bash-completion=1:2.1-4.3 bsdmainutils=9.0.12+nmu1 btrfs-tools=4.7.3-1 build-essential=12.3 cmake=3.7.2-1 createrepo=0.10.3-1 curl=7.52.1-5+deb9u16 dpkg-sig=0.13.1+nmu4 gcc=4:6.3.0-4 git=1:2.11.0-3+deb9u7 iptables=1.6.0+snapshot20161117-6 jq=1.5+dfsg-1.3 less=481-2.1 libapparmor-dev=2.11.0-3+deb9u2 libcap-dev=1:2.25-1 libdevmapper-dev=2:1.02.137-2 libnl-3-dev=3.2.27-2 libprotobuf-c0-dev libprotobuf-dev=3.0.0-9 libseccomp-dev=2.3.1-2.1+deb9u1 libsystemd-dev=232-25+deb9u14 libtool=2.4.6-2 libudev-dev=232-25+deb9u14 mercurial=4.0-1+deb9u2 net-tools=1.60+git20161116.90da8a0-1 pkg-config=0.29-4+b1 protobuf-compiler=3.0.0-9 protobuf-c-compiler=1.2.1-2 python-backports.ssl-match-hostname=3.5.0.1-1 python-dev=2.7.13-2 python-mock=2.0.0-3 python-pip=9.0.1-2+deb9u2 python-requests=2.12.4-1 python-setuptools=33.1.1-1 python-websocket=0.37.0-2 python-wheel=0.29.0-2 tar=1.29b-1.1+deb9u1 thin-provisioning-tools=0.6.1-4+b1 vim=2:8.0.0197-4+deb9u7 vim-common=2:8.0.0197-4+deb9u7 xfsprogs=4.9.0+nmu1 zip=3.0-11+b1 -y
#   Install Go
#   IMPORTANT: When updating this please note that stdlib archive/tar pkg is vendored
ENV GO_VERSION="1.8.5"
RUN curl -fsSL "https://golang.org/dl/go${GO_VERSION}.linux-arm64.tar.gz" | tar -xzC /usr/local
ENV PATH="/go/bin:/usr/local/go/bin:$PATH"
ENV GOPATH="/go"
#   Only install one version of the registry, because old version which support
#   schema1 manifests is not working on ARM64, we should skip integration-cli
#   tests for schema1 manifests on ARM64.
ENV REGISTRY_COMMIT="47a064d4195a9b56133891bbb13620c3ac83a827"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/distribution.git "$GOPATH/src/github.com/docker/distribution" \
 && (cd "$GOPATH/src/github.com/docker/distribution" \
 && git checkout -q "$REGISTRY_COMMIT" ) \
 && GOPATH="$GOPATH/src/github.com/docker/distribution/Godeps/_workspace:$GOPATH" go build -o /usr/local/bin/registry-v2 github.com/docker/distribution/cmd/registry \
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
ENV DOCKER_PY_COMMIT="1d6b5b203222ba5df7dedfcd1ee061a452f99c8a"
#   To run integration tests docker-pycreds is required.
RUN git clone https://github.com/docker/docker-py.git /docker-py \
 && cd /docker-py \
 && git checkout -q $DOCKER_PY_COMMIT \
 && pip install docker-pycreds==0.2.1 \
 && pip install -r test-requirements.txt
#   Install yamllint for validating swagger.yaml
RUN pip install yamllint==1.5.0
#   Install go-swagger for validating swagger.yaml
ENV GO_SWAGGER_COMMIT="c28258affb0b6251755d92489ef685af8d4ff3eb"
RUN git clone https://github.com/go-swagger/go-swagger.git /go/src/github.com/go-swagger/go-swagger \
 && (cd /go/src/github.com/go-swagger/go-swagger \
 && git checkout -q $GO_SWAGGER_COMMIT ) \
 && go install -v github.com/go-swagger/go-swagger/cmd/swagger
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
RUN ./contrib/download-frozen-image-v2.sh /docker-frozen-images aarch64/buildpack-deps:jessie@sha256:107f4a96837ed89c493fc205cd28508ed0b6b680b4bf3e514e9f0fa0f6667b77 aarch64/busybox:latest@sha256:5a06b8b2fdf22dd1f4085c6c3efd23ee99af01b2d668d286bc4be6d8baa10efb aarch64/debian:jessie@sha256:e6f90b568631705bd5cb27490977378ba762792b38d47c91c4da7a539f63079a aarch64/hello-world:latest@sha256:bd1722550b97668b23ede297abf824d4855f4d9f600dab7b4db1a963dae7ec9e
#   See also ensureFrozenImagesLinux() in "integration-cli/fixtures_linux_daemon_test.go" (which needs to be updated when adding images to this list)
#   Install tomlv, vndr, runc, containerd, tini, docker-proxy
#   Please edit hack/dockerfile/install-binaries.sh to update them.
COPY hack/dockerfile/binaries-commits /tmp/binaries-commits
COPY hack/dockerfile/install-binaries.sh /tmp/install-binaries.sh
RUN /tmp/install-binaries.sh tomlv vndr runc containerd tini proxy dockercli gometalinter
ENV PATH="/usr/local/cli:$PATH"
#   Wrap all commands in the "docker-in-docker" script to allow nested containers
ENTRYPOINT ["hack/dind"]
#   Options for hack/validate/gometalinter
ENV GOMETALINTER_OPTS="--deadline 4m -j2"
#   Upload docker source
COPY . /go/src/github.com/docker/docker
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
