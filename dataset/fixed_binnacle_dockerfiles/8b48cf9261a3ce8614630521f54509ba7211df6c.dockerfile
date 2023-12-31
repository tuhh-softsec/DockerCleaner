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
#   docker run --privileged docker hack/make.sh test
#
#   # Publish a release:
#   docker run --privileged \
#    -e AWS_S3_BUCKET=baz \
#    -e AWS_ACCESS_KEY=foo \
#    -e AWS_SECRET_KEY=bar \
#    -e GPG_PASSPHRASE=gloubiboulga \
#    docker hack/release.sh
#
#   Note: AppArmor used to mess with privileged mode, but this is no longer
#   the case. Therefore, you don't have to disable it anymore.
#
FROM armhf/ubuntu:trusty
#   Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor=2.10.95-0ubuntu2.6~14.04.4 aufs-tools=1:3.2+20130722-1.1 automake=1:1.14.1-2ubuntu1 bash-completion=1:2.1-4ubuntu0.2 btrfs-tools=3.12-1ubuntu0.2 build-essential=11.6ubuntu6 createrepo=0.10.3-1 curl=7.35.0-1ubuntu2.20 dpkg-sig=0.13.1+nmu1 git=1:1.9.1-1ubuntu0.10 iptables=1.4.21-1ubuntu1 jq=1.3-1.1ubuntu1.1 net-tools=1.60-25ubuntu2.1 libapparmor-dev=2.10.95-0ubuntu2.6~14.04.4 libcap-dev=1:2.24-0ubuntu2 libltdl-dev=2.4.2-1.7ubuntu1 libsqlite3-dev=3.8.2-1ubuntu2.2 libsystemd-journal-dev=204-5ubuntu20.31 libtool=2.4.2-1.7ubuntu1 mercurial=2.8.2-1ubuntu1.4 pkg-config=0.26-1ubuntu4 python-dev=2.7.5-5ubuntu3 python-mock=1.0.1-3 python-pip=1.5.4-1ubuntu4 python-websocket=0.12.0-1ubuntu2 xfsprogs=3.1.9ubuntu2.1 tar=1.27.1-1ubuntu0.1 -y
#   Get lvm2 source for compiling statically
ENV LVM2_VERSION="2.02.103"
RUN mkdir -p /usr/local/lvm2 \
 && curl -fsSL "https://mirrors.kernel.org/sourceware/lvm2/LVM2.${LVM2_VERSION}.tgz" | tar -xzC /usr/local/lvm2 --strip-components=1
#   see https://git.fedorahosted.org/cgit/lvm2.git/refs/tags for release tags
#   Compile and install lvm2
RUN cd /usr/local/lvm2 \
 && ./configure --build="$( gcc -print-multiarch ;)" --enable-static_link \
 && make device-mapper \
 && make install_device-mapper
#   see https://git.fedorahosted.org/cgit/lvm2.git/tree/INSTALL
#   Install Go
#  ENV GO_VERSION 1.5.3
#   TODO update GO_TOOLS_COMMIT below when this updates to 1.5+
ENV GO_VERSION="1.4.3"
RUN curl -fsSL "https://github.com/hypriot/golang-armbuilds/releases/download/v${GO_VERSION}/go${GO_VERSION}.linux-armv7.tar.gz" | tar -xzC /usr/local
#   temporarily using Hypriot's tarballs while we wait for official 1.6+
#  RUN curl -fsSL https://golang.org/dl/go${GO_VERSION}.linux-arm6.tar.gz \
#  		| tar -xzC /usr/local
ENV PATH="/go/bin:/usr/local/go/bin:$PATH"
ENV GOPATH="/go:/go/src/github.com/docker/docker/vendor"
#   we're building for armhf, which is ARMv7, so let's be explicit about that
ENV GOARCH="arm"
ENV GOARM="7"
#   This has been commented out and kept as reference because we don't support compiling with older Go anymore.
#   ENV GOFMT_VERSION 1.3.3
#   RUN curl -sSL https://storage.googleapis.com/golang/go${GOFMT_VERSION}.$(go env GOOS)-$(go env GOARCH).tar.gz | tar -C /go/bin -xz --strip-components=2 go/bin/gofmt
#  ENV GO_TOOLS_COMMIT 823804e1ae08dbb14eb807afc7db9993bc9e3cc3
#   TODO update this sha when we upgrade to Go 1.5+
ENV GO_TOOLS_COMMIT="069d2f3bcb68257b627205f0486d6cc69a231ff9"
#   Grab Go's cover tool for dead-simple code coverage testing
#   Grab Go's vet tool for examining go code to find suspicious constructs
#   and help prevent errors that the compiler might not catch
RUN git clone https://github.com/golang/tools.git /go/src/golang.org/x/tools \
 && (cd /go/src/golang.org/x/tools \
 && git checkout -q $GO_TOOLS_COMMIT ) \
 && go install -v golang.org/x/tools/cmd/cover \
 && go install -v golang.org/x/tools/cmd/vet
#   Grab Go's lint tool
#  ENV GO_LINT_COMMIT 32a87160691b3c96046c0c678fe57c5bef761456
#   TODO update this sha when we upgrade to Go 1.5+
ENV GO_LINT_COMMIT="f42f5c1c440621302702cb0741e9d2ca547ae80f"
RUN git clone https://github.com/golang/lint.git /go/src/github.com/golang/lint \
 && (cd /go/src/github.com/golang/lint \
 && git checkout -q $GO_LINT_COMMIT ) \
 && go install -v github.com/golang/lint/golint
#   install seccomp: the version shipped in trusty is too old
ENV SECCOMP_VERSION="2.2.3"
RUN set -x \
 && export SECCOMP_PATH="$( mktemp -d ;)" \
 && curl -fsSL "https://github.com/seccomp/libseccomp/releases/download/v${SECCOMP_VERSION}/libseccomp-${SECCOMP_VERSION}.tar.gz" | tar -xzC "$SECCOMP_PATH" --strip-components=1 \
 && (cd "$SECCOMP_PATH" \
 && ./configure --prefix=/usr/local \
 && make \
 && make install \
 && ldconfig ) \
 && rm -rf "$SECCOMP_PATH"
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
#   Install notary server
ENV NOTARY_VERSION="docker-v1.10-5"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/notary.git "$GOPATH/src/github.com/docker/notary" \
 && (cd "$GOPATH/src/github.com/docker/notary" \
 && git checkout -q "$NOTARY_VERSION" ) \
 && GOPATH="$GOPATH/src/github.com/docker/notary/Godeps/_workspace:$GOPATH" go build -o /usr/local/bin/notary-server github.com/docker/notary/cmd/notary-server \
 && GOPATH="$GOPATH/src/github.com/docker/notary/Godeps/_workspace:$GOPATH" go build -o /usr/local/bin/notary github.com/docker/notary/cmd/notary \
 && rm -rf "$GOPATH"
#   Get the "docker-py" source so we can run their integration tests
ENV DOCKER_PY_COMMIT="e2878cbcc3a7eef99917adc1be252800b0e41ece"
RUN git clone https://github.com/docker/docker-py.git /docker-py \
 && cd /docker-py \
 && git checkout -q $DOCKER_PY_COMMIT \
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
RUN ./contrib/download-frozen-image-v2.sh /docker-frozen-images armhf/buildpack-deps:jessie@sha256:ca6cce8e5bf5c952129889b5cc15cd6aa8d995d77e55e3749bbaadae50e476cb armhf/busybox:latest@sha256:d98a7343ac750ffe387e3d514f8521ba69846c216778919b01414b8617cfb3d4 armhf/debian:jessie@sha256:4a2187483f04a84f9830910fe3581d69b3c985cc045d9f01d8e2f3795b28107b armhf/hello-world:latest@sha256:161dcecea0225975b2ad5f768058212c1e0d39e8211098666ffa1ac74cfb7791
#   see also "hack/make/.ensure-frozen-images" (which needs to be updated any time this list is)
#   Download man page generator
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone --depth 1 -b v1.0.4 https://github.com/cpuguy83/go-md2man.git "$GOPATH/src/github.com/cpuguy83/go-md2man" \
 && git clone --depth 1 -b v1.4 https://github.com/russross/blackfriday.git "$GOPATH/src/github.com/russross/blackfriday" \
 && go get -v -d github.com/cpuguy83/go-md2man \
 && go build -v -o /usr/local/bin/go-md2man github.com/cpuguy83/go-md2man \
 && rm -rf "$GOPATH"
#   Download toml validator
ENV TOMLV_COMMIT="9baf8a8a9f2ed20a8e54160840c492f937eeaf9a"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/BurntSushi/toml.git "$GOPATH/src/github.com/BurntSushi/toml" \
 && (cd "$GOPATH/src/github.com/BurntSushi/toml" \
 && git checkout -q "$TOMLV_COMMIT" ) \
 && go build -v -o /usr/local/bin/tomlv github.com/BurntSushi/toml/cmd/tomlv \
 && rm -rf "$GOPATH"
#   Build/install the tool for embedding resources in Windows binaries
ENV RSRC_VERSION="v2"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone --depth 1 -b "$RSRC_VERSION" https://github.com/akavel/rsrc.git "$GOPATH/src/github.com/akavel/rsrc" \
 && go build -v -o /usr/local/bin/rsrc github.com/akavel/rsrc \
 && rm -rf "$GOPATH"
#   Wrap all commands in the "docker-in-docker" script to allow nested containers
ENTRYPOINT ["hack/dind"]
#   Upload docker source
COPY . /go/src/github.com/docker/docker
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
