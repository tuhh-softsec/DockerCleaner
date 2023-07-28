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
#   docker run --privileged docker hack/make.sh test-unit test-integration-cli test-docker-py
#
#   Note: AppArmor used to mess with privileged mode, but this is no longer
#   the case. Therefore, you don't have to disable it anymore.
#
FROM aarch64/ubuntu:xenial
#   Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor=2.10.95-0ubuntu2.11 aufs-tools=1:3.2+20130722-1.1ubuntu1 automake=1:1.15-4ubuntu1 bash-completion=1:2.1-4.2ubuntu1.1 btrfs-tools=4.4-1ubuntu1.1 build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 createrepo=0.10.3-1 curl=7.47.0-1ubuntu2.19 dpkg-sig=0.13.1+nmu2 g++=4:5.3.1-1ubuntu1 gcc=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 iptables=1.6.0-2ubuntu3 jq=1.5+dfsg-1ubuntu0.1 libapparmor-dev=2.10.95-0ubuntu2.11 libc6-dev=2.23-0ubuntu11.3 libcap-dev=1:2.24-12 libsystemd-dev=229-4ubuntu21.31 libyaml-dev=0.1.6-3 mercurial=3.7.3-1ubuntu1.2 net-tools=1.60-26ubuntu1 parallel=20161222-1~ubuntu0.16.04.1 pkg-config=0.29.1-0ubuntu1 python-dev=2.7.12-1~16.04 python-mock=1.3.0-2.1ubuntu1 python-pip=8.1.1-2ubuntu0.6 python-setuptools=20.7.0-1 python-websocket=0.18.0-2 golang-go=2:1.6-1ubuntu4 iproute2=4.3.0-1ubuntu3.16.04.5 iputils-ping=3:20121221-5ubuntu2 vim-common=2:7.4.1689-3ubuntu1.5 -y
#   Get lvm2 source for compiling statically
ENV LVM2_VERSION="2.02.103"
RUN mkdir -p /usr/local/lvm2 \
 && curl -fsSL "https://mirrors.kernel.org/sourceware/lvm2/LVM2.${LVM2_VERSION}.tgz" | tar -xzC /usr/local/lvm2 --strip-components=1
#   See https://git.fedorahosted.org/cgit/lvm2.git/refs/tags for release tags
#   Fix platform enablement in lvm2 to support aarch64 properly
RUN set -e \
 && for f in config.guess config.sub; do curl -fsSL -o "/usr/local/lvm2/autoconf/$f" "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=$f;hb=HEAD" ; done
#   "arch.c:78:2: error: #error the arch code needs to know about your machine type"
#   Compile and install lvm2
RUN cd /usr/local/lvm2 \
 && ./configure --build="$( gcc -print-multiarch ;)" --enable-static_link \
 && make device-mapper \
 && make install_device-mapper
#   See https://git.fedorahosted.org/cgit/lvm2.git/tree/INSTALL
#   Install seccomp: the version shipped upstream is too old
ENV SECCOMP_VERSION="2.3.2"
RUN set -x \
 && export SECCOMP_PATH="$( mktemp -d ;)" \
 && curl -fsSL "https://github.com/seccomp/libseccomp/releases/download/v${SECCOMP_VERSION}/libseccomp-${SECCOMP_VERSION}.tar.gz" | tar -xzC "$SECCOMP_PATH" --strip-components=1 \
 && (cd "$SECCOMP_PATH" \
 && ./configure --prefix=/usr/local \
 && make \
 && make install \
 && ldconfig ) \
 && rm -rf "$SECCOMP_PATH"
#   Install Go
#   We don't have official binary golang 1.7.5 tarballs for ARM64, either for Go or
#   bootstrap, so we use golang-go (1.6) as bootstrap to build Go from source code.
#   We don't use the official ARMv6 released binaries as a GOROOT_BOOTSTRAP, because
#   not all ARM64 platforms support 32-bit mode. 32-bit mode is optional for ARMv8.
#   IMPORTANT: When updating this please note that stdlib archive/tar pkg is vendored
ENV GO_VERSION="1.8.3"
RUN mkdir /usr/src/go \
 && curl -fsSL https://golang.org/dl/go${GO_VERSION}.src.tar.gz | tar -v -C /usr/src/go -xz --strip-components=1 \
 && cd /usr/src/go/src \
 && GOOS=linux GOARCH=arm64 GOROOT_BOOTSTRAP="$( go env GOROOT ;)" ./make.bash
ENV PATH="/go/bin:/usr/src/go/bin:$PATH"
ENV GOPATH="/go"
#   Dependency for golint
ENV GO_TOOLS_COMMIT="823804e1ae08dbb14eb807afc7db9993bc9e3cc3"
RUN git clone https://github.com/golang/tools.git /go/src/golang.org/x/tools \
 && (cd /go/src/golang.org/x/tools \
 && git checkout -q $GO_TOOLS_COMMIT )
#   Grab Go's lint tool
ENV GO_LINT_COMMIT="32a87160691b3c96046c0c678fe57c5bef761456"
RUN git clone https://github.com/golang/lint.git /go/src/github.com/golang/lint \
 && (cd /go/src/github.com/golang/lint \
 && git checkout -q $GO_LINT_COMMIT ) \
 && go install -v github.com/golang/lint/golint
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
ENV DOCKER_PY_COMMIT="a962578e515185cf06506050b2200c0b81aa84ef"
#   Before running the integration tests conftest.py is
#   loaded which results in loads auth.py that
#   imports the docker-pycreds module.
RUN git clone https://github.com/docker/docker-py.git /docker-py \
 && cd /docker-py \
 && git checkout -q $DOCKER_PY_COMMIT \
 && pip install wheel==0.40.0 \
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
RUN /tmp/install-binaries.sh tomlv vndr runc containerd tini proxy dockercli
ENV PATH="/usr/local/cli:$PATH"
#   Wrap all commands in the "docker-in-docker" script to allow nested containers
ENTRYPOINT ["hack/dind"]
#   Upload docker source
COPY . /go/src/github.com/docker/docker
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
