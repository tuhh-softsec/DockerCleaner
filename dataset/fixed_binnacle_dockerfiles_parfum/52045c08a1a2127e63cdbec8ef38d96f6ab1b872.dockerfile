#  This file describes the standard way to build Docker on aarch64, using docker
#
#  Usage:
#
#  # Assemble the full dev environment. This is slow the first time.
#  docker build -t docker -f Dockerfile.aarch64 .
#
#  # Mount your source in an interactive container for quick testing:
#  docker run -v `pwd`:/go/src/github.com/docker/docker --privileged -i -t docker bash
#
#  # Run the test suite:
#  docker run --privileged docker hack/make.sh test
#
#  Note: AppArmor used to mess with privileged mode, but this is no longer
#  the case. Therefore, you don't have to disable it anymore.
#
FROM aarch64/ubuntu:wily
#  Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor aufs-tools automake bash-completion btrfs-tools build-essential createrepo curl dpkg-sig g++ gcc git iptables jq libapparmor-dev libc6-dev libcap-dev libsqlite3-dev libsystemd-dev mercurial net-tools parallel pkg-config python-dev python-mock python-pip python-websocket gccgo -y
#  Install armhf loader to use armv6 binaries on armv8
RUN dpkg --add-architecture armhf \
 && apt-get update \
 && apt-get install --no-install-recommends libc6:armhf -y
#  Get lvm2 source for compiling statically
ENV LVM2_VERSION="2.02.103"
RUN mkdir -p /usr/local/lvm2 \
 && curl -fsSL "https://mirrors.kernel.org/sourceware/lvm2/LVM2.${LVM2_VERSION}.tgz" | tar -xzC /usr/local/lvm2 --strip-components=1
#  see https://git.fedorahosted.org/cgit/lvm2.git/refs/tags for release tags
#  fix platform enablement in lvm2 to support aarch64 properly
RUN set -e \
 && for f in config.guess config.sub; do curl -fsSL -o "/usr/local/lvm2/autoconf/$f" "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=$f;hb=HEAD" ; done
#  "arch.c:78:2: error: #error the arch code needs to know about your machine type"
#  Compile and install lvm2
RUN cd /usr/local/lvm2 \
 && ./configure --build="$( gcc -print-multiarch ;)" --enable-static_link \
 && make device-mapper \
 && make install_device-mapper
#  see https://git.fedorahosted.org/cgit/lvm2.git/tree/INSTALL
#  install seccomp: the version shipped in trusty is too old
ENV SECCOMP_VERSION="2.3.0"
RUN set -x \
 && export SECCOMP_PATH="$( mktemp -d ;)" \
 && curl -fsSL "https://github.com/seccomp/libseccomp/releases/download/v${SECCOMP_VERSION}/libseccomp-${SECCOMP_VERSION}.tar.gz" | tar -xzC "$SECCOMP_PATH" --strip-components=1 \
 && (cd "$SECCOMP_PATH" \
 && ./configure --prefix=/usr/local \
 && make \
 && make install \
 && ldconfig ) \
 && rm -rf "$SECCOMP_PATH"
#  Install Go
#  We don't have official binary tarballs for ARM64, eigher for Go or bootstrap,
#  so we use the official armv6 released binaries as a GOROOT_BOOTSTRAP, and
#  build Go from source code.
ENV GO_VERSION="1.5.4"
RUN mkdir /usr/src/go \
 && curl -fsSL https://storage.googleapis.com/golang/go${GO_VERSION}.src.tar.gz | tar -v -C /usr/src/go -xz --strip-components=1 \
 && cd /usr/src/go/src \
 && GOOS=linux GOARCH=arm64 GOROOT_BOOTSTRAP="$( go env GOROOT ;)" ./make.bash
ENV PATH="/usr/src/go/bin:$PATH"
ENV GOPATH="/go:/go/src/github.com/docker/docker/vendor"
#  Only install one version of the registry, because old version which support
#  schema1 manifests is not working on ARM64, we should skip integration-cli
#  tests for schema1 manifests on ARM64.
ENV REGISTRY_COMMIT="47a064d4195a9b56133891bbb13620c3ac83a827"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/distribution.git "$GOPATH/src/github.com/docker/distribution" \
 && (cd "$GOPATH/src/github.com/docker/distribution" \
 && git checkout -q "$REGISTRY_COMMIT" ) \
 && GOPATH="$GOPATH/src/github.com/docker/distribution/Godeps/_workspace:$GOPATH" go build -o /usr/local/bin/registry-v2 github.com/docker/distribution/cmd/registry \
 && rm -rf "$GOPATH"
#  Install notary server
ENV NOTARY_VERSION="docker-v1.11-3"
RUN set -x \
 && export GO15VENDOREXPERIMENT=1 \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/notary.git "$GOPATH/src/github.com/docker/notary" \
 && (cd "$GOPATH/src/github.com/docker/notary" \
 && git checkout -q "$NOTARY_VERSION" ) \
 && GOPATH="$GOPATH/src/github.com/docker/notary/vendor:$GOPATH" go build -o /usr/local/bin/notary-server github.com/docker/notary/cmd/notary-server \
 && GOPATH="$GOPATH/src/github.com/docker/notary/vendor:$GOPATH" go build -o /usr/local/bin/notary github.com/docker/notary/cmd/notary \
 && rm -rf "$GOPATH"
#  Get the "docker-py" source so we can run their integration tests
ENV DOCKER_PY_COMMIT="e2878cbcc3a7eef99917adc1be252800b0e41ece"
RUN git clone https://github.com/docker/docker-py.git /docker-py \
 && cd /docker-py \
 && git checkout -q $DOCKER_PY_COMMIT \
 && pip install -r test-requirements.txt
#  Set user.email so crosbymichael's in-container merge commits go smoothly
RUN git config --global user.email 'docker-dummy@example.com'
#  Add an unprivileged user to be used for tests which need it
RUN groupadd -r docker
RUN useradd --create-home --gid docker unprivilegeduser
VOLUME /var/lib/docker
WORKDIR /go/src/github.com/docker/docker
ENV DOCKER_BUILDTAGS="apparmor pkcs11 seccomp selinux"
#  Let us use a .bashrc file
RUN ln -sfv $PWD/.bashrc ~/.bashrc
#  Register Docker's bash completion.
RUN ln -sv $PWD/contrib/completion/bash/docker /etc/bash_completion.d/docker
#  Get useful and necessary Hub images so we can "docker load" locally instead of pulling
COPY contrib/download-frozen-image-v2.sh /go/src/github.com/docker/docker/contrib/
RUN ./contrib/download-frozen-image-v2.sh /docker-frozen-images aarch64/buildpack-deps:jessie@sha256:6aa1d6910791b7ac78265fd0798e5abd6cb3f27ae992f6f960f6c303ec9535f2 aarch64/busybox:latest@sha256:b23a6a37cf269dff6e46d2473b6e227afa42b037e6d23435f1d2bc40fc8c2828 aarch64/debian:jessie@sha256:4be74a41a7c70ebe887b634b11ffe516cf4fcd56864a54941e56bb49883c3170 aarch64/hello-world:latest@sha256:65a4a158587b307bb02db4de41b836addb0c35175bdc801367b1ac1ddeb9afda
#  see also "hack/make/.ensure-frozen-images" (which needs to be updated any time this list is)
#  Download man page generator
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone --depth 1 -b v1.0.4 https://github.com/cpuguy83/go-md2man.git "$GOPATH/src/github.com/cpuguy83/go-md2man" \
 && git clone --depth 1 -b v1.4 https://github.com/russross/blackfriday.git "$GOPATH/src/github.com/russross/blackfriday" \
 && go get -v -d github.com/cpuguy83/go-md2man \
 && go build -v -o /usr/local/bin/go-md2man github.com/cpuguy83/go-md2man \
 && rm -rf "$GOPATH"
#  Download toml validator
ENV TOMLV_COMMIT="9baf8a8a9f2ed20a8e54160840c492f937eeaf9a"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/BurntSushi/toml.git "$GOPATH/src/github.com/BurntSushi/toml" \
 && (cd "$GOPATH/src/github.com/BurntSushi/toml" \
 && git checkout -q "$TOMLV_COMMIT" ) \
 && go build -v -o /usr/local/bin/tomlv github.com/BurntSushi/toml/cmd/tomlv \
 && rm -rf "$GOPATH"
#  Install runc
ENV RUNC_COMMIT="e87436998478d222be209707503c27f6f91be0c5"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/opencontainers/runc.git "$GOPATH/src/github.com/opencontainers/runc" \
 && cd "$GOPATH/src/github.com/opencontainers/runc" \
 && git checkout -q "$RUNC_COMMIT" \
 && make static BUILDTAGS="seccomp apparmor selinux" \
 && cp runc /usr/local/bin/docker-runc \
 && rm -rf "$GOPATH"
#  Install containerd
ENV CONTAINERD_COMMIT="07c95162cdcead88dfe4ca0ffb3cea02375ec54d"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/containerd.git "$GOPATH/src/github.com/docker/containerd" \
 && cd "$GOPATH/src/github.com/docker/containerd" \
 && git checkout -q "$CONTAINERD_COMMIT" \
 && make static \
 && cp bin/containerd /usr/local/bin/docker-containerd \
 && cp bin/containerd-shim /usr/local/bin/docker-containerd-shim \
 && cp bin/ctr /usr/local/bin/docker-containerd-ctr \
 && rm -rf "$GOPATH"
#  Wrap all commands in the "docker-in-docker" script to allow nested containers
ENTRYPOINT ["hack/dind"]
#  Upload docker source
COPY . /go/src/github.com/docker/docker
