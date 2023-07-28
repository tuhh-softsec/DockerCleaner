#  This file describes the standard way to build Docker on s390x, using docker
#
#  Usage:
#
#  # Assemble the full dev environment. This is slow the first time.
#  docker build -t docker -f Dockerfile.s390x .
#
#  # Mount your source in an interactive container for quick testing:
#  docker run -v `pwd`:/go/src/github.com/docker/docker --privileged -i -t docker bash
#
#  # Run the test suite:
#  docker run --privileged docker hack/make.sh test-unit test-integration-cli test-docker-py
#
#  Note: AppArmor used to mess with privileged mode, but this is no longer
#  the case. Therefore, you don't have to disable it anymore.
#
FROM s390x/gcc:6.1
#  Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor aufs-tools automake bash-completion btrfs-tools build-essential createrepo curl dpkg-sig git iptables jq net-tools libapparmor-dev libcap-dev libltdl-dev libsqlite3-dev libsystemd-journal-dev libtool mercurial pkg-config python-dev python-mock python-pip python-websocket xfsprogs tar -y
#  glibc in Debian has a bug specific to s390x that won't be fixed until Debian 8.6 is released
#  - https://github.com/docker/docker/issues/24748
#  - https://sourceware.org/git/?p=glibc.git;a=commit;h=890b7a4b33d482b5c768ab47d70758b80227e9bc
#  - https://sourceware.org/git/?p=glibc.git;a=commit;h=2e807f29595eb5b1e5d0decc6e356a3562ecc58e
RUN echo 'deb http://httpredir.debian.org/debian jessie-proposed-updates main' >> /etc/apt/sources.list.d/pu.list \
 && apt-get update \
 && apt-get install libc6 -y \
 && rm -rf /var/lib/apt/lists/*
#  install seccomp: the version shipped in jessie is too old
ENV SECCOMP_VERSION="2.3.1"
RUN set -x \
 && export SECCOMP_PATH="$( mktemp -d ;)" \
 && curl -fsSL "https://github.com/seccomp/libseccomp/releases/download/v${SECCOMP_VERSION}/libseccomp-${SECCOMP_VERSION}.tar.gz" | tar -xzC "$SECCOMP_PATH" --strip-components=1 \
 && (cd "$SECCOMP_PATH" \
 && ./configure --prefix=/usr/local \
 && make \
 && make install \
 && ldconfig ) \
 && rm -rf "$SECCOMP_PATH"
#  Get lvm2 source for compiling statically
ENV LVM2_VERSION="2.02.103"
RUN mkdir -p /usr/local/lvm2 \
 && curl -fsSL "https://mirrors.kernel.org/sourceware/lvm2/LVM2.${LVM2_VERSION}.tgz" | tar -xzC /usr/local/lvm2 --strip-components=1
#  see https://git.fedorahosted.org/cgit/lvm2.git/refs/tags for release tags
#  fix platform enablement in lvm2 to support s390x properly
RUN set -e \
 && for f in config.guess config.sub; do curl -fsSL -o "/usr/local/lvm2/autoconf/$f" "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=$f;hb=HEAD" ; done
#  "arch.c:78:2: error: #error the arch code needs to know about your machine type"
#  Compile and install lvm2
RUN cd /usr/local/lvm2 \
 && ./configure --build="$( gcc -print-multiarch ;)" --enable-static_link \
 && make device-mapper \
 && make install_device-mapper
#  see https://git.fedorahosted.org/cgit/lvm2.git/tree/INSTALL
ENV GO_VERSION="1.7.1"
RUN curl -fsSL "https://storage.googleapis.com/golang/go${GO_VERSION}.linux-s390x.tar.gz" | tar -xzC /usr/local
ENV PATH="/go/bin:/usr/local/go/bin:$PATH"
ENV GOPATH="/go:/go/src/github.com/docker/docker/vendor"
#  Dependency for golint
ENV GO_TOOLS_COMMIT="823804e1ae08dbb14eb807afc7db9993bc9e3cc3"
RUN git clone https://github.com/golang/tools.git /go/src/golang.org/x/tools \
 && (cd /go/src/golang.org/x/tools \
 && git checkout -q $GO_TOOLS_COMMIT )
#  Grab Go's lint tool
ENV GO_LINT_COMMIT="32a87160691b3c96046c0c678fe57c5bef761456"
RUN git clone https://github.com/golang/lint.git /go/src/github.com/golang/lint \
 && (cd /go/src/github.com/golang/lint \
 && git checkout -q $GO_LINT_COMMIT ) \
 && go install -v github.com/golang/lint/golint
#  Install two versions of the registry. The first is an older version that
#  only supports schema1 manifests. The second is a newer version that supports
#  both. This allows integration-cli tests to cover push/pull with both schema1
#  and schema2 manifests.
ENV REGISTRY_COMMIT_SCHEMA1="ec87e9b6971d831f0eff752ddb54fb64693e51cd"
ENV REGISTRY_COMMIT="47a064d4195a9b56133891bbb13620c3ac83a827"
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
#  Install notary and notary-server
ENV NOTARY_VERSION="v0.3.0"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/notary.git "$GOPATH/src/github.com/docker/notary" \
 && (cd "$GOPATH/src/github.com/docker/notary" \
 && git checkout -q "$NOTARY_VERSION" ) \
 && GOPATH="$GOPATH/src/github.com/docker/notary/vendor:$GOPATH" go build -o /usr/local/bin/notary-server github.com/docker/notary/cmd/notary-server \
 && GOPATH="$GOPATH/src/github.com/docker/notary/vendor:$GOPATH" go build -o /usr/local/bin/notary github.com/docker/notary/cmd/notary \
 && rm -rf "$GOPATH"
#  Get the "docker-py" source so we can run their integration tests
ENV DOCKER_PY_COMMIT="e2655f658408f9ad1f62abdef3eb6ed43c0cf324"
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
ENV DOCKER_BUILDTAGS="apparmor selinux seccomp"
#  Let us use a .bashrc file
RUN ln -sfv $PWD/.bashrc ~/.bashrc
#  Register Docker's bash completion.
RUN ln -sv $PWD/contrib/completion/bash/docker /etc/bash_completion.d/docker
#  Get useful and necessary Hub images so we can "docker load" locally instead of pulling
COPY contrib/download-frozen-image-v2.sh /go/src/github.com/docker/docker/contrib/
RUN ./contrib/download-frozen-image-v2.sh /docker-frozen-images s390x/buildpack-deps:jessie@sha256:4d1381224acaca6c4bfe3604de3af6972083a8558a99672cb6989c7541780099 s390x/busybox:latest@sha256:dd61522c983884a66ed72d60301925889028c6d2d5e0220a8fe1d9b4c6a4f01b s390x/debian:jessie@sha256:b74c863400909eff3c5e196cac9bfd1f6333ce47aae6a38398d87d5875da170a s390x/hello-world:latest@sha256:780d80b3a7677c3788c0d5cd9168281320c8d4a6d9183892d8ee5cdd610f5699
#  see also "hack/make/.ensure-frozen-images" (which needs to be updated any time this list is)
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
ENV RUNC_COMMIT="cc29e3dded8e27ba8f65738f40d251c885030a28"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/opencontainers/runc.git "$GOPATH/src/github.com/opencontainers/runc" \
 && cd "$GOPATH/src/github.com/opencontainers/runc" \
 && git checkout -q "$RUNC_COMMIT" \
 && make static BUILDTAGS="seccomp apparmor selinux" \
 && cp runc /usr/local/bin/docker-runc \
 && rm -rf "$GOPATH"
#  Install containerd
ENV CONTAINERD_COMMIT="4c21ad662f71af56c0e6b29c0afef72df441d1ff"
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
