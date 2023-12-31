#   This file describes the standard way to build Docker on s390x, using docker
#
#   Usage:
#
#   # Assemble the full dev environment. This is slow the first time.
#   docker build -t docker -f Dockerfile.s390x .
#
#   # Mount your source in an interactive container for quick testing:
#   docker run -v `pwd`:/go/src/github.com/docker/docker --privileged -i -t docker bash
#
#   # Run the test suite:
#   docker run --privileged docker hack/make.sh test
#
#   Note: AppArmor used to mess with privileged mode, but this is no longer
#   the case. Therefore, you don't have to disable it anymore.
#
FROM s390x/gcc:6.1
#   Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor aufs-tools automake bash-completion btrfs-tools build-essential createrepo curl dpkg-sig git iptables jq net-tools libapparmor-dev libcap-dev libltdl-dev libsqlite3-dev libsystemd-journal-dev libtool mercurial pkg-config python-dev python-mock python-pip python-websocket xfsprogs tar -y
#   install seccomp: the version shipped in jessie is too old
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
#   Get lvm2 source for compiling statically
ENV LVM2_VERSION="2.02.103"
RUN mkdir -p /usr/local/lvm2 \
 && curl -fsSL "https://mirrors.kernel.org/sourceware/lvm2/LVM2.${LVM2_VERSION}.tgz" | tar -xzC /usr/local/lvm2 --strip-components=1
#   see https://git.fedorahosted.org/cgit/lvm2.git/refs/tags for release tags
#   fix platform enablement in lvm2 to support s390x properly
RUN set -e \
 && for f in config.guess config.sub; do curl -fsSL -o "/usr/local/lvm2/autoconf/$f" "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=$f;hb=HEAD" ; done
#   "arch.c:78:2: error: #error the arch code needs to know about your machine type"
#   Compile and install lvm2
RUN cd /usr/local/lvm2 \
 && ./configure --build="$( gcc -print-multiarch ;)" --enable-static_link \
 && make device-mapper \
 && make install_device-mapper
#   see https://git.fedorahosted.org/cgit/lvm2.git/tree/INSTALL
#   Note: Go comes from the base image (gccgo, specifically)
#   We can't compile Go proper because s390x isn't an officially supported architecture yet.
ENV PATH="/go/bin:$PATH"
ENV GOPATH="/go:/go/src/github.com/docker/docker/vendor"
#   This has been commented out and kept as reference because we don't support compiling with older Go anymore.
#   ENV GOFMT_VERSION 1.3.3
#   RUN curl -sSL https://storage.googleapis.com/golang/go${GOFMT_VERSION}.$(go env GOOS)-$(go env GOARCH).tar.gz | tar -C /go/bin -xz --strip-components=2 go/bin/gofmt
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
ENV GO_LINT_COMMIT="f42f5c1c440621302702cb0741e9d2ca547ae80f"
RUN git clone https://github.com/golang/lint.git /go/src/github.com/golang/lint \
 && (cd /go/src/github.com/golang/lint \
 && git checkout -q $GO_LINT_COMMIT ) \
 && go install -v github.com/golang/lint/golint
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
 && GOPATH="$GOPATH/src/github.com/docker/distribution/Godeps/_workspace:$GOPATH" go build -o /usr/local/bin/registry-v2 github.com/docker/distribution/cmd/registry \
 && (cd "$GOPATH/src/github.com/docker/distribution" \
 && git checkout -q "$REGISTRY_COMMIT_SCHEMA1" ) \
 && GOPATH="$GOPATH/src/github.com/docker/distribution/Godeps/_workspace:$GOPATH" go build -o /usr/local/bin/registry-v2-schema1 github.com/docker/distribution/cmd/registry \
 && rm -rf "$GOPATH"
#   Install notary and notary-server
#
#   Note: We have to explicitly set GO15VENDOREXPERIMENT=0 because gccgo does not
#   support vendoring: https://github.com/golang/go/issues/15628
ENV NOTARY_VERSION="v0.3.0"
RUN set -x \
 && export GO15VENDOREXPERIMENT=0 \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/notary.git "$GOPATH/src/github.com/docker/notary" \
 && (cd "$GOPATH/src/github.com/docker/notary" \
 && git checkout -q "$NOTARY_VERSION" \
 && ln -s . vendor/src ) \
 && GOPATH="$GOPATH/src/github.com/docker/notary/vendor:$GOPATH" go build -o /usr/local/bin/notary-server github.com/docker/notary/cmd/notary-server \
 && GOPATH="$GOPATH/src/github.com/docker/notary/vendor:$GOPATH" go build -o /usr/local/bin/notary github.com/docker/notary/cmd/notary \
 && rm -rf "$GOPATH"
#   Get the "docker-py" source so we can run their integration tests
ENV DOCKER_PY_COMMIT="7befe694bd21e3c54bb1d7825270ea4bd6864c13"
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
ENV DOCKER_BUILDTAGS="apparmor selinux seccomp"
#   Let us use a .bashrc file
RUN ln -sfv $PWD/.bashrc ~/.bashrc
#   Register Docker's bash completion.
RUN ln -sv $PWD/contrib/completion/bash/docker /etc/bash_completion.d/docker
#   Get useful and necessary Hub images so we can "docker load" locally instead of pulling
COPY contrib/download-frozen-image-v2.sh /go/src/github.com/docker/docker/contrib/
RUN ./contrib/download-frozen-image-v2.sh /docker-frozen-images s390x/buildpack-deps:jessie@sha256:4d1381224acaca6c4bfe3604de3af6972083a8558a99672cb6989c7541780099 s390x/busybox:latest@sha256:dd61522c983884a66ed72d60301925889028c6d2d5e0220a8fe1d9b4c6a4f01b s390x/debian:jessie@sha256:b74c863400909eff3c5e196cac9bfd1f6333ce47aae6a38398d87d5875da170a s390x/hello-world:latest@sha256:780d80b3a7677c3788c0d5cd9168281320c8d4a6d9183892d8ee5cdd610f5699
#   see also "hack/make/.ensure-frozen-images" (which needs to be updated any time this list is)
#   Download man page generator
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone --depth 1 -b v1.0.5 https://github.com/cpuguy83/go-md2man.git "$GOPATH/src/github.com/cpuguy83/go-md2man" \
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
#   Install runc
ENV RUNC_COMMIT="02f8fa7863dd3f82909a73e2061897828460d52f"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/opencontainers/runc.git "$GOPATH/src/github.com/opencontainers/runc" \
 && cd "$GOPATH/src/github.com/opencontainers/runc" \
 && git checkout -q "$RUNC_COMMIT" \
 && make static BUILDTAGS="seccomp apparmor selinux" \
 && cp runc /usr/local/bin/docker-runc \
 && rm -rf "$GOPATH"
#   Install containerd
ENV CONTAINERD_COMMIT="0366d7e9693c930cf18c0f50cc16acec064e96c5"
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
#   Wrap all commands in the "docker-in-docker" script to allow nested containers
ENTRYPOINT ["hack/dind"]
#   Upload docker source
COPY . /go/src/github.com/docker/docker
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
