#  This file describes the standard way to build Docker on ppc64le, using docker
#
#  Usage:
#
#  # Assemble the full dev environment. This is slow the first time.
#  docker build -t docker -f Dockerfile.ppc64le .
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
#  ppc64le/golang is a debian:jessie based image with golang installed
FROM ppc64le/golang:1.6.3
#  Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor aufs-tools automake bash-completion btrfs-tools build-essential createrepo curl dpkg-sig git iptables jq net-tools libapparmor-dev libcap-dev libltdl-dev libsqlite3-dev libsystemd-journal-dev libtool mercurial pkg-config python-dev python-mock python-pip python-websocket xfsprogs tar -y
#  Get lvm2 source for compiling statically
ENV LVM2_VERSION="2.02.103"
RUN mkdir -p /usr/local/lvm2 \
 && curl -fsSL "https://mirrors.kernel.org/sourceware/lvm2/LVM2.${LVM2_VERSION}.tgz" | tar -xzC /usr/local/lvm2 --strip-components=1
#  See https://git.fedorahosted.org/cgit/lvm2.git/refs/tags for release tags
#  Fix platform enablement in lvm2 to support ppc64le properly
RUN set -e \
 && for f in config.guess config.sub; do curl -fsSL -o "/usr/local/lvm2/autoconf/$f" "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=$f;hb=HEAD" ; done
#  "arch.c:78:2: error: #error the arch code needs to know about your machine type"
#  Compile and install lvm2
RUN cd /usr/local/lvm2 \
 && ./configure --build="$( gcc -print-multiarch ;)" --enable-static_link \
 && make device-mapper \
 && make install_device-mapper
#  See https://git.fedorahosted.org/cgit/lvm2.git/tree/INSTALL
#  Install seccomp: the version shipped in jessie is too old
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
#  Install Go
#  ppc64le doesn't have official go binaries, so use the version of go installed from the image
#  to build go from source.
#  NOTE: ppc64le has compatibility issues with older versions of go, so make sure the version >= 1.6
ENV GO_VERSION="1.7.3"
ENV GO_DOWNLOAD_URL="https://golang.org/dl/go${GO_VERSION}.src.tar.gz"
RUN set -x \
 && TEMPDIR="$( mktemp -d ;)" \
 && mv /usr/local/go $TEMPDIR \
 && GOROOT_BOOTSTRAP=$TEMPDIR/go \
 && cd /usr/local \
 && curl -fsSL "$GO_DOWNLOAD_URL" -o golang.tar.gz \
 && tar -C /usr/local -xzf golang.tar.gz \
 && rm golang.tar.gz \
 && cd go/src \
 && ./make.bash 2>&1 \
 && rm -rf $TEMPDIR
ENV GOROOT_BOOTSTRAP="/usr/local/go"
ENV PATH="/usr/local/go/bin/:$PATH"
ENV GOPATH="/go"
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
ENV NOTARY_VERSION="v0.4.2"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/notary.git "$GOPATH/src/github.com/docker/notary" \
 && (cd "$GOPATH/src/github.com/docker/notary" \
 && git checkout -q "$NOTARY_VERSION" ) \
 && GOPATH="$GOPATH/src/github.com/docker/notary/Godeps/_workspace:$GOPATH" go build -o /usr/local/bin/notary-server github.com/docker/notary/cmd/notary-server \
 && GOPATH="$GOPATH/src/github.com/docker/notary/Godeps/_workspace:$GOPATH" go build -o /usr/local/bin/notary github.com/docker/notary/cmd/notary \
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
ENV DOCKER_BUILDTAGS="apparmor pkcs11 seccomp selinux"
#  Let us use a .bashrc file
RUN ln -sfv $PWD/.bashrc ~/.bashrc
#  Register Docker's bash completion.
RUN ln -sv $PWD/contrib/completion/bash/docker /etc/bash_completion.d/docker
#  Get useful and necessary Hub images so we can "docker load" locally instead of pulling
COPY contrib/download-frozen-image-v2.sh /go/src/github.com/docker/docker/contrib/
RUN ./contrib/download-frozen-image-v2.sh /docker-frozen-images ppc64le/buildpack-deps:jessie@sha256:902bfe4ef1389f94d143d64516dd50a2de75bca2e66d4a44b1d73f63ddf05dda ppc64le/busybox:latest@sha256:38bb82085248d5a3c24bd7a5dc146f2f2c191e189da0441f1c2ca560e3fc6f1b ppc64le/debian:jessie@sha256:412845f51b6ab662afba71bc7a716e20fdb9b84f185d180d4c7504f8a75c4f91 ppc64le/hello-world:latest@sha256:186a40a9a02ca26df0b6c8acdfb8ac2f3ae6678996a838f977e57fac9d963974
#  See also "hack/make/.ensure-frozen-images" (which needs to be updated any time this list is)
#  Install tomlv, vndr, runc, containerd, grimes, docker-proxy
#  Please edit hack/dockerfile/install-binaries.sh to update them.
COPY hack/dockerfile/install-binaries.sh /tmp/install-binaries.sh
RUN /tmp/install-binaries.sh tomlv vndr runc containerd grimes proxy
#  Wrap all commands in the "docker-in-docker" script to allow nested containers
ENTRYPOINT ["hack/dind"]
#  Upload docker source
COPY . /go/src/github.com/docker/docker
