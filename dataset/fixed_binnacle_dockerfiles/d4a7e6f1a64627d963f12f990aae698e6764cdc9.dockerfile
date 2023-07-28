#   This file describes the standard way to build Docker on ppc64le, using docker
#
#   Usage:
#
#   # Assemble the full dev environment. This is slow the first time.
#   docker build -t docker -f Dockerfile.ppc64le .
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
FROM ppc64le/gcc:5.3
#   Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor aufs-tools automake bash-completion btrfs-tools build-essential createrepo curl dpkg-sig git iptables jq net-tools libapparmor-dev libcap-dev libltdl-dev libsqlite3-dev libsystemd-journal-dev libtool mercurial pkg-config python-dev python-mock python-pip python-websocket xfsprogs tar -y
#   Get lvm2 source for compiling statically
ENV LVM2_VERSION="2.02.103"
RUN mkdir -p /usr/local/lvm2 \
 && curl -fsSL "https://mirrors.kernel.org/sourceware/lvm2/LVM2.${LVM2_VERSION}.tgz" | tar -xzC /usr/local/lvm2 --strip-components=1
#   see https://git.fedorahosted.org/cgit/lvm2.git/refs/tags for release tags
#   fix platform enablement in lvm2 to support ppc64le properly
RUN set -e \
 && for f in config.guess config.sub; do curl -fsSL -o "/usr/local/lvm2/autoconf/$f" "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=$f;hb=HEAD" ; done
#   "arch.c:78:2: error: #error the arch code needs to know about your machine type"
#   Compile and install lvm2
RUN cd /usr/local/lvm2 \
 && ./configure --build="$( gcc -print-multiarch ;)" --enable-static_link \
 && make device-mapper \
 && make install_device-mapper
#   see https://git.fedorahosted.org/cgit/lvm2.git/tree/INSTALL
#   TODO install Go, using gccgo as GOROOT_BOOTSTRAP (Go 1.5+ supports ppc64le properly)
#   possibly a ppc64le/golang image?
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
#   TODO update this when we upgrade to Go 1.5.1+
#   Install notary server
#  ENV NOTARY_VERSION docker-v1.10-5
#  RUN set -x \
#  	&& export GOPATH="$(mktemp -d)" \
#  	&& git clone https://github.com/docker/notary.git "$GOPATH/src/github.com/docker/notary" \
#  	&& (cd "$GOPATH/src/github.com/docker/notary" && git checkout -q "$NOTARY_VERSION") \
#  	&& GOPATH="$GOPATH/src/github.com/docker/notary/Godeps/_workspace:$GOPATH" \
#  		go build -o /usr/local/bin/notary-server github.com/docker/notary/cmd/notary-server \
#  	&& rm -rf "$GOPATH"
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
ENV DOCKER_BUILDTAGS="apparmor selinux"
#   Let us use a .bashrc file
RUN ln -sfv $PWD/.bashrc ~/.bashrc
#   Register Docker's bash completion.
RUN ln -sv $PWD/contrib/completion/bash/docker /etc/bash_completion.d/docker
#   Get useful and necessary Hub images so we can "docker load" locally instead of pulling
COPY contrib/download-frozen-image-v2.sh /go/src/github.com/docker/docker/contrib/
RUN ./contrib/download-frozen-image-v2.sh /docker-frozen-images ppc64le/buildpack-deps:jessie@sha256:902bfe4ef1389f94d143d64516dd50a2de75bca2e66d4a44b1d73f63ddf05dda ppc64le/busybox:latest@sha256:38bb82085248d5a3c24bd7a5dc146f2f2c191e189da0441f1c2ca560e3fc6f1b ppc64le/debian:jessie@sha256:412845f51b6ab662afba71bc7a716e20fdb9b84f185d180d4c7504f8a75c4f91 ppc64le/hello-world:latest@sha256:186a40a9a02ca26df0b6c8acdfb8ac2f3ae6678996a838f977e57fac9d963974
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
