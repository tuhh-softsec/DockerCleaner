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
FROM aarch64/ubuntu:trusty
#   Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor=2.10.95-0ubuntu2.6~14.04.4 aufs-tools=1:3.2+20130722-1.1 automake=1:1.14.1-2ubuntu1 bash-completion=1:2.1-4ubuntu0.2 btrfs-tools=3.12-1ubuntu0.2 build-essential=11.6ubuntu6 createrepo=0.10.3-1 curl=7.35.0-1ubuntu2.20 dpkg-sig=0.13.1+nmu1 g++=4:4.8.2-1ubuntu6 gcc=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 iptables=1.4.21-1ubuntu1 jq=1.3-1.1ubuntu1.1 libapparmor-dev=2.10.95-0ubuntu2.6~14.04.4 libc6-dev=2.19-0ubuntu6.15 libcap-dev=1:2.24-0ubuntu2 libsqlite3-dev=3.8.2-1ubuntu2.2 libsystemd-journal-dev=204-5ubuntu20.31 mercurial=2.8.2-1ubuntu1.4 parallel=20161222-1~ubuntu0.14.04.2 pkg-config=0.26-1ubuntu4 python-dev=2.7.5-5ubuntu3 python-mock=1.0.1-3 python-pip=1.5.4-1ubuntu4 python-websocket=0.12.0-1ubuntu2 s3cmd=1.1.0* -y
#   Install armhf loader to use armv6 binaries on armv8
RUN dpkg --add-architecture armhf \
 && apt-get update \
 && apt-get install --no-install-recommends libc6:armhf -y
#   Get lvm2 source for compiling statically
ENV LVM2_VERSION="2.02.103"
RUN mkdir -p /usr/local/lvm2 \
 && curl -fsSL "https://mirrors.kernel.org/sourceware/lvm2/LVM2.${LVM2_VERSION}.tgz" | tar -xzC /usr/local/lvm2 --strip-components=1
#   see https://git.fedorahosted.org/cgit/lvm2.git/refs/tags for release tags
#   fix platform enablement in lvm2 to support aarch64 properly
RUN set -e \
 && for f in config.guess config.sub; do curl -fsSL -o "/usr/local/lvm2/autoconf/$f" "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=$f;hb=HEAD" ; done
#   "arch.c:78:2: error: #error the arch code needs to know about your machine type"
#   Compile and install lvm2
RUN cd /usr/local/lvm2 \
 && ./configure --build="$( gcc -print-multiarch ;)" --enable-static_link \
 && make device-mapper \
 && make install_device-mapper
#   see https://git.fedorahosted.org/cgit/lvm2.git/tree/INSTALL
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
#   Install Go
#   We don't have official binary tarballs for ARM64, eigher for Go or bootstrap,
#   so we use the official armv6 released binaries as a GOROOT_BOOTSTRAP, and
#   build Go from source code.
ENV BOOT_STRAP_VERSION="1.6beta1"
ENV GO_VERSION="1.5.3"
RUN mkdir -p /usr/src/go-bootstrap \
 && curl -fsSL https://storage.googleapis.com/golang/go${BOOT_STRAP_VERSION}.linux-arm6.tar.gz | tar -v -C /usr/src/go-bootstrap -xz --strip-components=1 \
 && mkdir /usr/src/go \
 && curl -fsSL https://storage.googleapis.com/golang/go${GO_VERSION}.src.tar.gz | tar -v -C /usr/src/go -xz --strip-components=1 \
 && cd /usr/src/go/src \
 && GOOS=linux GOARCH=arm64 GOROOT_BOOTSTRAP=/usr/src/go-bootstrap ./make.bash
ENV PATH="/usr/src/go/bin:$PATH"
ENV GOPATH="/go:/go/src/github.com/docker/docker/vendor"
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
#   Setup s3cmd config
RUN { echo '[default]' ;echo 'access_key=$AWS_ACCESS_KEY' ;echo 'secret_key=$AWS_SECRET_KEY' ; } > ~/.s3cfg
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
RUN ./contrib/download-frozen-image-v2.sh /docker-frozen-images aarch64/buildpack-deps:jessie@sha256:6aa1d6910791b7ac78265fd0798e5abd6cb3f27ae992f6f960f6c303ec9535f2 aarch64/busybox:latest@sha256:b23a6a37cf269dff6e46d2473b6e227afa42b037e6d23435f1d2bc40fc8c2828 aarch64/debian:jessie@sha256:4be74a41a7c70ebe887b634b11ffe516cf4fcd56864a54941e56bb49883c3170 aarch64/hello-world:latest@sha256:65a4a158587b307bb02db4de41b836addb0c35175bdc801367b1ac1ddeb9afda
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
#   Wrap all commands in the "docker-in-docker" script to allow nested containers
ENTRYPOINT ["hack/dind"]
#   Upload docker source
COPY . /go/src/github.com/docker/docker
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
