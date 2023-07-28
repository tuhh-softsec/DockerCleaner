#   This file describes the standard way to build Docker, using docker
#
#   Usage:
#
#   # Assemble the full dev environment. This is slow the first time.
#   docker build -t docker -f Dockerfile.gccgo .
#
FROM gcc:6.1
#   Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor aufs-tools btrfs-tools build-essential curl git iptables jq net-tools libapparmor-dev libcap-dev libsqlite3-dev mercurial net-tools parallel python-dev python-mock python-pip python-websocket -y
#   Get lvm2 source for compiling statically
RUN git clone -b v2_02_103 https://git.fedorahosted.org/git/lvm2.git /usr/local/lvm2
#   see https://git.fedorahosted.org/cgit/lvm2.git/refs/tags for release tags
#   Compile and install lvm2
RUN cd /usr/local/lvm2 \
 && ./configure --enable-static_link \
 && make device-mapper \
 && make install_device-mapper
#   see https://git.fedorahosted.org/cgit/lvm2.git/tree/INSTALL
#   install seccomp: the version shipped in jessie is too old
ENV SECCOMP_VERSION="v2.3.1"
RUN set -x \
 && export SECCOMP_PATH=$( mktemp -d ;) \
 && git clone https://github.com/seccomp/libseccomp.git "$SECCOMP_PATH" \
 && (cd "$SECCOMP_PATH" \
 && git checkout "$SECCOMP_VERSION" \
 && ./autogen.sh \
 && ./configure --prefix=/usr \
 && make \
 && make install ) \
 && rm -rf "$SECCOMP_PATH"
ENV GOPATH="/go:/go/src/github.com/docker/docker/vendor"
#   Get the "docker-py" source so we can run their integration tests
ENV DOCKER_PY_COMMIT="7befe694bd21e3c54bb1d7825270ea4bd6864c13"
RUN git clone https://github.com/docker/docker-py.git /docker-py \
 && cd /docker-py \
 && git checkout -q $DOCKER_PY_COMMIT
#   Add an unprivileged user to be used for tests which need it
RUN groupadd -r docker
RUN useradd --create-home --gid docker unprivilegeduser
VOLUME /var/lib/docker
WORKDIR /go/src/github.com/docker/docker
ENV DOCKER_BUILDTAGS="apparmor seccomp selinux"
#   Install runc
ENV RUNC_COMMIT="50a19c6ff828c58e5dab13830bd3dacde268afe5"
RUN set -x \
 && export GOPATH="$( mktemp -d ;)" \
 && git clone https://github.com/docker/runc.git "$GOPATH/src/github.com/opencontainers/runc" \
 && cd "$GOPATH/src/github.com/opencontainers/runc" \
 && git checkout -q "$RUNC_COMMIT" \
 && make static BUILDTAGS="seccomp apparmor selinux" \
 && cp runc /usr/local/bin/docker-runc \
 && rm -rf "$GOPATH"
#   Install containerd
ENV CONTAINERD_COMMIT="2a5e70cbf65457815ee76b7e5dd2a01292d9eca8"
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