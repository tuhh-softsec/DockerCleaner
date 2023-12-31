#   This file describes the standard way to build Docker, using docker
#
#   Usage:
#
#   # Assemble the full dev environment. This is slow the first time.
#   docker build -t docker .
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
#   Note: Apparmor used to mess with privileged mode, but this is no longer
#   the case. Therefore, you don't have to disable it anymore.
#
FROM ubuntu:14.04
MAINTAINER Tianon Gravi <admwiggin@gmail.com> (@tianon)
#   Packaged dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apparmor=2.10.95-0ubuntu2.6~14.04.4 aufs-tools=1:3.2+20130722-1.1 automake=1:1.14.1-2ubuntu1 btrfs-tools=3.12-1ubuntu0.2 build-essential=11.6ubuntu6 curl=7.35.0-1ubuntu2.20 dpkg-sig=0.13.1+nmu1 git=1:1.9.1-1ubuntu0.10 iptables=1.4.21-1ubuntu1 libapparmor-dev=2.10.95-0ubuntu2.6~14.04.4 libcap-dev=1:2.24-0ubuntu2 libsqlite3-dev=3.8.2-1ubuntu2.2 mercurial=2.8.2-1ubuntu1.4 parallel=20161222-1~ubuntu0.14.04.2 python-mock=1.0.1-3 python-pip=1.5.4-1ubuntu4 python-websocket=0.12.0-1ubuntu2 reprepro=4.13.1-1build1 ruby1.9.1=1.9.3.484-2ubuntu1.14 ruby1.9.1-dev=1.9.3.484-2ubuntu1.14 s3cmd=1.1.0* -y
#   Get lvm2 source for compiling statically
RUN git clone -b v2_02_103 https://git.fedorahosted.org/git/lvm2.git /usr/local/lvm2
#   see https://git.fedorahosted.org/cgit/lvm2.git/refs/tags for release tags
#   Compile and install lvm2
RUN cd /usr/local/lvm2 \
 && ./configure --enable-static_link \
 && make device-mapper \
 && make install_device-mapper
#   see https://git.fedorahosted.org/cgit/lvm2.git/tree/INSTALL
#   Install lxc
ENV LXC_VERSION="1.0.7"
RUN mkdir -p /usr/src/lxc \
 && curl -sSL https://linuxcontainers.org/downloads/lxc/lxc-${LXC_VERSION}.tar.gz | tar -v -C /usr/src/lxc/ -xz --strip-components=1
RUN cd /usr/src/lxc \
 && ./configure \
 && make \
 && make install \
 && ldconfig
#   Install Go
ENV GO_VERSION="1.4.1"
RUN curl -sSL https://golang.org/dl/go${GO_VERSION}.src.tar.gz | tar -v -C /usr/local -xz \
 && mkdir -p /go/bin
ENV PATH="/go/bin:/usr/local/go/bin:$PATH"
ENV GOPATH="/go:/go/src/github.com/docker/docker/vendor"
RUN cd /usr/local/go/src \
 && ./make.bash --no-clean 2>&1
#   Compile Go for cross compilation
ENV DOCKER_CROSSPLATFORMS="linux/386 linux/arm  darwin/amd64 darwin/386  freebsd/amd64 freebsd/386 freebsd/arm"
#   TODO when https://jenkins.dockerproject.com/job/Windows/ is green, add windows back to the list above
#  	windows/amd64 windows/386
#   (set an explicit GOARM of 5 for maximum compatibility)
ENV GOARM="5"
RUN cd /usr/local/go/src \
 && set -x \
 && for platform in $DOCKER_CROSSPLATFORMS; do GOOS=${platform%/*} GOARCH=${platform##*/} ./make.bash --no-clean 2>&1; done
#   We still support compiling with older Go, so need to grab older "gofmt"
ENV GOFMT_VERSION="1.3.3"
RUN curl -sSL https://storage.googleapis.com/golang/go${GOFMT_VERSION}.$( go env GOOS ;)-$( go env GOARCH ;).tar.gz | tar -C /go/bin -xz --strip-components=2 go/bin/gofmt
#   Grab Go's cover tool for dead-simple code coverage testing
RUN go get golang.org/x/tools/cmd/cover
#   TODO replace FPM with some very minimal debhelper stuff
RUN gem install fpm --version 1.3.2 --no-rdoc --no-ri
#   Get the "busybox" image source so we can build locally instead of pulling
RUN git clone -b buildroot-2014.02 https://github.com/jpetazzo/docker-busybox.git /docker-busybox
#   Get the "cirros" image source so we can import it instead of fetching it during tests
RUN curl -sSL -o /cirros.tar.gz https://github.com/ewindisch/docker-cirros/raw/1cded459668e8b9dbf4ef976c94c05add9bbd8e9/cirros-0.3.0-x86_64-lxc.tar.gz
#   Install registry
ENV REGISTRY_COMMIT="c448e0416925a9876d5576e412703c9b8b865e19"
RUN set -x \
 && git clone https://github.com/docker/distribution.git /go/src/github.com/docker/distribution \
 && (cd /go/src/github.com/docker/distribution \
 && git checkout -q $REGISTRY_COMMIT ) \
 && GOPATH=/go/src/github.com/docker/distribution/Godeps/_workspace:/go go build -o /go/bin/registry-v2 github.com/docker/distribution/cmd/registry
#   Get the "docker-py" source so we can run their integration tests
ENV DOCKER_PY_COMMIT="aa19d7b6609c6676e8258f6b900dea2eda1dbe95"
RUN git clone https://github.com/docker/docker-py.git /docker-py \
 && cd /docker-py \
 && git checkout -q $DOCKER_PY_COMMIT
#   Setup s3cmd config
RUN { echo '[default]' ;echo 'access_key=$AWS_ACCESS_KEY' ;echo 'secret_key=$AWS_SECRET_KEY' ; } > ~/.s3cfg
#   Set user.email so crosbymichael's in-container merge commits go smoothly
RUN git config --global user.email 'docker-dummy@example.com'
#   Add an unprivileged user to be used for tests which need it
RUN groupadd -r docker
RUN useradd --create-home --gid docker unprivilegeduser
VOLUME /var/lib/docker
WORKDIR /go/src/github.com/docker/docker
ENV DOCKER_BUILDTAGS="apparmor selinux btrfs_noversion"
#   Install man page generator
COPY vendor /go/src/github.com/docker/docker/vendor
#   (copy vendor/ because go-md2man needs golang.org/x/net)
RUN set -x \
 && git clone -b v1.0.1 https://github.com/cpuguy83/go-md2man.git /go/src/github.com/cpuguy83/go-md2man \
 && git clone -b v1.2 https://github.com/russross/blackfriday.git /go/src/github.com/russross/blackfriday \
 && go install -v github.com/cpuguy83/go-md2man
#   install toml validator
RUN git clone -b v0.1.0 https://github.com/BurntSushi/toml.git /go/src/github.com/BurntSushi/toml \
 && go install -v github.com/BurntSushi/toml/cmd/tomlv
#   Wrap all commands in the "docker-in-docker" script to allow nested containers
ENTRYPOINT ["hack/dind"]
#   Upload docker source
COPY . /go/src/github.com/docker/docker
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
