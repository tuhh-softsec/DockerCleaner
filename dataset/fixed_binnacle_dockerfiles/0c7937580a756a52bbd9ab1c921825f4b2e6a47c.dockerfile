#  # runtimebase ##
#   this stage contains all 3rd party dependencies
FROM ubuntu:18.04 AS runtimebase
LABEL org.qmstr.image="runtime"
ENV DEBIAN_FRONTEND="noninteractive"
#   install runtime deps
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 curl=7.58.0-2ubuntu3.24 autoconf=2.69-11 git=1:2.17.1-1ubuntu0.17 apt-utils=1.6.14 libgit2-dev=0.26.0+dfsg.1-1.1ubuntu0.2 libio-captureoutput-perl=1.1104-1 python=2.7.15~rc1-1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-distutils=3.6.9-1~18.04 protobuf-compiler=3.0.0-9.1ubuntu1.1 icu-devtools=60.2-3ubuntu3.2 libicu-dev=60.2-3ubuntu3.2 -y \
 && rm -rf /var/lib/apt/lists/*
ARG HUGO_VERSION
RUN curl -L --output /tmp/hugo.deb https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_${HUGO_VERSION}_Linux-64bit.deb
RUN dpkg -i /tmp/hugo.deb
RUN rm /tmp/hugo.deb
#   install ninka
RUN mkdir /ninka \
 && git clone https://github.com/dmgerman/ninka.git /ninka \
 && cd /ninka/comments \
 && make \
 && make install \
 && rm /usr/local/man/man1 \
 && cd /ninka \
 && perl Makefile.PL \
 && make \
 && make install \
 && rm -fr /ninka
#   install scancode
ARG SCANCODE_VERSION
RUN ICU_VERSION=$( icuinfo | sed -n 's:.*<param name="version">\([0-9]*\)\.[0-9]*</param>.*:\1:p' ;) pip install -U scancode-toolkit==${SCANCODE_VERSION}
#   install dgraph
ARG DGRAPH_VERSION
RUN curl -L --output /tmp/dgraph.sha256 https://github.com/dgraph-io/dgraph/releases/download/v"${DGRAPH_VERSION}"/dgraph-checksum-linux-amd64.sha256
RUN curl -L --output /tmp/dgraph.tar.gz https://github.com/dgraph-io/dgraph/releases/download/v"${DGRAPH_VERSION}"/dgraph-linux-amd64.tar.gz
RUN temp_dir=$( mktemp -d 2> /dev/null;) \
 && tar -C $temp_dir -xzf /tmp/dgraph.tar.gz \
 && sed -e s#/usr/local/bin#${temp_dir}# /tmp/dgraph.sha256 | sha256sum -c - \
 && mv ${temp_dir}/* /usr/local/bin/ \
 && rmdir ${temp_dir}
RUN rm /tmp/dgraph*
#  # builder ##
#   Create a container that only builds the software to be installed into the master container:
FROM ubuntu:18.04 AS builder
LABEL org.qmstr.image="builder"
ENV GOROOT="/opt/go"
ENV PATH="${GOPATH}/bin:/opt/go/bin:$PATH"
ENV DEBIAN_FRONTEND="noninteractive"
#   install dependecies
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 autoconf=2.69-11 make=4.1-9.1ubuntu1 git=1:2.17.1-1ubuntu0.17 libgit2-dev=0.26.0+dfsg.1-1.1ubuntu0.2 libio-captureoutput-perl=1.1104-1 virtualenv=15.1.0+ds-1.1 tar=1.29b-2ubuntu0.4 build-essential=12.4ubuntu1 pkg-config=0.29.1-0ubuntu2 protobuf-compiler=3.0.0-9.1ubuntu1.1 -y \
 && rm -rf /var/lib/apt/lists/*
ARG GO_VERSION
RUN curl -o /opt/go.tar.gz https://dl.google.com/go/go${GO_VERSION}.linux-amd64.tar.gz
RUN cd /opt \
 && tar -xf go.tar.gz
#   install golang tools
RUN mkdir /qmstr
COPY clients /qmstr/clients
COPY masterserver /qmstr/masterserver
COPY modules /qmstr/modules
COPY proto /qmstr/proto
COPY lib /qmstr/lib
COPY Makefile /qmstr/Makefile
COPY go.mod /qmstr/go.mod
COPY go.sum /qmstr/go.sum
COPY versions.env /qmstr/versions.env
ARG GOPROXY
WORKDIR /qmstr
RUN make gotest
RUN make install_qmstr_all
#  # runtime ##
#   the runtime stage contains all the elements needed to run the master and the analysis tools:
FROM runtimebase AS runtime
LABEL org.qmstr.image="runtime"
#   init html reporter data
COPY --from=builder /qmstr /tmp/qmstr
RUN /tmp/qmstr/modules/reporters/qmstr-reporter-html/setup.sh /usr/share/qmstr /tmp/qmstr
RUN rm -fr /tmp/qmstr
COPY ci/common.inc /common.inc
#   copy qmstr installation from previous stage
COPY --from=builder /usr/local/bin/* /usr/local/bin/
EXPOSE 50051/tcp
VOLUME /buildroot
#  # master ##
#   release master container, based on the runtime stage:
FROM runtime AS master
LABEL org.qmstr.image="master"
ENV GOPATH="/go"
ENV PATH="${GOPATH}/bin:$PATH"
COPY ci/docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
#  # web ##
#   debug stage for container running ratel
FROM ubuntu:18.04 AS web
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends socat=1.7.3.2-2ubuntu2 -y
COPY --from=runtime /usr/local/bin/dgraph-ratel /usr/local/bin/dgraph-ratel
COPY ci/ratel-entrypoint.sh /entrypoint.sh
EXPOSE 8000/tcp
EXPOSE 8080/tcp
CMD /entrypoint.sh
#  # dev ##
#   development container for the master and other tools, based on the runtime stage:
#   The $GOROOT/src directory can be mounted in as a volume, to allow testing of local changes.
FROM runtime AS dev
LABEL org.qmstr.image="dev"
ENV GOPATH="/go"
ENV PATH="${GOPATH}/bin:$PATH"
#   install golang
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 golang=2:1.10~4ubuntu1 autoconf=2.69-11 git=1:2.17.1-1ubuntu0.17 libio-captureoutput-perl=1.1104-1 python-dev=2.7.15~rc1-1 python-virtualenv=15.1.0+ds-1.1 protobuf-compiler=3.0.0-9.1ubuntu1.1 -y
EXPOSE 2345/tcp
#   install additional go deps
RUN go get -u -v github.com/derekparker/delve/cmd/dlv
VOLUME /go/src
#   cache wheels
COPY --from=builder /root/.pex/build /root/.pex/build
RUN rm /root/.pex/build/*qmstr*
COPY ci/dev-entrypoint.sh /dev-entrypoint.sh
RUN chmod +x /dev-entrypoint.sh
ENTRYPOINT ["/dev-entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
