#  Common builder
ARG GO_IMAGE
FROM ${GO_IMAGE} AS builder
COPY hack/dockerfile/install/tini.installer /
COPY hack/dockerfile/install/proxy.installer /
RUN apt-get update \
 && apt-get install --no-install-recommends bash btrfs-tools ca-certificates cmake gcc git libc-dev libgcc-6-dev libltdl-dev libseccomp-dev libtool make -y
RUN grep "_COMMIT=" /*.installer | cut -f2- -d: > /binaries-commits
#  dockerd
FROM builder AS dockerd-builder
RUN apt-get install --no-install-recommends libsystemd-dev -y
WORKDIR /go/src/github.com/docker/docker
COPY . /go/src/github.com/docker/docker
ARG VERSION
ARG GITCOMMIT
ARG BUILDTIME
ARG PLATFORM
ARG PRODUCT
ARG DEFAULT_PRODUCT_LICENSE
ENV VERSION="${VERSION}"
ENV GITCOMMIT="${GITCOMMIT}"
ENV BUILDTIME="${BUILDTIME}"
ENV PLATFORM="${PLATFORM}"
ENV PRODUCT="${PRODUCT}"
ENV DEFAULT_PRODUCT_LICENSE="${DEFAULT_PRODUCT_LICENSE}"
#  TODO The way we set the version could easily be simplified not to depend on hack/...
RUN bash ./hack/make/.go-autogen
RUN go build -o /sbin/dockerd -tags 'autogen apparmor seccomp selinux journald exclude_graphdriver_devicemapper' -i -buildmode=pie -a -ldflags '-w' github.com/docker/docker/cmd/dockerd
#  docker-proxy
#  TODO if libnetwork folds into the docker tree this can be combined above
FROM builder AS proxy-builder
RUN git clone https://github.com/docker/libnetwork.git /go/src/github.com/docker/libnetwork
WORKDIR /go/src/github.com/docker/libnetwork
RUN . /binaries-commits \
 && git checkout -q "$LIBNETWORK_COMMIT" \
 && CGO_ENABLED=0 go build -buildmode=pie -ldflags="$PROXY_LDFLAGS" -o /sbin/docker-proxy github.com/docker/libnetwork/cmd/proxy
#  docker-init - TODO move this out, last time we bumped was 2016!
FROM builder AS init-builder
RUN git clone https://github.com/krallin/tini.git /tini
WORKDIR /tini
RUN . /binaries-commits \
 && git checkout -q "$TINI_COMMIT" \
 && cmake . \
 && make tini-static \
 && cp tini-static /sbin/docker-init
#  runc
FROM builder AS runc-builder
RUN apt-get install --no-install-recommends libseccomp-dev -y
RUN git clone https://github.com/opencontainers/runc.git /go/src/github.com/opencontainers/runc
WORKDIR /go/src/github.com/opencontainers/runc
RUN . /binaries-commits \
 && git checkout -q "$RUNC_COMMIT" \
 && make BUILDTAGS='seccomp apparmor' static \
 && make install
#  Final docker image
FROM scratch
ARG VERSION
ARG GITCOMMIT
ARG BUILDTIME
ARG PLATFORM
ARG ENGINE_IMAGE
COPY --from=dockerd-builder /sbin/dockerd /bin/
COPY --from=proxy-builder /sbin/docker-proxy /bin/
COPY --from=init-builder /sbin/docker-init /bin/
COPY --from=runc-builder /usr/local/sbin/runc /bin/
LABEL org.opencontainers.image.authors="Docker Inc." \
      org.opencontainers.image.created="${BUILDTIME}" \
      org.opencontainers.image.documentation="https://docs.docker.com/" \
      org.opencontainers.image.licenses="Apache-2.0" \
      org.opencontainers.image.revision="${GITCOMMIT}" \
      org.opencontainers.image.url="https://www.docker.com/products/docker-engine" \
      org.opencontainers.image.vendor="Docker Inc." \
      org.opencontainers.image.version="${VERSION}" \
      com.docker.distribution_based_engine="{\"platform\":\"${PLATFORM}\",\"engine_image\":\"${ENGINE_IMAGE}\",\"containerd_min_version\":\"1.2.0-beta.1\",\"runtime\":\"host_install\"}"
ENTRYPOINT ["/bin/dockerd"]
