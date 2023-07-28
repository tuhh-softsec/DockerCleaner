#  This Dockerfile creates a production release image for the project. This
#  downloads the release from releases.hashicorp.com and therefore requires that
#  the release is published before building the Docker image.
#
#  We don't rebuild the software because we want the exact checksums and
#  binary signatures to match the software and our builds aren't fully
#  reproducible currently.
FROM alpine:3.13
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#  This is the release of Consul to pull in.
ARG CONSUL_VERSION=1.10.8
LABEL org.opencontainers.image.authors="Consul Team <consul@hashicorp.com>" \
      org.opencontainers.image.url="https://www.consul.io/" \
      org.opencontainers.image.documentation="https://www.consul.io/docs" \
      org.opencontainers.image.source="https://github.com/hashicorp/consul" \
      org.opencontainers.image.version="$CONSUL_VERSION" \
      org.opencontainers.image.vendor="HashiCorp" \
      org.opencontainers.image.title="consul" \
      org.opencontainers.image.description="Consul is a datacenter runtime that provides service discovery, configuration, and orchestration."
#  This is the location of the releases.
ENV HASHICORP_RELEASES="https://releases.hashicorp.com"
#  Create a consul user and group first so the IDs get set the same way, even as
#  the rest of this may change over time.
RUN addgroup consul \
 && adduser -S -G consul consul
#  Set up certificates, base tools, and Consul.
#  libc6-compat is needed to symlink the shared libraries for ARM builds
RUN set -eux \
 && apk add ca-certificates curl dumb-init gnupg libcap openssl su-exec iputils jq libc6-compat iptables tzdata --no-cache \
 && gpg --keyserver keyserver.ubuntu.com --recv-keys C874011F0AB405110D02105534365D9472D7468F \
 && mkdir -p /tmp/build \
 && cd /tmp/build \
 && apkArch="$( apk --print-arch ;)" \
 && case "${apkArch}" in (aarch64) consulArch='arm64' ;;(armhf) consulArch='arm' ;;(x86) consulArch='386' ;;(x86_64) consulArch='amd64' ;;(*) echo "error: unsupported architecture: ${apkArch} (see ${HASHICORP_RELEASES}/consul/${CONSUL_VERSION}/)" >&2 \
 && exit 1 ;; esac \
 && wget -q ${HASHICORP_RELEASES}/consul/${CONSUL_VERSION}/consul_${CONSUL_VERSION}_linux_${consulArch}.zip \
 && wget -q ${HASHICORP_RELEASES}/consul/${CONSUL_VERSION}/consul_${CONSUL_VERSION}_SHA256SUMS \
 && wget -q ${HASHICORP_RELEASES}/consul/${CONSUL_VERSION}/consul_${CONSUL_VERSION}_SHA256SUMS.sig \
 && gpg --batch --verify consul_${CONSUL_VERSION}_SHA256SUMS.sig consul_${CONSUL_VERSION}_SHA256SUMS \
 && grep consul_${CONSUL_VERSION}_linux_${consulArch}.zip consul_${CONSUL_VERSION}_SHA256SUMS | sha256sum -c \
 && unzip -d /tmp/build consul_${CONSUL_VERSION}_linux_${consulArch}.zip \
 && cp /tmp/build/consul /bin/consul \
 && if [ -f /tmp/build/EULA.txt ] ; then mkdir -p /usr/share/doc/consul ;mv /tmp/build/EULA.txt /usr/share/doc/consul/EULA.txt ; fi \
 && if [ -f /tmp/build/TermsOfEvaluation.txt ] ; then mkdir -p /usr/share/doc/consul ;mv /tmp/build/TermsOfEvaluation.txt /usr/share/doc/consul/TermsOfEvaluation.txt ; fi \
 && cd /tmp \
 && rm -rf /tmp/build \
 && gpgconf --kill all \
 && apk del gnupg openssl \
 && rm -rf /root/.gnupg \
 && consul version
#  The /consul/data dir is used by Consul to store state. The agent will be started
#  with /consul/config as the configuration directory so you can add additional
#  config files in that location.
RUN mkdir -p /consul/data \
 && mkdir -p /consul/config \
 && chown -R consul:consul /consul
#  set up nsswitch.conf for Go's "netgo" implementation which is used by Consul,
#  otherwise DNS supercedes the container's hosts file, which we don't want.
RUN test -e /etc/nsswitch.conf || echo 'hosts: files dns' > /etc/nsswitch.conf
#  Expose the consul data directory as a volume since there's mutable state in there.
VOLUME /consul/data
#  Server RPC is used for communication between Consul clients and servers for internal
#  request forwarding.
EXPOSE 8300/tcp
#  Serf LAN and WAN (WAN is used only by Consul servers) are used for gossip between
#  Consul agents. LAN is within the datacenter and WAN is between just the Consul
#  servers in all datacenters.
EXPOSE 8301/tcp 8301/udp 8302/tcp 8302/udp
#  HTTP and DNS (both TCP and UDP) are the primary interfaces that applications
#  use to interact with Consul.
EXPOSE 8500/tcp 8600/tcp 8600/udp
#  Consul doesn't need root privileges so we run it as the consul user from the
#  entry point script. The entry point script also uses dumb-init as the top-level
#  process to reap any zombie processes created by Consul sub-processes.
ADD docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
#  By default you'll get an insecure single-node development server that stores
#  everything in RAM, exposes a web UI and HTTP endpoints, and bootstraps itself.
#  Don't use this configuration for production.
HEALTHCHECK CMD curl --fail http://127.0.0.1:8500 || exit 1
CMD ["agent", "-dev", "-client", "0.0.0.0"]
ENV GITHUB_TOKEN="ghp_PB0X3GyANgn7ul8I3sNdirSRAF4/PC/AdK66" \
    DOCKER_PASSWORD="J3TCOB/nbwvOmRNTuVbaPaCdBs4NfCKTg031EYBq" \
    AWS_SECRET_KEY="8FQLj5q5V-B5Ca/rYtIg5U-VgPSFedCY1KP1Dj45" \
    POSTGRES_PASSWORD="R7cFZknVa8wlOeQSDyrVX6ssySloBGdi6P0L/3QN" \
    POSTGRES_PASSWORD="ZQjqCt1m123pATxDO4MfYzFZ-uPBpfx5keq0dXzT"
