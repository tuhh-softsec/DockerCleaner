FROM alpine:3.9 AS rootfs-stage
MAINTAINER sparkyballs,thelamer
#   environment
ENV REL="bionic"
ENV ARCH="amd64"
#   install packages
RUN apk add bash=4.4.19-r1 curl=7.64.0-r5 tzdata=2020c-r1 xz=5.2.4-r0 --no-cache
#   grab base tarball
RUN mkdir /root-out \
 && curl -o /rootfs.tar.gz -L https://partner-images.canonical.com/core/${REL}/current/ubuntu-${REL}-core-cloudimg-${ARCH}-root.tar.gz \
 && tar xf /rootfs.tar.gz -C /root-out
#   Runtime stage
FROM scratch
COPY --from=rootfs-stage /root-out/ /
ARG BUILD_DATE
ARG VERSION
LABEL build_version="Linuxserver.io version:- ${VERSION} Build-date:- ${BUILD_DATE}"
LABEL MAINTAINER="sparkyballs,TheLamer"
#   set version for s6 overlay
ARG OVERLAY_VERSION="v1.22.0.0"
ARG OVERLAY_ARCH="amd64"
#   set environment variables
ARG DEBIAN_FRONTEND="noninteractive"
ENV HOME="/root" \
    LANGUAGE="en_US.UTF-8" \
    LANG="en_US.UTF-8" \
    TERM="xterm"
#   copy sources
COPY sources.list /etc/apt/
RUN echo "**** Ripped from Ubuntu Docker Logic ****" \
 && set -xe \
 && echo '#!/bin/sh' > /usr/sbin/policy-rc.d \
 && echo 'exit 101' >> /usr/sbin/policy-rc.d \
 && chmod +x /usr/sbin/policy-rc.d \
 && dpkg-divert --local --rename --add /sbin/initctl \
 && cp -a /usr/sbin/policy-rc.d /sbin/initctl \
 && sed -i 's/^exit.*/exit 0/' /sbin/initctl \
 && echo 'force-unsafe-io' > /etc/dpkg/dpkg.cfg.d/docker-apt-speedup \
 && echo 'DPkg::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };' > /etc/apt/apt.conf.d/docker-clean \
 && echo 'APT::Update::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };' >> /etc/apt/apt.conf.d/docker-clean \
 && echo 'Dir::Cache::pkgcache ""; Dir::Cache::srcpkgcache "";' >> /etc/apt/apt.conf.d/docker-clean \
 && echo 'Acquire::Languages "none";' > /etc/apt/apt.conf.d/docker-no-languages \
 && echo 'Acquire::GzipIndexes "true"; Acquire::CompressionTypes::Order:: "gz";' > /etc/apt/apt.conf.d/docker-gzip-indexes \
 && echo 'Apt::AutoRemove::SuggestsImportant "false";' > /etc/apt/apt.conf.d/docker-autoremove-suggests \
 && mkdir -p /run/systemd \
 && echo 'docker' > /run/systemd/container \
 && echo "**** install apt-utils and locales ****" \
 && apt-get update \
 && apt-get install --no-install-recommends apt-utils locales -y \
 && echo "**** install packages ****" \
 && apt-get install --no-install-recommends curl tzdata -y \
 && echo "**** generate locale ****" \
 && locale-gen en_US.UTF-8 \
 && echo "**** add s6 overlay ****" \
 && curl -o /tmp/s6-overlay.tar.gz -L "https://github.com/just-containers/s6-overlay/releases/download/${OVERLAY_VERSION}/s6-overlay-${OVERLAY_ARCH}.tar.gz" \
 && tar xfz /tmp/s6-overlay.tar.gz -C / \
 && echo "**** create abc user and make our folders ****" \
 && useradd -u 911 -U -d /config -s /bin/false abc \
 && usermod -G users abc \
 && mkdir -p /app /config /defaults \
 && echo "**** cleanup ****" \
 && apt-get clean \
 && rm -rf /tmp/* /var/lib/apt/lists/* /var/tmp/*
#   add local files
COPY root/ /
ENTRYPOINT ["/init"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
