FROM ubuntu:bionic
#  FROM arm64=arm64v8/ubuntu:bionic
#  get the apt-cacher proxy set
ARG APTPROXY
RUN echo "Acquire::http { Proxy \"$APTPROXY\"; };" >> /etc/apt/apt.conf.d/01proxy \
 && cat /etc/apt/apt.conf.d/01proxy \
 && apt-get update \
 && apt-get install --no-install-recommends build-essential ca-certificates cpio curl dosfstools gccgo genisoimage gettext git isolinux less libblkid-dev libmount-dev libselinux1-dev locales module-init-tools mtools openssh-client pkg-config qemu qemu-kvm rsync sudo syslinux-common vim wget xorriso xz-utils telnet -y
# ######### Dapper Configuration #####################
ENV DAPPER_ENV="VERSION DEV_BUILD RUNTEST DEBUG APTPROXY ENGINE_REGISTRY_MIRROR KERNEL_CHECK APPEND_SYSTEM_IMAGES APPEND_USER_IMAGES"
ENV DAPPER_DOCKER_SOCKET="true"
ENV DAPPER_SOURCE="/go/src/github.com/rancher/os"
ENV DAPPER_OUTPUT="./bin ./dist ./build/initrd ./build/kernel"
ENV DAPPER_RUN_ARGS="--privileged"
ENV TRASH_CACHE="${DAPPER_SOURCE}/.trash-cache"
ENV SHELL="/bin/bash"
WORKDIR ${DAPPER_SOURCE}
# ######### General Configuration #####################
ARG DAPPER_HOST_ARCH=amd64
ARG HOST_ARCH=${DAPPER_HOST_ARCH}
ARG ARCH=${HOST_ARCH}
ARG OS_REPO=rancher
ARG HOSTNAME_DEFAULT=rancher
ARG DISTRIB_ID=RancherOS
ARG SELINUX_POLICY_URL=https://github.com/rancher/refpolicy/releases/download/v0.0.3/policy.29
ARG KERNEL_VERSION=4.14.128-rancher
ARG KERNEL_URL_amd64=https://github.com/rancher/os-kernel/releases/download/v${KERNEL_VERSION}/linux-${KERNEL_VERSION}-x86.tar.gz
ARG KERNEL_URL_arm64=https://github.com/rancher/os-kernel/releases/download/v${KERNEL_VERSION}/linux-${KERNEL_VERSION}-arm64.tar.gz
ARG BUILD_DOCKER_URL_amd64=https://get.docker.com/builds/Linux/x86_64/docker-1.10.3
ARG BUILD_DOCKER_URL_arm64=https://github.com/rancher/docker/releases/download/v1.10.3-ros1/docker-1.10.3_arm64
ARG OS_RELEASES_YML=https://releases.rancher.com/os
ARG OS_SERVICES_REPO=https://raw.githubusercontent.com/${OS_REPO}/os-services
ARG IMAGE_NAME=${OS_REPO}/os
ARG OS_CONSOLE=default
ARG OS_AUTOFORMAT=false
ARG OS_BASE_URL_amd64=https://github.com/rancher/os-base/releases/download/v2018.02.11-1/os-base_amd64.tar.xz
ARG OS_BASE_URL_arm64=https://github.com/rancher/os-base/releases/download/v2018.02.11-1/os-base_arm64.tar.xz
ARG OS_INITRD_BASE_URL_amd64=https://github.com/rancher/os-initrd-base/releases/download/v2018.02.11-1/os-initrd-base-amd64.tar.gz
ARG OS_INITRD_BASE_URL_arm64=https://github.com/rancher/os-initrd-base/releases/download/v2018.02.11-1/os-initrd-base-arm64.tar.gz
ARG SYSTEM_DOCKER_VERSION=17.06-ros6
ARG SYSTEM_DOCKER_URL_amd64=https://github.com/rancher/os-system-docker/releases/download/${SYSTEM_DOCKER_VERSION}/docker-amd64-${SYSTEM_DOCKER_VERSION}.tgz
ARG SYSTEM_DOCKER_URL_arm64=https://github.com/rancher/os-system-docker/releases/download/${SYSTEM_DOCKER_VERSION}/docker-arm64-${SYSTEM_DOCKER_VERSION}.tgz
ARG USER_DOCKER_VERSION=18.06.3
ARG USER_DOCKER_ENGINE_VERSION=docker-${USER_DOCKER_VERSION}-ce
ARG AZURE_SERVICE=false
ARG PROXMOXVE_SERVICE=false
# #####################################################
#  Set up environment and export all ARGS as ENV
ENV ARCH="${ARCH}" \
    HOST_ARCH="${HOST_ARCH}" \
    XZ_DEFAULTS="-T0"
ENV BUILD_DOCKER_URL="BUILD_DOCKER_URL_${ARCH}" \
    BUILD_DOCKER_URL_amd64="${BUILD_DOCKER_URL_amd64}" \
    BUILD_DOCKER_URL_arm64="${BUILD_DOCKER_URL_arm64}" \
    DAPPER_HOST_ARCH="${DAPPER_HOST_ARCH}" \
    DISTRIB_ID="${DISTRIB_ID}" \
    DOWNLOADS="/usr/src/downloads" \
    GOPATH="/go" \
    GO_VERSION="1.8.5" \
    GOARCH="$ARCH" \
    HOSTNAME_DEFAULT="${HOSTNAME_DEFAULT}" \
    IMAGE_NAME="${IMAGE_NAME}" \
    KERNEL_VERSION="${KERNEL_VERSION}" \
    KERNEL_URL="KERNEL_URL_${ARCH}" \
    KERNEL_URL_amd64="${KERNEL_URL_amd64}" \
    KERNEL_URL_arm64="${KERNEL_URL_arm64}" \
    OS_BASE_URL="OS_BASE_URL_${ARCH}" \
    OS_BASE_URL_amd64="${OS_BASE_URL_amd64}" \
    OS_BASE_URL_arm64="${OS_BASE_URL_arm64}" \
    OS_INITRD_BASE_URL="OS_INITRD_BASE_URL_${ARCH}" \
    OS_INITRD_BASE_URL_amd64="${OS_INITRD_BASE_URL_amd64}" \
    OS_INITRD_BASE_URL_arm64="${OS_INITRD_BASE_URL_arm64}" \
    OS_RELEASES_YML="${OS_RELEASES_YML}" \
    OS_REPO="${OS_REPO}" \
    OS_SERVICES_REPO="${OS_SERVICES_REPO}" \
    OS_CONSOLE="${OS_CONSOLE}" \
    OS_AUTOFORMAT="${OS_AUTOFORMAT}" \
    REPO_VERSION="master" \
    SELINUX_POLICY_URL="${SELINUX_POLICY_URL}" \
    SYSTEM_DOCKER_URL="SYSTEM_DOCKER_URL_${ARCH}" \
    SYSTEM_DOCKER_URL_amd64="${SYSTEM_DOCKER_URL_amd64}" \
    SYSTEM_DOCKER_URL_arm64="${SYSTEM_DOCKER_URL_arm64}" \
    USER_DOCKER_VERSION="${USER_DOCKER_VERSION}" \
    USER_DOCKER_ENGINE_VERSION="${USER_DOCKER_ENGINE_VERSION}" \
    AZURE_SERVICE="${AZURE_SERVICE}" \
    PROXMOXVE_SERVICE="${PROXMOXVE_SERVICE}"
ENV PATH="${GOPATH}/bin:/usr/local/go/bin:$PATH"
RUN mkdir -p ${DOWNLOADS}
#  Download kernel
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
RUN echo "... Downloading ${!KERNEL_URL}" ; if [ -n "${!KERNEL_URL}" ] ; then curl -fL ${!KERNEL_URL} > ${DOWNLOADS}/kernel.tar.gz; fi
#  Download SELinux Policy
RUN curl -pfL ${SELINUX_POLICY_URL} > ${DOWNLOADS}/$( basename ${SELINUX_POLICY_URL} ;)
#  Install Go
RUN wget -O - https://storage.googleapis.com/golang/go${GO_VERSION}.linux-${GOARCH}.tar.gz | tar -xzf - -C /usr/local \
 && go get github.com/rancher/trash
RUN mkdir -p ${GOPATH}/src/golang.org/x \
 && cd ${GOPATH}/src/golang.org/x/ \
 && git clone https://github.com/golang/tools \
 && cd tools \
 && git checkout 6adeb8aab2ded9eb693b831d5fd090c10a6ebdfa -b temp \
 && go get golang.org/x/lint/golint
#  Install Host Docker
RUN curl -fL ${!BUILD_DOCKER_URL} > /usr/bin/docker \
 && chmod +x /usr/bin/docker
#  Install dapper
RUN curl -sL https://releases.rancher.com/dapper/latest/dapper-`uname -s `-`uname -m | sed 's/arm.*/arm/' ` > /usr/bin/dapper \
 && chmod +x /usr/bin/dapper
RUN cd ${DOWNLOADS} \
 && curl -pfL ${!OS_BASE_URL} | tar xvJf -
ENTRYPOINT ["./scripts/entry"]
CMD ["ci"]
