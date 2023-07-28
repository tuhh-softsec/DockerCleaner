FROM ubuntu:16.04
LABEL name="deis-go-dev" \
      maintainer="Matt Boersma <matt.boersma@microsoft.com>"
ENV AZCLI_VERSION="2.0.66" \
    GO_VERSION="1.12.6" \
    GLIDE_VERSION="v0.13.2" \
    GLIDE_HOME="/root" \
    HELM_VERSION="v2.14.1" \
    KUBECTL_VERSION="v1.12.8" \
    ETCDCTL_VERSION="v3.1.8" \
    GOLANGCI_LINT_VERSION="v1.17.1" \
    PROTOBUF_VERSION="3.7.0" \
    SHELLCHECK_VERSION="v0.6.0" \
    SHFMT_VERSION="2.6.4" \
    PATH="$PATH:/usr/local/go/bin:/go/bin:/usr/local/bin/docker" \
    GOPATH="/go"
#   This is a huge one-liner to optimize the Docker image layer.
#   We disable source repos to speed up apt-get update.
RUN sed -i -e 's/^deb-src/#deb-src/' /etc/apt/sources.list \
 && export DEBIAN_FRONTEND=noninteractive \
 && apt-get update \
 && apt-get upgrade -y --no-install-recommends \
 && apt-get install --no-install-recommends bash=4.3-14ubuntu1.4 build-essential=12.1ubuntu2 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 git-core=1:2.7.4-0ubuntu1.10 jq=1.5+dfsg-1ubuntu0.1 libffi-dev=3.2.1-4 libssl-dev=1.0.2g-1ubuntu4.20 man mercurial=3.7.3-1ubuntu1.2 net-tools=1.60-26ubuntu1 netcat=1.10-41 openssh-client=1:7.2p2-4ubuntu2.10 procps=2:3.3.10-4ubuntu2.5 python=2.7.12-1~16.04 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python-setuptools=20.7.0-1 rsync=3.1.1-3ubuntu1.3 ruby=1:2.3.0+1 unzip=6.0-20ubuntu1.1 upx util-linux=2.27.1-6ubuntu3.10 vim=2:7.4.1689-3ubuntu1.5 wamerican=7.1-1 wget=1.17.1-1ubuntu1.5 zip=3.0-11 -y \
 && curl -L https://storage.googleapis.com/golang/go${GO_VERSION}.linux-amd64.tar.gz | tar -C /usr/local -xz \
 && curl -sSL https://github.com/Masterminds/glide/releases/download/${GLIDE_VERSION}/glide-${GLIDE_VERSION}-linux-amd64.tar.gz | tar -vxz -C /usr/local/bin --strip=1 \
 && curl -sSL -o /tmp/protoc.zip https://github.com/google/protobuf/releases/download/v${PROTOBUF_VERSION}/protoc-${PROTOBUF_VERSION}-linux-x86_64.zip \
 && unzip /tmp/protoc.zip 'bin/protoc' -d /usr/local \
 && rm /tmp/protoc.zip \
 && curl -sSL https://storage.googleapis.com/kubernetes-release/release/${KUBECTL_VERSION}/bin/linux/amd64/kubectl -o /usr/local/bin/kubectl \
 && chmod +x /usr/local/bin/kubectl \
 && mkdir -p ${GOPATH}/src/k8s.io/helm \
 && curl -sSL https://storage.googleapis.com/kubernetes-helm/helm-${HELM_VERSION}-linux-amd64.tar.gz | tar -vxz -C /usr/local/bin --strip=1 \
 && mkdir -p /go/bin \
 && curl https://raw.githubusercontent.com/golang/dep/master/install.sh | sh \
 && curl -sSL https://aka.ms/downloadazcopy-v10-linux | tar -vxz -C /usr/local/bin --strip=1 \
 && mv /usr/local/bin/azcopy /usr/local/bin/azcopy-preview \
 && wget https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod/pool/main/a/azcopy/azcopy_7.2.0-netcore_ubuntu16.04_x64.deb \
 && dpkg -i azcopy_7.2.0-netcore_ubuntu16.04_x64.deb || echo "missing dependencies" \
 && rm -f azcopy_7.2.0-netcore_ubuntu16.04_x64.deb \
 && apt-get update \
 && apt-get install --no-install-recommends -f -y \
 && curl -fsSLO https://get.docker.com/builds/Linux/x86_64/docker-17.05.0-ce.tgz \
 && tar xzvf docker-17.05.0-ce.tgz -C /usr/local/bin \
 && chmod +x -R /usr/local/bin/docker \
 && rm docker-17.05.0-ce.tgz \
 && curl -L https://github.com/coreos/etcd/releases/download/${ETCDCTL_VERSION}/etcd-${ETCDCTL_VERSION}-linux-amd64.tar.gz -o /tmp/etcd-${ETCDCTL_VERSION}.tar.gz \
 && tar -C /tmp -xvzf /tmp/etcd-${ETCDCTL_VERSION}.tar.gz --strip-components=1 etcd-${ETCDCTL_VERSION}-linux-amd64/etcdctl \
 && mv /tmp/etcdctl /usr/local/bin/etcdctl \
 && rm /tmp/etcd-${ETCDCTL_VERSION}.tar.gz \
 && go get -u -v github.com/AlekSi/gocov-xml github.com/axw/gocov/gocov github.com/constabulary/gb/... github.com/derekparker/delve/cmd/dlv github.com/dgrijalva/jwt-go/cmd/jwt github.com/golang/protobuf/protoc-gen-go github.com/haya14busa/goverage github.com/jteeuwen/go-bindata/... github.com/mitchellh/gox github.com/onsi/ginkgo/ginkgo github.com/hashicorp/packer gopkg.in/alecthomas/gometalinter.v2 \
 && ln -s ${GOPATH}/bin/gometalinter.v2 ${GOPATH}/bin/gometalinter \
 && gometalinter.v2 --install \
 && curl -sfL https://install.goreleaser.com/github.com/golangci/golangci-lint.sh | sh -s -- -b ${GOPATH}/bin ${GOLANGCI_LINT_VERSION} \
 && curl -o /usr/local/bin/shellcheck -sSL https://shellcheck.storage.googleapis.com/shellcheck-${SHELLCHECK_VERSION}.linux-x86_64 \
 && chmod +x /usr/local/bin/shellcheck \
 && go get -u mvdan.cc/sh/cmd/shfmt \
 && git -C "$GOPATH/src/mvdan.cc/sh" checkout -q "v$SHFMT_VERSION" \
 && go install -a -ldflags '-extldflags "-static"' mvdan.cc/sh/cmd/shfmt \
 && pip install shyaml==0.6.2 azure-cli==${AZCLI_VERSION} --disable-pip-version-check --no-cache-dir \
 && apt-get purge --auto-remove -y libffi-dev python-dev python-pip \
 && apt-get autoremove -y \
 && apt-get clean -y \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man /usr/share/doc ${GOPATH}/pkg/* ${GOPATH}/src/* /root/cache /root/.cache \
 && go clean -cache -testcache -modcache
WORKDIR /go
COPY . /
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
