FROM ubuntu:bionic
ENV JQ_VERSION="1.5"
ENV JQ_CHECKSUM="d8e36831c3c94bb58be34dd544f44a6c6cb88568"
ENV go_version="1.12.6"
ENV cf_cli_version="6.43.0"
ENV bosh_cli_version="5.5.1"
ENV bbl_version="8.1.0"
ENV terraform_version="0.12.3"
ENV credhub_cli_version="2.5.1"
ENV git_crypt_version="0.6.0"
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.6.14 build-essential=12.4ubuntu1 git=1:2.17.1-1ubuntu0.17 libreadline7=7.0-3 libreadline6-dev libsqlite3-dev=3.22.0-1ubuntu0.7 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libxslt-dev libyaml-dev=0.1.7-2ubuntu3 netcat-openbsd=1.187-1ubuntu0.1 openssl=1.1.1-1ubuntu2.1~18.04.21 software-properties-common=0.96.24.32.20 sqlite=2.8.17-14fakesync1 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 curl=7.58.0-2ubuntu3.24 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 zlibc=0.9k-4.3 -y \
 && add-apt-repository ppa:brightbox/ruby-ng -y \
 && apt-get update \
 && apt-get install --no-install-recommends ruby2.5=2.5.1-1ubuntu1.13 ruby2.5-dev=2.5.1-1ubuntu1.13 -y \
 && apt-get remove -y --purge software-properties-common
#   jq
RUN wget https://github.com/stedolan/jq/releases/download/jq-${JQ_VERSION}/jq-linux64 --output-document="/usr/bin/jq" \
 && cd /usr/bin \
 && echo "${JQ_CHECKSUM} jq" | sha1sum -c - \
 && chmod +x jq
#   bosh-cli
RUN wget https://s3.amazonaws.com/bosh-cli-artifacts/bosh-cli-${bosh_cli_version}-linux-amd64 --output-document="/usr/bin/bosh" \
 && chmod +x /usr/bin/bosh
#   cf-cli
RUN cd /tmp \
 && wget -q -O cf.deb "https://cli.run.pivotal.io/stable?release=debian64&version=${cf_cli_version}&source=github-rel" \
 && dpkg -i cf.deb \
 && rm cf.deb
#   credhub-cli
RUN wget https://github.com/cloudfoundry-incubator/credhub-cli/releases/download/${credhub_cli_version}/credhub-linux-${credhub_cli_version}.tgz -P /tmp \
 && tar xzvf /tmp/credhub-linux-${credhub_cli_version}.tgz -C /usr/local/bin \
 && chmod +x /usr/local/bin/credhub
#   bbl and dependencies
RUN wget https://github.com/cloudfoundry/bosh-bootloader/releases/download/v${bbl_version}/bbl-v${bbl_version}_linux_x86-64 -P /tmp \
 && mv /tmp/bbl-* /usr/local/bin/bbl \
 && cd /usr/local/bin \
 && chmod +x bbl
RUN wget https://github.com/cloudfoundry/bosh-bootloader/archive/v${bbl_version}.tar.gz -P /tmp \
 && mkdir -p /var/repos/bosh-bootloader \
 && tar xvf /tmp/v${bbl_version}.tar.gz --strip-components=1 -C /var/repos/bosh-bootloader \
 && rm -rf /tmp/*
RUN wget "https://releases.hashicorp.com/terraform/${terraform_version}/terraform_${terraform_version}_linux_amd64.zip" -P /tmp \
 && cd /tmp \
 && curl https://releases.hashicorp.com/terraform/${terraform_version}/terraform_${terraform_version}_SHA256SUMS | grep linux_amd64 | shasum -c - \
 && unzip "/tmp/terraform_${terraform_version}_linux_amd64.zip" -d /tmp \
 && mv /tmp/terraform /usr/local/bin/terraform \
 && cd /usr/local/bin \
 && chmod +x terraform \
 && rm -rf /tmp/*
#   git-crypt
RUN wget https://github.com/AGWA/git-crypt/archive/${git_crypt_version}.tar.gz -O /tmp/git-crypt.tar.gz \
 && tar xzvf /tmp/git-crypt.tar.gz -C /tmp \
 && cd /tmp/git-crypt-${git_crypt_version} \
 && make PREFIX=/usr/local install \
 && rm -rf /tmp/*
ENV GOPATH="/go"
ENV PATH="/go/bin:/usr/local/go/bin:$PATH"
RUN wget https://golang.org/dl/go${go_version}.linux-amd64.tar.gz -P /tmp \
 && tar xzvf /tmp/go${go_version}.linux-amd64.tar.gz -C /usr/local \
 && mkdir ${GOPATH} \
 && rm -rf /tmp/*
#   Log Cache CLI
RUN go get -u code.cloudfoundry.org/log-cache-cli/cmd/cf-lc-plugin \
 && cf install-plugin ${GOPATH}/bin/cf-lc-plugin -f
RUN go get -u github.com/cloudfoundry/uptimer \
 && go get -u github.com/onsi/ginkgo/... \
 && cd ${GOPATH}/src/github.com/cloudfoundry/uptimer \
 && ginkgo -r -randomizeSuites -randomizeAllSpecs \
 && cd -
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
