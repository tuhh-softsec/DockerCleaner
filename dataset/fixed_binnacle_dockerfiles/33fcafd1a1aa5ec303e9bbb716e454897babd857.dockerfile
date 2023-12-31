#  ##
#   Builds a container used for compiling the tectonic-installer binaries and javascript applications.
#
#   Node install inspired by: https://github.com/dockerfile/nodejs/blob/master/Dockerfile
#
#   Update the builder image manually by running:
#   (See README.md/##Upstream-and-CoreOS-Terraform)
#
#   docker build -t quay.io/coreos/tectonic-builder:<next-semver> -f images/builder/Dockerfile .
#   docker push quay.io/coreos/tectonic-builder:<next-semver>
#
#   docker build \
#   --build-arg TERRAFORM_URL=<upstream terraform download url> \
#   -t quay.io/coreos/tectonic-builder:<next-semver>-upstream-terraform \
#   -f images/builder/Dockerfile .
#   docker push quay.io/coreos/tectonic-builder:<next-semver>-upstream-terraform
#  ##
FROM golang:1.9.2-stretch
#  ## For golang testing stuff
RUN go get -u github.com/golang/lint/golint
RUN go get github.com/jstemmer/go-junit-report
#  ## Tools used by 'make structure-check'
RUN go get github.com/segmentio/terraform-docs
RUN go get github.com/s-urbaniak/terraform-examples
RUN go get github.com/bronze1man/yaml2json
#  ## License parser
RUN go get github.com/coreos/license-bill-of-materials
#  ## 'grafiti' for cluster cleanup
ENV GRAFITI_VERSION="\"v0.1.1\""
RUN git clone -q https://github.com/coreos/grafiti.git ${GOPATH}/src/github.com/coreos/grafiti \
 && cd ${GOPATH}/src/github.com/coreos/grafiti \
 && git checkout -q tags/${GRAFITI_VERSION} \
 && make install
#   /go needs to be writable by jenkins user like it is in the upstream golang image
RUN chmod 777 -R /go
#  ## Install Shellcheck, Terraform, NodeJS and Yarn
ENV SHELLCHECK_VERSION="v0.4.6"
ENV TERRAFORM_VERSION="0.10.7"
ENV NODE_VERSION="v8.1.2"
ENV YARN_VERSION="v0.24.6"
ENV MATCHBOXVERSION="v0.6.1"
#   yarn needs a home writable by any user running the container
ENV HOME="/opt/home"
RUN mkdir -p ${HOME}
RUN chmod 777 -R ${HOME}
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential sudo curl wget git autoconf automake unzip libtool jq awscli gnupg1 openvpn xvfb xauth -y -q
#   Install Terraform
#   TERRAFORM_URL enables us to build the upstream-terraform Tectonic builder
#   image (See README.md/##Upstream-and-CoreOS-Terraform)
ARG TERRAFORM_URL=https://releases.hashicorp.com/terraform/${TERRAFORM_VERSION}/terraform_${TERRAFORM_VERSION}_linux_amd64.zip
RUN curl -L ${TERRAFORM_URL} | funzip > /usr/local/bin/terraform \
 && chmod +x /usr/local/bin/terraform
#   Install NodeJS
RUN cd /tmp \
 && wget --quiet -O /tmp/node.tar.gz https://nodejs.org/dist/${NODE_VERSION}/node-${NODE_VERSION}-linux-x64.tar.gz \
 && tar xf node.tar.gz \
 && rm -f /tmp/node.tar.gz \
 && cd node-* \
 && cp -r lib/node_modules /usr/local/lib/node_modules \
 && cp bin/node /usr/local/bin \
 && ln -s /usr/local/lib/node_modules/npm/bin/npm-cli.js /usr/local/bin/npm
#   so any container user can install global node modules if needed
RUN chmod 777 /usr/local/lib/node_modules
#   Install Yarn
RUN cd /tmp \
 && wget --quiet -O /tmp/yarn.tar.gz https://github.com/yarnpkg/yarn/releases/download/${YARN_VERSION}/yarn-${YARN_VERSION}.tar.gz \
 && tar xf yarn.tar.gz \
 && rm -f /tmp/yarn.tar.gz \
 && mv /tmp/dist /usr/local/yarn \
 && ln -s /usr/local/yarn/bin/yarn /usr/local/bin/yarn
#   Install Azure-cli
RUN yarn global add azure-cli
#   Install Shellcheck
RUN cd /tmp \
 && wget --quiet https://storage.googleapis.com/shellcheck/shellcheck-${SHELLCHECK_VERSION}.linux.x86_64.tar.xz \
 && tar xJf shellcheck-${SHELLCHECK_VERSION}.linux.x86_64.tar.xz \
 && mv /tmp/shellcheck-${SHELLCHECK_VERSION}/shellcheck /usr/local/bin/shellcheck
#   Install Chrome for installer gui tests
#   Use Chrome beta because v60 or higher is needed for headless mode
RUN wget --quiet -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list' \
 && apt-get update \
 && apt-get install --no-install-recommends google-chrome-beta ca-certificates -y -q
#   matchbox for baremetal installer gui tests
RUN cd /tmp \
 && wget --quiet -O /tmp/matchbox.tar.gz https://github.com/coreos/matchbox/releases/download/${MATCHBOXVERSION}/matchbox-${MATCHBOXVERSION}-linux-amd64.tar.gz \
 && tar xzvf /tmp/matchbox.tar.gz \
 && rm -f /tmp/matchbox.tar.gz \
 && cp matchbox-*/matchbox /usr/local/bin
#   cleanup
RUN rm -rf /tmp/node-v*
RUN rm -f /tmp/yarn.tar.gz
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
