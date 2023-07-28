FROM ubuntu:18.04
ENV DEBIAN_FRONTEND="noninteractive"
#  Common deps
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 xz-utils=5.2.2-1.3ubuntu0.1 wget=1.19.4-1ubuntu2.2 gpg=2.2.4-1ubuntu1.6 -y
#  Install node and yarn
#  From: https://github.com/nodejs/docker-node/blob/6b8d86d6ad59e0d1e7a94cec2e909cad137a028f/8/Dockerfile
#   gpg keys listed at https://github.com/nodejs/node#release-keys
RUN set -ex \
 && for key in 4ED778F539E3634C779C87C6D7062848A1AB005C B9E2F5981AA6E0CD28160D9FF13993A75599653C 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 B9AE9905FFD7803F25714661B63B535A4C206CA9 77984A986EBC2AA786BC0F66B01FBB92821C587A 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 FD3A5288F042B6850C66B31F09FE44734EB7990E 8FCCA13FEF1D0C2E91008E09770F7A9A5AE15600 C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 DD8F2338BAE7501E3DD5AC78C273792F7D83545D A48C2BEE680E841632CD4E44F07496B3EB3C1762; do gpg --batch --keyserver ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver pgp.mit.edu --recv-keys "$key" || gpg --batch --keyserver keyserver.pgp.com --recv-keys "$key" || gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done
ARG NODE_VERSION=10.15.3
ENV NODE_VERSION="$NODE_VERSION"
RUN ARCH= \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && case "${dpkgArch##*-}" in (amd64) ARCH='x64' ;;(ppc64el) ARCH='ppc64le' ;;(s390x) ARCH='s390x' ;;(arm64) ARCH='arm64' ;;(armhf) ARCH='armv7l' ;;(i386) ARCH='x86' ;;(*) echo "unsupported architecture" ; exit 1 ;; esac \
 && curl -SLO "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-$ARCH.tar.xz" \
 && curl -SLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " node-v$NODE_VERSION-linux-$ARCH.tar.xz$" SHASUMS256.txt | sha256sum -c - \
 && tar -xJf "node-v$NODE_VERSION-linux-$ARCH.tar.xz" -C /usr/local --strip-components=1 --no-same-owner \
 && rm "node-v$NODE_VERSION-linux-$ARCH.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt \
 && ln -s /usr/local/bin/node /usr/local/bin/nodejs
ENV YARN_VERSION="1.13.0"
RUN set -ex \
 && for key in 6A010C5166006599AA17F08146C2130DFD2497F5; do gpg --batch --keyserver ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver pgp.mit.edu --recv-keys "$key" || gpg --batch --keyserver keyserver.pgp.com --recv-keys "$key" || gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done \
 && curl -fSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz" \
 && curl -fSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz.asc" \
 && gpg --batch --verify yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz \
 && mkdir -p /opt/yarn \
 && tar -xzf yarn-v$YARN_VERSION.tar.gz -C /opt/yarn --strip-components=1 \
 && ln -s /opt/yarn/bin/yarn /usr/local/bin/yarn \
 && ln -s /opt/yarn/bin/yarn /usr/local/bin/yarnpkg \
 && rm yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz
#  Developer tools
#  # Git and sudo (sudo needed for user override)
RUN apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 sudo=1.8.21p2-3ubuntu1.5 -y
#  LSPs
#  #GO
ENV GO_VERSION="1.11.4"
ENV GOPATH="/usr/local/go-packages"
ENV GO_ROOT="/usr/local/go"
ENV PATH="$PATH:/usr/local/go/bin"
ENV PATH="$PATH:${GOPATH}/bin"
RUN curl -sS https://storage.googleapis.com/golang/go$GO_VERSION.linux-amd64.tar.gz | tar -C /usr/local -xzf - \
 && go get -u -v github.com/nsf/gocode \
 && go get -u -v github.com/uudashr/gopkgs/cmd/gopkgs \
 && go get -u -v github.com/ramya-rao-a/go-outline \
 && go get -u -v github.com/acroca/go-symbols \
 && go get -u -v golang.org/x/tools/cmd/guru \
 && go get -u -v golang.org/x/tools/cmd/gorename \
 && go get -u -v github.com/fatih/gomodifytags \
 && go get -u -v github.com/haya14busa/goplay/cmd/goplay \
 && go get -u -v github.com/josharian/impl \
 && go get -u -v github.com/tylerb/gotype-live \
 && go get -u -v github.com/rogpeppe/godef \
 && go get -u -v golang.org/x/tools/cmd/godoc \
 && go get -u -v github.com/zmb3/gogetdoc \
 && go get -u -v golang.org/x/tools/cmd/goimports \
 && go get -u -v sourcegraph.com/sqs/goreturns \
 && go get -u -v github.com/golang/lint/golint \
 && go get -u -v github.com/cweill/gotests/... \
 && go get -u -v github.com/alecthomas/gometalinter \
 && go get -u -v honnef.co/go/tools/... \
 && go get -u -v github.com/sourcegraph/go-langserver \
 && go get -u -v github.com/derekparker/delve/cmd/dlv \
 && go get -u -v github.com/davidrjenni/reftools/cmd/fillstruct
#  Java
RUN apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 maven=3.6.0-1~18.04.1 gradle=4.4.1-5ubuntu2~18.04 -y
#  C/C++
#   public LLVM PPA, development version of LLVM
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
 && echo "deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic main" > /etc/apt/sources.list.d/llvm.list \
 && apt-get update \
 && apt-get install --no-install-recommends clang-tools-9=1:9-2~ubuntu18.04.2 -y \
 && ln -s /usr/bin/clangd-9 /usr/bin/clangd
#  Python 2
RUN apt-get update \
 && apt-get install --no-install-recommends python=2.7.15~rc1-1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 -y \
 && pip install python-language-server==0.36.2
#  PHP
RUN apt-get install --no-install-recommends php=1:7.2+60ubuntu1 curl=7.58.0-2ubuntu3.24 php-cli=1:7.2+60ubuntu1 php-mbstring=1:7.2+60ubuntu1 unzip=6.0-21ubuntu1.2 -y
#   https://getcomposer.org/doc/faqs/how-to-install-composer-programmatically.md
#   https://linuxconfig.org/how-to-install-php-composer-on-debian-linux
RUN curl -s -o composer-setup.php https://getcomposer.org/installer \
 && php composer-setup.php --install-dir=/usr/local/bin --filename=composer \
 && rm composer-setup.php
#  Ruby
RUN apt-get install --no-install-recommends ruby=1:2.5.1 ruby-dev=1:2.5.1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y \
 && gem install solargraph --version 0.49.0
#  # User account
RUN adduser --disabled-password --gecos '' theia \
 && adduser theia sudo \
 && echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chmod g+rw /home \
 && mkdir -p /home/project \
 && chown -R theia:theia /home/theia \
 && chown -R theia:theia /home/project
#  Theia
#  #Needed for node-gyp, nsfw build
RUN apt-get update \
 && apt-get install --no-install-recommends python=2.7.15~rc1-1 build-essential=12.4ubuntu1 -y
USER theia
ARG version=latest
WORKDIR /home/theia
COPY $version.package.json ./package.json
ARG GITHUB_TOKEN
#   using "NODE_OPTIONS=..." to avoid out-of-memory problem in CI
RUN yarn --cache-folder ./ycache \
 && rm -rf ./ycache \
 && NODE_OPTIONS="--max_old_space_size=4096" yarn theia build
EXPOSE 3000/tcp
ENV SHELL="/bin/bash"
ENTRYPOINT ["yarn", "theia", "start", "/home/project", "--hostname=0.0.0.0"]
# Please add your HEALTHCHECK here!!!
