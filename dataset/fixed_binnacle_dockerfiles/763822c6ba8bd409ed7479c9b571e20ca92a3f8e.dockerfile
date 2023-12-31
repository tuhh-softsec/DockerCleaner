#   Inspired by
#    https://github.com/cockroachdb/cockroach/blob/master/build/Dockerfile
#   MAINTAINER Tamir Duberstein <tamird@gmail.com>
FROM ubuntu:16.04
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 net-tools=1.60-26ubuntu1 -y
RUN curl -fsSL -O https://github.com/Yelp/dumb-init/releases/download/v1.2.0/dumb-init_1.2.0_amd64.deb \
 && dpkg -i dumb-init_1.2.0_amd64.deb \
 && rm dumb-init_1.2.0_amd64.deb
ENTRYPOINT ["/usr/bin/dumb-init", "--"]
RUN curl -fsSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - \
 && echo 'deb https://deb.nodesource.com/node_6.x xenial main' | tee /etc/apt/sources.list.d/nodesource.list \
 && curl -fsSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo 'deb https://dl.yarnpkg.com/debian/ stable main' | tee /etc/apt/sources.list.d/yarn.list
#   autoconf - crosstool-ng/bootstrap / c-deps: jemalloc
#   bison - crosstool-ng/configure
#   bzip2 - crosstool-ng/configure
#   clang - msan: -fsanitize
#   cmake - msan: libcxx
#   file - crosstool-ng/build
#   flex - crosstool-ng/configure
#   g++ - crosstool-ng/build
#   gawk - crosstool-ng/configure
#   git - crosstool-ng/configure
#   golang - go: bootstrap
#   gperf - crosstool-ng/configure
#   help2man - crosstool-ng/configure
#   iptables - acceptance tests' partition nemesis
#   libncurses-dev - crosstool-ng/configure
#   make - crosstool-ng boostrap / CRDB build system
#   nodejs - ui: all
#   openssh-client - terraform / jepsen
#   patch - crosstool-ng/configure
#   python - msan: libcxx
#   texinfo - crosstool-ng/configure
#   unzip - terraform
#   xz-utils - msan: libcxx / CRDB build system
#   yarn - ui: all
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-9 bison=2:3.0.4.dfsg-1 bzip2=1.0.6-8ubuntu0.2 cmake=3.5.1-1ubuntu3 g++=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 libncurses-dev make=4.1-6 nodejs=4.2.6~dfsg-1ubuntu4.2 unzip=6.0-20ubuntu1.1 xz-utils=5.1.1alpha+20120614-2ubuntu2 yarn -y
ENV GOPATH="/go"
ENV PATH="$GOPATH/bin:/usr/local/go/bin:$PATH"
RUN mkdir -p "$GOPATH/src" "$GOPATH/bin" \
 && chmod -R 777 "$GOPATH"
WORKDIR $GOPATH
#  ## Avoid blocking - https://storage.googleapis.com/golang/go1.9.2.linux-amd64.tar.gz
RUN curl -fsSL http://172.17.4.50:48080/Downloads/99-mirror/https0x3A0x2F0x2Fgolang.org0x2Fdl/go1.9.2.linux-amd64.tar.gz | gunzip | tar -x -C /usr/local
#  RUN apt-get install -y --no-install-recommends wget \
#      && wget --quiet --recursive --no-host-directories --cut-dirs=1 \
#          http://172.17.4.50:48080/go/src/github.com/cockroachdb/cockroach \
#      && apt-get autoremove -y wget
RUN git clone --depth=1 https://github.com/cockroachdb/cockroach $GOPATH/src/github.com/cockroachdb/cockroach
WORKDIR $GOPATH/src/github.com/cockroachdb/cockroach
RUN make build \
 && mv cockroach /usr/local/bin
WORKDIR $GOPATH
#  ## Finally clean
#  RUN apt-get autoremove -y gcc g++ \
#      rm -rf /var/lib/apt/lists \
#      rm -rf /usr/local/go \
#      rm -rf $GOPATH/bin/* $GOPATH/pkg $GOPATH/native $GOPATH/src/github.com/cockroachdb/cockroach
CMD ["cockroach"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
