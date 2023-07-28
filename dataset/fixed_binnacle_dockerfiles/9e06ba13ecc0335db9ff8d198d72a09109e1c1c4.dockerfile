FROM ubuntu:18.04
LABEL maintainer="Michael Mayer <michael@liquidbytes.net>"
ARG BUILD_TAG
ENV DEBIAN_FRONTEND="noninteractive"
#   Configure apt-get
RUN echo 'Acquire::Retries "10";' > /etc/apt/apt.conf.d/80retry
RUN echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/80recommends
RUN echo 'APT::Install-Suggests "false";' > /etc/apt/apt.conf.d/80suggests
RUN echo 'APT::Get::Assume-Yes "true";' > /etc/apt/apt.conf.d/80forceyes
RUN echo 'APT::Get::Fix-Missing "true";' > /etc/apt/apt.conf.d/80fixmissin
#   Install dev / build dependencies
RUN apt-get update \
 && apt-get upgrade \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 curl=7.58.0-2ubuntu3.24 chrpath=0.16-2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libxft-dev=2.3.2-1 libfreetype6=2.8.1-2ubuntu2.2 libfreetype6-dev=2.8.1-2ubuntu2.2 libfontconfig1=2.12.6-0ubuntu2 libfontconfig1-dev=2.12.6-0ubuntu2 libhdf5-serial-dev=1.10.0-patch1+docs-4 libpng-dev=1.6.34-1ubuntu0.18.04.2 libzmq3-dev=4.2.5-1ubuntu0.2 pkg-config=0.29.1-0ubuntu2 software-properties-common=0.96.24.32.20 rsync=3.1.2-2.1ubuntu1.6 unzip=6.0-21ubuntu1.2 zip=3.0-11build1 g++=4:7.4.0-1ubuntu2.3 gcc=4:7.4.0-1ubuntu2.3 libc6-dev=2.27-3ubuntu1.6 gpg-agent=2.2.4-1ubuntu1.6 apt-utils=1.6.14 make=4.1-9.1ubuntu1 nano=2.9.3-2 wget=1.19.4-1ubuntu2.2 git=1:2.17.1-1ubuntu0.17 mysql-client=5.7.41-0ubuntu0.18.04.1 libgtk-3-bin=3.22.30-1ubuntu4 tzdata=2022g-0ubuntu0.18.04 gconf-service=3.2.6-4ubuntu1 chromium-browser=111.0.5563.64-0ubuntu0.18.04.5 firefox=111.0.1+build2-0ubuntu0.18.04.1 libheif-examples=1.1.0-2 exiftool
#   Install RAW to JPEG converter
RUN add-apt-repository ppa:pmjdebruijn/darktable-release \
 && apt-get update \
 && apt-get install --no-install-recommends darktable=2.4.2-1 \
 && apt-get upgrade \
 && apt-get dist-upgrade
#   Install TensorFlow C library
RUN curl -L "https://storage.googleapis.com/tensorflow/libtensorflow/libtensorflow-cpu-linux-x86_64-1.13.1.tar.gz" | tar -C "/usr/local" -xz
RUN ldconfig
#   Show TensorFlow debug log
ENV TF_CPP_MIN_LOG_LEVEL="0"
#   Install NodeJS
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get update \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Install and configure NodeJS Package Manager (npm)
ENV NODE_ENV="production"
RUN npm install npm@9.6.4 testcafe@2.5.0 chromedriver@112.0.0 --unsafe-perm=true --allow-root -g
RUN npm config set cache ~/.cache/npm
#   Install Go
ENV GOLANG_VERSION="1.12.6"
RUN set -eux ; url="https://golang.org/dl/go${GOLANG_VERSION}.linux-amd64.tar.gz" ; wget -O go.tgz "$url" ; echo "dbcf71a3c1ea53b8d54ef1b48c85a39a6c9a935d01fc8291ff2b92028e59913c *go.tgz" | sha256sum -c - ; tar -C /usr/local -xzf go.tgz ; rm go.tgz ; export PATH="/usr/local/go/bin:$PATH" ; go version
#   Configure Go environment
ENV GOPATH="/go"
ENV GOBIN="$GOPATH/bin"
ENV PATH="$GOBIN:/usr/local/go/bin:$PATH"
ENV GO111MODULE="on"
RUN mkdir -p "$GOPATH/src" "$GOPATH/bin" \
 && chmod -R 777 "$GOPATH"
#   Download TensorFlow model and test files
RUN rm -rf /tmp/* \
 && mkdir -p /tmp/photoprism
RUN wget "https://dl.photoprism.org/tensorflow/nasnet.zip?${BUILD_TAG}" -O /tmp/photoprism/nasnet.zip
RUN wget "https://dl.photoprism.org/fixtures/testdata.zip?${BUILD_TAG}" -O /tmp/photoprism/testdata.zip
#   Install goimports and richgo (colorizes "go test" output)
RUN env GO111MODULE=off /usr/local/go/bin/go get -u golang.org/x/tools/cmd/goimports
RUN env GO111MODULE=off /usr/local/go/bin/go get -u github.com/kyoh86/richgo
RUN echo "alias go=richgo" > /root/.bash_aliases
#   Configure broadwayd (HTML5 display server)
#   Command: broadwayd -p 8080 -a 0.0.0.0 :5
ENV GDK_BACKEND="broadway"
ENV BROADWAY_DISPLAY=":5"
#   Set up project directory
WORKDIR "/go/src/github.com/photoprism/photoprism"
#   Expose HTTP port plus 4000 for TiDB, 8080 for broadwayd and 9515 for chromedriver
EXPOSE 80/tcp 2342/tcp 4000/tcp 8080/tcp 9515/tcp
#   Keep container running (services can be started manually using a terminal)
CMD tail -f /dev/null
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
