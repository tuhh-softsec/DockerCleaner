FROM ubuntu:trusty
MAINTAINER Evan Cordell <cordell.evan@gmail.com>
#  # Prepare
RUN apt-get clean all \
 && : \
 && apt-get upgrade -y
#   Build Tools
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libssl-dev=1.0.1f-1ubuntu2.27 libreadline6-dev=6.3-4ubuntu2 libyaml-dev=0.1.4-3ubuntu3.1 pkg-config=0.26-1ubuntu4 software-properties-common=0.92.37.8 -y ) \
 && apt-add-repository ppa:ubuntu-lxc/lxd-stable \
 && (apt-get update ;apt-get install --no-install-recommends make=3.81-8.2ubuntu3 wget=1.15-1ubuntu1.14.04.5 tar=1.27.1-1ubuntu0.1 git=1:1.9.1-1ubuntu0.10 curl=7.35.0-1ubuntu2.20 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  # Install libsodium
ENV LIBSODIUM_VERSION="1.0.8"
RUN wget https://github.com/jedisct1/libsodium/releases/download/$LIBSODIUM_VERSION/libsodium-$LIBSODIUM_VERSION.tar.gz \
 && tar xzvf libsodium-$LIBSODIUM_VERSION.tar.gz \
 && cd libsodium-$LIBSODIUM_VERSION \
 && ./configure \
 && make \
 && make check \
 && sudo make install \
 && cd .. \
 && rm -rf libsodium-$LIBSODIUM_VERSION \
 && sudo ldconfig
#   Install Python
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends python3-pip=1.5.4-1ubuntu4 python3-dev=3.4.0-0ubuntu2 python3-software-properties=0.92.37.8 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Install Ruby
RUN cd /tmp \
 && wget -q http://cache.ruby-lang.org/pub/ruby/2.1/ruby-2.1.2.tar.gz \
 && tar xzf ruby-2.1.2.tar.gz \
 && cd ruby-2.1.2 \
 && ./configure --enable-shared --prefix=/usr \
 && make \
 && make install \
 && cd .. \
 && rm -rf ruby-2.1.2* \
 && cd ..
#   Install Node
ENV NODE_PREFIX="/usr/local"
ENV NODE_PATH="$NODE_PREFIX/lib/node_modules"
ENV NODE_VERSION="0.11.14"
ENV NPM_VERSION="2.1.6"
RUN wget -q "http://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-x64.tar.gz" \
 && tar -xzf "node-v$NODE_VERSION-linux-x64.tar.gz" -C $NODE_PREFIX --strip-components=1 \
 && rm "node-v$NODE_VERSION-linux-x64.tar.gz" \
 && npm install npm@"$NPM_VERSION" -g \
 && npm cache clear
#   Install Go
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends golang=2:1.2.1-2ubuntu1 bzr=2.6.0+bzr6593-1ubuntu1.6 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && mkdir /usr/go
ENV GOROOT="/usr/lib/go"
ENV GOPATH="/usr/go"
#   Install PHP
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends php5=5.5.9+dfsg-1ubuntu4.29 php5-dev=5.5.9+dfsg-1ubuntu4.29 php-pear=5.5.9+dfsg-1ubuntu4.29 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Install Rust
ENV RUST_VERSION="1.12.0"
RUN : \
 && curl -sO https://static.rust-lang.org/dist/rust-$RUST_VERSION-x86_64-unknown-linux-gnu.tar.gz \
 && tar -xvzf rust-$RUST_VERSION-x86_64-unknown-linux-gnu.tar.gz \
 && ./rust-$RUST_VERSION-x86_64-unknown-linux-gnu/install.sh \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* rust-$RUST_VERSION-x86_64-unknown-linux-gnu{,.tar.gz}
#   Install libmacaroons
RUN wget -O - http://ubuntu.hyperdex.org/hyperdex.gpg.key | apt-key add - \
 && echo "deb [arch=amd64] http://ubuntu.hyperdex.org trusty main" >> /etc/apt/sources.list.d/hyperdex.list \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends python-macaroons -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
WORKDIR /usr/src
#   Install pymacaroons
RUN pip3 install pymacaroons pytest pytest-html
#   Install ruby-macaroons
RUN gem install macaroons --version 0.6.1
#   Install macaroons.js
RUN npm install macaroons.js@0.3.9 -g
#   Install go-macaroons
RUN go get launchpad.net/gorun \
 && go get gopkg.in/macaroon.v1 \
 && go get gopkg.in/macaroon-bakery.v1/bakery
#   Install php-macaroons
COPY implementations/php-macaroons /usr/src/implementations/php-macaroons
RUN pecl install libsodium-1.0.6 \
 && echo "extension=libsodium.so" >> /etc/php5/cli/php.ini \
 && curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/bin/composer \
 && cd implementations/php-macaroons \
 && composer install
#   Install rust-macaroons
RUN mkdir /usr/rust \
 && cd /usr/rust \
 && git clone https://github.com/cryptosphere/rust-macaroons.git /usr/rust/rust-macaroons \
 && cd rust-macaroons \
 && cargo build
#   Add source
COPY . /usr/src
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
