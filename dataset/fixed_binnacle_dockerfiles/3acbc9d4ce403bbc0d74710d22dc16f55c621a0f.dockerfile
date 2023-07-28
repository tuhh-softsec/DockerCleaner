FROM ubuntu:18.04 AS build-dep
#   Use bash for the shell
SHELL ["bash", "-c"]
#   Install Node
ENV NODE_VER="8.15.0"
RUN echo "Etc/UTC" > /etc/localtime \
 && apt-get update \
 && apt-get install --no-install-recommends wget=1.19.4-1ubuntu2.2 make=4.1-9.1ubuntu1 gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 python=2.7.15~rc1-1 -y \
 && cd ~ \
 && wget https://nodejs.org/download/release/v$NODE_VER/node-v$NODE_VER.tar.gz \
 && tar xf node-v$NODE_VER.tar.gz \
 && cd node-v$NODE_VER \
 && ./configure --prefix=/opt/node \
 && make -j$( nproc ;) > /dev/null \
 && make install
#   Install jemalloc
ENV JE_VER="5.1.0"
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-11 -y \
 && cd ~ \
 && wget https://github.com/jemalloc/jemalloc/archive/$JE_VER.tar.gz \
 && tar xf $JE_VER.tar.gz \
 && cd jemalloc-$JE_VER \
 && ./autogen.sh \
 && ./configure --prefix=/opt/jemalloc \
 && make -j$( nproc ;) > /dev/null \
 && make install_bin install_include install_lib
#   Install ruby
ENV RUBY_VER="2.6.1"
ENV CPPFLAGS="-I/opt/jemalloc/include"
ENV LDFLAGS="-L/opt/jemalloc/lib/"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 bison=2:3.0.4.dfsg-1build1 libyaml-dev=0.1.7-2ubuntu3 libgdbm-dev=1.14.1-6 libreadline-dev=7.0-3 libncurses5-dev=6.1-1ubuntu1.18.04 libffi-dev=3.2.1-8 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 -y \
 && cd ~ \
 && wget https://cache.ruby-lang.org/pub/ruby/${RUBY_VER%.*}/ruby-$RUBY_VER.tar.gz \
 && tar xf ruby-$RUBY_VER.tar.gz \
 && cd ruby-$RUBY_VER \
 && ./configure --prefix=/opt/ruby --with-jemalloc --with-shared --disable-install-doc \
 && ln -s /opt/jemalloc/lib/* /usr/lib/ \
 && make -j$( nproc ;) > /dev/null \
 && make install
ENV PATH="${PATH}:/opt/ruby/bin:/opt/node/bin"
RUN npm install yarn@1.22.19 -g \
 && gem install bundler --version 2.4.12 \
 && apt-get update \
 && apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 libicu-dev=60.2-3ubuntu3.2 libidn11-dev=1.33-2.1ubuntu1.2 libpq-dev=10.23-0ubuntu0.18.04.1 libprotobuf-dev=3.0.0-9.1ubuntu1.1 protobuf-compiler=3.0.0-9.1ubuntu1.1 -y
COPY Gemfile* package.json yarn.lock /opt/mastodon/
RUN cd /opt/mastodon \
 && bundle install -j$( nproc ;) --deployment --without development test \
 && yarn install --pure-lockfile
FROM ubuntu:18.04
#   Copy over all the langs needed for runtime
COPY --from=build-dep /opt/node /opt/node
COPY --from=build-dep /opt/ruby /opt/ruby
COPY --from=build-dep /opt/jemalloc /opt/jemalloc
#   Add more PATHs to the PATH
ENV PATH="${PATH}:/opt/ruby/bin:/opt/node/bin:/opt/mastodon/bin"
#   Create the mastodon user
ARG UID=991
ARG GID=991
RUN apt-get update \
 && echo "Etc/UTC" > /etc/localtime \
 && ln -s /opt/jemalloc/lib/* /usr/lib/ \
 && apt-get install --no-install-recommends whois=5.3.0 wget=1.19.4-1ubuntu2.2 -y \
 && addgroup --gid $GID mastodon \
 && useradd -m -u $UID -g $GID -d /opt/mastodon mastodon \
 && echo "mastodon:`head /dev/urandom | tr -dc A-Za-z0-9 | head -c 24 | mkpasswd -s -m sha-256 `" | chpasswd
#   Install mastodon runtime deps
RUN apt-get install --no-install-recommends libssl1.1=1.1.1-1ubuntu2.1~18.04.21 libpq5=10.23-0ubuntu0.18.04.1 imagemagick=8:6.9.7.4+dfsg-16ubuntu6.15 ffmpeg=7:3.4.11-0ubuntu0.1 libicu60=60.2-3ubuntu3.2 libprotobuf10=3.0.0-9.1ubuntu1.1 libidn11=1.33-2.1ubuntu1.2 libyaml-0-2=0.1.7-2ubuntu3 file=1:5.32-2ubuntu0.4 ca-certificates=20211016ubuntu0.18.04.1 tzdata=2022g-0ubuntu0.18.04 libreadline7=7.0-3 -y \
 && apt-get install --no-install-recommends gcc=4:7.4.0-1ubuntu2.3 -y \
 && ln -s /opt/mastodon /mastodon \
 && gem install bundler --version 2.4.12 \
 && rm -rf /var/cache \
 && rm -rf /var/lib/apt/lists/*
#   Add tini
ENV TINI_VERSION="0.18.0"
ENV TINI_SUM="12d20136605531b09a2c2dac02ccee85e1b874eb322ef6baf7561cd93f93c855"
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tini https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini
RUN echo "$TINI_SUM tini" | sha256sum -c -
RUN chmod +x /tini
#   Copy over mastodon source, and dependencies from building, and set permissions
COPY --chown=mastodon:mastodon . /opt/mastodon
COPY --chown=mastodon:mastodon --from=build-dep /opt/mastodon /opt/mastodon
#   Run mastodon services in prod mode
ENV RAILS_ENV="production"
ENV NODE_ENV="production"
#   Tell rails to serve static files
ENV RAILS_SERVE_STATIC_FILES="true"
#   Set the run user
USER mastodon
#   Precompile assets
RUN cd ~ \
 && OTP_SECRET=precompile_placeholder SECRET_KEY_BASE=precompile_placeholder rails assets:precompile \
 && yarn cache clean
#   Set the work dir and the container entry point
WORKDIR /opt/mastodon
ENTRYPOINT ["/tini", "--"]
# Please add your HEALTHCHECK here!!!
