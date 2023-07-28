FROM ubuntu:18.04
#  ## SYSTEM DEPENDENCIES
ENV DEBIAN_FRONTEND="noninteractive" \
    LC_ALL="en_US.UTF-8" \
    LANG="en_US.UTF-8"
#   Everything from `make` onwards in apt-get install is only installed to ensure
#   Python support works with all packages (which may require specific libraries
#   at install time).
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 dirmngr=2.2.4-1ubuntu1.6 git=1:2.17.1-1ubuntu0.17 bzr=2.7.0+bzr6622-10 mercurial=4.5.3-1ubuntu2.2 gnupg2=2.2.4-1ubuntu1.6 curl=7.58.0-2ubuntu3.24 wget=1.19.4-1ubuntu2.2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 liblzma-dev=5.2.2-1.3ubuntu0.1 tzdata=2022g-0ubuntu0.18.04 zip=3.0-11build1 unzip=6.0-21ubuntu1.2 locales=2.27-3ubuntu1.6 openssh-client=1:7.6p1-4ubuntu0.7 make=4.1-9.1ubuntu1 libpq-dev=10.23-0ubuntu0.18.04.1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libbz2-dev=1.0.6-8.1ubuntu0.2 libffi-dev=3.2.1-8 libreadline-dev=7.0-3 libsqlite3-dev=3.22.0-1ubuntu0.7 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 llvm=1:6.0-41~exp5~ubuntu1 libncurses5-dev=6.1-1ubuntu1.18.04 libncursesw5-dev=6.1-1ubuntu1.18.04 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 xz-utils=5.2.2-1.3ubuntu0.1 tk-dev=8.6.0+9 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libxmlsec1-dev=1.2.25-1build1 libgeos-dev=3.6.2-1build2 python3-enchant=2.0.0-1 -y \
 && locale-gen en_US.UTF-8
#  ## RUBY
#   Install Ruby 2.6.2, update RubyGems, and install Bundler
ENV BUNDLE_SILENCE_ROOT_WARNING="1"
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys C3173AA6 \
 && echo "deb http://ppa.launchpad.net/brightbox/ruby-ng/ubuntu bionic main" > /etc/apt/sources.list.d/brightbox.list \
 && apt-get update \
 && apt-get install --no-install-recommends ruby2.6 ruby2.6-dev -y \
 && gem update --system 3.0.3 \
 && gem install bundler --version 1.17.3 --no-document
#  ## PYTHON
#   Install Python 2.7 and 3.7 with pyenv. Using pyenv lets us support multiple Pythons
ENV PYENV_ROOT="/usr/local/.pyenv" \
    PATH="/usr/local/.pyenv/bin:$PATH"
RUN git clone https://github.com/pyenv/pyenv.git /usr/local/.pyenv \
 && cd /usr/local/.pyenv \
 && git checkout v1.2.11 \
 && cd - \
 && pyenv install 3.7.3 \
 && pyenv install 2.7.16 \
 && pyenv global 3.7.3
#  ## JAVASCRIPT
#   Install Node 10.0 and Yarn
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash - \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -y \
 && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update \
 && apt-get install --no-install-recommends yarn -y
#  ## ELM
#   Install Elm 0.18 and Elm 0.19
ENV PATH="$PATH:/node_modules/.bin"
RUN npm install elm@0.18.0 \
 && wget "https://github.com/elm/compiler/releases/download/0.19.0/binaries-for-linux.tar.gz" \
 && tar xzf binaries-for-linux.tar.gz \
 && mv elm /usr/local/bin/elm19 \
 && rm -f binaries-for-linux.tar.gz
#  ## PHP
#   Install PHP 7.3 and Composer
ENV COMPOSER_ALLOW_SUPERUSER="1"
RUN echo "deb http://ppa.launchpad.net/ondrej/php/ubuntu bionic main" >> /etc/apt/sources.list.d/ondrej-php.list \
 && echo "deb-src http://ppa.launchpad.net/ondrej/php/ubuntu bionic main" >> /etc/apt/sources.list.d/ondrej-php.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 4F4EA0AAE5267A6C \
 && apt-get update \
 && apt-get install --no-install-recommends php7.3 php7.3-xml php7.3-json php7.3-zip php7.3-mbstring php7.3-intl php7.3-common php7.3-gettext php7.3-curl php7.3-bcmath php7.3-gmp php7.3-imagick php7.3-gd php7.3-redis php7.3-soap php7.3-ldap php7.3-memcached php7.3-sqlite3 php7.3-apcu php7.3-tidy php7.3-mongodb php7.3-zmq php7.3-mysql php7.3-imap php7.3-geoip -y \
 && curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer
#  ## GO
#   Install Go and dep
RUN curl https://dl.google.com/go/go1.12.5.linux-amd64.tar.gz | tar -xz -C /opt \
 && wget -O /opt/go/bin/dep https://github.com/golang/dep/releases/download/0.5.2/dep-linux-amd64 \
 && chmod +x /opt/go/bin/dep \
 && mkdir /opt/go/gopath
ENV PATH="/opt/go/bin:$PATH" \
    GOPATH="/opt/go/gopath"
#  ## ELIXIR
#   Install Erlang, Elixir and Hex
ENV PATH="$PATH:/usr/local/elixir/bin"
RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb \
 && dpkg -i erlang-solutions_1.0_all.deb \
 && apt-get update \
 && apt-get install --no-install-recommends esl-erlang -y \
 && wget https://github.com/elixir-lang/elixir/releases/download/v1.8.1/Precompiled.zip \
 && unzip -d /usr/local/elixir -x Precompiled.zip \
 && rm -f Precompiled.zip \
 && mix local.hex --force
#  ## RUST
#   Install Rust 1.33.0
#   RUSTUP_USE_CURL is necessary because rustup <=1.17.0 doesn't support proxy
#   authentication. We can remove it once something later than 1.17.0 is out as
#   https://github.com/rust-lang/rustup.rs/pull/1746 should have fixed the issue.
ENV RUSTUP_HOME="/opt/rust" \
    PATH="${PATH}:/opt/rust/bin" \
    RUSTUP_USE_CURL="1"
RUN export CARGO_HOME=/opt/rust ; curl https://sh.rustup.rs -sSf | sh -s -- -y
#  ## NEW NATIVE HELPERS
COPY terraform/helpers /opt/terraform/helpers
COPY python/helpers /opt/python/helpers
COPY dep/helpers /opt/dep/helpers
COPY go_modules/helpers /opt/go_modules/helpers
COPY hex/helpers /opt/hex/helpers
COPY composer/helpers /opt/composer/helpers
COPY npm_and_yarn/helpers /opt/npm_and_yarn/helpers
ENV DEPENDABOT_NATIVE_HELPERS_PATH="/opt" \
    PATH="$PATH:/opt/terraform/bin:/opt/python/bin:/opt/go_modules/bin:/opt/dep/bin" \
    MIX_HOME="/opt/hex/mix"
RUN bash /opt/terraform/helpers/build /opt/terraform \
 && bash /opt/python/helpers/build /opt/python \
 && bash /opt/dep/helpers/build /opt/dep \
 && bash /opt/go_modules/helpers/build /opt/go_modules \
 && bash /opt/npm_and_yarn/helpers/build /opt/npm_and_yarn \
 && bash /opt/hex/helpers/build /opt/hex \
 && bash /opt/composer/helpers/build /opt/composer
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
