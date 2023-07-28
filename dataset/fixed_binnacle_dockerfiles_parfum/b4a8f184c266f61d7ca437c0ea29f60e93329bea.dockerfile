FROM ubuntu:16.04
#  ------------------------------------------------------
#  --- Environments and base directories
#  Environments
#  - Language
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    TOOL_VER_BITRISE_CLI="1.21.0" \
    TOOL_VER_RUBY="2.3.4" \
    TOOL_VER_GO="1.8.1"
#  ------------------------------------------------------
#  --- Base pre-installed tools
RUN apt-get update -qq
#  Generate proper EN US UTF-8 locale
#  Install the "locales" package - required for locale-gen
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends install -y locales \
 && locale-gen en_US.UTF-8
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install git mercurial curl wget rsync sudo expect python build-essential
#  ------------------------------------------------------
#  --- Pre-installed but not through apt-get
#  install Ruby from source
#   from source: mainly because of GEM native extensions,
#   this is the most reliable way to use Ruby on Ubuntu if GEM native extensions are required
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install build-essential zlib1g-dev libssl-dev libreadline6-dev libyaml-dev libsqlite3-dev \
 && cd ${BITRISE_PREP_DIR} \
 && wget -q http://cache.ruby-lang.org/pub/ruby/ruby-${TOOL_VER_RUBY}.tar.gz \
 && tar -xvzf ruby-${TOOL_VER_RUBY}.tar.gz \
 && cd ruby-${TOOL_VER_RUBY} \
 && ./configure --prefix=/usr/local \
 && make \
 && make install \
 && cd ${BITRISE_PREP_DIR} \
 && rm -rf ruby-${TOOL_VER_RUBY} \
 && rm ruby-${TOOL_VER_RUBY}.tar.gz \
 && gem install bundler --no-document \
 && gem update --system --no-document
#  install Go
#   from official binary package
RUN wget -q https://storage.googleapis.com/golang/go${TOOL_VER_GO}.linux-amd64.tar.gz -O go-bins.tar.gz \
 && tar -C /usr/local -xvzf go-bins.tar.gz \
 && rm go-bins.tar.gz
#  ENV setup
ENV PATH="$PATH:/usr/local/go/bin"
#  Go Workspace dirs & envs
#  From the official Golang Dockerfile
#   https://github.com/docker-library/golang
ENV GOPATH="/go"
ENV PATH="$GOPATH/bin:$PATH"
#  755 because Ruby complains if 777 (warning: Insecure world writable dir ... in PATH)
RUN mkdir -p "$GOPATH/src" "$GOPATH/bin" \
 && chmod -R 755 "$GOPATH"
#  Install NodeJS
#   from official docs: https://nodejs.org/en/download/package-manager/#debian-and-ubuntu-based-linux-distributions
# RUN curl -sL https://deb.nodesource.com/setup_7.x | sudo -E bash -
# RUN sudo apt-get install -y nodejs
#  ------------------------------------------------------
#  --- Bitrise CLI
#
#  Install Bitrise CLI
RUN wget -q https://github.com/bitrise-io/bitrise/releases/download/${TOOL_VER_BITRISE_CLI}/bitrise-$( uname -s ;)-$( uname -m ;) -O /usr/local/bin/bitrise \
 && chmod +x /usr/local/bin/bitrise \
 && bitrise setup \
 && bitrise envman -version \
 && bitrise stepman -version \
 && bitrise stepman setup -c https://github.com/bitrise-io/bitrise-steplib.git \
 && bitrise stepman update
#  Project specific
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install ruby-dev nodejs liblzma-dev
WORKDIR /go/src/github.com/bitrise-io/bitrise-workflow-editor
