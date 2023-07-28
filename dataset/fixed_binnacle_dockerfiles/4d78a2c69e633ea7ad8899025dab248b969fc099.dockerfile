FROM ubuntu:18.04
USER root
ENV PATH="${PATH}:/root/.asdf/shims:/root/.asdf/bin"
RUN mkdir -p /root/app
WORKDIR /root/app
#   prerequisite packages
RUN apt-get update -qq \
 && apt-get upgrade -qq -y \
 && (apt-get update ;apt-get install --no-install-recommends automake=1:1.15.1-3ubuntu2 autoconf=2.69-11 pkg-config=0.29.1-0ubuntu2 gcc-6=6.5.0-2ubuntu1~18.04 build-essential=12.4ubuntu1 bison=2:3.0.4.dfsg-1build1 libedit-dev=3.1-20170329-1 libreadline-dev=7.0-3 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libgdbm-dev=1.14.1-6 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libpng-dev=1.6.34-1ubuntu0.18.04.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libyaml-dev=0.1.7-2ubuntu3 libxslt-dev libffi-dev=3.2.1-8 libgmp3-dev=2:6.1.2+dfsg-2ubuntu0.1 libtool=2.4.6-2 libncurses-dev libssh-dev=0.8.0~20170825.94fa1e38-1ubuntu0.7 unixodbc-dev=2.3.4-1.1ubuntu3 libzip-dev=1.1.2-1.1 libbz2-dev=1.0.6-8.1ubuntu0.2 libevent-dev=2.1.8-stable-4build1 libicu-dev=60.2-3ubuntu3.2 liblzma-dev=5.2.2-1.3ubuntu0.1 apt-transport-https=1.6.14 ca-certificates=20211016ubuntu0.18.04.1 gnupg2=2.2.4-1ubuntu1.6 software-properties-common=0.96.24.32.20 bubblewrap=0.2.1-1ubuntu0.1 xorg-dev=1:7.7+19ubuntu7.1 vim=2:8.0.1453-1ubuntu1.11 git=1:2.17.1-1ubuntu0.17 curl=7.58.0-2ubuntu3.24 wget=1.19.4-1ubuntu2.2 time=1.7-25.1build1 unzip=6.0-21ubuntu1.2 -qq -y ) \
 && apt-get clean -qq -y \
 && apt-get autoclean -qq -y \
 && apt-get autoremove -qq -y \
 && rm -rf /var/cache/debconf/*-old \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /usr/share/doc/*
#   D
RUN curl -fsS https://dlang.org/install.sh | bash -s ldc
#   Mono
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb https://download.mono-project.com/repo/ubuntu stable-bionic main" | tee /etc/apt/sources.list.d/mono-official-stable.list
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends mono-devel=4.6.2.7+dfsg-1ubuntu1 -qq -y )
#   Pony
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E04F0923 B3B48BDA
RUN add-apt-repository "deb https://dl.bintray.com/pony-language/ponylang-debian $( lsb_release -cs ;) main"
RUN (apt-get update ;apt-get install --no-install-recommends ponyc -qq -y )
#  Swift
RUN wget -q -O - https://swift.org/keys/all-keys.asc | gpg --import -
RUN wget https://swift.org/builds/swift-5.0.1-release/ubuntu1804/swift-5.0.1-RELEASE/swift-5.0.1-RELEASE-ubuntu18.04.tar.gz
RUN tar xzf swift-5.0.1-RELEASE-ubuntu18.04.tar.gz
RUN mv swift-5.0.1-RELEASE-ubuntu18.04 /usr/share/swift
RUN rm swift-5.0.1-RELEASE-ubuntu18.04.tar.gz
RUN echo "export PATH=/usr/share/swift/usr/bin:$PATH" >> ~/.bashrc
#  Powershell
RUN wget -q https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb
RUN dpkg -i packages-microsoft-prod.deb
RUN :
RUN add-apt-repository universe
RUN (apt-get update ;apt-get install --no-install-recommends powershell -y )
#   apt languages
RUN DEBIAN_FRONTEND=noninteractive apt-get install -qq -y cython fp-compiler gfortran ocaml perl6 sbcl tcl guile-2.2 luajit
#   asdf languages
RUN git clone https://github.com/asdf-vm/asdf.git /root/.asdf --branch v0.7.2
RUN chmod 755 $HOME/.asdf/asdf.sh
RUN echo "$HOME/.asdf/asdf.sh" >> ~/.bashrc
RUN asdf update
RUN asdf plugin-add java
RUN asdf plugin-add ruby
RUN asdf plugin-add clojure
RUN asdf plugin-add crystal
RUN asdf plugin-add dart https://github.com/baysao/asdf-dart.git
RUN asdf plugin-add dotnet-core
RUN asdf plugin-add erlang
RUN asdf plugin-add elixir
RUN asdf plugin-add elm
RUN asdf plugin-add golang
RUN asdf plugin-add haskell
RUN asdf plugin-add julia
RUN asdf plugin-add lua
RUN asdf plugin-add nim
RUN asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git \
 && bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring
RUN asdf plugin-add perl https://github.com/BeijingPM/asdf-perl.git
RUN asdf plugin-add php
RUN asdf plugin-add python
RUN asdf plugin-add rust
RUN asdf plugin-add R
COPY .tool-versions /root/.
RUN asdf install
RUN asdf install
RUN asdf install
COPY . /root/app
CMD ./run.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
