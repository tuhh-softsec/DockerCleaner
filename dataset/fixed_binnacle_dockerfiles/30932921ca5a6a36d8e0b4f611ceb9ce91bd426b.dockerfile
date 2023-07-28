FROM ubuntu:16.04
LABEL maintainer="Netlify"
#  ###############################################################################
#
#   Dependencies
#
#  ###############################################################################
ENV LANGUAGE="en_US:en"
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV PANDOC_VERSION="2.4"
#   language export needed for ondrej/php PPA https://github.com/oerdnj/deb.sury.org/issues/56
RUN export DEBIAN_FRONTEND=noninteractive \
 && apt-get update -y \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 language-pack-en-base=1:16.04+20160627 apt-transport-https=1.2.35 gnupg-curl=1.4.20-1ubuntu3.3 -y \
 && echo 'Acquire::Languages {"none";};' > /etc/apt/apt.conf.d/60language \
 && echo 'LANG="en_US.UTF-8"' > /etc/default/locale \
 && echo 'LANGUAGE="en_US:en"' >> /etc/default/locale \
 && locale-gen en_US.UTF-8 \
 && update-locale en_US.UTF-8 \
 && apt-key adv --fetch-keys https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc \
 && apt-key adv --fetch-keys https://packagecloud.io/github/git-lfs/gpgkey \
 && apt-add-repository -y -s 'deb https://packagecloud.io/github/git-lfs/ubuntu/ xenial main' \
 && add-apt-repository -y ppa:ondrej/php \
 && add-apt-repository -y ppa:openjdk-r/ppa \
 && add-apt-repository -y ppa:git-core/ppa \
 && add-apt-repository -y ppa:rwky/graphicsmagick \
 && add-apt-repository -y ppa:deadsnakes/ppa \
 && add-apt-repository -y ppa:kelleyk/emacs \
 && apt-add-repository -y 'deb https://packages.erlang-solutions.com/ubuntu xenial contrib' \
 && apt-get update -y \
 && apt-get install --no-install-recommends advancecomp=1.20-1ubuntu0.2 apache2-utils=2.4.18-2ubuntu3.17 autoconf=2.69-9 automake=1:1.15-4ubuntu1 bison=2:3.0.4.dfsg-1 build-essential=12.1ubuntu2 bzr=2.7.0-2ubuntu3.1 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 doxygen=1.8.11-1ubuntu0.1 elixir=1.1.0~0.20150708-1 emacs25-nox esl-erlang expect=5.45-7 fontconfig=2.11.94-0ubuntu1.1 fontconfig-config=2.11.94-0ubuntu1.1 g++=4:5.3.1-1ubuntu1 gawk=1:4.1.3+dfsg-0.1 git=1:2.7.4-0ubuntu1.10 git-lfs gifsicle=1.88-1 gobject-introspection=1.46.0-3ubuntu1 graphicsmagick=1.3.23-1ubuntu0.6 graphviz=2.38.0-12ubuntu2.1 gtk-doc-tools=1.25-1ubuntu1.1 gnupg2=2.1.11-6ubuntu2.1 imagemagick=8:6.8.9.9-7ubuntu5.16 jpegoptim=1.4.3-1 jq=1.5+dfsg-1ubuntu0.1 language-pack-ar=1:16.04+20160627 language-pack-ca=1:16.04+20160627 language-pack-cs=1:16.04+20160627 language-pack-da=1:16.04+20160627 language-pack-de=1:16.04+20160627 language-pack-el=1:16.04+20160627 language-pack-es=1:16.04+20160627 language-pack-eu=1:16.04+20160627 language-pack-fi=1:16.04+20161009 language-pack-fr=1:16.04+20180711 language-pack-gl=1:16.04+20171210 language-pack-he=1:16.04+20160627 language-pack-hi=1:16.04+20160627 language-pack-it=1:16.04+20161009 language-pack-ja=1:16.04+20171210 language-pack-ka=1:16.04+20160627 language-pack-ko=1:16.04+20160627 language-pack-nn=1:16.04+20160627 language-pack-pl=1:16.04+20180711 language-pack-pt=1:16.04+20160627 language-pack-ro=1:16.04+20160627 language-pack-ru=1:16.04+20160627 language-pack-sv=1:16.04+20180711 language-pack-ta=1:16.04+20160627 language-pack-th=1:16.04+20160627 language-pack-tr=1:16.04+20160627 language-pack-uk=1:16.04+20160627 language-pack-vi=1:16.04+20160627 language-pack-zh-hans=1:16.04+20160627 language-pack-zh-hant=1:16.04+20160627 libasound2=1.1.0-0ubuntu1 libcurl3=7.47.0-1ubuntu2.19 libcurl3-gnutls=7.47.0-1ubuntu2.19 libcurl3-openssl-dev libenchant1c2a=1.6.0-10.1build2 libexif-dev=0.6.21-2ubuntu0.6 libffi-dev=3.2.1-4 libfontconfig1=2.11.94-0ubuntu1.1 libgconf-2-4=3.2.6-3ubuntu6 libgd-dev=2.1.1-4ubuntu0.16.04.12 libgdbm-dev=1.8.3-13.1 libgif-dev=5.1.4-0.3~16.04.1 libglib2.0-dev=2.48.2-0ubuntu4.8 libgmp3-dev=2:6.1.0+dfsg-2 libgtk-3-0=3.18.9-1ubuntu3.3 libgtk2.0-0=2.24.30-1ubuntu1.16.04.2 libicu-dev=55.1-7ubuntu0.5 libimage-exiftool-perl=10.10-1 libjpeg-progs=1:9b-1ubuntu1 libjpeg-turbo8-dev=1.4.2-0ubuntu3.4 libmagickwand-dev=8:6.8.9.9-7ubuntu5.16 libmcrypt-dev=2.5.8-3.3 libncurses5-dev=6.0+20160213-1ubuntu1 libnss3=2:3.28.4-0ubuntu0.16.04.14 libpng12-dev=1.2.54-1ubuntu1.1 libreadline6-dev=6.3-8ubuntu2 librsvg2-bin=2.40.13-3ubuntu0.2 libsm6=2:1.2.2-1 libsqlite3-dev=3.11.0-1ubuntu1.5 libssl-dev=1.0.2g-1ubuntu4.20 libtiff5-dev=4.0.6-1ubuntu0.8 libwebp-dev=0.4.4-1 libwebp5=0.4.4-1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxrender1=1:0.9.9-0ubuntu1 libxslt-dev libxss1=1:1.2.2-1 libxtst6=2:1.2.2-1 libyaml-dev=0.1.6-3 mercurial=3.7.3-1ubuntu1.2 nasm=2.11.08-1ubuntu0.1 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 optipng=0.7.6-1ubuntu0.16.04.1 php5.6 php5.6-xml php5.6-mbstring php5.6-gd php5.6-sqlite3 php5.6-curl php5.6-zip php7.2 php7.2-xml php7.2-mbstring php7.2-gd php7.2-sqlite3 php7.2-curl php7.2-zip pngcrush=1.7.85-1 python-setuptools=20.7.0-1 python=2.7.12-1~16.04 python-dev=2.7.12-1~16.04 python3=3.5.1-3 python3-dev=3.5.1-3 python3.7 python3.7-dev rsync=3.1.1-3ubuntu1.3 software-properties-common=0.96.20.10 sqlite3=3.11.0-1ubuntu1.5 ssh=1:7.2p2-4ubuntu2.10 strace=4.11-1ubuntu3 swig=3.0.8-0ubuntu3 tree=1.7.0-3 unzip=6.0-20ubuntu1.1 virtualenv=15.0.1+ds-3ubuntu1.1 wget=1.17.1-1ubuntu1.5 xfonts-base=1:1.0.4+nmu1 xfonts-75dpi=1:1.0.4+nmu1 xvfb=2:1.18.4-0ubuntu0.12 zip=3.0-11 libunwind8-dev=1.1-4.1 libicu-dev=55.1-7ubuntu0.5 libcurl3=7.47.0-1ubuntu2.19 liblttng-ust0=2.7.1-1 libkrb5-3=1.13.2+dfsg-5ubuntu2.2 -y \
 && /var/lib/dpkg/info/ca-certificates-java.postinst configure \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && apt-get autoremove -y \
 && unset DEBIAN_FRONTEND
#  ###############################################################################
#
#   Pandoc
#
#  ###############################################################################
RUN wget -nv https://github.com/wkhtmltopdf/wkhtmltopdf/releases/download/0.12.5/wkhtmltox_0.12.5-1.xenial_amd64.deb \
 && dpkg -i wkhtmltox_0.12.5-1.xenial_amd64.deb \
 && rm wkhtmltox_0.12.5-1.xenial_amd64.deb \
 && wkhtmltopdf -V
#   install Pandoc (more recent version to what is provided in Ubuntu 14.04)
RUN wget https://github.com/jgm/pandoc/releases/download/$PANDOC_VERSION/pandoc-$PANDOC_VERSION-1-amd64.deb \
 && dpkg -i pandoc-$PANDOC_VERSION-1-amd64.deb \
 && rm pandoc-$PANDOC_VERSION-1-amd64.deb \
 && pandoc -v
#  ###############################################################################
#
#   Libvips
#
#  ###############################################################################
WORKDIR /tmp
#   this actually builds v8.6.2
RUN curl -sLo vips-8.6.2.tar.gz https://github.com/jcupitt/libvips/archive/v8.6.2.tar.gz \
 && tar xvf vips-8.6.2.tar.gz \
 && cd libvips-8.6.2 \
 && ./autogen.sh \
 && ./configure --enable-debug=no --enable-docs=no --without-python --without-orc --without-fftw --without-gsf $1 \
 && make \
 && make install \
 && ldconfig
WORKDIR /
#  ###############################################################################
#
#   User
#
#  ###############################################################################
RUN adduser --system --disabled-password --uid 2500 --quiet buildbot --home /opt/buildhome
#  ###############################################################################
#
#   Ruby
#
#  ###############################################################################
#  # TODO: Consider switching to rbenv or asdf-vm
USER buildbot
RUN gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB \
 && curl -sL https://get.rvm.io | bash -s stable --with-gems="bundler" --autolibs=read-fail
ENV PATH="/usr/local/rvm/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
#   Match this set latest Stable releases we can support on https://www.ruby-lang.org/en/downloads/
RUN /bin/bash -c "source ~/.rvm/scripts/rvm \
 && rvm install 2.6.2 \
 && rvm use 2.6.2 \
 && gem install bundler \
 && rvm use 2.6.2 --default \
 && rvm cleanup all"
ENV PATH="/usr/local/rvm/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
USER root
#  ###############################################################################
#
#   Node.js
#
#  ###############################################################################
RUN curl -o- -L https://yarnpkg.com/install.sh > /usr/local/bin/yarn-installer.sh
#   Install node.js
USER buildbot
RUN git clone https://github.com/creationix/nvm.git ~/.nvm \
 && cd ~/.nvm \
 && git checkout v0.34.0 \
 && cd /
ENV ELM_VERSION="0.19.0-bugfix6"
ENV YARN_VERSION="1.13.0"
RUN /bin/bash -c ". ~/.nvm/nvm.sh \
 && nvm install 10 \
 && nvm use 10 \
 && npm install -g sm grunt-cli bower elm@$ELM_VERSION \
 && bash /usr/local/bin/yarn-installer.sh --version $YARN_VERSION \
 && nvm alias default node \
 && nvm cache clear"
USER root
#  ###############################################################################
#
#   Python
#
#  ###############################################################################
ENV PIPENV_RUNTIME="2.7"
USER buildbot
RUN virtualenv -p python2.7 --no-site-packages /opt/buildhome/python2.7 \
 && /bin/bash -c 'source /opt/buildhome/python2.7/bin/activate' \
 && ln -nfs /opt/buildhome/python2.7 /opt/buildhome/python2.7.11
RUN virtualenv -p python3.5 --no-site-packages /opt/buildhome/python3.5 \
 && /bin/bash -c 'source /opt/buildhome/python3.5/bin/activate' \
 && ln -nfs /opt/buildhome/python3.5 /opt/buildhome/python3.5.6
RUN virtualenv -p python3.7 --no-site-packages /opt/buildhome/python3.7 \
 && /bin/bash -c 'source /opt/buildhome/python3.7/bin/activate' \
 && ln -nfs /opt/buildhome/python3.7 /opt/buildhome/python3.7.2
RUN /opt/buildhome/python${PIPENV_RUNTIME}/bin/pip install pipenv
USER root
#  ###############################################################################
#
#   Binrc
#
#  ###############################################################################
ENV BINRC_VERSION="0.2.9"
RUN mkdir /opt/binrc \
 && cd /opt/binrc \
 && curl -sL https://github.com/netlify/binrc/releases/download/v${BINRC_VERSION}/binrc_${BINRC_VERSION}_Linux-64bit.tar.gz | tar zxvf - \
 && ln -s /opt/binrc/binrc_${BINRC_VERSION}_linux_amd64/binrc_${BINRC_VERSION}_linux_amd64 /usr/local/bin/binrc
#   Create a place for binrc to link/persist installs to the PATH
USER buildbot
RUN mkdir -p /opt/buildhome/.binrc/bin
ENV PATH="\"/opt/buildhome/.binrc/bin:$PATH\""
USER root
#  ###############################################################################
#
#   Hugo
#
#  ###############################################################################
ENV HUGO_VERSION="0.54.0"
RUN binrc install gohugoio/hugo ${HUGO_VERSION} -c /opt/buildhome/.binrc | xargs -n 1 -I{} ln -s {} /usr/local/bin/hugo_${HUGO_VERSION} \
 && ln -s /usr/local/bin/hugo_${HUGO_VERSION} /usr/local/bin/hugo
#  ###############################################################################
#
#   Zip-it-and-ship-it
#
#  ###############################################################################
ENV ZIP_IT_AND_SHIP_IT_VERSION="0.3.1"
RUN binrc install netlify/zip-it-and-ship-it ${ZIP_IT_AND_SHIP_IT_VERSION} -c /opt/buildhome/.binrc | xargs -n 1 -I{} ln -s {} /usr/local/bin/zip-it-and-ship-it_${ZIP_IT_AND_SHIP_IT_VERSION} \
 && ln -s /usr/local/bin/zip-it-and-ship-it_${ZIP_IT_AND_SHIP_IT_VERSION} /usr/local/bin/zip-it-and-ship-it
#  ###############################################################################
#
#   Clojure
#
#  ###############################################################################
RUN mkdir /opt/leiningen \
 && cd /opt/leiningen \
 && curl -sL https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > lein \
 && chmod +x lein \
 && ln -s /opt/leiningen/lein /usr/local/bin/lein
RUN mkdir /opt/boot-clj \
 && cd /opt/boot-clj \
 && curl -sL https://github.com/boot-clj/boot-bin/releases/download/2.5.2/boot.sh > boot \
 && chmod +x boot \
 && ln -s /opt/boot-clj/boot /usr/local/bin/boot
USER buildbot
RUN lein
RUN boot -u
#  ###############################################################################
#
#   PHP
#
#  ###############################################################################
USER root
#   set default to 5.6
RUN update-alternatives --set php /usr/bin/php5.6 \
 && update-alternatives --set phar /usr/bin/phar5.6 \
 && update-alternatives --set phar.phar /usr/bin/phar.phar5.6
RUN wget -nv https://raw.githubusercontent.com/composer/getcomposer.org/72bb6f65aa902c76c7ca35514f58cf79a293657d/web/installer -O - | php -- --quiet \
 && mv composer.phar /usr/local/bin/composer
USER buildbot
RUN mkdir -p /opt/buildhome/.php \
 && ln -s /usr/bin/php5.6 /opt/buildhome/.php/php
ENV PATH="\"/opt/buildhome/.php:$PATH\""
#  ###############################################################################
#
#   Cask
#
#  ###############################################################################
USER buildbot
RUN rm -rf /opt/buildhome/.cask \
 && git clone https://github.com/cask/cask.git /opt/buildhome/.cask
ENV PATH="\"$PATH:/opt/buildhome/.cask/bin\""
#  ##
#   LZ4 Compression
#  ##
USER root
ENV LZ4_VERSION="1.8.0"
RUN curl -sL https://github.com/lz4/lz4/archive/v${LZ4_VERSION}.tar.gz | tar xzvf - \
 && cd lz4-${LZ4_VERSION} \
 && make \
 && make install \
 && cd .. \
 && rm -rf lz4-${LZ4_VERSION}
#  ###############################################################################
#
#   Go
#
#  ###############################################################################
USER buildbot
RUN mkdir -p /opt/buildhome/.gimme/bin/ \
 && mkdir -p /opt/buildhome/.gimme/env/ \
 && curl -sL -o /opt/buildhome/.gimme/bin/gimme https://raw.githubusercontent.com/travis-ci/gimme/master/gimme \
 && chmod u+x /opt/buildhome/.gimme/bin/gimme
ENV PATH="\"$PATH:/opt/buildhome/.gimme/bin\""
ENV GOPATH="\"/opt/buildhome/.gimme_cache/gopath\""
ENV GOCACHE="\"/opt/buildhome/.gimme_cache/gocache\""
#   Install the default version
ENV GIMME_GO_VERSION="\"1.12\""
ENV GIMME_ENV_PREFIX="\"/opt/buildhome/.gimme/env\""
RUN gimme
#  ###############################################################################
#
#   Wasmer
#
#  ###############################################################################
RUN curl https://get.wasmer.io -sSfL | sh
#  ###############################################################################
#
#   Dotnet Core
#
#  ###############################################################################
WORKDIR /tmp
RUN wget https://dot.net/v1/dotnet-install.sh
RUN chmod u+x /tmp/dotnet-install.sh
RUN /tmp/dotnet-install.sh -c Current
ENV PATH="\"$PATH:/opt/buildhome/.dotnet/tools\""
ENV PATH="\"$PATH:/opt/buildhome/.dotnet\""
ENV DOTNET_ROOT="\"/opt/buildhome/.dotnet\""
#  populate local package cache
RUN dotnet new
WORKDIR /
#   Cleanup
USER root
#   Add buildscript for local testing
COPY run-build-functions.sh /usr/local/bin/run-build-functions.sh
COPY run-build.sh /usr/local/bin/build
COPY buildbot-git-config /root/.gitconfig
RUN rm -r /tmp/*
USER buildbot
ARG NF_IMAGE_VERSION
ENV NF_IMAGE_VERSION="${NF_IMAGE_VERSION:-latest}"
ARG NF_IMAGE_TAG
ENV NF_IMAGE_TAG="${NF_IMAGE_TAG:-latest}"
# Please add your HEALTHCHECK here!!!
