#   Copyright 2017-2017 Amazon.com, Inc. or its affiliates. All Rights Reserved.
#
#   Licensed under the Amazon Software License (the "License"). You may not use this file except in compliance with the License.
#   A copy of the License is located at
#
#      http://aws.amazon.com/asl/
#
#   or in the "license" file accompanying this file.
#   This file is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, express or implied.
#   See the License for the specific language governing permissions and limitations under the License.
#
FROM ubuntu:14.04.5
ENV DOCKER_BUCKET="download.docker.com" \
    DOCKER_VERSION="17.09.0-ce" \
    DOCKER_CHANNEL="stable" \
    DOCKER_SHA256="a9e90a73c3cdfbf238f148e1ec0eaff5eb181f92f35bdd938fd7dab18e1c4647" \
    DIND_COMMIT="3b5fac462d21ca164b3778647420016315289034" \
    DOCKER_COMPOSE_VERSION="1.21.2" \
    GITVERSION_VERSION="3.6.5"
#   Install git, SSH, and other utilities
RUN set -ex \
 && echo 'Acquire::CompressionTypes::Order:: "gz";' > /etc/apt/apt.conf.d/99use-gzip-compression \
 && apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.0.1ubuntu2.24 -y \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
 && echo "deb https://download.mono-project.com/repo/ubuntu stable-trusty main" | tee /etc/apt/sources.list.d/mono-official-stable.list \
 && apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y \
 && apt-add-repository ppa:git-core/ppa \
 && apt-get update \
 && apt-get install --no-install-recommends git=1:2.* -y \
 && git version \
 && apt-get install --no-install-recommends openssh-client=1:6.6* -y \
 && mkdir ~/.ssh \
 && touch ~/.ssh/known_hosts \
 && ssh-keyscan -t rsa,dsa -H github.com >> ~/.ssh/known_hosts \
 && ssh-keyscan -t rsa,dsa -H bitbucket.org >> ~/.ssh/known_hosts \
 && chmod 600 ~/.ssh/known_hosts \
 && apt-get install --no-install-recommends ca-certificates=20170717~14.04.2 mono-devel=3.2.8+dfsg-4ubuntu1.1 wget=1.15-* python=2.7.* python2.7-dev=2.7.* fakeroot=1.20-* tar=1.27.* gzip=1.6-* zip=3.0-* autoconf=2.69-* automake=1:1.14.* bzip2=1.0.* file=1:5.14-* g++=4:4.8.* gcc=4:4.8.* imagemagick=8:6.7.* libbz2-dev=1.0.* libc6-dev=2.19-* libcurl4-openssl-dev=7.35.* libdb-dev=1:5.3.* libevent-dev=2.0.* libffi-dev=3.1~* libgeoip-dev=1.6.* libglib2.0-dev=2.40.* libjpeg-dev=8c-* libkrb5-dev=1.12+* liblzma-dev=5.1.* libmagickcore-dev=8:6.7.* libmagickwand-dev=8:6.7.* libmysqlclient-dev=5.5.* libncurses5-dev=5.9+* libpng12-dev=1.2.* libpq-dev=9.3.* libreadline-dev=6.3-* libsqlite3-dev=3.8.* libssl-dev=1.0.* libtool=2.4.* libwebp-dev=0.4.* libxml2-dev=2.9.* libxslt1-dev=1.1.* libyaml-dev=0.1.* make=3.81-* patch=2.7.* xz-utils=5.1.* zlib1g-dev=1:1.2.* unzip=6.0-* curl=7.35.* e2fsprogs=1.42.* iptables=1.4.* xfsprogs=3.1.* xz-utils=5.1.* less=458-* groff=1.22.* liberror-perl=0.17-* asciidoc=8.6.* build-essential=11.* bzr=2.6.* cvs=2:1.12.* cvsps=2.1-* docbook-xml=4.5-* docbook-xsl=1.78.* dpkg-dev=1.17.* libdbd-sqlite3-perl=1.40-* libdbi-perl=1.630-* libdpkg-perl=1.17.* libhttp-date-perl=6.02-* libio-pty-perl=1:1.08-* libserf-1-1=1.3.* libsvn-perl=1.8.* libsvn1=1.8.* libtcl8.6=8.6.* libtimedate-perl=2.3000-* libunistring0=0.9.* libxml2-utils=2.9.* libyaml-perl=0.84-* python-bzrlib=2.6.* python-configobj=4.7.* sgml-base=1.26+* sgml-data=2.0.* subversion=1.8.* tcl=8.6.* tcl8.6=8.6.* xml-core=0.13+* xmlto=0.0.* xsltproc=1.1.* -y \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean
#   Download and set up GitVersion
RUN set -ex \
 && wget "https://github.com/GitTools/GitVersion/releases/download/v${GITVERSION_VERSION}/GitVersion_${GITVERSION_VERSION}.zip" -O /tmp/GitVersion_${GITVERSION_VERSION}.zip \
 && mkdir -p /usr/local/GitVersion_${GITVERSION_VERSION} \
 && unzip /tmp/GitVersion_${GITVERSION_VERSION}.zip -d /usr/local/GitVersion_${GITVERSION_VERSION} \
 && rm /tmp/GitVersion_${GITVERSION_VERSION}.zip \
 && echo "mono /usr/local/GitVersion_${GITVERSION_VERSION}/GitVersion.exe $@" >> /usr/local/bin/gitversion \
 && chmod +x /usr/local/bin/gitversion
#   Install Docker
RUN set -ex \
 && curl -fSL "https://${DOCKER_BUCKET}/linux/static/${DOCKER_CHANNEL}/x86_64/docker-${DOCKER_VERSION}.tgz" -o docker.tgz \
 && echo "${DOCKER_SHA256} *docker.tgz" | sha256sum -c - \
 && tar --extract --file docker.tgz --strip-components 1 --directory /usr/local/bin/ \
 && rm docker.tgz \
 && docker -v \
 && addgroup dockremap \
 && useradd -g dockremap dockremap \
 && echo 'dockremap:165536:65536' >> /etc/subuid \
 && echo 'dockremap:165536:65536' >> /etc/subgid \
 && wget "https://raw.githubusercontent.com/docker/docker/${DIND_COMMIT}/hack/dind" -O /usr/local/bin/dind \
 && curl -L https://github.com/docker/compose/releases/download/${DOCKER_COMPOSE_VERSION}/docker-compose-Linux-x86_64 > /usr/local/bin/docker-compose \
 && chmod +x /usr/local/bin/dind /usr/local/bin/docker-compose \
 && docker-compose version
#   Install dependencies by all python images equivalent to buildpack-deps:jessie
#   on the public repos.
RUN set -ex \
 && wget "https://bootstrap.pypa.io/2.6/get-pip.py" -O /tmp/get-pip.py \
 && python /tmp/get-pip.py \
 && pip install awscli==1.* \
 && rm -fr /var/lib/apt/lists/* /tmp/* /var/tmp/*
VOLUME /var/lib/docker
COPY dockerd-entrypoint.sh /usr/local/bin/
ENV RUBY_MAJOR="2.3" \
    RUBY_VERSION="2.3.1" \
    RUBY_DOWNLOAD_SHA256="b87c738cb2032bf4920fef8e3864dc5cf8eae9d89d8d523ce0236945c5797dcd" \
    RUBYGEMS_VERSION="2.6.7" \
    BUNDLER_VERSION="1.13.5" \
    GEM_HOME="/usr/local/bundle"
ENV BUNDLE_PATH="$GEM_HOME" \
    BUNDLE_BIN="$GEM_HOME/bin" \
    BUNDLE_SILENCE_ROOT_WARNING="1" \
    BUNDLE_APP_CONFIG="$GEM_HOME"
ENV PATH="$BUNDLE_BIN:$PATH"
RUN set -ex \
 && mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc \
 && apt-get update \
 && apt-get install --no-install-recommends bison=2:3.0.2.dfsg-2 libgdbm-dev=1.8.3-12build1 ruby=1:1.9.3.4 -y \
 && wget "https://cache.ruby-lang.org/pub/ruby/$RUBY_MAJOR/ruby-$RUBY_VERSION.tar.gz" -O /tmp/ruby.tar.gz \
 && echo "$RUBY_DOWNLOAD_SHA256 /tmp/ruby.tar.gz" | sha256sum -c - \
 && mkdir -p /usr/src/ruby \
 && tar -xzf /tmp/ruby.tar.gz -C /usr/src/ruby --strip-components=1 \
 && cd /usr/src/ruby \
 && { echo '#define ENABLE_PATH_CHECK 0' ;echo ;cat file.c ; } > file.c.new \
 && mv file.c.new file.c \
 && autoconf \
 && ./configure --disable-install-doc \
 && make -j"$( nproc ;)" \
 && make install \
 && apt-get purge -y --auto-remove bison libgdbm-dev ruby \
 && cd / \
 && rm -r /usr/src/ruby \
 && gem update --system "$RUBYGEMS_VERSION" \
 && gem install bundler --version 2.4.12 \
 && mkdir -p "$GEM_HOME" "$BUNDLE_BIN" \
 && chmod 777 "$GEM_HOME" "$BUNDLE_BIN" \
 && rm -fr /var/lib/apt/lists/* /tmp/* /var/tmp/*
CMD ["irb"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
