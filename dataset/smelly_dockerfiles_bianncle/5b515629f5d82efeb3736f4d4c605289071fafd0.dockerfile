#  Copyright 2017-2017 Amazon.com, Inc. or its affiliates. All Rights Reserved.
#
#  Licensed under the Amazon Software License (the "License"). You may not use this file except in compliance with the License.
#  A copy of the License is located at
#
#     http://aws.amazon.com/asl/
#
#  or in the "license" file accompanying this file.
#  This file is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, express or implied.
#  See the License for the specific language governing permissions and limitations under the License.
#
FROM ubuntu:14.04.5
ENV LANG="C.UTF-8"
ENV DOCKER_BUCKET="download.docker.com" \
    DOCKER_VERSION="17.09.0-ce" \
    DOCKER_CHANNEL="stable" \
    DOCKER_SHA256="a9e90a73c3cdfbf238f148e1ec0eaff5eb181f92f35bdd938fd7dab18e1c4647" \
    DIND_COMMIT="3b5fac462d21ca164b3778647420016315289034" \
    DOCKER_COMPOSE_VERSION="1.21.2" \
    GITVERSION_VERSION="3.6.5"
#  Install git, SSH, and other utilities
RUN set -ex \
 && echo 'Acquire::CompressionTypes::Order:: "gz";' > /etc/apt/apt.conf.d/99use-gzip-compression \
 && apt-get update \
 && apt-get install -y apt-transport-https \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
 && echo "deb https://download.mono-project.com/repo/ubuntu stable-trusty main" | tee /etc/apt/sources.list.d/mono-official-stable.list \
 && apt-get update \
 && apt-get install --no-install-recommends software-properties-common -y \
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
 && apt-get install --no-install-recommends ca-certificates mono-devel wget=1.15-* fakeroot=1.20-* autoconf=2.69-* automake=1:1.14.* less=458-* groff=1.22.* bzip2=1.0.* file=1:5.14-* g++=4:4.8.* gcc=4:4.8.* imagemagick=8:6.7.* libbz2-dev=1.0.* libc6-dev=2.19-* curl=7.35.* libdb-dev=1:5.3.* libevent-dev=2.0.* libffi-dev=3.1~* libgeoip-dev=1.6.* libglib2.0-dev=2.40.* libjpeg-dev=8c-* libkrb5-dev=1.12+* liblzma-dev=5.1.* libmagickcore-dev=8:6.7.* libmagickwand-dev=8:6.7.* libmysqlclient-dev=5.5.* libncurses5-dev=5.9+* libpng12-dev=1.2.* libpq-dev=9.3.* libreadline-dev=6.3-* libsqlite3-dev=3.8.* libssl-dev=1.0.* libtool=2.4.* libwebp-dev=0.4.* libxml2-dev=2.9.* libxslt1-dev=1.1.* libyaml-dev=0.1.* make=3.81-* patch=2.7.* xz-utils=5.1.* zlib1g-dev=1:1.2.* tcl=8.6.* tk=8.6.* e2fsprogs=1.42.* iptables=1.4.* xfsprogs=3.1.* xz-utils=5.1.* liberror-perl=0.17-* unzip=6.0-* asciidoc=8.6.* build-essential=11.* bzr=2.6.* cvs=2:1.12.* cvsps=2.1-* docbook-xml=4.5-* docbook-xsl=1.78.* dpkg-dev=1.17.* gettext=0.18.* gettext-base=0.18.* libapr1=1.5.* libaprutil1=1.5.* libasprintf0c2=0.18.* libdbd-sqlite3-perl=1.40-* libdbi-perl=1.630-* libdpkg-perl=1.17.* libhttp-date-perl=6.02-* libio-pty-perl=1:1.08-* libserf-1-1=1.3.* libsvn-perl=1.8.* libsvn1=1.8.* libtcl8.6=8.6.* libtimedate-perl=2.3000-* libunistring0=0.9.* libxml2-utils=2.9.* libyaml-perl=0.84-* python-bzrlib=2.6.* python-configobj=4.7.* sgml-base=1.26+* sgml-data=2.0.* subversion=1.8.* tcl=8.6.* tcl8.6=8.6.* xml-core=0.13+* xmlto=0.0.* xsltproc=1.1.* -y \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean
#  Download and set up GitVersion
RUN set -ex \
 && wget "https://github.com/GitTools/GitVersion/releases/download/v${GITVERSION_VERSION}/GitVersion_${GITVERSION_VERSION}.zip" -O /tmp/GitVersion_${GITVERSION_VERSION}.zip \
 && mkdir -p /usr/local/GitVersion_${GITVERSION_VERSION} \
 && unzip /tmp/GitVersion_${GITVERSION_VERSION}.zip -d /usr/local/GitVersion_${GITVERSION_VERSION} \
 && rm /tmp/GitVersion_${GITVERSION_VERSION}.zip \
 && echo "mono /usr/local/GitVersion_${GITVERSION_VERSION}/GitVersion.exe $@" >> /usr/local/bin/gitversion \
 && chmod +x /usr/local/bin/gitversion
#  Install Docker
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
VOLUME /var/lib/docker
COPY dockerd-entrypoint.sh /usr/local/bin/
ENV PATH="/usr/local/bin:$PATH" \
    GPG_KEY="26DEA9D4613391EF3E25C9FF0A5B101836580288" \
    PYTHON_VERSION="3.3.6" \
    PYTHON_PIP_VERSION="8.1.2"
RUN set -ex \
 && apt-get update \
 && apt-get install --no-install-recommends tcl-dev tk-dev -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && (gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$GPG_KEY" || gpg --keyserver pgp.mit.edu --recv-keys "$GPG_KEY" || gpg --keyserver keyserver.ubuntu.com --recv-keys "$GPG_KEY" ) \
 && gpg --batch --verify python.tar.xz.asc python.tar.xz \
 && rm -r "$GNUPGHOME" python.tar.xz.asc \
 && mkdir -p /usr/src/python \
 && tar -xJC /usr/src/python --strip-components=1 -f python.tar.xz \
 && rm python.tar.xz \
 && cd /usr/src/python \
 && ./configure --enable-loadable-sqlite-extensions --enable-shared \
 && make -j$( nproc ;) \
 && make install \
 && ldconfig \
 && if [ ! -e /usr/local/bin/pip3 ] ; then : \
 && wget -O /tmp/get-pip.py 'https://bootstrap.pypa.io/get-pip.py' \
 && python3 /tmp/get-pip.py "pip==$PYTHON_PIP_VERSION" \
 && rm /tmp/get-pip.py ; fi \
 && pip3 install --no-cache-dir --upgrade --force-reinstall "pip==$PYTHON_PIP_VERSION" \
 && pip install awscli==1.* --no-cache-dir \
 && [ "$( pip list | tac | tac | awk -F '[ ()]+' '$1 == "pip" { print $2; exit }' ;)" = "$PYTHON_PIP_VERSION" ] \
 && find /usr/local -depth ( ( -type d -a -name test -o -name tests ) -o ( -type f -a -name '*.pyc' -o -name '*.pyo' ) ) -exec rm -rf '{}' + \
 && apt-get purge -y --auto-remove tcl-dev tk-dev \
 && rm -rf /usr/src/python ~/.cache \
 && cd /usr/local/bin \
 && { [ -e easy_install ] || ln -s easy_install-* easy_install ; } \
 && ln -s idle3 idle \
 && ln -s pydoc3 pydoc \
 && ln -s python3 python \
 && ln -s python3-config python-config \
 && rm -fr /var/lib/apt/lists/* /tmp/* /var/tmp/*
CMD ["python3"]
