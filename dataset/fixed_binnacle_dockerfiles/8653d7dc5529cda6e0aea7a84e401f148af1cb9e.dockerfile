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
FROM ubuntu:18.04
ENV RUBY_MAJOR="2.6" \
    PYTHON_VERSION="3.7.2" \
    PHP_VERSION="7.3.1" \
    NODE_VERSION="10.15.0" \
    NODE_8_VERSION="8.11.0" \
    NVM_VERSION="0.33.5" \
    GOLANG_VERSION="1.11.4" \
    DOTNET_SDK_VERSION="2.2.102" \
    DOCKER_VERSION="18.09.1" \
    DOCKER_COMPOSE_VERSION="1.23.2"
#  ****************        Utilities     ********************************************* 
ENV DOCKER_BUCKET="download.docker.com" \
    DOCKER_CHANNEL="stable" \
    DOCKER_SHA256="c9959e42b637fb7362899ac1d1aeef2a966fa0ea85631da91f4c4a7a9ec29644" \
    DIND_COMMIT="3b5fac462d21ca164b3778647420016315289034" \
    GITVERSION_VERSION="4.0.0" \
    DEBIAN_FRONTEND="noninteractive" \
    SRC_DIR="/usr/src"
#   Install git, SSH, and other utilities
RUN set -ex \
 && echo 'Acquire::CompressionTypes::Order:: "gz";' > /etc/apt/apt.conf.d/99use-gzip-compression \
 && apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.6.14 -y \
 && apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y \
 && apt-add-repository ppa:git-core/ppa \
 && apt-get update \
 && apt-get install --no-install-recommends git=1:2.* -y \
 && git version \
 && apt-get install --no-install-recommends openssh-client=1:7.6p1-4ubuntu0.7 -y \
 && mkdir ~/.ssh \
 && touch ~/.ssh/known_hosts \
 && ssh-keyscan -t rsa,dsa -H github.com >> ~/.ssh/known_hosts \
 && ssh-keyscan -t rsa,dsa -H bitbucket.org >> ~/.ssh/known_hosts \
 && chmod 600 ~/.ssh/known_hosts \
 && apt-get install --no-install-recommends wget=1.19.4-1ubuntu2.2 python3=3.6.7-1~18.04 python3-dev=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-setuptools=39.0.1-2ubuntu0.1 fakeroot=1.22-2ubuntu1 ca-certificates=20211016ubuntu0.18.04.1 jq=1.5+dfsg-2 netbase=5.4 gnupg=2.2.4-1ubuntu1.6 dirmngr=2.2.4-1ubuntu1.6 bzr=2.7.0+bzr6622-10 mercurial=4.5.3-1ubuntu2.2 procps=2:3.3.12-3ubuntu1.2 tar=1.29b-2ubuntu0.4 gzip=1.6-5ubuntu1.2 zip=3.0-11build1 autoconf=2.69-11 automake=1:1.15.1-3ubuntu2 bzip2=1.0.6-8.1ubuntu0.2 file=1:5.32-2ubuntu0.4 g++=4:7.4.0-1ubuntu2.3 gcc=4:7.4.0-1ubuntu2.3 imagemagick=8:6.9.7.4+dfsg-16ubuntu6.15 libbz2-dev=1.0.6-8.1ubuntu0.2 libc6-dev=2.27-3ubuntu1.6 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libdb-dev=1:5.3.21~exp1ubuntu2 libevent-dev=2.1.8-stable-4build1 libffi-dev=3.2.1-8 libgeoip-dev=1.6.12-1 libglib2.0-dev=2.56.4-0ubuntu0.18.04.9 libjpeg-dev=8c-2ubuntu8 libkrb5-dev=1.16-2ubuntu0.4 liblzma-dev=5.2.2-1.3ubuntu0.1 libmagickcore-dev=8:6.9.7.4+dfsg-16ubuntu6.15 libmagickwand-dev=8:6.9.7.4+dfsg-16ubuntu6.15 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 libncurses5-dev=6.1-1ubuntu1.18.04 libpq-dev=10.23-0ubuntu0.18.04.1 libreadline-dev=7.0-3 libsqlite3-dev=3.22.0-1ubuntu0.7 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libtool=2.4.6-2 libwebp-dev=0.6.1-2ubuntu0.18.04.1 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libxslt1-dev=1.1.29-5ubuntu0.3 libyaml-dev=0.1.7-2ubuntu3 make=4.1-9.1ubuntu1 patch=2.7.6-2ubuntu1.1 xz-utils=5.2.2-1.3ubuntu0.1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 unzip=6.0-21ubuntu1.2 curl=7.58.0-2ubuntu3.24 e2fsprogs=1.44.1-1ubuntu1.4 iptables=1.6.1-2ubuntu2 xfsprogs=4.9.0+nmu1ubuntu2 mono-devel=4.6.2.7+dfsg-1ubuntu1 less=487-0.1 groff=1.22.3-10 liberror-perl=0.17025-1 asciidoc=8.6.10-2 build-essential=12.4ubuntu1 bzr=2.7.0+bzr6622-10 cvs=2:1.12.13+real-26 cvsps=2.1-8 docbook-xml=4.5-8 docbook-xsl=1.79.1+dfsg-2 dpkg-dev=1.19.0.5ubuntu2.4 libdbd-sqlite3-perl=1.56-1 libdbi-perl=1.640-1ubuntu0.3 libdpkg-perl=1.19.0.5ubuntu2.4 libhttp-date-perl=6.02-1 libio-pty-perl=1:1.08-1.1build4 libserf-1-1=1.3.9-6 libsvn-perl=1.9.7-4ubuntu1.1 libsvn1=1.9.7-4ubuntu1.1 libtcl8.6=8.6.8+dfsg-3 libtimedate-perl=2.3000-2 libxml2-utils=2.9.4+dfsg1-6.1ubuntu1.8 libyaml-perl=1.24-1 python-bzrlib=2.7.0+bzr6622-10 python-configobj=5.0.6-2 sgml-base=1.29 sgml-data=2.0.10 subversion=1.9.7-4ubuntu1.1 tcl=8.6.0+9 tcl8.6=8.6.8+dfsg-3 xml-core=0.18 xmlto=0.0.28-2 xsltproc=1.1.29-5ubuntu0.3 tk=8.6.0+9 gettext=0.19.8.1-6ubuntu0.3 gettext-base=0.19.8.1-6ubuntu0.3 libapr1=1.6.3-2 libaprutil1=1.6.1-2ubuntu0.1 xvfb=2:1.19.6-1ubuntu4.14 expect=5.45.4-1 -y \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
 && echo "deb https://download.mono-project.com/repo/ubuntu stable-trusty main" | tee /etc/apt/sources.list.d/mono-official-stable.list \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean
#   Download and set up GitVersion
RUN set -ex \
 && wget "https://github.com/GitTools/GitVersion/releases/download/v${GITVERSION_VERSION}/GitVersion-bin-net40-v${GITVERSION_VERSION}.zip" -O /tmp/GitVersion_${GITVERSION_VERSION}.zip \
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
#   https://docs.aws.amazon.com/eks/latest/userguide/install-aws-iam-authenticator.html https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_CLI_installation.html
RUN curl -sS -o /usr/local/bin/aws-iam-authenticator https://amazon-eks.s3-us-west-2.amazonaws.com/1.11.5/2018-12-06/bin/linux/amd64/aws-iam-authenticator \
 && curl -sS -o /usr/local/bin/kubectl https://amazon-eks.s3-us-west-2.amazonaws.com/1.11.5/2018-12-06/bin/linux/amd64/kubectl \
 && curl -sS -o /usr/local/bin/ecs-cli https://s3.amazonaws.com/amazon-ecs-cli/ecs-cli-linux-amd64-latest \
 && chmod +x /usr/local/bin/kubectl /usr/local/bin/aws-iam-authenticator /usr/local/bin/ecs-cli
RUN set -ex \
 && pip3 install awscli boto3
VOLUME /var/lib/docker
#   Configure SSH
COPY ssh_config /root/.ssh/config
COPY dockerd-entrypoint.sh /usr/local/bin/
#  ****************        RUBY     ********************************************* 
ENV RUBY_VERSION="2.6.0" \
    RUBY_DOWNLOAD_SHA256="f3c35b924a11c88ff111f0956ded3cdc12c90c04b72b266ac61076d3697fc072" \
    RUBYGEMS_VERSION="3.0.1" \
    BUNDLER_VERSION="2.0.1" \
    GEM_HOME="/usr/local/bundle"
ENV BUNDLE_PATH="$GEM_HOME" \
    BUNDLE_BIN="$GEM_HOME/bin" \
    BUNDLE_SILENCE_ROOT_WARNING="1" \
    BUNDLE_APP_CONFIG="$GEM_HOME"
ENV PATH="$BUNDLE_BIN:$PATH"
RUN mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc \
 && apt-get update \
 && apt-get install --no-install-recommends bison=2:3.0.4.dfsg-1build1 libgdbm-dev=1.14.1-6 ruby=1:2.5.1 -y \
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
#  ****************   END RUBY     ********************************************* 
#  ****************        PYTHON     ********************************************* 
ENV PATH="/usr/local/bin:$PATH" \
    GPG_KEY="0D96DF4D4110E5C43FBFB17F2D347EA6AA65421D" \
    PYTHON_PIP_VERSION="18.1" \
    LC_ALL="C.UTF-8" \
    LANG="C.UTF-8"
RUN apt-get update \
 && apt-get install --no-install-recommends tcl-dev=8.6.0+9 tk-dev=8.6.0+9 -y \
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
 && pip install pipenv==2023.3.20 virtualenv==20.21.0 --no-cache-dir \
 && [ "$( pip list | tac | tac | awk -F '[ ()]+' '$1 == "pip" { print $2; exit }' ;)" = "$PYTHON_PIP_VERSION" ] \
 && find /usr/local -depth
#  ****************      END PYTHON     ********************************************* 
#  ****************      PHP     ****************************************************
ENV GPG_KEYS="CBAF69F173A0FEA4B537F470D66C9593118BCCB6 F38252826ACD957EF380D39F2F7956BC5DA04B5D"
ENV PHP_DOWNLOAD_SHA="cfe93e40be0350cd53c4a579f52fe5d8faf9c6db047f650a4566a2276bf33362" \
    PHPPATH="/php" \
    PHP_INI_DIR="/usr/local/etc/php" \
    PHP_CFLAGS="-fstack-protector -fpic -fpie -O2" \
    PHP_LDFLAGS="-Wl,-O1 -Wl,--hash-style=both -pie"
ENV PHP_SRC_DIR="$SRC_DIR/php" \
    PHP_CPPFLAGS="$PHP_CFLAGS" \
    PHP_URL="https://secure.php.net/get/php-$PHP_VERSION.tar.xz/from/this/mirror" \
    PHP_ASC_URL="https://secure.php.net/get/php-$PHP_VERSION.tar.xz.asc/from/this/mirror"
RUN set -xe ; mkdir -p $SRC_DIR ; cd $SRC_DIR ; wget -O php.tar.xz "$PHP_URL" ; echo "$PHP_DOWNLOAD_SHA *php.tar.xz" | sha256sum -c - ; wget -O php.tar.xz.asc "$PHP_ASC_URL" ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do (gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver pgp.mit.edu --recv-keys "$key" || gpg --keyserver keyserver.pgp.com --recv-keys "$key" ) ; done ; gpg --batch --verify php.tar.xz.asc php.tar.xz ; rm -rf "$GNUPGHOME" ; set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends libedit-dev=3.1-20170329-1 dpkg-dev=1.19.0.5ubuntu2.4 libargon2-0-dev=0~20161029-1.1 -y ; rm -rf /var/lib/apt/lists/* ; apt-get clean ; export CFLAGS="$PHP_CFLAGS" CPPFLAGS="$PHP_CPPFLAGS" LDFLAGS="$PHP_LDFLAGS" ; mkdir -p $PHP_SRC_DIR ; tar -Jxf $SRC_DIR/php.tar.xz -C $PHP_SRC_DIR --strip-components=1 ; cd $PHP_SRC_DIR ; gnuArch="$( dpkg-architecture -qDEB_BUILD_GNU_TYPE ;)" ; debMultiarch="$( dpkg-architecture -qDEB_BUILD_MULTIARCH ;)" ; if [ ! -d /usr/include/curl ] ; then ln -sT "/usr/include/$debMultiarch/curl" /usr/local/include/curl ; fi ; ./configure --build="$gnuArch" --with-config-file-path="$PHP_INI_DIR" --with-config-file-scan-dir="$PHP_INI_DIR/conf.d" --disable-cgi --enable-ftp --enable-mbstring --enable-mysqlnd --enable-sockets --enable-pcntl --with-password-argon2 --with-curl --with-pdo-pgsql --with-pdo-mysql --with-libedit --with-openssl --with-zlib $( test "$gnuArch" = 's390x-linux-gnu' \
 && echo '--without-pcre-jit' ;) --with-libdir="lib/$debMultiarch" ${PHP_EXTRA_CONFIGURE_ARGS:-} ; make -j "$( nproc ;)" ; make install ; find /usr/local/bin /usr/local/sbin -type f -executable -exec strip --strip-all '{}' + || true ; make clean ; cd / ; rm -rf $PHP_SRC_DIR ; rm $SRC_DIR/php.tar.xz ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; find /usr/local -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; php --version ; pecl update-channels ; rm -rf /tmp/pear ~/.pearrc ; mkdir "$PHP_INI_DIR" ; mkdir "$PHP_INI_DIR/conf.d" ; touch "$PHP_INI_DIR/conf.d/memory.ini" \
 && echo "memory_limit = 1G;" >> "$PHP_INI_DIR/conf.d/memory.ini"
ENV PATH="$PHPPATH/bin:/usr/local/php/bin:$PATH"
#   Install Composer globally
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/bin --filename=composer
#  ****************      END PHP     ****************************************************
#  ****************      NODEJS     ****************************************************
ENV N_SRC_DIR="$SRC_DIR/n"
RUN git clone https://github.com/tj/n $N_SRC_DIR \
 && cd $N_SRC_DIR \
 && make install \
 && n $NODE_8_VERSION \
 && npm install grunt@1.6.1 --save-dev -g \
 && npm install grunt-cli@1.4.3 --save-dev -g \
 && npm install webpack@5.79.0 --save-dev -g \
 && n $NODE_VERSION \
 && npm install grunt@1.6.1 --save-dev -g \
 && npm install grunt-cli@1.4.3 --save-dev -g \
 && npm install webpack@5.79.0 --save-dev -g \
 && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update \
 && apt-get install --no-install-recommends yarn -y \
 && cd / \
 && rm -rf $N_SRC_DIR
#  ****************      END NODEJS     ****************************************************
#  ****************      JAVA     ****************************************************
#   Copy install tools
COPY tools /opt/tools
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64" \
    JDK_HOME="/usr/lib/jvm/java-8-openjdk-amd64" \
    JRE_HOME="/usr/lib/jvm/java-8-openjdk-amd64/jre" \
    ANT_VERSION="1.10.5" \
    MAVEN_HOME="/opt/maven" \
    MAVEN_VERSION="3.6.0" \
    MAVEN_CONFIG="/root/.m2" \
    INSTALLED_GRADLE_VERSIONS="4.10.2 5.2.1" \
    GRADLE_VERSION="5.2.1" \
    SBT_VERSION="1.2.8" \
    JDK_VERSION="11.0.2" \
    JDK_VERSION_TAG="9" \
    ANDROID_HOME="/usr/local/android-sdk-linux" \
    GRADLE_PATH="$SRC_DIR/gradle" \
    ANDROID_SDK_MANAGER_VER="4333796" \
    ANDROID_SDK_BUILD_TOOLS="build-tools;28.0.3" \
    ANDROID_SDK_PLATFORM_TOOLS="platforms;android-28" \
    ANDROID_SDK_EXTRAS="extras;android;m2repository extras;google;m2repository extras;google;google_play_services" \
    JDK_DOWNLOAD_SHA256="99be79935354f5c0df1ad293620ea36d13f48ec3ea870c838f20c504c9668b57" \
    ANT_DOWNLOAD_SHA512="acfa34c4f820d882f26ec67cf885d7dd484d534a7e99b33b05779e03da61849610328d2dbb4bfaa201e1ae75a0f0901e9c2bb793ed7bd76d3e4497e6ca5de371" \
    MAVEN_DOWNLOAD_SHA512="fae9c12b570c3ba18116a4e26ea524b29f7279c17cbaadc3326ca72927368924d9131d11b9e851b8dc9162228b6fdea955446be41207a5cfc61283dd8a561d2f" \
    GRADLE_DOWNLOAD_SHA256="748c33ff8d216736723be4037085b8dc342c6a0f309081acf682c9803e407357" \
    ANDROID_SDK_MANAGER_SHA256="92ffee5a1d98d856634e8b71132e8a95d96c83a63fde1099be3d86df3106def9"
ENV JDK_DOWNLOAD_TAR="openjdk-${JDK_VERSION}_linux-x64_bin.tar.gz"
ENV PATH="${PATH}:/opt/tools:${ANDROID_HOME}/tools:${ANDROID_HOME}/tools/bin:${ANDROID_HOME}/platform-tools"
RUN set -ex \
 && apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y \
 && add-apt-repository -y ppa:openjdk-r/ppa \
 && apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 -y \
 && apt-get install --no-install-recommends ca-certificates-java=20180516ubuntu1~18.04.1 -y \
 && update-ca-certificates -f \
 && dpkg --add-architecture i386 \
 && apt-get update \
 && apt-get install --no-install-recommends libc6-i386=2.27-3ubuntu1.6 lib32stdc++6=8.4.0-1ubuntu1~18.04 lib32gcc1=1:8.4.0-1ubuntu1~18.04 lib32ncurses5=6.1-1ubuntu1.18.04 lib32z1=1:1.2.11.dfsg-0ubuntu2.2 libqt5widgets5=5.9.5+dfsg-0ubuntu2.6 -y --force-yes \
 && wget "https://dl.google.com/android/repository/sdk-tools-linux-${ANDROID_SDK_MANAGER_VER}.zip" -O /tmp/android-sdkmanager.zip \
 && echo "${ANDROID_SDK_MANAGER_SHA256} /tmp/android-sdkmanager.zip" | sha256sum -c - \
 && mkdir -p ${ANDROID_HOME} \
 && unzip /tmp/android-sdkmanager.zip -d ${ANDROID_HOME} \
 && chown -R root.root ${ANDROID_HOME} \
 && ln -s ${ANDROID_HOME}/tools/android /usr/bin/android \
 && android-accept-licenses.sh "sdkmanager --verbose platform-tools ${ANDROID_SDK_BUILD_TOOLS} ${ANDROID_SDK_PLATFORM_TOOLS} ${ANDROID_SDK_EXTRAS}" \
 && android-accept-licenses.sh "sdkmanager --licenses" \
 && apt-get install --no-install-recommends python-setuptools=39.0.1-2ubuntu0.1 -y \
 && curl -LSso /var/tmp/apache-ant-$ANT_VERSION-bin.tar.gz https://archive.apache.org/dist/ant/binaries/apache-ant-$ANT_VERSION-bin.tar.gz \
 && echo "$ANT_DOWNLOAD_SHA512 /var/tmp/apache-ant-$ANT_VERSION-bin.tar.gz" | sha512sum -c - \
 && tar -xzf /var/tmp/apache-ant-$ANT_VERSION-bin.tar.gz -C /opt \
 && update-alternatives --install /usr/bin/ant ant /opt/apache-ant-$ANT_VERSION/bin/ant 10000 \
 && mkdir -p $MAVEN_HOME \
 && curl -LSso /var/tmp/apache-maven-$MAVEN_VERSION-bin.tar.gz https://apache.org/dist/maven/maven-3/$MAVEN_VERSION/binaries/apache-maven-$MAVEN_VERSION-bin.tar.gz \
 && echo "$MAVEN_DOWNLOAD_SHA512 /var/tmp/apache-maven-$MAVEN_VERSION-bin.tar.gz" | sha512sum -c - \
 && tar xzvf /var/tmp/apache-maven-$MAVEN_VERSION-bin.tar.gz -C $MAVEN_HOME --strip-components=1 \
 && update-alternatives --install /usr/bin/mvn mvn /opt/maven/bin/mvn 10000 \
 && mkdir -p $MAVEN_CONFIG \
 && mkdir -p $GRADLE_PATH \
 && for version in $INSTALLED_GRADLE_VERSIONS; do { wget "https://services.gradle.org/distributions/gradle-$version-all.zip" -O "$GRADLE_PATH/gradle-$version-all.zip" \
 && unzip "$GRADLE_PATH/gradle-$version-all.zip" -d /usr/local \
 && mkdir "/tmp/gradle-$version" \
 && "/usr/local/gradle-$version/bin/gradle" -p "/tmp/gradle-$version" wrapper \
 && perl -pi -e "s/gradle-$version-bin.zip/gradle-$version-all.zip/" "/tmp/gradle-$version/gradle/wrapper/gradle-wrapper.properties" \
 && "/tmp/gradle-$version/gradlew" -p "/tmp/gradle-$version" init \
 && rm -rf "/tmp/gradle-$version" \
 && if [ "$version" != "$GRADLE_VERSION" ] ; then rm -rf "/usr/local/gradle-$version" ; fi ; } ; done \
 && ln -s /usr/local/gradle-$GRADLE_VERSION/bin/gradle /usr/bin/gradle \
 && rm -rf $GRADLE_PATH \
 && echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list \
 && apt-get install --no-install-recommends apt-transport-https=1.6.14 -y \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 \
 && apt-get update \
 && apt-get install --no-install-recommends sbt=$SBT_VERSION -y \
 && rm -fr /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && apt-get clean
#  ****************     END JAVA     ****************************************************
#  ****************     GO     **********************************************************
ENV GOLANG_DOWNLOAD_SHA256="fb26c30e6a04ad937bbc657a1b5bba92f80096af1e8ee6da6430c045a8db3a5b" \
    GOPATH="/go" \
    DEP_VERSION="0.5.0" \
    DEP_BINARY="dep-linux-amd64"
RUN set -ex \
 && mkdir -p "$GOPATH/src" "$GOPATH/bin" \
 && chmod -R 777 "$GOPATH" \
 && apt-get update \
 && apt-get install --no-install-recommends pkg-config=0.29.1-0ubuntu2 -y \
 && apt-get clean \
 && wget "https://storage.googleapis.com/golang/go$GOLANG_VERSION.linux-amd64.tar.gz" -O /tmp/golang.tar.gz \
 && echo "$GOLANG_DOWNLOAD_SHA256 /tmp/golang.tar.gz" | sha256sum -c - \
 && tar -xzf /tmp/golang.tar.gz -C /usr/local \
 && rm -fr /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && wget "https://github.com/golang/dep/releases/download/v$DEP_VERSION/$DEP_BINARY" -O "$GOPATH/bin/dep" \
 && chmod +x "$GOPATH/bin/dep"
ENV PATH="$GOPATH/bin:/usr/local/go/bin:$PATH"
#  ****************     END GO     **********************************************************
#  ****************     .NET-CORE     *******************************************************
RUN set -ex \
 && apt-get update \
 && apt-get install --no-install-recommends libc6=2.27-3ubuntu1.6 libgcc1=1:8.4.0-1ubuntu1~18.04 libgssapi-krb5-2=1.16-2ubuntu0.4 liblttng-ust0=2.10.1-1 libstdc++6=8.4.0-1ubuntu1~18.04 zlib1g=1:1.2.11.dfsg-0ubuntu2.2 software-properties-common=0.96.24.32.20 -y \
 && add-apt-repository ppa:ubuntu-toolchain-r/test -y \
 && apt-get update \
 && rm -rf /var/lib/apt/lists/*
#   Install .NET Core SDK
ENV DOTNET_SDK_DOWNLOAD_URL="https://dotnetcli.blob.core.windows.net/dotnet/Sdk/$DOTNET_SDK_VERSION/dotnet-sdk-$DOTNET_SDK_VERSION-linux-x64.tar.gz"
ENV DOTNET_SDK_DOWNLOAD_SHA="d7ed76a0efe2b07ac0bb3af611072b3b99f646200759cb5905a7944b1687f34d42b4b74a3a5c77dbe251f769c6c3878fc30a8d0f8f44e44bf4c7116699f3f948"
RUN set -ex \
 && curl -SL $DOTNET_SDK_DOWNLOAD_URL --output dotnet.tar.gz \
 && echo "$DOTNET_SDK_DOWNLOAD_SHA dotnet.tar.gz" | sha512sum -c - \
 && mkdir -p /usr/share/dotnet \
 && tar -zxf dotnet.tar.gz -C /usr/share/dotnet \
 && rm dotnet.tar.gz \
 && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet
#   Add .NET Core Global Tools install folder to PATH
ENV PATH="\"~/.dotnet/tools/:$PATH\""
#   Trigger the population of the local package cache
ENV NUGET_XMLDOC_MODE="skip"
RUN set -ex \
 && mkdir warmup \
 && cd warmup \
 && dotnet new \
 && cd .. \
 && rm -rf warmup \
 && rm -rf /tmp/NuGetScratch
#   Install Powershell Core
#   See instructions at https://docs.microsoft.com/en-us/powershell/scripting/setup/installing-powershell-core-on-linux
ENV POWERSHELL_VERSION="6.1.1"
ENV POWERSHELL_DOWNLOAD_URL="https://github.com/PowerShell/PowerShell/releases/download/v$POWERSHELL_VERSION/powershell-$POWERSHELL_VERSION-linux-x64.tar.gz"
ENV POWERSHELL_DOWNLOAD_SHA="822CB473A5B3D076584181BB5D308035A9FBD079A68762E9E6C0D7543E05B513"
RUN set -ex \
 && curl -SL $POWERSHELL_DOWNLOAD_URL --output powershell.tar.gz \
 && echo "$POWERSHELL_DOWNLOAD_SHA powershell.tar.gz" | sha256sum -c - \
 && mkdir -p /opt/microsoft/powershell/$POWERSHELL_VERSION \
 && tar zxf powershell.tar.gz -C /opt/microsoft/powershell/$POWERSHELL_VERSION \
 && rm powershell.tar.gz \
 && ln -s /opt/microsoft/powershell/$POWERSHELL_VERSION/pwsh /usr/bin/pwsh
#  ****************     END .NET-CORE     *******************************************************
#  ****************    HEADLESS BROWSERS     *******************************************************
RUN set -ex \
 && apt-add-repository "deb http://archive.canonical.com/ubuntu $( lsb_release -sc ;) partner" \
 && apt-add-repository ppa:malteworld/ppa \
 && apt-get update \
 && apt-get install --no-install-recommends libgtk-3-0=3.22.30-1ubuntu4 libglib2.0-0=2.56.4-0ubuntu0.18.04.9 libdbus-glib-1-2=0.110-2 libdbus-1-3=1.12.2-1ubuntu1.4 libasound2=1.1.3-5ubuntu0.6 -y \
 && wget -O ~/FirefoxSetup.tar.bz2 "https://download.mozilla.org/?product=firefox-latest&os=linux64" \
 && tar xjf ~/FirefoxSetup.tar.bz2 -C /opt/ \
 && ln -s /opt/firefox/firefox /usr/local/bin/firefox \
 && rm ~/FirefoxSetup.tar.bz2 \
 && firefox --version
#   Install Chrome
RUN set -ex \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/google-chrome-stable_current_amd64.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
 && (dpkg -i /tmp/google-chrome-stable_current_amd64.deb || apt-get install --no-install-recommends -fy ) \
 && rm -rf /tmp/google-chrome-stable_current_amd64.deb \
 && sed -i 's|HERE/chrome"|HERE/chrome" --disable-setuid-sandbox --no-sandbox|g' "/opt/google/chrome/google-chrome" \
 && google-chrome --version
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
