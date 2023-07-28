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
#   Copy install tools
COPY tools /opt/tools
ENV NODE_VERSION="8.11.0" \
    ANDROID_HOME="/usr/local/android-sdk-linux" \
    JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64" \
    JAVA_VERSION="8" \
    JDK_VERSION="8u171-b11-2~14.04" \
    JDK_HOME="/usr/lib/jvm/java-8-openjdk-amd64" \
    JRE_HOME="/usr/lib/jvm/java-8-openjdk-amd64/jre" \
    INSTALLED_GRADLE_VERSIONS="2.14.1 3.5 4.0.2 4.1 4.2.1 4.3.1 4.4 4.6" \
    GRADLE_VERSION="4.6" \
    ANDROID_SDK_MANAGER_VER="3859397" \
    ANDROID_SDK_MANAGER_SHA256="444e22ce8ca0f67353bda4b85175ed3731cae3ffa695ca18119cbacef1c1bea0" \
    ANDROID_SDK_BUILD_TOOLS="build-tools;19.1.0 build-tools;20.0.0 build-tools;21.1.2 build-tools;22.0.1 build-tools;23.0.3 build-tools;24.0.3 build-tools;25.0.3 build-tools;26.0.3 build-tools;27.0.3" \
    ANDROID_SDK_PLATFORM_TOOLS="platforms;android-19 platforms;android-20 platforms;android-21 platforms;android-22 platforms;android-23 platforms;android-24 platforms;android-25 platforms;android-26 platforms;android-27" \
    ANDROID_SDK_EXTRAS="extras;android;m2repository extras;google;m2repository extras;google;google_play_services"
ENV PATH="${PATH}:/opt/tools:${ANDROID_HOME}/tools:${ANDROID_HOME}/tools/bin:${ANDROID_HOME}/platform-tools"
#   gpg keys listed at https://github.com/nodejs/node#release-team
RUN set -ex \
 && for key in 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 B9AE9905FFD7803F25714661B63B535A4C206CA9 77984A986EBC2AA786BC0F66B01FBB92821C587A 56730D5401028683275BD23C23EFEFE93C4CFFFE 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 FD3A5288F042B6850C66B31F09FE44734EB7990E 8FCCA13FEF1D0C2E91008E09770F7A9A5AE15600 C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 DD8F2338BAE7501E3DD5AC78C273792F7D83545D 9554F04D7259F04124DE6B476D5A82AC7E37093B 93C7E9E91B49E432C2F75674B0A78B0A6C481CF6 114F43EE0176B71C7BC219DD50A3051F888C628D 7937DFD2AB06298B2293C3187D33FF9D0246406D; do gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done
#   Install nodejs
RUN set -ex \
 && wget "https://nodejs.org/download/release/v$NODE_VERSION/node-v$NODE_VERSION-linux-x64.tar.gz" -O node-v$NODE_VERSION-linux-x64.tar.gz \
 && wget "https://nodejs.org/download/release/v$NODE_VERSION/SHASUMS256.txt.asc" -O SHASUMS256.txt.asc \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " node-v$NODE_VERSION-linux-x64.tar.gz$" SHASUMS256.txt | sha256sum -c - \
 && tar -xzf "node-v$NODE_VERSION-linux-x64.tar.gz" -C /usr/local --strip-components=1 \
 && rm "node-v$NODE_VERSION-linux-x64.tar.gz" SHASUMS256.txt.asc SHASUMS256.txt \
 && ln -s /usr/local/bin/node /usr/local/bin/nodejs \
 && rm -fr /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Install java8
RUN set -ex \
 && apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y \
 && add-apt-repository -y ppa:openjdk-r/ppa \
 && (echo oracle-java${JAVA_VERSION}-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections ) \
 && apt-get update \
 && apt-get install --no-install-recommends openjdk-${JAVA_VERSION}-jdk=${JDK_VERSION} -y \
 && update-ca-certificates -f \
 && apt-get install --no-install-recommends less=458-* groff=1.22.2-* -y -qq \
 && dpkg --add-architecture i386 \
 && apt-get update \
 && apt-get install --no-install-recommends expect=5.45-* libc6-i386=2.19-* lib32stdc++6=4.8.4-* lib32gcc1=1:4.9.3-* lib32ncurses5=5.9+20140118-* lib32z1=1:1.2.8.dfsg-* libqt5widgets5=5.2.1+dfsg-* -y --force-yes \
 && apt-get clean \
 && mkdir -p /usr/src/gradle \
 && for version in $INSTALLED_GRADLE_VERSIONS; do { wget "https://services.gradle.org/distributions/gradle-$version-all.zip" -O "/usr/src/gradle/gradle-$version-all.zip" \
 && unzip "/usr/src/gradle/gradle-$version-all.zip" -d /usr/local \
 && mkdir "/tmp/gradle-$version" \
 && "/usr/local/gradle-$version/bin/gradle" -p "/tmp/gradle-$version" wrapper \
 && perl -pi -e "s/gradle-$version-bin.zip/gradle-$version-all.zip/" "/tmp/gradle-$version/gradle/wrapper/gradle-wrapper.properties" \
 && "/tmp/gradle-$version/gradlew" -p "/tmp/gradle-$version" init \
 && rm -rf "/tmp/gradle-$version" \
 && if [ "$version" != "$GRADLE_VERSION" ] ; then rm -rf "/usr/local/gradle-$version" ; fi ; } ; done \
 && ln -s /usr/local/gradle-$GRADLE_VERSION/bin/gradle /usr/bin/gradle \
 && rm -rf /usr/src/gradle \
 && wget "https://dl.google.com/android/repository/sdk-tools-linux-${ANDROID_SDK_MANAGER_VER}.zip" -O /tmp/android-sdkmanager.zip \
 && echo "${ANDROID_SDK_MANAGER_SHA256} /tmp/android-sdkmanager.zip" | sha256sum -c - \
 && mkdir -p ${ANDROID_HOME} \
 && unzip /tmp/android-sdkmanager.zip -d ${ANDROID_HOME} \
 && chown -R root.root ${ANDROID_HOME} \
 && ln -s ${ANDROID_HOME}/tools/android /usr/bin/android \
 && android-accept-licenses.sh "sdkmanager --verbose platform-tools ${ANDROID_SDK_BUILD_TOOLS} ${ANDROID_SDK_PLATFORM_TOOLS} ${ANDROID_SDK_EXTRAS}" \
 && android-accept-licenses.sh "sdkmanager --licenses" \
 && rm -fr /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
