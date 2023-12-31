#   When updating the JRE: Refer to n4js/docs/org.eclipse.n4js.design/chapters/03_releng/releng.adoc: "Update of the embedded JRE"
#   openjdk:11.0.2-jdk-stretch
FROM openjdk@sha256:2c5663a6d16bbf7bb62e20155caf7577a30dad19f67e4db07986f00d096bde1a
LABEL version="0.1.3" \
      description="Build n4js with Maven. Includes xvfb, node." \
      maintainer="N4JS Team <n4js-dev@eclipse.org>"
ENV DEBIAN_FRONTEND="noninteractive"
ENV TZ="Europe/Berlin"
VOLUME /workspace
WORKDIR /workspace
ENTRYPOINT ["/usr/local/bin/mvn-entrypoint.sh"]
CMD ["mvn"]
#
#   Maven installation:
#   see https://github.com/carlossg/docker-maven/tree/ecf54b9839caed8aa2bcf9b8f7bb19594634ee89/jdk-8
#   and https://hub.docker.com/_/maven/
#
ARG MAVEN_VERSION=3.6.0
ARG USER_HOME_DIR="/home/build"
ARG SHA=fae9c12b570c3ba18116a4e26ea524b29f7279c17cbaadc3326ca72927368924d9131d11b9e851b8dc9162228b6fdea955446be41207a5cfc61283dd8a561d2f
ARG BASE_URL=https://archive.apache.org/dist/maven/maven-3/${MAVEN_VERSION}/binaries
ARG MAVEN_CENTRAL_URL=https://repo.maven.apache.org/maven2/
#   Assigning build-user to 1000
RUN groupadd --gid 1000 build \
 && useradd --uid 1000 --gid build --shell /bin/bash --create-home build
#   Add `build` user to the `docker` group
RUN groupadd -g 994 docker \
 && usermod -aG docker build
RUN mkdir -p /usr/share/maven /usr/share/maven/ref \
 && curl -fsSL -o /tmp/apache-maven.tar.gz ${BASE_URL}/apache-maven-$MAVEN_VERSION-bin.tar.gz \
 && echo "${SHA} /tmp/apache-maven.tar.gz" | sha512sum -c - \
 && tar -xzf /tmp/apache-maven.tar.gz -C /usr/share/maven --strip-components=1 \
 && rm -f /tmp/apache-maven.tar.gz \
 && ln -s /usr/share/maven/bin/mvn /usr/bin/mvn \
 && chown -R build:build /usr/share/maven
ENV MAVEN_HOME="/usr/share/maven"
ENV MAVEN_CONFIG="\"$USER_HOME_DIR/.m2\""
ENV MAVEN_OPTS="\"-Xms1536m -Xmx2048M\""
COPY copy-reference-files.sh /usr/local/bin/
COPY mvn-entrypoint.sh /usr/local/bin/
COPY settings.xml /usr/share/maven/ref/
RUN sed -i -e 's|MAVEN_CENTRAL_URL|'"$MAVEN_CENTRAL_URL"'|g' /usr/share/maven/ref/settings.xml \
 && /usr/local/bin/copy-reference-files.sh \
 && mkdir -p /usr/share/maven/ref/repository/ \
 && chown -R build:build /usr/share/maven/ref
#
#   The remaining part has been inspired by:
#   https://github.com/nodejs/docker-node/blob/master/8.9/wheezy/Dockerfile
#
#   Assigning node-user to 1001
RUN groupadd --gid 1001 node \
 && useradd --uid 1001 --gid node --shell /bin/bash --create-home node
RUN set -ex \
 && for key in 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 FD3A5288F042B6850C66B31F09FE44734EB7990E 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 DD8F2338BAE7501E3DD5AC78C273792F7D83545D C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 B9AE9905FFD7803F25714661B63B535A4C206CA9 56730D5401028683275BD23C23EFEFE93C4CFFFE 77984A986EBC2AA786BC0F66B01FBB92821C587A; do gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done
#
#   Node install
#
ENV NODE_VERSION="10.13.0"
RUN ARCH= \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && case "${dpkgArch##*-}" in (amd64) ARCH='x64' ;;(ppc64el) ARCH='ppc64le' ;;(*) echo "unsupported architecture" ; exit 1 ;; esac \
 && curl -SLO "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-$ARCH.tar.xz" \
 && curl -SLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " node-v$NODE_VERSION-linux-$ARCH.tar.xz$" SHASUMS256.txt | sha256sum -c - \
 && tar -xJf "node-v$NODE_VERSION-linux-$ARCH.tar.xz" -C /usr/local --strip-components=1 \
 && rm "node-v$NODE_VERSION-linux-$ARCH.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt \
 && ln -s /usr/local/bin/node /usr/local/bin/nodejs
#
#   Yarn install
#
ENV YARN_VERSION="1.15.2"
RUN set -ex \
 && for key in 6A010C5166006599AA17F08146C2130DFD2497F5; do gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done \
 && curl -fSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz" \
 && curl -fSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz.asc" \
 && gpg --batch --verify yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz \
 && mkdir -p /opt \
 && tar -xzf yarn-v$YARN_VERSION.tar.gz -C /opt/ \
 && ln -s /opt/yarn-v$YARN_VERSION/bin/yarn /usr/local/bin/yarn \
 && ln -s /opt/yarn-v$YARN_VERSION/bin/yarnpkg /usr/local/bin/yarnpkg \
 && rm yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz
#
#   xvfb install
#
RUN apt-get update \
 && apt-get install --no-install-recommends rsync=3.2.7-1 xauth=1:1.1.2-1 xvfb=2:21.1.7-1ubuntu2 -y
#
#   jq install
#
RUN apt-get update \
 && apt-get install --no-install-recommends jq=1.6-2.1ubuntu3 -y
#
#   docker install
#
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=2.6.0 ca-certificates=20230311 curl=7.88.1-7ubuntu1 gnupg2=2.2.40-1ubuntu2 software-properties-common=0.99.35 -y
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN lsb_release -cs
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian $( lsb_release -cs ;) stable"
RUN apt-get update \
 && apt-get install --no-install-recommends docker-ce -y
EXPOSE 4873/tcp
EXPOSE 4874/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
