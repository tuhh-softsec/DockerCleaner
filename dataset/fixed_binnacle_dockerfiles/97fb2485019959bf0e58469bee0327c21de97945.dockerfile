#  upstream https://github.com/jenkinsci/docker-jnlp-slave
FROM debian:stretch
MAINTAINER 若虚 <slpcat@qq.com>
ENV TERM="xterm" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    TIMEZONE="Asia/Shanghai" \
    SSH_KNOWN_HOSTS="github.com"
RUN echo 'deb http://mirrors.aliyun.com/debian stretch-backports main' > /etc/apt/sources.list.d/backports.list \
 && sed -i 's/deb.debian.org/mirrors.aliyun.com/' /etc/apt/sources.list
#   Set timezone and locales
RUN echo "${TIMEZONE}" > /etc/timezone \
 && echo "$LANG UTF-8" > /etc/locale.gen \
 && : \
 && DEBIAN_FRONTEND=noninteractive apt-get install -yq apt-utils curl dialog vim-tiny locales \
 && ln -sf /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && update-locale LANG=$LANG \
 && locale-gen $LANG \
 && DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales
#  openjdk8 apt-get install -y openjdk-8-jdk-headless
#   Add Oracle Java PPA
RUN apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.2-1+deb9u1 apt-transport-https=1.4.11 gnupg2=2.1.18-8~deb9u4 ca-certificates=20200601~deb9u2 -y ) \
 && echo 'deb http://ppa.launchpad.net/webupd8team/java/ubuntu xenial main' > /etc/apt/sources.list.d/webupd8team-ubuntu-java.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EEA14886 \
 && apt-get update -y \
 && echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && (apt-get update ;apt-get install --no-install-recommends libpq-dev=9.6.24-0+deb9u1 oracle-java8-installer -y ) \
 && rm -f /var/cache/oracle-jdk8-installer/jdk-*.tar.g
RUN apt-get update -y \
 && apt-get upgrade -y \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.3 bzip2=1.0.6-8.1 sudo=1.8.19p1-2.1+deb9u3 git=1:2.11.0-3+deb9u7 iptables=1.6.0+snapshot20161117-6 jq=1.5+dfsg-1.3 unzip=6.0-21+deb9u2 zip=3.0-11+b1 -y )
ARG MAVEN_VERSION=3.5.3
ARG USER_HOME_DIR="/root"
ARG SHA=b52956373fab1dd4277926507ab189fb797b3bc51a2a267a193c931fffad8408
#  ARG BASE_URL=https://apache.osuosl.org/maven/maven-3/${MAVEN_VERSION}/binaries
ARG BASE_URL=http://mirrors.tuna.tsinghua.edu.cn/apache/maven/maven-3/${MAVEN_VERSION}/binaries
RUN mkdir -p /usr/share/maven /usr/share/maven/ref \
 && curl -fsSL -o /tmp/apache-maven.tar.gz ${BASE_URL}/apache-maven-${MAVEN_VERSION}-bin.tar.gz \
 && echo "${SHA} /tmp/apache-maven.tar.gz" | sha256sum -c - \
 && tar -xzf /tmp/apache-maven.tar.gz -C /usr/share/maven --strip-components=1 \
 && rm -f /tmp/apache-maven.tar.gz \
 && ln -s /usr/share/maven/bin/mvn /usr/bin/mvn
ENV MAVEN_HOME="/usr/share/maven"
ENV MAVEN_CONFIG="\"$USER_HOME_DIR/.m2\""
COPY mvn-entrypoint.sh /usr/local/bin/mvn-entrypoint.sh
COPY settings-docker.xml /usr/share/maven/ref/
#  Install Docker in Docker
RUN curl -fsSL https://download.docker.com/linux/$( . /etc/os-release ;echo "$ID" ;)/gpg | apt-key add - \
 && add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/$( . /etc/os-release ;echo "$ID" ;) $( lsb_release -cs ;) stable" \
 && apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends docker-ce -y )
COPY daemon.json /etc/docker/daemon.json
#   jenkins slave
ENV HOME="/home/jenkins"
RUN groupadd -g 10000 jenkins \
 && useradd -c "Jenkins user" -d $HOME -u 10000 -g 10000 -m jenkins \
 && usermod -a -G docker jenkins \
 && sed -i '/^root/a\jenkins ALL=(ALL:ALL) NOPASSWD:ALL' /etc/sudoers
LABEL Description="This is a base image, which provides the Jenkins agent executable (slave.jar)" \
      Vendor="Jenkins project" \
      Version="3.20"
ARG VERSION=3.20
ARG AGENT_WORKDIR=/home/jenkins/agent
RUN curl --create-dirs -sSLo /usr/share/jenkins/slave.jar https://repo.jenkins-ci.org/public/org/jenkins-ci/main/remoting/${VERSION}/remoting-${VERSION}.jar \
 && chmod 755 /usr/share/jenkins \
 && chmod 644 /usr/share/jenkins/slave.jar
COPY jenkins-slave /usr/local/bin/jenkins-slave
USER jenkins
ENV AGENT_WORKDIR="${AGENT_WORKDIR}"
RUN mkdir /home/jenkins/.jenkins \
 && mkdir -p ${AGENT_WORKDIR}
VOLUME /home/jenkins/.jenkins
VOLUME ${AGENT_WORKDIR}
WORKDIR /home/jenkins
ENTRYPOINT ["jenkins-slave"]
# Please add your HEALTHCHECK here!!!
