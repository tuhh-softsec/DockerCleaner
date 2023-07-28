#
#   jenkins 1.6x (http://jenkins-ci.org)
#
#   build:
#     docker build --force-rm=true -t subicura/jenkins .
#   run:
#     docker run -p 80:8080 -v /home/ubuntu/jenkins:/app/jenkins \
#                           -v /var/run/docker.sock:/var/run/docker.sock \
#                           -d subicura/jenkins
#
FROM ubuntu:14.04
MAINTAINER chungsub.kim@purpleworks.co.kr
ENV DEBIAN_FRONTEND="noninteractive"
#   update ubuntu
RUN sudo sed -i 's/archive.ubuntu.com/ubuntu.mirrors.yg.ucloud.cn/' /etc/apt/sources.list \
 && sudo sed -i 's/security.ubuntu.com/ubuntu.mirrors.yg.ucloud.cn/' /etc/apt/sources.list
#   update ubuntu latest
RUN echo "2015.09.01"
RUN : \
 && apt-get -qq -y dist-upgrade
#   install essential packages
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 software-properties-common=0.92.37.8 git=1:1.9.1-1ubuntu0.10 wget=1.15-1ubuntu1.14.04.5 curl=7.35.0-1ubuntu2.20 -qq -y )
#   install dev library
RUN (apt-get update ;apt-get install --no-install-recommends bison=2:3.0.2.dfsg-2 openssl=1.0.1f-1ubuntu2.27 libreadline6=6.3-4ubuntu2 libreadline6-dev=6.3-4ubuntu2 curl=7.35.0-1ubuntu2.20 git-core=1:1.9.1-1ubuntu0.10 zlib1g=1:1.2.8.dfsg-1ubuntu1.1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libssl-dev=1.0.1f-1ubuntu2.27 libyaml-dev=0.1.4-3ubuntu3.1 libsqlite3-0=3.8.2-1ubuntu2.2 libsqlite3-dev=3.8.2-1ubuntu2.2 sqlite3=3.8.2-1ubuntu2.2 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt-dev autoconf=2.69-6 libc6-dev=2.19-0ubuntu6.15 ncurses-dev libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libmysqlclient-dev=5.5.62-0ubuntu0.14.04.1 imagemagick=8:6.7.7.10-6ubuntu3.13 libmagickcore-dev=8:6.7.7.10-6ubuntu3.13 libmagickwand-dev=8:6.7.7.10-6ubuntu3.13 -qq -y )
#   install java
RUN echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && add-apt-repository -y ppa:webupd8team/java \
 && apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends oracle-java7-installer -qq -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/oracle-jdk7-installer
ENV JAVA_HOME="/usr/lib/jvm/java-7-oracle"
#   install tomcat
RUN mkdir /app \
 && cd /app \
 && wget -q -O - http://mirror.bit.edu.cn/apache/tomcat/tomcat-7/v7.0.64/bin/apache-tomcat-7.0.64.tar.gz | tar xfz - \
 && ln -s apache-tomcat-7.0.64 tomcat
#   install jenkins
RUN rm -rf /app/tomcat/webapps/*
COPY jenkins.war /app/tomcat/webapps/ROOT.war
#   install rbenv
RUN git clone https://github.com/sstephenson/rbenv.git ~/.rbenv \
 && mkdir -p ~/.rbenv/plugins \
 && git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
#   install node
RUN curl -sL https://deb.nodesource.com/setup_0.12 | sudo bash - \
 && (apt-get update ;apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 -qq -y ) \
 && npm config set registry http://r.cnpmjs.org \
 && npm install bower@1.8.14 -g \
 && npm install grunt-cli@1.4.3 -g
#   install docker
RUN curl -sSL https://get.docker.com/ | sh
#   setup jenkins
WORKDIR /app
ENV JENKINS_HOME="/app/jenkins/home"
#   add plugins
RUN mkdir -p /app/plugins
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/scm-api.hpi
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/git.hpi
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/git-client.hpi
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/git-server.hpi
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/ssh-agent.hpi
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/slack.hpi
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/violations.hpi
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/ansicolor.hpi
#   add plugins (ruby)
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/rbenv.hpi
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/rake.hpi
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/ruby-runtime.hpi
RUN wget -q -P /app/plugins http://updates.jenkins-ci.org/latest/rubyMetrics.hpi
#   env
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
ENV TERM="xterm"
#   volume
VOLUME ["/app/jenkins"]
#   expose
EXPOSE 8080/tcp
#   run
COPY run.sh /app/run.sh
RUN ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
CMD /app/run.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
