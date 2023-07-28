#
#    Author: Hari Sekhon
#    Date: 2016-01-16 09:58:07 +0000 (Sat, 16 Jan 2016)
#
#    vim:ts=4:sts=4:sw=4:et
#
#    https://github.com/harisekhon/Dockerfiles
#
#    If you're using my code you're welcome to connect with me on LinkedIn and optionally send me feedback to help improve or steer this or other code I publish
#
#    https://www.linkedin.com/in/harisekhon
#
FROM harisekhon/alpine-java:latest
MAINTAINER Hari Sekhon (https://www.linkedin.com/in/harisekhon)
LABEL Description="Alpine Dev Build"
#  ENV GRADLE_HOME=/opt/gradle
ENV GROOVY_HOME="/opt/groovy"
ENV JYTHON_HOME="/opt/jython"
#  ENV MAVEN_HOME=/opt/maven
ENV SBT_HOME="/opt/sbt"
ENV PATH="$PATH:$GRADLE_HOME/bin:$GROOVY_HOME/bin:$JYTHON_HOME/bin:$MAVEN_HOME/bin:$SBT_HOME/bin"
RUN set -euxo pipefail \
 && mkdir -p /opt \
 && apk add acf-openssl=0.10.1-r6 alpine-sdk=1.0-r1 bash=5.2.15-r0 bind-tools=9.18.13-r0 curl=7.88.1-r1 cyrus-sasl-dev=2.1.28-r3 expat-dev=2.5.0-r0 expect=5.45.4-r3 ethtool=6.0-r0 fping=5.1-r1 gcc=12.2.1_git20220924-r4 git=2.38.4-r1 go=1.19.8-r0 gradle=7.5.1-r3 grep=3.8-r1 jwhois=4.0-r6 libev=4.33-r0 libevdev=1.13.0-r0 libressl-dev=3.6.2-r0 lsof=4.96.4-r0 make=4.3-r1 mariadb-dev=10.6.12-r0 maven=3.8.6-r0 netcat-openbsd=1.130-r4 net-tools=2.10-r0 nmap=7.93-r0 nmap-ncat=7.93-r0 perl=5.36.0-r0 perl-dbd-mysql=4.050-r5 perl-libwww=6.67-r0 procps=3.3.17-r2 py-mysqldb py-pip py-setuptools python-dev ruby=3.1.4-r0 ruby-dev=3.1.4-r0 snappy-dev=1.1.9-r4 socat=1.7.4.4-r0 strace=6.0-r0 sysstat=12.6.1-r0 tar=1.34-r2 tcpdump=4.99.4-r0 unzip=6.0-r13 vim=9.0.0999-r0 wget=1.21.3-r2 which=2.21-r3 zip=3.0-r10 --no-cache
#   didn't have Gradle / Groovy / Maven / SBT packages ...
#   Maven - has this now
#  RUN set -euxo pipefail && \
#      curl https://raw.githubusercontent.com/HariSekhon/bash-tools/master/install_maven.sh | sh
#   Gradle - has this now
#  RUN set -euxo pipefail && curl https://raw.githubusercontent.com/HariSekhon/bash-tools/master/install_gradle.sh | sh
#   SBT
RUN set -euxo pipefail \
 && cd /opt \
 && curl https://raw.githubusercontent.com/HariSekhon/bash-tools/master/install_sbt.sh | sh
#   Groovy
RUN set -euxo pipefail \
 && curl https://raw.githubusercontent.com/HariSekhon/bash-tools/master/install_groovy.sh | sh
#   Jython
RUN set -euxo pipefail \
 && wget https://raw.githubusercontent.com/HariSekhon/devops-python-tools/master/jython_install.sh \
 && wget https://raw.githubusercontent.com/HariSekhon/devops-python-tools/master/jython_autoinstall.exp \
 && sh jython_install.sh \
 && rm -f jython_install.sh jython_autoinstall.exp
CMD /bin/bash
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
