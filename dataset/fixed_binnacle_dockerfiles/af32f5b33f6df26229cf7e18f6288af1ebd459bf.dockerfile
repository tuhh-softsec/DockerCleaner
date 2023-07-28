FROM ubuntu:14.04
MAINTAINER xenron <xenron@hotmail.com>
#   install software package
RUN apt-get update -y \
 && apt-get install --no-install-recommends vim=2:7.4.052-1ubuntu3.1 tar=1.27.1-1ubuntu0.1 unzip=6.0-9ubuntu1.5 dnsmasq=2.68-1ubuntu0.2 wget=1.15-1ubuntu1.14.04.5 net-tools=1.60-25ubuntu2.1 curl=7.35.0-1ubuntu2.20 openssh-server=1:6.6p1-2ubuntu2.13 nano=2.2.6-1ubuntu1 g++=4:4.8.2-1ubuntu6 autoconf=2.69-6 automake=1:1.14.1-2ubuntu1 libtool=2.4.2-1.7ubuntu1 cmake=2.8.12.2-0ubuntu3 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 pkg-config=0.26-1ubuntu4 libssl-dev=1.0.1f-1ubuntu2.27 -y
RUN mkdir -p /opt
#   Protocol buffers
RUN wget -q -o out.log -P /tmp https://github.com/google/protobuf/releases/download/v2.5.0/protobuf-2.5.0.tar.gz \
 && tar xzf /tmp/protobuf-2.5.0.tar.gz -C /opt \
 && cd /opt/protobuf-2.5.0 \
 && ./autogen.sh \
 && ./configure --prefix=/usr \
 && make \
 && make check \
 && make install \
 && protoc --version
#   Clean
RUN apt-get clean -y \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/{apt,dpkg,cache,log}/ \
 && rm -rf /tmp
#   Java Version
ENV JAVA_VERSION_MAJOR="8"
ENV JAVA_VERSION_MINOR="111"
ENV JAVA_VERSION_BUILD="14"
ENV JAVA_PACKAGE="jdk"
#   Download and unarchive Java
RUN mkdir -p /opt \
 && curl -jksSLH "Cookie: oraclelicense=accept-securebackup-cookie" http://download.oracle.com/otn-pub/java/jdk/${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-b${JAVA_VERSION_BUILD}/${JAVA_PACKAGE}-${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-linux-x64.tar.gz | gunzip -c - | tar -xf - -C /opt \
 && ln -s /opt/jdk1.${JAVA_VERSION_MAJOR}.0_${JAVA_VERSION_MINOR} /opt/jdk \
 && rm -rf /opt/jdk/*src.zip /opt/jdk/lib/missioncontrol /opt/jdk/lib/visualvm /opt/jdk/lib/*javafx* /opt/jdk/jre/lib/plugin.jar /opt/jdk/jre/lib/ext/jfxrt.jar /opt/jdk/jre/bin/javaws /opt/jdk/jre/lib/javaws.jar /opt/jdk/jre/lib/desktop /opt/jdk/jre/plugin /opt/jdk/jre/lib/deploy* /opt/jdk/jre/lib/*javafx* /opt/jdk/jre/lib/*jfx* /opt/jdk/jre/lib/amd64/libdecora_sse.so /opt/jdk/jre/lib/amd64/libprism_*.so /opt/jdk/jre/lib/amd64/libfxplugins.so /opt/jdk/jre/lib/amd64/libglass.so /opt/jdk/jre/lib/amd64/libgstreamer-lite.so /opt/jdk/jre/lib/amd64/libjavafx*.so /opt/jdk/jre/lib/amd64/libjfx*.so
#   move all configuration files into container
COPY files/* /usr/local/
#   set environment variable 
ENV JAVA_HOME="/opt/jdk "
ENV PATH="${PATH}:${JAVA_HOME}/bin"
#   configure ssh free key access
RUN mkdir /var/run/sshd \
 && ssh-keygen -t rsa -f ~/.ssh/id_rsa -P '' \
 && cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys \
 && mv /usr/local/ssh_config ~/.ssh/config \
 && sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
#   Hadoop
RUN wget -q -o out.log -P /tmp https://archive.apache.org/dist/hadoop/common/hadoop-2.7.3/hadoop-2.7.3-src.tar.gz \
 && tar xzf /tmp/hadoop-2.7.3-src.tar.gz -C /opt \
 && rm /tmp/hadoop-2.7.3-src.tar.gz \
 && mv /opt/hadoop-2.7.3-src /opt/hadoop
#   Scala
RUN wget -q -o out.log -P /tmp http://downloads.lightbend.com/scala/2.11.8/scala-2.11.8.tgz \
 && tar xzf /tmp/scala-2.11.8.tgz -C /opt \
 && rm /tmp/scala-2.11.8.tgz \
 && mv /opt/scala-2.11.8 /opt/scala
#   Sbt
RUN wget -q -o out.log -P /tmp https://dl.bintray.com/sbt/native-packages/sbt/0.13.13/sbt-0.13.13.tgz \
 && tar xzf /tmp/sbt-0.13.13.tgz -C /opt \
 && rm /tmp/sbt-0.13.13.tgz \
 && mv /opt/sbt-launcher-packaging-0.13.13 /opt/sbt
#   Spark
RUN wget -q -o out.log -P /tmp https://archive.apache.org/dist/spark/spark-1.6.3/spark-1.6.3.tgz \
 && tar xzf /tmp/spark-1.6.3.tgz -C /opt \
 && rm /tmp/spark-1.6.3.tgz \
 && mv /opt/spark-1.6.3 /opt/spark
RUN mv /usr/local/bashrc ~/.bashrc
RUN . ~/.bashrc \
 && cd /opt/spark \
 && sbt assembly -Pyarn -Phadoop-2.7 -Pspark-ganglia-lgpl -Pkinesis-asl -Phive
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
