FROM debian:jessie-slim
MAINTAINER Leonardo Loures <luvres@hotmail.com>
RUN apt-get update \
 && apt-get install --no-install-recommends openssh-server openssh-client bash-completion bzip2 unzip rsync curl net-tools nano sudo supervisor -y \
 && ssh-keygen -t dsa -P '' -f ~/.ssh/id_dsa \
 && ssh-keygen -t rsa -P '' -f ~/.ssh/id_rsa \
 && cat ~/.ssh/id_dsa.pub >> ~/.ssh/authorized_keys \
 && chmod 0600 ~/.ssh/authorized_keys \
 && sed -i '/StrictHostKeyChecking/s/#//g' /etc/ssh/ssh_config \
 && sed -i '/StrictHostKeyChecking/s/ask/no/g' /etc/ssh/ssh_config \
 && JAVA_VERSION_MAJOR=8 \
 && JAVA_VERSION_MINOR=181 \
 && JAVA_VERSION_BUILD=13 \
 && JAVA_PACKAGE=jdk \
 && URL=96a7b8442fe848ef90c96a2fad6ed6d1 \
 && curl -jkSLH "Cookie: oraclelicense=accept-securebackup-cookie" http://download.oracle.com/otn-pub/java/jdk/${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-b${JAVA_VERSION_BUILD}/${URL}/${JAVA_PACKAGE}-${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-linux-x64.tar.gz | tar -xzf - -C /usr/local \
 && ln -s /usr/local/jdk1.${JAVA_VERSION_MAJOR}.0_${JAVA_VERSION_MINOR} /opt/jdk \
 && rm -rf /opt/jdk/*src.zip /opt/jdk/lib/missioncontrol /opt/jdk/lib/visualvm /opt/jdk/lib/*javafx* /opt/jdk/jre/plugin /opt/jdk/jre/bin/javaws /opt/jdk/jre/bin/jjs /opt/jdk/jre/bin/orbd /opt/jdk/jre/bin/pack200 /opt/jdk/jre/bin/policytool /opt/jdk/jre/bin/rmid /opt/jdk/jre/bin/rmiregistry /opt/jdk/jre/bin/servertool /opt/jdk/jre/bin/tnameserv /opt/jdk/jre/bin/unpack200 /opt/jdk/jre/lib/javaws.jar /opt/jdk/jre/lib/deploy* /opt/jdk/jre/lib/desktop /opt/jdk/jre/lib/*javafx* /opt/jdk/jre/lib/*jfx* /opt/jdk/jre/lib/amd64/libdecora_sse.so /opt/jdk/jre/lib/amd64/libprism_*.so /opt/jdk/jre/lib/amd64/libfxplugins.so /opt/jdk/jre/lib/amd64/libglass.so /opt/jdk/jre/lib/amd64/libgstreamer-lite.so /opt/jdk/jre/lib/amd64/libjavafx*.so /opt/jdk/jre/lib/amd64/libjfx*.so /opt/jdk/jre/lib/ext/jfxrt.jar /opt/jdk/jre/lib/ext/nashorn.jar /opt/jdk/jre/lib/oblique-fonts /opt/jdk/jre/lib/plugin.jar /tmp/* /var/cache/apt/*
ENV JAVA_HOME="/opt/jdk"
ENV PATH="${PATH}:${JAVA_HOME}/bin:${JAVA_HOME}/sbin"
#   Hadoop
RUN HADOOP_VERSION=2.8.5 \
 && curl http://ftp.unicamp.br/pub/apache/hadoop/common/hadoop-${HADOOP_VERSION}/hadoop-${HADOOP_VERSION}.tar.gz | tar -xzf - -C /usr/local/ \
 && rm -fR /usr/local/hadoop-${HADOOP_VERSION}/share/doc /usr/local/hadoop-${HADOOP_VERSION}/share/hadoop/common/jdiff \
 && ln -s /usr/local/hadoop-${HADOOP_VERSION}/ /opt/hadoop
ENV HADOOP_HOME="/opt/hadoop"
ENV HADOOP_INSTALL="$HADOOP_HOME"
ENV HADOOP_COMMON_HOME="$HADOOP_HOME"
ENV HADOOP_MAPRED_HOME="$HADOOP_HOME"
ENV HADOOP_HDFS_HOME="$HADOOP_HOME"
ENV YARN_HOME="$HADOOP_HOME"
ENV PATH="$PATH:$HADOOP_HOME/bin:$HADOOP_HOME/sbin"
#   Configurations Pseudo Distributed
COPY hadoop-env.sh $HADOOP_HOME/etc/hadoop/hadoop-env.sh
COPY core-site.xml $HADOOP_HOME/etc/hadoop/core-site.xml
COPY hdfs-site.xml $HADOOP_HOME/etc/hadoop/hdfs-site.xml
COPY mapred-site.xml $HADOOP_HOME/etc/hadoop/mapred-site.xml
COPY yarn-site.xml $HADOOP_HOME/etc/hadoop/yarn-site.xml
COPY start.sh /etc/start.sh
RUN chmod +x /etc/start.sh \
 && hdfs namenode -format
#   Set up S6 init system
#  ADD https://github.com/just-containers/s6-overlay/releases/download/v1.18.1.5/s6-overlay-amd64.tar.gz /tmp/
#  RUN gunzip -c /tmp/s6-overlay-amd64.tar.gz | tar -xf - -C /
#   Supervidor
COPY supervisord.conf /etc/supervisord.conf
WORKDIR /root
#   Hdfs ports
EXPOSE 50010/tcp 50020/tcp 50070/tcp 50075/tcp 50090/tcp 8020/tcp 9000/tcp
#   Mapred ports
EXPOSE 10020/tcp 19888/tcp
#   Yarn ports
EXPOSE 8030/tcp 8031/tcp 8032/tcp 8033/tcp 8040/tcp 8042/tcp 8088/tcp
#   Other ports
EXPOSE 22/tcp
#  RUN mkdir /etc/services.d/ssh \
#      && echo '#!/usr/bin/bash' >/etc/services.d/ssh/run \
#      && echo 'exec /etc/init.d/ssh start' >>/etc/services.d/ssh/run
#  ENTRYPOINT ["/init"]
#  CMD ["/etc/start.sh"]
ENTRYPOINT ["/etc/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
