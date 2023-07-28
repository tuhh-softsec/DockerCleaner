FROM centos
MAINTAINER Leonardo Loures <luvres@hotmail.com>
RUN yum install -y openssh-server openssh-clients bzip2 unzip rsync net-tools sudo which \
 && yum update -y \
 && yum clean all
#   SSH Key Passwordless
RUN ssh-keygen -t dsa -P '' -f ~/.ssh/id_dsa \
 && cat ~/.ssh/id_dsa.pub >> ~/.ssh/authorized_keys \
 && chmod 0600 ~/.ssh/authorized_keys
RUN ssh-keygen -q -t rsa -b 2048 -f /etc/ssh/ssh_host_rsa_key -N '' \
 && ssh-keygen -q -t ecdsa -f /etc/ssh/ssh_host_ecdsa_key -N '' \
 && ssh-keygen -t dsa -f /etc/ssh/ssh_host_ed25519_key -N ''
RUN sed -i '/StrictHostKeyChecking/s/#//g' /etc/ssh/ssh_config \
 && sed -i '/StrictHostKeyChecking/s/ask/no/g' /etc/ssh/ssh_config
ENV RPASS="@p4sS_-_#sECURITy*Cre4t3+bigZone"
RUN echo root:$RPASS | chpasswd
#   Timezone
RUN ln -sf /usr/share/zoneinfo/America/Sao_Paulo /etc/localtime
#   Java
RUN JAVA_VERSION_MAJOR=8 \
 && JAVA_VERSION_MINOR=112 \
 && JAVA_VERSION_BUILD=15 \
 && JAVA_PACKAGE=jdk \
 && curl -jkSLH "Cookie: oraclelicense=accept-securebackup-cookie" http://download.oracle.com/otn-pub/java/jdk/${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-b${JAVA_VERSION_BUILD}/${JAVA_PACKAGE}-${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-linux-x64.tar.gz | tar -xzf - -C /usr/local \
 && ln -s /usr/local/jdk1.${JAVA_VERSION_MAJOR}.0_${JAVA_VERSION_MINOR} /opt/jdk \
 && rm -rf /opt/jdk/*src.zip /opt/jdk/lib/missioncontrol /opt/jdk/lib/visualvm /opt/jdk/lib/*javafx* /opt/jdk/jre/plugin /opt/jdk/jre/bin/javaws /opt/jdk/jre/bin/jjs /opt/jdk/jre/bin/orbd /opt/jdk/jre/bin/pack200 /opt/jdk/jre/bin/policytool /opt/jdk/jre/bin/rmid /opt/jdk/jre/bin/rmiregistry /opt/jdk/jre/bin/servertool /opt/jdk/jre/bin/tnameserv /opt/jdk/jre/bin/unpack200 /opt/jdk/jre/lib/javaws.jar /opt/jdk/jre/lib/deploy* /opt/jdk/jre/lib/desktop /opt/jdk/jre/lib/*javafx* /opt/jdk/jre/lib/*jfx* /opt/jdk/jre/lib/amd64/libdecora_sse.so /opt/jdk/jre/lib/amd64/libprism_*.so /opt/jdk/jre/lib/amd64/libfxplugins.so /opt/jdk/jre/lib/amd64/libglass.so /opt/jdk/jre/lib/amd64/libgstreamer-lite.so /opt/jdk/jre/lib/amd64/libjavafx*.so /opt/jdk/jre/lib/amd64/libjfx*.so /opt/jdk/jre/lib/ext/jfxrt.jar /opt/jdk/jre/lib/ext/nashorn.jar /opt/jdk/jre/lib/oblique-fonts /opt/jdk/jre/lib/plugin.jar /tmp/* /var/cache/apk/*
ENV JAVA_HOME="/opt/jdk"
ENV PATH="${PATH}:${JAVA_HOME}/bin:${JAVA_HOME}/sbin"
#   Hadoop
ENV HADOOP_VERSION="2.7.3"
RUN curl http://ftp.unicamp.br/pub/apache/hadoop/common/hadoop-${HADOOP_VERSION}/hadoop-${HADOOP_VERSION}.tar.gz | tar -xzf - -C /usr/local/ \
 && rm -fR /usr/local/hadoop-${HADOOP_VERSION}/share/doc /usr/local/hadoop-${HADOOP_VERSION}/share/hadoop/common/jdiff \
 && ln -s /usr/local/hadoop-${HADOOP_VERSION}/ /opt/hadoop
ENV HADOOP_HOME="/opt/hadoop"
ENV HADOOP_INSTALL="$HADOOP_HOME"
ENV HADOOP_COMMON_HOME="$HADOOP_HOME"
ENV HADOOP_MAPRED_HOME="$HADOOP_HOME"
ENV HADOOP_HDFS_HOME="$HADOOP_HOME"
ENV YARN_HOME="$HADOOP_HOME"
ENV PATH="$PATH:$HADOOP_HOME/bin:$HADOOP_HOME/sbin"
#   Spark
ENV SPARK_VERSION="2.0.2"
RUN curl http://d3kbcqa49mib13.cloudfront.net/spark-${SPARK_VERSION}-bin-hadoop2.7.tgz | tar -xzf - -C /usr/local/ \
 && ln -s /usr/local/spark-${SPARK_VERSION}-bin-hadoop2.7/ /opt/spark
ENV SPARK_HOME="/opt/spark"
ENV PATH="$PATH:$SPARK_HOME/bin"
#   HBase
ENV HBASE_VERSION="1.2.4"
RUN curl http://ftp.unicamp.br/pub/apache/hbase/${HBASE_VERSION}/hbase-${HBASE_VERSION}-bin.tar.gz | tar -xzf - -C /usr/local/ \
 && rm -fR /usr/local/hbase-${HBASE_VERSION}/docs \
 && ln -s /usr/local/hbase-${HBASE_VERSION}/ /opt/hbase
ENV HBASE_HOME="/opt/hbase"
ENV PATH="$PATH:$HBASE_HOME/bin"
#   Configurations Fully Distributed
COPY start.sh /etc/start.sh
RUN chmod +x /etc/start.sh \
 && mkdir -p /tmp/hdfs/datanode
#   Hdfs ports
EXPOSE 50010/tcp 50020/tcp 50070/tcp 50075/tcp 50090/tcp 8020/tcp 9000/tcp
#   Mapred ports
EXPOSE 10020/tcp 19888/tcp
#   Yarn ports
EXPOSE 8030/tcp 8031/tcp 8032/tcp 8033/tcp 8040/tcp 8042/tcp 8088/tcp
#  Other ports
EXPOSE 49707/tcp 22/tcp 2122/tcp
ENTRYPOINT ["/etc/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
