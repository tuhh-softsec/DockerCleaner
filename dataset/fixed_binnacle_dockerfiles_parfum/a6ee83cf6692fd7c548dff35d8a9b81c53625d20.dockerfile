#
#  This image is modified version of sequenceiq/hadoop-docker
#    * sequenceiq/hadoop-docker <https://github.com/sequenceiq/hadoop-docker>
#
#  The modifications are
#    * Use local hadoop package
#    * Change template files to indicate docker master node
#    * Modify bootstrap script
#
#  Author: Kai Sasaki
#  Date:   2015 Sep,13
#
#  Creates multi node hadoop cluster on Docker
FROM sequenceiq/pam:ubuntu-14.04
MAINTAINER lewuathe
USER root
#  install dev tools
RUN apt-get update && apt-get install --no-install-recommends curl tar sudo openssh-server openssh-client rsync -y
#  passwordless ssh
RUN rm -f /etc/ssh/ssh_host_dsa_key /etc/ssh/ssh_host_rsa_key /root/.ssh/id_rsa
RUN ssh-keygen -q -N "" -t dsa -f /etc/ssh/ssh_host_dsa_key
RUN ssh-keygen -q -N "" -t rsa -f /etc/ssh/ssh_host_rsa_key
RUN ssh-keygen -q -N "" -t rsa -f /root/.ssh/id_rsa
RUN cp /root/.ssh/id_rsa.pub /root/.ssh/authorized_keys
#  java
RUN mkdir -p /usr/java/default \
 && curl -Ls 'http://download.oracle.com/otn-pub/java/jdk/8u131-b11/d54c1d3a095b4ff2b6607d096fa80163/jdk-8u131-linux-x64.tar.gz' -H 'Cookie: oraclelicense=accept-securebackup-cookie' | tar --strip-components=1 -xz -C /usr/java/default/
#  ADD jdk-8u112-linux-x64.tar.gz /usr/java
#  RUN sudo ln -s /usr/java/jdk1.8.0_112/ /usr/java/default
ENV JAVA_HOME="/usr/java/default"
ENV PATH="$PATH:$JAVA_HOME/bin"
#  download native support
RUN mkdir -p /tmp/native
RUN curl -Ls http://dl.bintray.com/sequenceiq/sequenceiq-bin/hadoop-native-64-2.7.0.tar | tar -x -C /tmp/native
ENV HADOOP_VERSION="3.2.0-SNAPSHOT"
ADD hadoop-${HADOOP_VERSION}.tar.gz /usr/local/
WORKDIR /usr/local
RUN ln -s /usr/local/hadoop-${HADOOP_VERSION} /usr/local/hadoop
ENV HADOOP_HOME="/usr/local/hadoop"
ENV HADOOP_COMMON_HOME="/usr/local/hadoop"
ENV HADOOP_HDFS_HOME="/usr/local/hadoop"
ENV HADOOP_MAPRED_HOME="/usr/local/hadoop"
ENV HADOOP_YARN_HOME="/usr/local/hadoop"
ENV HADOOP_CONF_DIR="/usr/local/hadoop/etc/hadoop"
ENV YARN_CONF_DIR="/usr/local/hadoop/etc/hadoop"
ENV HADOOP_LOG_DIR="/var/log/hadoop"
RUN mkdir /var/log/hadoop
RUN sed -i '/^export JAVA_HOME/ s:.*:export JAVA_HOME=/usr/java/default\nexport H=/usr/local/hadoop\nexport HADOOP_HOME=/usr/local/hadoop\n:' $HADOOP_HOME/etc/hadoop/hadoop-env.sh
RUN sed -i '/^export HADOOP_CONF_DIR/ s:.*:export HADOOP_CONF_DIR=/usr/local/hadoop/etc/hadoop/:' $HADOOP_HOME/etc/hadoop/hadoop-env.sh
# RUN . $HADOOP_HOME/etc/hadoop/hadoop-env.sh
RUN mkdir $HADOOP_HOME/input
RUN cp $HADOOP_HOME/etc/hadoop/*.xml $HADOOP_HOME/input
COPY core-site.xml $HADOOP_HOME/etc/hadoop/core-site.xml
COPY hdfs-site.xml $HADOOP_HOME/etc/hadoop/hdfs-site.xml
COPY mapred-site.xml $HADOOP_HOME/etc/hadoop/mapred-site.xml
COPY yarn-site.xml $HADOOP_HOME/etc/hadoop/yarn-site.xml
COPY log4j.properties $HADOOP_HOME/etc/hadoop/log4j.properties
RUN $HADOOP_HOME/bin/hdfs namenode -format
#  fixing the libhadoop.so like a boss
RUN rm -rf /usr/local/hadoop/lib/native
RUN mv /tmp/native /usr/local/hadoop/lib
COPY ssh_config /root/.ssh/config
RUN chmod 600 /root/.ssh/config
RUN chown root:root /root/.ssh/config
#  workingaround docker.io build error
RUN ls -la /usr/local/hadoop/etc/hadoop/*-env.sh
RUN chmod +x /usr/local/hadoop/etc/hadoop/*-env.sh
RUN ls -la /usr/local/hadoop/etc/hadoop/*-env.sh
#  fix the 254 error code
RUN sed -i "/^[^#]*UsePAM/ s/.*/#&/" /etc/ssh/sshd_config
RUN echo "UsePAM no" >> /etc/ssh/sshd_config
RUN echo "Port 2122" >> /etc/ssh/sshd_config
RUN service ssh start
#  Hdfs ports
EXPOSE 9000/tcp 50010/tcp 50020/tcp 50070/tcp 50075/tcp 50090/tcp
#  See https://issues.apache.org/jira/browse/HDFS-9427
EXPOSE 9871/tcp 9870/tcp 9820/tcp 9869/tcp 9868/tcp 9867/tcp 9866/tcp 9865/tcp 9864/tcp
#  Mapred ports
EXPOSE 19888/tcp
# Yarn ports
EXPOSE 8030/tcp 8031/tcp 8032/tcp 8033/tcp 8040/tcp 8042/tcp 8088/tcp 8188/tcp
# Other ports
EXPOSE 49707/tcp 2122/tcp
