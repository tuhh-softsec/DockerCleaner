#   Licensed to the Apache Software Foundation (ASF) under one or more
#   contributor license agreements.  See the NOTICE file distributed with
#   this work for additional information regarding copyright ownership.
#   The ASF licenses this file to You under the Apache License, Version 2.0
#   (the "License"); you may not use this file except in compliance with
#   the License.  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM centos:centos6
ENV SPARK_PROFILE="2.1"
ENV SPARK_VERSION="2.1.2"
ENV HADOOP_PROFILE="2.7"
ENV HADOOP_VERSION="2.7.0"
#   Update the image with the latest packages
RUN yum update -y ; yum clean all
#   Get utils
RUN yum install -y wget tar curl \
 && yum clean all
#   Remove old jdk
RUN yum remove java ; yum remove jdk
#   install jdk7
RUN yum install -y java-1.7.0-openjdk-devel
ENV JAVA_HOME="/usr/lib/jvm/java"
ENV PATH="$PATH:$JAVA_HOME/bin"
#   install hadoop 
RUN yum install -y curl which tar sudo openssh-server openssh-clients rsync
#   hadoop
RUN curl -s https://archive.apache.org/dist/hadoop/core/hadoop-$HADOOP_VERSION/hadoop-$HADOOP_VERSION.tar.gz | tar -xz -C /usr/local/
RUN cd /usr/local \
 && ln -s ./hadoop-$HADOOP_VERSION hadoop
ENV HADOOP_PREFIX="/usr/local/hadoop"
ENV HADOOP_COMMON_HOME="/usr/local/hadoop"
ENV HADOOP_HDFS_HOME="/usr/local/hadoop"
ENV HADOOP_MAPRED_HOME="/usr/local/hadoop"
ENV HADOOP_YARN_HOME="/usr/local/hadoop"
ENV HADOOP_CONF_DIR="/usr/local/hadoop/etc/hadoop"
RUN sed -i '/^export JAVA_HOME/ s:.*:export JAVA_HOME=/usr/lib/jvm/jre-1.7.0-openjdk.x86_64\nexport HADOOP_PREFIX=/usr/local/hadoop\nexport HADOOP_HOME=/usr/local/hadoop\n:' $HADOOP_PREFIX/etc/hadoop/hadoop-env.sh
RUN sed -i '/^export HADOOP_CONF_DIR/ s:.*:export HADOOP_CONF_DIR=/usr/local/hadoop/etc/hadoop/:' $HADOOP_PREFIX/etc/hadoop/hadoop-env.sh
RUN mkdir $HADOOP_PREFIX/input
RUN cp $HADOOP_PREFIX/etc/hadoop/*.xml $HADOOP_PREFIX/input
#   hadoop configurations
COPY hdfs_conf/core-site.xml $HADOOP_PREFIX/etc/hadoop/core-site.xml
COPY hdfs_conf/hdfs-site.xml $HADOOP_PREFIX/etc/hadoop/hdfs-site.xml
COPY hdfs_conf/mapred-site.xml $HADOOP_PREFIX/etc/hadoop/mapred-site.xml
COPY hdfs_conf/yarn-site.xml $HADOOP_PREFIX/etc/hadoop/yarn-site.xml
RUN mkdir /data/
RUN chmod 777 /data/
RUN $HADOOP_PREFIX/bin/hdfs namenode -format
RUN rm /usr/local/hadoop/lib/native/*
RUN curl -Ls http://dl.bintray.com/sequenceiq/sequenceiq-bin/hadoop-native-64-$HADOOP_VERSION.tar | tar -x -C /usr/local/hadoop/lib/native/
#   install spark
RUN curl -s http://archive.apache.org/dist/spark/spark-$SPARK_VERSION/spark-$SPARK_VERSION-bin-hadoop$HADOOP_PROFILE.tgz | tar -xz -C /usr/local/
RUN cd /usr/local \
 && ln -s spark-$SPARK_VERSION-bin-hadoop$HADOOP_PROFILE spark
ENV SPARK_HOME="/usr/local/spark"
ENV YARN_CONF_DIR="$HADOOP_PREFIX/etc/hadoop"
ENV PATH="$PATH:$SPARK_HOME/bin:$HADOOP_PREFIX/bin"
#   passwordless ssh
RUN ssh-keygen -q -N "" -t dsa -f /etc/ssh/ssh_host_dsa_key
RUN ssh-keygen -q -N "" -t rsa -f /etc/ssh/ssh_host_rsa_key
RUN ssh-keygen -q -N "" -t rsa -f /root/.ssh/id_rsa
RUN cp /root/.ssh/id_rsa.pub /root/.ssh/authorized_keys
COPY ssh_config /root/.ssh/config
RUN chmod 600 /root/.ssh/config
RUN chown root:root /root/.ssh/config
RUN chmod +x /usr/local/hadoop/etc/hadoop/*-env.sh
#   update boot script
COPY entrypoint.sh /etc/entrypoint.sh
RUN chown root.root /etc/entrypoint.sh
RUN chmod 700 /etc/entrypoint.sh
#   Hdfs ports
EXPOSE 50010/tcp 50020/tcp 50070/tcp 50075/tcp 50090/tcp
#   Mapred ports
EXPOSE 9000/tcp 9001/tcp
#  Yarn ports
EXPOSE 8030/tcp 8031/tcp 8032/tcp 8033/tcp 8040/tcp 8042/tcp 8088/tcp
#  spark
EXPOSE 8080/tcp 7077/tcp 8888/tcp 8081/tcp
ENTRYPOINT ["/etc/entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
