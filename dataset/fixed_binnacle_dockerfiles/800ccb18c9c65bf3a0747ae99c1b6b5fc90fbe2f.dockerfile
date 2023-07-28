FROM ubuntu:latest
#  FROM openjdk:8-jdk
MAINTAINER Furcy Pin
ENV DEBIAN_FRONTEND="noninteractive"
#   update
RUN :
RUN yes | apt upgrade
RUN yes | (apt-get update ;apt-get install --no-install-recommends apt-utils=2.6.0 )
RUN yes | (apt-get update ;apt-get install --no-install-recommends net-tools=2.10-0.1ubuntu3 )
RUN yes | (apt-get update ;apt-get install --no-install-recommends gawk=1:5.2.1-2 )
#   Install wget
RUN yes | (apt-get update ;apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 )
#   Install curl
RUN yes | (apt-get update ;apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 )
#   Install java
RUN yes | (apt-get update ;apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu2 )
#   Install sbt
ENV SBT_VERSION="0.13.15"
RUN wget http://dl.bintray.com/sbt/debian/sbt-${SBT_VERSION}.deb -O /tmp/sbt.deb \
 && dpkg -i /tmp/sbt.deb \
 && rm -f /tmp/sbt.deb
#   Install Hadoop
ENV HADOOP_VERSION="2.7.3"
ENV HADOOP_HOME="/opt/hadoop-$HADOOP_VERSION"
ENV HADOOP_CONF_DIR="$HADOOP_HOME/conf"
ENV PATH="$PATH:$HADOOP_HOME/bin"
RUN curl -sL "https://archive.apache.org/dist/hadoop/common/hadoop-$HADOOP_VERSION/hadoop-$HADOOP_VERSION.tar.gz" | gunzip | tar -x -C /opt/ \
 && rm -rf $HADOOP_HOME/share/doc \
 && chown -R root:root $HADOOP_HOME \
 && mkdir -p $HADOOP_HOME/logs \
 && mkdir -p $HADOOP_CONF_DIR \
 && chmod 777 $HADOOP_CONF_DIR \
 && chmod 777 $HADOOP_HOME/logs
#   Install Hive
ENV HIVE_VERSION="2.0.1"
ENV HIVE_HOME="/opt/apache-hive-$HIVE_VERSION-bin"
ENV HIVE_CONF_DIR="$HIVE_HOME/conf"
ENV PATH="$PATH:$HIVE_HOME/bin"
RUN curl -sL "https://archive.apache.org/dist/hive/hive-$HIVE_VERSION/apache-hive-$HIVE_VERSION-bin.tar.gz" | gunzip | tar -x -C /opt/ \
 && chown -R root:root $HIVE_HOME \
 && mkdir -p $HIVE_HOME/hcatalog/var/log \
 && mkdir -p $HIVE_HOME/var/log \
 && mkdir -p /data/hive/ \
 && mkdir -p $HIVE_CONF_DIR \
 && chmod 777 $HIVE_HOME/hcatalog/var/log \
 && chmod 777 $HIVE_HOME/var/log
RUN ln -s $HADOOP_HOME/share/hadoop/tools/lib/aws-java-sdk-1.7.4.jar $HIVE_HOME/lib/.
RUN ln -s $HADOOP_HOME/share/hadoop/tools/lib/hadoop-aws-2.7.3.jar $HIVE_HOME/lib/.
#   Install Spark
ENV SPARK_VERSION="2.2.0"
ENV SPARK_HOME="/opt/spark-$SPARK_VERSION-bin-hadoop2.7"
ENV SPARK_CONF_DIR="$SPARK_HOME/conf"
ENV PATH="$PATH:$SPARK_HOME/bin"
RUN curl -sL "https://archive.apache.org/dist/spark/spark-$SPARK_VERSION/spark-$SPARK_VERSION-bin-hadoop2.7.tgz" | gunzip | tar -x -C /opt/ \
 && chown -R root:root $SPARK_HOME \
 && mkdir -p /data/spark/ \
 && mkdir -p $SPARK_HOME/logs \
 && mkdir -p $SPARK_CONF_DIR \
 && chmod 777 $SPARK_HOME/logs
#   Install Readline Wrapper
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends rlwrap=0.46.1-1 -y ) \
 && rm -rf /var/lib/apt/lists/*
RUN ln -s $HADOOP_HOME/share/hadoop/tools/lib/aws-java-sdk-1.7.4.jar $SPARK_HOME/jars/.
RUN ln -s $HADOOP_HOME/share/hadoop/tools/lib/hadoop-aws-2.7.3.jar $SPARK_HOME/jars/.
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/jre"
#   Configure
COPY files/hive-site.xml $HIVE_CONF_DIR/
COPY files/hive-site.xml $SPARK_CONF_DIR/
COPY files/start.sh /
COPY files/init.sh /
COPY files/beeline.sh /
EXPOSE 22/tcp
EXPOSE 4040/tcp
EXPOSE 9083/tcp
EXPOSE 10000/tcp
#  RUN /bin/bash /beeline.sh -e "add jar file:///quetzal/target/quetzal-RDF-0.0.1-SNAPSHOT.jar;"
#  ENTRYPOINT ["/beeline.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
