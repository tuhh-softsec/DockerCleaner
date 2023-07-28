FROM ubuntu:14.04
ENV SCALA_VERSION="2.10.4"
ENV SPARK_VERSION="1.4.1"
EXPOSE 80/tcp 4042/tcp 9160/tcp 9042/tcp 9200/tcp 7077/tcp 38080/tcp 38081/tcp 6060/tcp 6061/tcp 8090/tcp 8099/tcp 10000/tcp 50070/tcp 50090/tcp 9092/tcp 6066/tcp 9000/tcp 19999/tcp 6379/tcp 6081/tcp 7474/tcp 8787/tcp 5601/tcp 8989/tcp 7979/tcp 4040/tcp
RUN apt-get update \
 && apt-get install curl -y \
 && apt-get install wget -y \
 && apt-get install vim -y \
 && cd ~ \
 && apt-get install git -y \
 && apt-get install openssh-server -y \
 && apt-get install default-jdk -y \
 && wget https://s3.amazonaws.com/fluxcapacitor.com/packages/sbt-0.13.8.tgz \
 && tar xvzf sbt-0.13.8.tgz \
 && rm sbt-0.13.8.tgz \
 && ln -s /root/sbt/bin/sbt /usr/local/bin \
 && cd ~ \
 && git clone https://github.com/bythebay/pipeline.git \
 && sbt clean clean-files
RUN cd ~ \
 && DEBIAN_FRONTEND=noninteractive apt-get -y install mysql-server \
 && apt-get install mysql-client -y \
 && apt-get install libmysql-java -y \
 && wget http://archive.apache.org/dist/cassandra/2.2.0/apache-cassandra-2.2.0-bin.tar.gz \
 && tar xvzf apache-cassandra-2.2.0-bin.tar.gz \
 && rm apache-cassandra-2.2.0-bin.tar.gz \
 && wget http://packages.confluent.io/archive/1.0/confluent-1.0-2.10.4.tar.gz \
 && tar xvzf confluent-1.0-2.10.4.tar.gz \
 && rm confluent-1.0-2.10.4.tar.gz \
 && wget https://s3.amazonaws.com/fluxcapacitor.com/packages/spark-1.4.1-bin-fluxcapacitor.tgz \
 && tar xvzf spark-1.4.1-bin-fluxcapacitor.tgz \
 && rm spark-1.4.1-bin-fluxcapacitor.tgz \
 && apt-get install screen -y \
 && wget https://s3.eu-central-1.amazonaws.com/spark-notebook/emr/spark-notebook-0.6.0-scala-2.10.4-spark-1.4.1-hadoop-2.6.0-with-hive-with-parquet.tgz \
 && tar xvzf spark-notebook-0.6.0-scala-2.10.4-spark-1.4.1-hadoop-2.6.0-with-hive-with-parquet.tgz \
 && rm spark-notebook-0.6.0-scala-2.10.4-spark-1.4.1-hadoop-2.6.0-with-hive-with-parquet.tgz \
 && wget https://s3.amazonaws.com/fluxcapacitor.com/packages/spark-jobserver-0.5.2-fluxcapacitor.tar.gz \
 && tar xvzf spark-jobserver-0.5.2-fluxcapacitor.tar.gz \
 && rm spark-jobserver-0.5.2-fluxcapacitor.tar.gz \
 && mkdir -p ~/pipeline/logs/spark-jobserver
RUN cd ~/pipeline \
 && git reset --hard \
 && git pull \
 && chmod a+rx *.sh \
 && ln ~/pipeline/config/spark-jobserver/pipeline.sh ~/spark-jobserver-0.5.2/config \
 && ln ~/pipeline/config/spark-jobserver/pipeline.conf ~/spark-jobserver-0.5.2/config \
 && cd ~/spark-jobserver-0.5.2 \
 && sbt job-server-tests/package \
 && bin/server_package.sh pipeline \
 && cp /tmp/job-server/* . \
 && rm -rf /tmp/job-server \
 && cd ~ \
 && mv ~/.profile ~/.profile.orig \
 && ln -s ~/pipeline/config/bash/.profile ~/.profile \
 && cd ~/pipeline \
 && sbt feeder/assembly \
 && cd ~/pipeline \
 && sbt streaming/package
WORKDIR /root
