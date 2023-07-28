#   Ubuntu 16.04 (Xenial)
FROM ubuntu:16.04
#   set AZTK version compatibility
ENV AZTK_DOCKER_IMAGE_VERSION="0.1.0"
#   set version of python required for aztk
ENV AZTK_PYTHON_VERSION="3.5.2"
#   modify these ARGs on build time to specify your desired versions of Spark/Hadoop
ENV SPARK_VERSION_KEY="1.6.3"
ENV SPARK_FULL_VERSION="spark-${SPARK_VERSION_KEY}-bin-without-hadoop"
ENV HADOOP_VERSION="2.8.3"
ENV LANG="C.UTF-8" \
    LC_ALL="C.UTF-8"
#   set env vars
ENV JAVA_HOME="/usr/lib/jvm/java-1.8.0-openjdk-amd64"
ENV SPARK_HOME="/home/spark-current"
ENV PATH="$SPARK_HOME/bin:$PATH"
RUN apt-get clean \
 && apt-get update -y \
 && apt-get install --no-install-recommends make=4.1-6 build-essential=12.1ubuntu2 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libssl-dev=1.0.2g-1ubuntu4.20 libbz2-dev=1.0.6-8ubuntu0.2 libreadline-dev=6.3-8ubuntu2 libsqlite3-dev=3.11.0-1ubuntu1.5 maven=3.3.9-3 wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 llvm=1:3.8-33ubuntu3.1 git=1:2.7.4-0ubuntu1.10 libncurses5-dev=6.0+20160213-1ubuntu1 libncursesw5-dev=6.0+20160213-1ubuntu1 python3-pip=8.1.1-2ubuntu0.6 python3-venv=3.5.1-3 xz-utils=5.1.1alpha+20120614-2ubuntu2 tk-dev=8.6.0+9 -y \
 && apt-get update -y \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y \
 && apt-add-repository ppa:webupd8team/java -y \
 && apt-get update -y \
 && apt-get install --no-install-recommends default-jdk=2:1.8-56ubuntu2 -y \
 && ln -s /usr/bin/python3.5 /usr/bin/python \
 && /usr/bin/python -m pip install --upgrade pip setuptools wheel \
 && apt-get remove -y python3-pip \
 && git clone https://github.com/apache/spark.git \
 && cd spark \
 && git checkout tags/v${SPARK_VERSION_KEY} \
 && export MAVEN_OPTS="-Xmx3g -XX:ReservedCodeCacheSize=1024m" \
 && ./make-distribution.sh --name custom-spark --tgz -Pnetlib-lgpl -Phive -Phive-thriftserver -Dhadoop.version=${HADOOP_VERSION} -Phadoop-2.6 -DskipTests \
 && tar -xvzf /spark/spark-${SPARK_VERSION_KEY}-bin-custom-spark.tgz --directory=/home \
 && ln -s "/home/spark-${SPARK_VERSION_KEY}-bin-custom-spark" /home/spark-current \
 && rm -rf /spark \
 && echo "<project>" "<modelVersion>4.0.0</modelVersion>" "<groupId>groupId</groupId>" "<artifactId>artifactId</artifactId>" "<version>1.0</version>" "<dependencies>" "<dependency>" "<groupId>org.apache.hadoop</groupId>" "<artifactId>hadoop-azure-datalake</artifactId>" "<version>${HADOOP_VERSION}</version>" "<exclusions>" "<exclusion>" "<groupId>org.apache.hadoop</groupId>" "<artifactId>hadoop-common</artifactId>" "</exclusion>" "</exclusions> " "</dependency>" "<dependency>" "<groupId>org.apache.hadoop</groupId>" "<artifactId>hadoop-azure</artifactId>" "<version>${HADOOP_VERSION}</version>" "<exclusions>" "<exclusion>" "<groupId>org.apache.hadoop</groupId>" "<artifactId>hadoop-common</artifactId>" "</exclusion>" "<exclusion>" "<groupId>com.fasterxml.jackson.core</groupId>" "<artifactId>jackson-core</artifactId>" "</exclusion>" "</exclusions> " "</dependency>" "<dependency>" "<groupId>com.microsoft.sqlserver</groupId>" "<artifactId>mssql-jdbc</artifactId>" "<version>6.4.0.jre8</version>" "</dependency>" "<dependency>" "<groupId>com.microsoft.azure</groupId>" "<artifactId>azure-storage</artifactId>" "<version>2.2.0</version>" "<exclusions>" "<exclusion>" "<groupId>com.fasterxml.jackson.core</groupId>" "<artifactId>jackson-core</artifactId>" "</exclusion>" "<exclusion>" "<groupId>org.apache.commons</groupId>" "<artifactId>commons-lang3</artifactId>" "</exclusion>" "<exclusion>" "<groupId>org.slf4j</groupId>" "<artifactId>slf4j-api</artifactId>" "</exclusion>" "</exclusions>" "</dependency>" "<dependency>" "<groupId>com.microsoft.azure</groupId>" "<artifactId>azure-cosmosdb-spark_2.1.0_2.11</artifactId>" "<version>1.1.1</version>" "<exclusions>" "<exclusion>" "<groupId>org.apache.tinkerpop</groupId>" "<artifactId>tinkergraph-gremlin</artifactId>" "</exclusion>" "<exclusion>" "<groupId>org.apache.tinkerpop</groupId>" "<artifactId>spark-gremlin</artifactId>" "</exclusion>" "<exclusion>" "<groupId>io.netty</groupId>" "<artifactId>*</artifactId>" "</exclusion>" "<exclusion>" "<groupId>com.fasterxml.jackson.core</groupId>" "<artifactId>jackson-annotations</artifactId>" "</exclusion>" "</exclusions> " "</dependency>" "</dependencies>" "</project>" > /tmp/pom.xml \
 && cd /tmp \
 && mvn dependency:copy-dependencies -DoutputDirectory="${SPARK_HOME}/jars/" \
 && apt-get --purge autoremove -y maven python3-pip \
 && apt-get autoremove -y \
 && apt-get autoclean -y \
 && rm -rf /tmp/* \
 && rm -rf /root/.cache \
 && rm -rf /root/.m2 \
 && rm -rf /var/lib/apt/lists/*
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
