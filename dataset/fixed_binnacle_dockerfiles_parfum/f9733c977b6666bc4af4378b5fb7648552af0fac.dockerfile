FROM centos:centos6
#  Add yum repositories
COPY config/yum-repos/elasticsearch.repo /etc/yum.repos.d/elasticsearch.repo
#  Install RPM packages
COPY scripts/install-rpm-packages.sh /opt/lumify/scripts/install-rpm-packages.sh
RUN /bin/bash /opt/lumify/scripts/install-rpm-packages.sh
#  Install NPM packages
COPY scripts/install-npm-packages.sh /opt/lumify/scripts/install-npm-packages.sh
RUN /bin/bash /opt/lumify/scripts/install-npm-packages.sh
#  Passwordless SSH
COPY scripts/install-ssh.sh /opt/lumify/scripts/install-ssh.sh
COPY config/ssh_config /root/.ssh/config
# RUN /bin/bash /opt/lumify/scripts/install-ssh.sh
COPY demo/keys/ssh_host_dsa_key /etc/ssh/ssh_host_dsa_key
COPY demo/keys/ssh_host_dsa_key.pub /etc/ssh/ssh_host_dsa_key.pub
COPY demo/keys/ssh_host_rsa_key /etc/ssh/ssh_host_rsa_key
COPY demo/keys/ssh_host_rsa_key.pub /etc/ssh/ssh_host_rsa_key.pub
COPY demo/keys/id_rsa /root/.ssh/id_rsa
COPY demo/keys/id_rsa.pub /root/.ssh/id_rsa.pub
COPY demo/keys/id_rsa.pub /root/.ssh/authorized_keys
RUN chmod 600 /root/.ssh/config \
 && chown root:root /root/.ssh/config
EXPOSE 22/tcp
#  Install Java
COPY scripts/install-java.sh /opt/lumify/scripts/install-java.sh
COPY config/java/java.sh /etc/profile.d/java.sh
ENV PATH="$PATH:/opt/jdk/bin"
ENV JAVA_HOME="/opt/jdk"
ENV _JAVA_OPTIONS="-Djava.net.preferIPv4Stack=true"
RUN /bin/bash /opt/lumify/scripts/install-java.sh
#  Install Maven
COPY scripts/install-maven.sh /opt/lumify/scripts/install-maven.sh
ENV PATH="$PATH:/opt/maven/bin"
ENV MVN_HOME="/opt/maven"
RUN /bin/bash /opt/lumify/scripts/install-maven.sh
#  Install ZooKeeper
COPY scripts/install-zookeeper.sh /opt/lumify/scripts/install-zookeeper.sh
ENV PATH="$PATH:/opt/zookeeper/bin"
ENV ZOOKEEPER_HOME="/opt/zookeeper"
RUN /bin/bash /opt/lumify/scripts/install-zookeeper.sh
EXPOSE 2181/tcp 2888/tcp 3888/tcp
#  Install Hadoop
COPY scripts/install-hadoop.sh /opt/lumify/scripts/install-hadoop.sh
COPY config/hadoop/core-site.xml.template /opt/hadoop-2.3.0/etc/hadoop/core-site.xml.template
COPY config/hadoop/hadoop-native-64bit.tar.gz /opt/hadoop-2.3.0/hadoop-native-64bit.tar.gz
ENV HADOOP_PREFIX="/opt/hadoop"
ENV HADOOP_COMMON_HOME="/opt/hadoop"
ENV HADOOP_HDFS_HOME="/opt/hadoop"
ENV HADOOP_MAPRED_HOME="/opt/hadoop"
ENV HADOOP_YARN_HOME="/opt/hadoop"
ENV HADOOP_CONF_DIR="/opt/hadoop/etc/hadoop"
ENV YARN_CONF_DIR="/opt/hadoop/etc/hadoop"
ENV PATH="$PATH:/opt/hadoop/bin"
RUN /bin/bash /opt/lumify/scripts/install-hadoop.sh
COPY config/hadoop/hdfs-site.xml /opt/hadoop-2.3.0/etc/hadoop/hdfs-site.xml
COPY config/hadoop/mapred-site.xml /opt/hadoop-2.3.0/etc/hadoop/mapred-site.xml
COPY config/hadoop/yarn-site.xml /opt/hadoop-2.3.0/etc/hadoop/yarn-site.xml
VOLUME ["/var/lib/hadoop-hdfs", "/var/local/hadoop"]
EXPOSE 8020/tcp 8032/tcp 8088/tcp 9000/tcp 50010/tcp 50020/tcp 50030/tcp 50060/tcp 50070/tcp 50075/tcp 50090/tcp
#  Install Accumulo
COPY scripts/install-accumulo.sh /opt/lumify/scripts/install-accumulo.sh
ENV PATH="$PATH:/opt/accumulo/bin"
RUN /bin/bash /opt/lumify/scripts/install-accumulo.sh
EXPOSE 9997/tcp 9999/tcp 50091/tcp 50095/tcp
#  Install ElasticSearch
COPY scripts/install-elasticsearch.sh /opt/lumify/scripts/install-elasticsearch.sh
ENV PATH="$PATH:/opt/elasticsearch/bin"
RUN /bin/bash /opt/lumify/scripts/install-elasticsearch.sh
COPY config/elasticsearch/elasticsearch.yml /opt/elasticsearch/config/elasticsearch.yml
VOLUME ["/opt/elasticsearch-1.4.4/data/"]
EXPOSE 9200/tcp 9300/tcp
#  Install RabbitMQ
COPY scripts/install-rabbitmq.sh /opt/lumify/scripts/install-rabbitmq.sh
ENV PATH="$PATH:/opt/rabbitmq/sbin"
RUN /bin/bash /opt/lumify/scripts/install-rabbitmq.sh
COPY config/rabbitmq/etc/rabbitmq/rabbitmq.config /opt/rabbitmq_server-3.4.1/etc/rabbitmq/rabbitmq.config
VOLUME ["/opt/rabbitmq_server-3.4.1/var"]
EXPOSE 5672/tcp 5673/tcp 15672/tcp
#  Install Jetty
COPY scripts/install-jetty.sh /opt/lumify/scripts/install-jetty.sh
RUN /bin/bash /opt/lumify/scripts/install-jetty.sh
ADD config/jetty/start.ini /opt/jetty/start.ini
COPY config/jetty/jetty-logging.properties /opt/jetty/resources/jetty-logging.properties
COPY config/jetty/jetty.xml /opt/jetty/etc/jetty.xml
COPY config/jetty/jetty-http.xml /opt/jetty/etc/jetty-http.xml
COPY config/jetty/jetty-https.xml /opt/jetty/etc/jetty-https.xml
COPY config/jetty/jetty-ssl.xml /opt/jetty/etc/jetty-ssl.xml
COPY config/jetty/jetty.jks /opt/jetty/etc/jetty.jks
ENV PATH="$PATH:/opt/jetty/bin"
VOLUME ["/opt/jetty/webapps"]
EXPOSE 8080/tcp 8443/tcp
#  Install FFmpeg
COPY scripts/install-ffmpeg.sh /opt/lumify/scripts/install-ffmpeg.sh
RUN /bin/bash /opt/lumify/scripts/install-ffmpeg.sh
#  Install Tesseract-ocr
COPY scripts/install-tesseract-ocr.sh /opt/lumify/scripts/install-tesseract-ocr.sh
RUN /bin/bash /opt/lumify/scripts/install-tesseract-ocr.sh
#  Install CCExtractor
COPY scripts/install-ccextractor.sh /opt/lumify/scripts/install-ccextractor.sh
RUN /bin/bash /opt/lumify/scripts/install-ccextractor.sh
#  Install OpenCV
COPY scripts/install-opencv.sh /opt/lumify/scripts/install-opencv.sh
RUN /bin/bash /opt/lumify/scripts/install-opencv.sh
#  Install CMU Sphinx
COPY scripts/install-sphinx.sh /opt/lumify/scripts/install-sphinx.sh
RUN /bin/bash /opt/lumify/scripts/install-sphinx.sh
#  Install Clavin
COPY scripts/install-clavin.sh /opt/lumify/scripts/install-clavin.sh
RUN /bin/bash /opt/lumify/scripts/install-clavin.sh
#  Install Lumify Web App
COPY demo/.tmp/root.war /opt/jetty/webapps/root.war
COPY demo/.tmp/log4j.xml /opt/lumify/config/log4j.xml
COPY demo/.tmp/ontology-minimal /opt/lumify/ontology
COPY demo/lumify.properties /opt/lumify/config/lumify.properties
COPY demo/.tmp/knownEntities /opt/lumify/config/knownEntities
COPY demo/.tmp/opencv /opt/lumify/config/opencv
COPY demo/.tmp/opennlp /opt/lumify/config/opennlp
#  add scripts
COPY scripts/docker-entrypoint.sh /opt/docker-entrypoint.sh
ADD demo/start.sh /opt/start.sh
COPY demo/stop.sh /opt/stop.sh
COPY demo/status.sh /opt/status.sh
RUN chmod +x /opt/docker-entrypoint.sh \
 && chmod +x /opt/start.sh \
 && chmod +x /opt/stop.sh \
 && chmod +x /opt/status.sh
#  Set working environment
VOLUME ["/tmp", "/var/log", "/opt/lumify", "/opt/lumify-source"]
WORKDIR /home/root/lumify
ENTRYPOINT ["/opt/docker-entrypoint.sh"]
CMD ["start"]
