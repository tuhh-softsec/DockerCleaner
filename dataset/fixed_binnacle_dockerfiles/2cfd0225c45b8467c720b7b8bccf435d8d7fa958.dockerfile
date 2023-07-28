FROM ubuntu:16.04
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 python3.5=3.5.2-2ubuntu0~16.04.13 python3-pip=8.1.1-2ubuntu0.6 git=1:2.7.4-0ubuntu1.10 vim=2:7.4.1689-3ubuntu1.5 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 net-tools=1.60-26ubuntu1 -y
#   Install Hadoop.
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"
ENV HADOOP_VERSION="3.2.0 "
ENV HADOOP_URL="https://archive.apache.org/dist/hadoop/common/stable/hadoop-$HADOOP_VERSION.tar.gz"
RUN set -x \
 && curl -fsSL "$HADOOP_URL" -o /tmp/hadoop.tar.gz \
 && tar -xzf /tmp/hadoop.tar.gz -C /opt/ \
 && rm /tmp/hadoop.tar.gz*
#   Configure Hadoop
RUN ln -s /opt/hadoop-$HADOOP_VERSION/etc/hadoop /etc/hadoop
RUN mkdir /opt/hadoop-$HADOOP_VERSION/logs
RUN mkdir /hadoop-data
ENV HADOOP_PREFIX="/opt/hadoop-$HADOOP_VERSION"
ENV HADOOP_CONF_DIR="/etc/hadoop"
ENV MULTIHOMED_NETWORK="1"
ENV PATH="$HADOOP_PREFIX/bin/:$PATH"
ENV HIVE_SITE_CONF_javax_jdo_option_ConnectionURL="jdbc:mysql://localhost:3306/hive?createDatabaseIfNotExist=true"
ENV HIVE_SITE_CONF_javax_jdo_option_ConnectionDriverName="com.mysql.jdbc.Driver"
ENV HIVE_SITE_CONF_javax_jdo_option_ConnectionUserName="root"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV HIVE_SITE_CONF_datanucleus_autoCreateSchema="false"
#  ENV HIVE_SITE_CONF_hive_metastore_uris=thrift://hive-metastore:9083
ENV HIVE_SITE_CONF_hive_server2_transport_mode="binary"
#  ENV HIVE_SITE_CONF_hive_server2_use_SSL=false
ENV HIVE_SITE_CONF_hive_server2_authentication="NOSASL"
ENV HIVE_SITE_CONF_hive_server2_enable_doAs="false"
ENV HIVE_SITE_CONF_hive_metastore_schema_verification="false"
ENV HIVE_SITE_CONF_datanucleus_schema_autoCreateTables="true"
ENV HDFS_CONF_dfs_namenode_datanode_registration_ip___hostname___check="false"
ENV CORE_CONF_fs_defaultFS="hdfs://localhost:8020"
ENV CORE_CONF_hadoop_http_staticuser_user="root"
ENV CORE_CONF_hadoop_proxyuser_hue_hosts="*"
ENV CORE_CONF_hadoop_proxyuser_hue_groups="*"
ENV CORE_CONF_hadoop.proxyuser.root.hosts="*"
ENV CORE_CONF_hadoop.proxyuser.root.groups="*"
ENV HDFS_CONF_dfs_webhdfs_enabled="true"
ENV HDFS_CONF_dfs_permissions_enabled="false"
ENV YARN_CONF_yarn_log___aggregation___enable="true"
ENV YARN_CONF_yarn_resourcemanager_recovery_enabled="true"
ENV YARN_CONF_yarn_resourcemanager_store_class="org.apache.hadoop.yarn.server.resourcemanager.recovery.FileSystemRMStateStore"
ENV YARN_CONF_yarn_resourcemanager_fs_state___store_uri="/rmstate"
ENV YARN_CONF_yarn_nodemanager_remote___app___log___dir="/app-logs"
ENV YARN_CONF_yarn_log_server_url="http://historyserver:8188/applicationhistory/logs/"
ENV YARN_CONF_yarn_timeline___service_enabled="true"
ENV YARN_CONF_yarn_timeline___service_generic___application___history_enabled="true"
ENV YARN_CONF_yarn_resourcemanager_system___metrics___publisher_enabled="true"
ENV YARN_CONF_yarn_resourcemanager_hostname="resourcemanager"
ENV YARN_CONF_yarn_timeline___service_hostname="historyserver"
ENV YARN_CONF_yarn_resourcemanager_address="resourcemanager:8032"
ENV YARN_CONF_yarn_resourcemanager_scheduler_address="resourcemanager:8030"
ENV YARN_CONF_yarn_resourcemanager_resource__tracker_address="resourcemanager:8031"
COPY entrypoint.sh /entrypoint.sh
RUN chmod a+x /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
RUN mkdir /cmd
#   For HDFS NameNode
HEALTHCHECK CMD curl -f http://localhost:50070/ || exit 1
ENV HDFS_CONF_dfs_namenode_name_dir="file:///hadoop/dfs/name"
RUN mkdir -p /hadoop/dfs/name
VOLUME /hadoop/dfs/name
COPY cmd/start_namenode.sh /cmd/start_namenode.sh
RUN chmod a+x /cmd/start_namenode.sh
#   For HDFS DataNode
HEALTHCHECK CMD curl -f http://localhost:50075/ || exit 1
ENV HDFS_CONF_dfs_datanode_data_dir="file:///hadoop/dfs/data"
RUN mkdir -p /hadoop/dfs/data
VOLUME /hadoop/dfs/data
COPY cmd/start_datanode.sh /cmd/start_datanode.sh
RUN chmod a+x /cmd/start_datanode.sh
#   For ResourceManager
HEALTHCHECK CMD curl -f http://localhost:8088/ || exit 1
COPY cmd/start_resourcemanager.sh /cmd/start_resourcemanager.sh
RUN chmod a+x /cmd/start_resourcemanager.sh
#   For NodeManager
HEALTHCHECK CMD curl -f http://localhost:8042/ || exit 1
COPY cmd/start_nodemanager.sh /cmd/start_nodemanager.sh
RUN chmod a+x /cmd/start_nodemanager.sh
#   For Hive
ARG HIVE_VERSION
ENV HIVE_VERSION="${HIVE_VERSION:-2.3.2}"
ENV HIVE_HOME="/opt/hive"
ENV PATH="$HIVE_HOME/bin:$PATH"
ENV HADOOP_HOME="/opt/hadoop-$HADOOP_VERSION"
RUN apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 procps=2:3.3.10-4ubuntu2.5 -y
WORKDIR /opt
RUN wget --quiet http://archive.apache.org/dist/hive/hive-$HIVE_VERSION/apache-hive-$HIVE_VERSION-bin.tar.gz
RUN tar -xzf apache-hive-$HIVE_VERSION-bin.tar.gz \
 && mv apache-hive-$HIVE_VERSION-bin hive \
 && rm apache-hive-$HIVE_VERSION-bin.tar.gz \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
COPY conf/ $HIVE_HOME/conf
COPY cmd/start_hiveserver2.sh /cmd/start_hiveserver2.sh
RUN chmod +x /cmd/start_hiveserver2.sh
#   Start all hadoop components
COPY cmd/start_all.sh /cmd/start_all.sh
RUN chmod +x /cmd/start_all.sh
#   install mysql without a password prompt
RUN ["/bin/bash", "-c", "debconf-set-selections", "<<<", "'mysql-server", "mysql-server/root_password", "password", "root'"]
RUN ["/bin/bash", "-c", "debconf-set-selections", "<<<", "'mysql-server", "mysql-server/root_password_again", "password", "root'"]
RUN apt-get update \
 && apt-get install --no-install-recommends mysql-server=5.7.33-0ubuntu0.16.04.1 -y
VOLUME /var/lib/mysql
WORKDIR /tmp
RUN wget --quiet https://dev.mysql.com/get/Downloads/Connector-J/mysql-connector-java-5.1.47.tar.gz
RUN tar -xzf mysql-connector-java-5.1.47.tar.gz
RUN cp mysql-connector-java-5.1.47/mysql-connector-java-5.1.47.jar /opt/hive/lib/
RUN rm -rf /tmp/mysql-connector-java-5.1.47.tar.gz
RUN rm -rf /tmp/mysql-connector-java-5.1.47
RUN mkdir /dataset
COPY dataset/popularize_churn.sql /dataset/popularize_churn.sql
COPY dataset/popularize_iris.sql /dataset/popularize_iris.sql
COPY dataset/create_model_db.sql /dataset/create_model_db.sql
#   Install the Go compiler.
RUN wget --quiet https://dl.google.com/go/go1.11.5.linux-amd64.tar.gz
RUN tar -C /usr/local -xzf go1.11.5.linux-amd64.tar.gz
RUN rm go1.11.5.linux-amd64.tar.gz
RUN apt-get install --no-install-recommends build-essential=12.1ubuntu2 -y
ENV PATH="$PATH:/usr/local/go/bin"
#   Setup Go source workspace.
RUN mkdir -p /go/bin
ENV GOPATH="/go"
ENV PATH="$PATH:$GOPATH/bin"
#   Install python and tensorflow env for run test
ARG CONDA_OS=Linux
RUN cd / \
 && curl -sL https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -o mconda-install.sh \
 && bash -x mconda-install.sh -b -p miniconda \
 && rm mconda-install.sh
ENV PATH="/miniconda/bin:$PATH"
RUN ls /miniconda/bin \
 && /miniconda/bin/conda create -y -q -n sqlflow-dev python=3.6 \
 && echo ". /miniconda/etc/profile.d/conda.sh" >> ~/.bashrc \
 && echo "source activate sqlflow-dev" >> ~/.bashrc \
 && bash -c "source activate sqlflow-dev \
 && python -m pip install tensorflow==2.0.0-alpha0 mysql-connector-python impyla jupyter"
#   Install protobuf
RUN wget --quiet https://github.com/protocolbuffers/protobuf/releases/download/v3.6.1/protoc-3.6.1-linux-x86_64.zip \
 && apt-get install --no-install-recommends unzip=6.0-20ubuntu1.1 -y \
 && unzip -qq protoc-3.6.1-linux-x86_64.zip -d /usr/local \
 && rm protoc-3.6.1-linux-x86_64.zip \
 && go get github.com/golang/protobuf/protoc-gen-go \
 && mv /go/bin/protoc-gen-go /usr/local/bin/
RUN echo "go get -t sqlflow.org/gohive \
 && go test -v sqlflow.org/gohive" > /build_and_test.bash
RUN chmod +x /build_and_test.bash
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user