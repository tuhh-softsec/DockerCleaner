#
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
#
#   WARNING: THIS DOCKERFILE IS NOT INTENDED FOR PRODUCTION USE OR DEPLOYMENT. AT
#            THIS POINT, THIS IS ONLY INTENDED FOR USE IN AUTOMATED TESTS.
FROM ubuntu:xenial
USER root
ENV DEBIAN_FRONTEND="noninteractive"
ENV LANGUAGE="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV LC_CTYPE="en_US.UTF-8"
ENV LC_MESSAGES="en_US.UTF-8"
ENV HADOOP_VERSION="2.6.0"
ENV HADOOP_DISTRO="cdh"
ENV HADOOP_HOME="/tmp/hadoop-${HADOOP_DISTRO}"
ENV HIVE_HOME="/tmp/hive"
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"
RUN mkdir ${HADOOP_HOME} \
 && mkdir ${HIVE_HOME} \
 && mkdir /tmp/minicluster \
 && mkdir -p /user/hive/warehouse \
 && chmod -R 777 ${HIVE_HOME} \
 && chmod -R 777 /user/
#   Add nodejs repo and key
COPY nodesource.gpg.key /tmp/nodesource.gpg.key
RUN apt-key add /tmp/nodesource.gpg.key
RUN echo 'deb http://deb.nodesource.com/node_8.x xenial main' > /etc/apt/sources.list.d/nodesource.list
RUN echo 'deb-src http://deb.nodesource.com/node_8.x xenial main' >> /etc/apt/sources.list.d/nodesource.list
RUN apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 gcc=4:5.3.1-1ubuntu1 g++=4:5.3.1-1ubuntu1 python-dev=2.7.12-1~16.04 python3-dev=3.5.1-3 python-pip=8.1.1-2ubuntu0.6 python3-pip=8.1.1-2ubuntu0.6 python-virtualenv=15.0.1+ds-3ubuntu1.1 python3-venv=3.5.1-3 python-setuptools=20.7.0-1 python-pkg-resources=20.7.0-1 python3-setuptools=20.7.0-1 python3-pkg-resources=20.7.0-1 make=4.1-6 nodejs=4.2.6~dfsg-1ubuntu4.2 vim=2:7.4.1689-3ubuntu1.5 less=481-2.1ubuntu0.2 git=1:2.7.4-0ubuntu1.10 unzip=6.0-20ubuntu1.1 sudo=1.8.16-0ubuntu1.10 ldap-utils=2.4.42+dfsg-2ubuntu3.13 mysql-client-core-5.7=5.7.33-0ubuntu0.16.04.1 mysql-client-5.7=5.7.33-0ubuntu0.16.04.1 libmysqlclient-dev=5.7.33-0ubuntu0.16.04.1 postgresql-client=9.5+173ubuntu0.3 sqlite3=3.11.0-1ubuntu1.5 libkrb5-dev=1.13.2+dfsg-5ubuntu2.2 libsasl2-dev=2.1.26.dfsg1-14ubuntu0.2 krb5-user=1.13.2+dfsg-5ubuntu2.2 openssh-client=1:7.2p2-4ubuntu2.10 openssh-server=1:7.2p2-4ubuntu2.10 python-selinux=2.4-3build2 sasl2-bin=2.1.26.dfsg1-14ubuntu0.2 libsasl2-2=2.1.26.dfsg1-14ubuntu0.2 libsasl2-dev=2.1.26.dfsg1-14ubuntu0.2 libsasl2-modules=2.1.26.dfsg1-14ubuntu0.2 locales=2.23-0ubuntu11.3 -y \
 && rm -rf /var/lib/apt/lists/*
RUN sed -i 's/^# en_US.UTF-8 UTF-8$/en_US.UTF-8 UTF-8/g' /etc/locale.gen \
 && locale-gen \
 && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
#   Install Hadoop
#   --absolute-names is a work around to avoid this issue https://github.com/docker/hub-feedback/issues/727
RUN cd /tmp \
 && wget -q https://archive.cloudera.com/cdh5/cdh/5/hadoop-${HADOOP_VERSION}-cdh5.11.0.tar.gz \
 && tar xzf hadoop-${HADOOP_VERSION}-cdh5.11.0.tar.gz --absolute-names --strip-components 1 -C $HADOOP_HOME \
 && rm hadoop-${HADOOP_VERSION}-cdh5.11.0.tar.gz
#   Install Hive
RUN cd /tmp \
 && wget -q https://archive.cloudera.com/cdh5/cdh/5/hive-1.1.0-cdh5.11.0.tar.gz \
 && tar xzf hive-1.1.0-cdh5.11.0.tar.gz --strip-components 1 -C $HIVE_HOME \
 && rm hive-1.1.0-cdh5.11.0.tar.gz
#   Install MiniCluster
RUN cd /tmp \
 && wget -q https://github.com/bolkedebruin/minicluster/releases/download/1.1/minicluster-1.1-SNAPSHOT-bin.zip \
 && unzip minicluster-1.1-SNAPSHOT-bin.zip -d /tmp \
 && rm minicluster-1.1-SNAPSHOT-bin.zip
RUN adduser airflow \
 && echo "airflow ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/airflow \
 && chmod 0440 /etc/sudoers.d/airflow
#   Install Python requirements
RUN sudo -H pip install --upgrade pip \
 && sudo -H pip install wheel tox \
 && sudo -H pip3 install --upgrade pip \
 && sudo -H pip3 install wheel tox \
 && rm -rf ~/.cache
EXPOSE 8080/tcp
WORKDIR /home/airflow
ENV PATH="\"$PATH:/tmp/hive/bin:$ADDITIONAL_PATH\""
USER airflow
# Please add your HEALTHCHECK here!!!
