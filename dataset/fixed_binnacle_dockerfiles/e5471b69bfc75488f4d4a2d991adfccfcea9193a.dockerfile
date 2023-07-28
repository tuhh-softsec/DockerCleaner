#
#   Licensed to the Apache Software Foundation (ASF) under one
#   or more contributor license agreements.  See the NOTICE file
#   distributed with this work for additional information
#   regarding copyright ownership.  The ASF licenses this file
#   to you under the Apache License, Version 2.0 (the
#   "License"); you may not use this file except in compliance
#   with the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing,
#   software distributed under the License is distributed on an
#   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#   KIND, either express or implied.  See the License for the
#   specific language governing permissions and limitations
#   under the License.
#   Using official python runtime base image
FROM ubuntu:18.04
SHELL ["/bin/bash", "-c"]
ENV DEBIAN_FRONTEND="noninteractive"
#   Java installation.
ENV LANG="C.UTF-8"
#   add a simple script that can auto-detect the appropriate JAVA_HOME value
#   based on whether the JDK or only the JRE is installed
RUN { echo '#!/bin/sh' ;echo 'set -e' ;echo ;echo 'dirname "$(dirname "$(readlink -f "$(which javac || which java)")")"' ; } > /usr/local/bin/docker-java-home \
 && chmod +x /usr/local/bin/docker-java-home
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
#   lsb
RUN apt-get update \
 && apt-get install --no-install-recommends lsb-release=9.20170808ubuntu1 mysql-server=5.7.41-0ubuntu0.18.04.1 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 libsasl2-dev=2.1.27~101-g0780600+dfsg-3ubuntu2.4 mysql-client=5.7.41-0ubuntu0.18.04.1 -y \
 && apt-get clean
RUN apt-get update \
 && apt-get install --no-install-recommends bzip2=1.0.6-8.1ubuntu0.2 unzip=6.0-21ubuntu1.2 apt-transport-https=1.6.14 xz-utils=5.2.2-1.3ubuntu0.1 -y \
 && apt-get clean
RUN apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 -y \
 && [ "$JAVA_HOME" = "$( docker-java-home ;)" ] \
 && apt-get clean
RUN /var/lib/dpkg/info/ca-certificates-java.postinst configure
RUN apt-get update \
 && apt-get install --no-install-recommends postgresql=10+190ubuntu0.1 postgresql-contrib=10+190ubuntu0.1 -y \
 && apt-get clean
RUN apt-get update \
 && apt-get install --no-install-recommends python-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 virtualenvwrapper=4.3.1-2 -y \
 && apt-get clean
RUN apt-get update \
 && apt-get install --no-install-recommends git-all=1:2.17.1-1ubuntu0.17 tig=2.3.0-1 tmux=2.6-3ubuntu0.3 vim=2:8.0.1453-1ubuntu1.11 less=487-0.1 curl=7.58.0-2ubuntu3.24 gnupg2=2.2.4-1ubuntu1.6 software-properties-common=0.96.24.32.20 libpq-dev=10.23-0ubuntu0.18.04.1 -y \
 && apt-get clean
RUN export CLOUD_SDK_REPO="cloud-sdk-$( lsb_release -c -s ;)" \
 && echo "deb https://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
 && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && apt-get update \
 && apt-get install --no-install-recommends google-cloud-sdk -y \
 && apt-get clean
#   Install python 3.6 for airflow's compatibility,
#   python-dev and necessary libraries to build all python packages
RUN add-apt-repository ppa:deadsnakes/ppa \
 && apt-get update \
 && apt-get install --no-install-recommends python3.6=3.6.9-1~18.04ubuntu1.12 python3.6-dev=3.6.9-1~18.04ubuntu1.12 python3.5 python3.5-dev python-dev=2.7.15~rc1-1 build-essential=12.4ubuntu1 autoconf=2.69-11 libtool=2.4.6-2 libkrb5-dev=1.16-2ubuntu0.4 -y \
 && apt-get clean
WORKDIR /workspace
RUN mkdir -pv /airflow/dags
#   Set airflow home
ENV AIRFLOW_HOME="/airflow"
RUN apt-get update \
 && apt-get install --no-install-recommends mlocate=0.26-2ubuntu3.1 -y \
 && apt-get clean
RUN updatedb
#   Setup un-privileged user with passwordless sudo access.
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.21p2-3ubuntu1.5 -y \
 && apt-get clean
RUN groupadd -r airflow \
 && useradd -m -r -g airflow -G sudo airflow
RUN echo 'airflow ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers
RUN apt-get update \
 && apt-get install --no-install-recommends python-setuptools=39.0.1-2ubuntu0.1 python3-setuptools=39.0.1-2ubuntu0.1 \
 && apt-get clean
RUN pip install virtualenvwrapper==4.8.4 --upgrade \
 && pip3 install --upgrade virtualenvwrapper
RUN source /usr/share/virtualenvwrapper/virtualenvwrapper.sh \
 && mkvirtualenv -p /usr/bin/python3.6 airflow36 \
 && mkvirtualenv -p /usr/bin/python3.5 airflow35
#  # Preinstall airflow
#  # Airflow requires this variable be set on installation to avoid a GPL dependency.
ENV SLUGIFY_USES_TEXT_UNIDECODE="yes"
#   Note. Increase this number to force rebuilding to the latest dependencies
ENV REBUILD_AIRFLOW_BREEZE_VERSION="6"
ARG AIRFLOW_REPO_URL=https://github.com/apache/airflow
ARG AIRFLOW_REPO_BRANCH=master
RUN echo Checking out airflow source from ${AIRFLOW_REPO_URL}, branch: ${AIRFLOW_REPO_BRANCH}
RUN git clone ${AIRFLOW_REPO_URL} temp_airflow
RUN cd temp_airflow \
 && git checkout ${AIRFLOW_REPO_BRANCH}
#   Speed up the installation of cassandra driver
ENV CASS_DRIVER_BUILD_CONCURRENCY="8"
ENV CASS_DRIVER_NO_CYTHON="1"
RUN . /usr/share/virtualenvwrapper/virtualenvwrapper.sh \
 && cd temp_airflow \
 && workon airflow36 \
 && pip install --no-use-pep517 -e .[devel_ci]
RUN . /usr/share/virtualenvwrapper/virtualenvwrapper.sh \
 && cd temp_airflow \
 && workon airflow35 \
 && pip install --no-use-pep517 -e .[devel_ci]
RUN apt-get update \
 && apt-get install --no-install-recommends jq=1.5+dfsg-2 -y \
 && apt-get clean
RUN rm -rf temp_airflow
RUN mkdir -pv /airflow/output
RUN sed -i "s/^#listen_addresses.*/listen_addresses = '*'/" /etc/postgresql/10/main/postgresql.conf
RUN sed -i "s/127.0.0.1\/32/0.0.0.0\/0/" /etc/postgresql/10/main/pg_hba.conf
RUN pip install awscli==1.27.114 botocore==1.29.114
#  # Add config and scripts
COPY airflow.cfg /airflow/airflow.cfg
COPY _init.sh /airflow/_init.sh
COPY _setup_gcp_key.sh /airflow/_setup_gcp_key.sh
COPY _reset.sh /airflow/_reset.sh
COPY _create_links.sh /airflow/_create_links.sh
COPY _setup_gcp_connection.py /airflow/_setup_gcp_connection.py
COPY _decrypt_encrypted_variables.sh /airflow/_decrypt_encrypted_variables.sh
COPY _bash_aliases /root/.bash_aliases
COPY _inputrc /root/.inputrc
COPY cloudbuild /root/cloudbuild
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
