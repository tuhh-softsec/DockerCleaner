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
#   WARNING: THIS DOCKERFILE IS NOT INTENDED FOR PRODUCTION USE OR DEPLOYMENT.
#
#   Base image for the whole Docker file
ARG APT_DEPS_IMAGE="airflow-apt-deps"
ARG PYTHON_BASE_IMAGE="python:3.6-slim"
#  ###########################################################################################################
#   This is the base image with APT dependencies needed by Airflow. It is based on a python slim image
#   Parameters:
#      PYTHON_BASE_IMAGE - base python image (python:x.y-slim)
#  ###########################################################################################################
FROM ${PYTHON_BASE_IMAGE} AS airflow-apt-deps
SHELL ["/bin/bash", "-o", "pipefail", "-e", "-u", "-x", "-c"]
#   Need to repeat the empty argument here otherwise it will not be set for this stage
#   But the default value carries from the one set before FROM
ARG PYTHON_BASE_IMAGE
ENV PYTHON_BASE_IMAGE="${PYTHON_BASE_IMAGE}"
ARG AIRFLOW_VERSION="2.0.0.dev0"
ENV AIRFLOW_VERSION="$AIRFLOW_VERSION"
#   Print versions
RUN echo "Base image: ${PYTHON_BASE_IMAGE}"
RUN echo "Airflow version: ${AIRFLOW_VERSION}"
#   Make sure noninteractie debian install is used and language variables set
ENV DEBIAN_FRONTEND="noninteractive" \
    LANGUAGE="C.UTF-8" \
    LANG="C.UTF-8" \
    LC_ALL="C.UTF-8" \
    LC_CTYPE="C.UTF-8" \
    LC_MESSAGES="C.UTF-8"
#   By increasing this number we can do force build of all dependencies
ARG DEPENDENCIES_EPOCH_NUMBER="1"
#   Increase the value below to force renstalling of all dependencies
ENV DEPENDENCIES_EPOCH_NUMBER="${DEPENDENCIES_EPOCH_NUMBER}"
#   Install curl and gnupg2 - needed to download nodejs in the next step
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 gnupg2=2.2.40-1ubuntu2 -y ) \
 && apt-get autoremove -yqq --purge \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Install basic apt dependencies
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash - \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-utils=2.6.0 build-essential=12.9ubuntu3 curl=7.88.1-7ubuntu1 dirmngr=2.2.40-1ubuntu2 freetds-bin=1.3.17+ds-2 freetds-dev=1.3.17+ds-2 git=1:2.39.2-1ubuntu1 gosu=1.14-1 libffi-dev=3.4.4-1 libkrb5-dev=1.20.1-1build1 libpq-dev=15.2-1 libsasl2-2=2.1.28+dfsg-10 libsasl2-dev=2.1.28+dfsg-10 libsasl2-modules=2.1.28+dfsg-10 libssl-dev=3.0.8-1ubuntu1 locales=2.37-0ubuntu2 netcat nodejs=18.13.0+dfsg1-1ubuntu2 rsync=3.2.7-1 sasl2-bin=2.1.28+dfsg-10 sudo=1.9.13p1-1ubuntu2 -y ) \
 && apt-get autoremove -yqq --purge \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Install MySQL client from Oracle repositories (Debian installs mariadb)
RUN KEY="A4A9406876FCBD3C456770C88C718D3B5072E1F5" \
 && GNUPGHOME="$( mktemp -d ;)" \
 && export GNUPGHOME \
 && for KEYSERVER in $( shuf -e ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 keyserver.ubuntu.com hkp://keyserver.ubuntu.com:80 pgp.mit.edu ;); do gpg --keyserver "${KEYSERVER}" --recv-keys "${KEY}" \
 && break || true ; done \
 && gpg --export "${KEY}" | apt-key add - \
 && gpgconf --kill all rm -rf "${GNUPGHOME}" ; apt-key list > /dev/null \
 && echo "deb http://repo.mysql.com/apt/ubuntu/ trusty mysql-5.6" | tee -a /etc/apt/sources.list.d/mysql.list \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libmysqlclient-dev=8.0.32-0ubuntu4 mysql-client=8.0.32-0ubuntu4 -y ) \
 && apt-get autoremove -yqq --purge \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN adduser airflow \
 && echo "airflow ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/airflow \
 && chmod 0440 /etc/sudoers.d/airflow
#  ###########################################################################################################
#   This is an image with all APT dependencies needed by CI. It is built on top of the airlfow APT image
#   Parameters:
#       airflow-apt-deps - this is the base image for CI deps image.
#  ###########################################################################################################
FROM airflow-apt-deps AS airflow-ci-apt-deps
SHELL ["/bin/bash", "-o", "pipefail", "-e", "-u", "-x", "-c"]
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"
ARG APT_DEPS_IMAGE
ENV APT_DEPS_IMAGE="${APT_DEPS_IMAGE}"
RUN echo "${APT_DEPS_IMAGE}"
#   Note the ifs below might be removed if Buildkit will become usable. It should skip building this
#   image automatically if it is not used. For now we still go through all layers below but they are empty
RUN if [[ "${APT_DEPS_IMAGE}" == "airflow-ci-apt-deps" ]] ; then mkdir -pv /usr/share/man/man1 \
 && mkdir -pv /usr/share/man/man7 \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends gnupg=2.2.40-1ubuntu2 krb5-user=1.20.1-1build1 ldap-utils=2.6.3+dfsg-1~exp1ubuntu2 less=590-1.2 lsb-release=12.0-1ubuntu1 net-tools=2.10-0.1ubuntu3 openjdk-8-jdk=8u362-ga-0ubuntu2 openssh-client=1:9.0p1-1ubuntu8 openssh-server=1:9.0p1-1ubuntu8 postgresql-client=15+248 python-selinux sqlite3=3.40.1-1 tmux=3.3a-3 unzip=6.0-27ubuntu1 vim=2:9.0.1000-4ubuntu2 -y ) \
 && apt-get autoremove -yqq --purge \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* ; fi
ENV HADOOP_DISTRO="cdh" \
    HADOOP_MAJOR="5" \
    HADOOP_DISTRO_VERSION="5.11.0" \
    HADOOP_VERSION="2.6.0" \
    HIVE_VERSION="1.1.0"
ENV HADOOP_URL="https://archive.cloudera.com/${HADOOP_DISTRO}${HADOOP_MAJOR}/${HADOOP_DISTRO}/${HADOOP_MAJOR}/"
ENV HADOOP_HOME="/tmp/hadoop-cdh" \
    HIVE_HOME="/tmp/hive"
RUN if [[ "${APT_DEPS_IMAGE}" == "airflow-ci-apt-deps" ]] ; then mkdir -pv ${HADOOP_HOME} \
 && mkdir -pv ${HIVE_HOME} \
 && mkdir /tmp/minicluster \
 && mkdir -pv /user/hive/warehouse \
 && chmod -R 777 ${HIVE_HOME} \
 && chmod -R 777 /user/ ; fi
#   Install Hadoop
#   --absolute-names is a work around to avoid this issue https://github.com/docker/hub-feedback/issues/727
RUN if [[ "${APT_DEPS_IMAGE}" == "airflow-ci-apt-deps" ]] ; then HADOOP_URL=${HADOOP_URL}hadoop-${HADOOP_VERSION}-${HADOOP_DISTRO}${HADOOP_DISTRO_VERSION}.tar.gz \
 && HADOOP_TMP_FILE=/tmp/hadoop.tar.gz \
 && curl -sL ${HADOOP_URL} > ${HADOOP_TMP_FILE} \
 && tar xzf ${HADOOP_TMP_FILE} --absolute-names --strip-components 1 -C ${HADOOP_HOME} \
 && rm ${HADOOP_TMP_FILE} ; fi
#   Install Hive
RUN if [[ "${APT_DEPS_IMAGE}" == "airflow-ci-apt-deps" ]] ; then HIVE_URL=${HADOOP_URL}hive-${HIVE_VERSION}-${HADOOP_DISTRO}${HADOOP_DISTRO_VERSION}.tar.gz \
 && HIVE_TMP_FILE=/tmp/hive.tar.gz \
 && curl -sL ${HIVE_URL} > ${HIVE_TMP_FILE} \
 && tar xzf ${HIVE_TMP_FILE} --strip-components 1 -C ${HIVE_HOME} \
 && rm ${HIVE_TMP_FILE} ; fi
ENV MINICLUSTER_URL="https://github.com/bolkedebruin/minicluster/releases/download/"
ENV MINICLUSTER_VER="1.1"
#   Install MiniCluster TODO: install it differently. Installing to /tmp is probably a bad idea
RUN if [[ "${APT_DEPS_IMAGE}" == "airflow-ci-apt-deps" ]] ; then MINICLUSTER_URL=${MINICLUSTER_URL}${MINICLUSTER_VER}/minicluster-${MINICLUSTER_VER}-SNAPSHOT-bin.zip \
 && MINICLUSTER_TMP_FILE=/tmp/minicluster.zip \
 && curl -sL ${MINICLUSTER_URL} > ${MINICLUSTER_TMP_FILE} \
 && unzip ${MINICLUSTER_TMP_FILE} -d /tmp \
 && rm ${MINICLUSTER_TMP_FILE} ; fi
ENV PATH="\"${PATH}:/tmp/hive/bin\""
#  ###########################################################################################################
#   This is the target image - it installs PIP and NPM dependencies including efficient caching
#   mechanisms - it might be used to build the bare airflow build or CI build
#   Parameters:
#      APT_DEPS_IMAGE - image with APT dependencies. It might either be base deps image with airflow
#                       dependencies or CI deps image that contains also CI-required dependencies
#  ###########################################################################################################
FROM ${APT_DEPS_IMAGE} AS main
SHELL ["/bin/bash", "-o", "pipefail", "-e", "-u", "-x", "-c"]
WORKDIR /opt/airflow
RUN echo "Airflow version: ${AIRFLOW_VERSION}"
ARG APT_DEPS_IMAGE
ENV APT_DEPS_IMAGE="${APT_DEPS_IMAGE}"
ARG AIRFLOW_USER=airflow
ENV AIRFLOW_USER="${AIRFLOW_USER}"
ARG HOME=/home/airflow
ENV HOME="${HOME}"
ARG AIRFLOW_HOME=${HOME}/airflow
ENV AIRFLOW_HOME="${AIRFLOW_HOME}"
ARG AIRFLOW_SOURCES=/opt/airflow
ENV AIRFLOW_SOURCES="${AIRFLOW_SOURCES}"
RUN mkdir -pv ${AIRFLOW_HOME} mkdir -pv ${AIRFLOW_HOME}/dags mkdir -pv ${AIRFLOW_HOME}/logs \
 && chown -R ${AIRFLOW_USER}.${AIRFLOW_USER} ${AIRFLOW_HOME}
#   Increase the value here to force reinstalling Apache Airflow pip dependencies
ARG PIP_DEPENDENCIES_EPOCH_NUMBER="1"
ENV PIP_DEPENDENCIES_EPOCH_NUMBER="${PIP_DEPENDENCIES_EPOCH_NUMBER}"
#   Optimizing installation of Cassandra driver
#   Speeds up building the image - cassandra driver without CYTHON saves around 10 minutes
ARG CASS_DRIVER_NO_CYTHON="1"
#   Build cassandra driver on multiple CPUs
ARG CASS_DRIVER_BUILD_CONCURRENCY="8"
ENV CASS_DRIVER_BUILD_CONCURRENCY="${CASS_DRIVER_BUILD_CONCURRENCY}"
ENV CASS_DRIVER_NO_CYTHON="${CASS_DRIVER_NO_CYTHON}"
#   By default PIP install run without cache to make image smaller
ARG PIP_NO_CACHE_DIR="true"
ENV PIP_NO_CACHE_DIR="${PIP_NO_CACHE_DIR}"
RUN echo "Pip no cache dir: ${PIP_NO_CACHE_DIR}"
#   PIP version used to install dependencies
ARG PIP_VERSION="19.0.1"
ENV PIP_VERSION="${PIP_VERSION}"
RUN echo "Pip version: ${PIP_VERSION}"
RUN pip install pip==${PIP_VERSION} --upgrade
#   We are copying everything with airflow:airflow user:group even if we use root to run the scripts
#   This is fine as root user will be able to use those dirs anyway.
#   Airflow sources change frequently but dependency configuration won't change that often
#   We copy setup.py and other files needed to perform setup of dependencies
#   This way cache here will only be invalidated if any of the
#   version/setup configuration change but not when airflow sources change
COPY --chown=airflow:airflow setup.py ${AIRFLOW_SOURCES}/setup.py
COPY --chown=airflow:airflow setup.cfg ${AIRFLOW_SOURCES}/setup.cfg
COPY --chown=airflow:airflow airflow/version.py ${AIRFLOW_SOURCES}/airflow/version.py
COPY --chown=airflow:airflow airflow/__init__.py ${AIRFLOW_SOURCES}/airflow/__init__.py
COPY --chown=airflow:airflow airflow/bin/airflow ${AIRFLOW_SOURCES}/airflow/bin/airflow
#   Airflow Extras installed
ARG AIRFLOW_EXTRAS="all"
ENV AIRFLOW_EXTRAS="${AIRFLOW_EXTRAS}"
RUN echo "Installing with extras: ${AIRFLOW_EXTRAS}."
#   First install only dependencies but no Apache Airflow itself
#   This way regular changes in sources of Airflow will not trigger reinstallation of all dependencies
#   And this Docker layer will be reused between builds.
RUN pip install --no-use-pep517 -e ".[${AIRFLOW_EXTRAS}]"
COPY --chown=airflow:airflow airflow/www/package.json ${AIRFLOW_SOURCES}/airflow/www/package.json
COPY --chown=airflow:airflow airflow/www/package-lock.json ${AIRFLOW_SOURCES}/airflow/www/package-lock.json
WORKDIR ${AIRFLOW_SOURCES}/airflow/www
#   Install necessary NPM dependencies (triggered by changes in package-lock.json)
RUN gosu ${AIRFLOW_USER} npm ci
COPY --chown=airflow:airflow airflow/www/ ${AIRFLOW_SOURCES}/airflow/www/
#   Package NPM for production
RUN gosu ${AIRFLOW_USER} npm run prod
#   Always apt-get update/upgrade here to get latest dependencies before
#   we redo pip install
RUN : \
 && apt-get upgrade -y --no-install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Cache for this line will be automatically invalidated if any
#   of airflow sources change
COPY --chown=airflow:airflow . ${AIRFLOW_SOURCES}/
WORKDIR ${AIRFLOW_SOURCES}
#   Always add-get update/upgrade here to get latest dependencies before
#   we redo pip install
RUN : \
 && apt-get upgrade -y --no-install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Additional python deps to install
ARG ADDITIONAL_PYTHON_DEPS=""
RUN if [[ -n "${ADDITIONAL_PYTHON_DEPS}" ]] ; then pip install ${ADDITIONAL_PYTHON_DEPS} ; fi
COPY --chown=airflow:airflow ./scripts/docker/entrypoint.sh /entrypoint.sh
USER ${AIRFLOW_USER}
WORKDIR ${AIRFLOW_SOURCES}
ENV PATH="${HOME}:${PATH}"
EXPOSE 8080/tcp
ENTRYPOINT ["/usr/local/bin/dumb-init", "--", "/entrypoint.sh"]
CMD ["--help"]
# Please add your HEALTHCHECK here!!!
