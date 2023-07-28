#   Copyright 2018 AT&T Intellectual Property.  All other rights reserved.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#   Docker image to run Airflow on Kubernetes
ARG FROM=ubuntu:16.04
FROM ${FROM}
LABEL org.opencontainers.image.authors="airship-discuss@lists.airshipit.org, irc://#airshipit@freenode"
LABEL org.opencontainers.image.url="https://airshipit.org"
LABEL org.opencontainers.image.documentation="https://airship-shipyard.readthedocs.org"
LABEL org.opencontainers.image.source="https://opendev.org/airship/shipyard"
LABEL org.opencontainers.image.vendor="The Airship Authors"
LABEL org.opencontainers.image.licenses="Apache-2.0"
#   Do not prompt user for choices on installation/configuration of packages
#   Set port 8080 for Airflow Web
#   Set port 5555 for Airflow Flower
#   Set port 8793 for Airflow Worker
ENV container="docker"
ENV WEB_PORT="8080"
ENV FLOWER_PORT="5555"
ENV WORKER_PORT="8793"
ENV SLUGIFY_USES_TEXT_UNIDECODE="yes"
#   Expose port for applications
EXPOSE $WEB_PORT
EXPOSE $FLOWER_PORT
EXPOSE $WORKER_PORT
#   Set ARG for usage during build
ARG AIRFLOW_HOME=/usr/local/airflow
ARG DEBIAN_FRONTEND=noninteractive
ARG ctx_base=src/bin
#   Kubectl version
ARG KUBECTL_VERSION=1.10.2
RUN set -ex \
 && apt-get update -qq \
 && apt-get install --no-install-recommends ca-certificates=20230311 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 g++=4:12.2.0-3ubuntu1 libffi-dev=3.4.4-1 libssl-dev=3.0.8-1ubuntu1 libpq-dev=15.2-1 locales=2.37-0ubuntu2 netcat netbase=6.4 python3=3.11.2-1 python3-setuptools=66.1.1-1 python3-pip=23.0.1+dfsg-1 python3-dev=3.11.2-1 python3-dateutil=2.8.2-1 make=4.3-4.1build1 -y \
 && python3 -m pip install -U pip \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man /usr/share/doc /usr/share/doc-base
#   Things that change mostly infrequently
RUN useradd -ms /bin/bash -d ${AIRFLOW_HOME} airflow \
 && curl -L -o /usr/local/bin/kubectl https://storage.googleapis.com/kubernetes-release/release/v${KUBECTL_VERSION}/bin/linux/amd64/kubectl \
 && chmod +x /usr/local/bin/kubectl
#   Dependency requirements
#   Note - removing snakebite (python 2 vs. 3). See:
#      https://github.com/puckel/docker-airflow/issues/77
COPY images/airflow/requirements.txt /tmp/
RUN pip3 install -r /tmp/requirements.txt --no-cache-dir \
 && pip3 uninstall -y snakebite || true
#   Copy scripts used in the container:
COPY images/airflow/script/*.sh ${AIRFLOW_HOME}/
#   Copy configuration (e.g. logging config for Airflow):
COPY images/airflow/config/*.py ${AIRFLOW_HOME}/config/
#   Change permissions
RUN chown -R airflow: ${AIRFLOW_HOME}
#   Setting the version explicitly for PBR
ENV PBR_VERSION="0.1a1"
#   Shipyard
#
#   Shipyard provides core functionality used by the Airflow plugins/operators
#   Since Shipyard and Airflow are built together as images, this should prevent
#   stale or out-of-date code between these parts.
#   Shipyard requirements, source and installation
COPY ${ctx_base}/shipyard_airflow/requirements.txt /tmp/api_requirements.txt
RUN pip3 install -r /tmp/api_requirements.txt --no-cache-dir
COPY ${ctx_base}/shipyard_airflow /tmp/shipyard/
RUN cd /tmp/shipyard \
 && python3 setup.py install
#   Note: The value for the dags and plugins directories that are sourced
#   from the values.yaml of the Shipyard Helm chart need to align with these
#   directories. If they do not, airflow will not find the intended dags and
#   plugins.
#
#   Note: In the case of building images using the provided Makefile, a test is
#   run against the built-in dags provided with Airflow. Since there is no Helm
#   chart to reconfigure the airflow.cfg with these directories, these dags and
#   plugins are not known to Airflow during the image test.
#
#   Copy the plugins and dags that will be used by this Airflow image:
COPY ${ctx_base}/shipyard_airflow/shipyard_airflow/plugins ${AIRFLOW_HOME}/plugins/
COPY ${ctx_base}/shipyard_airflow/shipyard_airflow/dags ${AIRFLOW_HOME}/dags/
#   Set work directory
USER airflow
WORKDIR ${AIRFLOW_HOME}
#   Execute entrypoint
ENTRYPOINT ["./entrypoint.sh"]
# Please add your HEALTHCHECK here!!!
