#
#   Copyright 2016 Confluent Inc.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM centos:centos7
ARG COMMIT_ID=unknown
LABEL io.confluent.docker.git.id="$COMMIT_ID"
ARG BUILD_NUMBER=-1
LABEL io.confluent.docker.build.number="$BUILD_NUMBER"
ARG CONFLUENT_PACKAGES_REPO=$CONFLUENT_PACKAGES_REPO
ARG ALLOW_UNSIGNED=false
#  Set an env var so that it's available in derived images
ENV ALLOW_UNSIGNED="${ALLOW_UNSIGNED}"
MAINTAINER partner-support@confluent.io
LABEL io.confluent.docker="true"
#   # Python
#   # TODO: get this version
#   # ENV PYTHON_VERSION="2.7.9-1"
ENV PYTHON_PIP_VERSION="9.0.1"
#   Confluent
ENV SCALA_VERSION="2.11"
ARG KAFKA_VERSION=$KAFKA_VERSION
ARG CONFLUENT_MAJOR_VERSION=$CONFLUENT_MAJOR_VERSION
ARG CONFLUENT_MINOR_VERSION=$CONFLUENT_MINOR_VERSION
ARG CONFLUENT_PATCH_VERSION=$CONFLUENT_PATCH_VERSION
ARG CONFLUENT_MVN_LABEL=$CONFLUENT_MVN_LABEL
ARG CONFLUENT_PLATFORM_LABEL=$CONFLUENT_PLATFORM_LABEL
ENV KAFKA_VERSION="$KAFKA_VERSION"
ENV CONFLUENT_MAJOR_VERSION="$CONFLUENT_MAJOR_VERSION"
ENV CONFLUENT_MINOR_VERSION="$CONFLUENT_MINOR_VERSION"
ENV CONFLUENT_PATCH_VERSION="$CONFLUENT_PATCH_VERSION"
ENV CONFLUENT_MVN_LABEL="$CONFLUENT_MVN_LABEL"
ENV CONFLUENT_PLATFORM_LABEL="$CONFLUENT_PLATFORM_LABEL"
ENV CONFLUENT_VERSION="$CONFLUENT_MAJOR_VERSION.$CONFLUENT_MINOR_VERSION.$CONFLUENT_PATCH_VERSION"
#   Zulu
ENV ZULU_OPENJDK_VERSION="8-8.17.0.3"
#   This affects how strings in Java class files are interpreted.  We want UTF-8 and this is the only locale in the
#   base image that supports it
ENV LANG="C.UTF-8"
RUN echo "===> clean yum caches ....." \
 && yum clean all
RUN echo "===> Installing curl wget netcat python...." \
 && yum install -y curl git wget nc python
RUN echo "===> Installing python packages ..." \
 && curl -fSL "https://bootstrap.pypa.io/get-pip.py" | python \
 && pip install pip==${PYTHON_PIP_VERSION} --no-cache-dir --upgrade \
 && pip install git+https://github.com/confluentinc/confluent-docker-utils@v0.0.20 --no-cache-dir \
 && yum remove -y git
RUN echo "Installing Zulu OpenJDK ${ZULU_OPENJDK_VERSION}" \
 && rpm --import http://repos.azulsystems.com/RPM-GPG-KEY-azulsystems \
 && curl -o /etc/yum.repos.d/zulu.repo http://repos.azulsystems.com/rhel/zulu.repo \
 && yum -q -y update \
 && yum -q -y install zulu-${ZULU_OPENJDK_VERSION}
RUN echo "===> Adding confluent repository...${CONFLUENT_PACKAGES_REPO}/rpm/${CONFLUENT_MAJOR_VERSION}.${CONFLUENT_MINOR_VERSION}" \
 && if [ "x${ALLOW_UNSIGNED}" = "xtrue" ] ; then echo "===> GPG signatures are disabled for SNAPSHOT builds..." \
 && printf "[Confluent.dist] \nname=Confluent repository (dist) \nbaseurl=${CONFLUENT_PACKAGES_REPO}/rpm/${CONFLUENT_MAJOR_VERSION}.${CONFLUENT_MINOR_VERSION}/ \ngpgcheck=0 \nenabled=1 \n\n[Confluent] \nname=Confluent repository \nbaseurl=${CONFLUENT_PACKAGES_REPO}/rpm/${CONFLUENT_MAJOR_VERSION}.${CONFLUENT_MINOR_VERSION}/ \ngpgcheck=0 \nenabled=1 " > /etc/yum.repos.d/confluent.repo; else echo "===> Using Confluent's archive.key..." \
 && rpm --import ${CONFLUENT_PACKAGES_REPO}/rpm/${CONFLUENT_MAJOR_VERSION}.${CONFLUENT_MINOR_VERSION}/archive.key \
 && printf "[Confluent.dist] \nname=Confluent repository (dist) \nbaseurl=${CONFLUENT_PACKAGES_REPO}/rpm/${CONFLUENT_MAJOR_VERSION}.${CONFLUENT_MINOR_VERSION}/ \ngpgcheck=1 \ngpgkey=${CONFLUENT_PACKAGES_REPO}/rpm/${CONFLUENT_MAJOR_VERSION}.${CONFLUENT_MINOR_VERSION}/archive.key \nenabled=1 \n\n[Confluent] \nname=Confluent repository \nbaseurl=${CONFLUENT_PACKAGES_REPO}/rpm/${CONFLUENT_MAJOR_VERSION}.${CONFLUENT_MINOR_VERSION}/ \ngpgcheck=1 \ngpgkey=${CONFLUENT_PACKAGES_REPO}/rpm/${CONFLUENT_MAJOR_VERSION}.${CONFLUENT_MINOR_VERSION}/archive.key \nenabled=1 " > /etc/yum.repos.d/confluent.repo; fi
ENV CUB_CLASSPATH="/etc/confluent/docker/docker-utils.jar"
COPY include/etc/confluent/docker /etc/confluent/docker
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
