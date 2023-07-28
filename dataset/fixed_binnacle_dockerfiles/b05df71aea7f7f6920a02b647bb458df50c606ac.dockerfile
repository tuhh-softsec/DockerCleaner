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
FROM ubuntu:16.04
MAINTAINER Apache Software Foundation <dev@zeppelin.apache.org>
#   `Z_VERSION` will be updated by `dev/change_zeppelin_version.sh`
ENV Z_VERSION="0.9.0-SNAPSHOT"
ENV LOG_TAG="[ZEPPELIN_${Z_VERSION}]:" \
    Z_HOME="/zeppelin" \
    LANG="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8"
RUN echo "$LOG_TAG update and install basic packages" \
 && apt-get update -y \
 && apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 -y \
 && locale-gen $LANG \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y \
 && apt-get -y autoclean \
 && apt-get -y dist-upgrade \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 -y
RUN echo "$LOG_TAG install tini related packages" \
 && apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 grep=2.25-1~16.04.1 sed=4.2.2-7 dpkg=1.18.4ubuntu1.7 -y \
 && TINI_VERSION=`curl https://github.com/krallin/tini/releases/latest | grep -o "/v.*\\"" | sed 's:^..\\(.*\\).$:\\1:' ` \
 && curl -L "https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini_${TINI_VERSION}.deb" > tini.deb \
 && dpkg -i tini.deb \
 && rm tini.deb
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
RUN echo "$LOG_TAG Install java8" \
 && apt-get update -y \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 -y \
 && rm -rf /var/lib/apt/lists/*
#   should install conda first before numpy, matploylib since pip and python will be installed by conda
RUN echo "$LOG_TAG Install miniconda2 related packages" \
 && apt-get update -y \
 && apt-get install --no-install-recommends bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 libglib2.0-0=2.48.2-0ubuntu4.8 libxext6=2:1.3.3-1 libsm6=2:1.2.2-1 libxrender1=1:0.9.9-0ubuntu1 git=1:2.7.4-0ubuntu1.10 mercurial=3.7.3-1ubuntu1.2 subversion=1.9.3-2ubuntu1.3 -y \
 && echo 'export PATH=/opt/conda/bin:$PATH' > /etc/profile.d/conda.sh \
 && wget --quiet https://repo.continuum.io/miniconda/Miniconda2-4.3.11-Linux-x86_64.sh -O ~/miniconda.sh \
 && /bin/bash ~/miniconda.sh -b -p /opt/conda \
 && rm ~/miniconda.sh
ENV PATH="/opt/conda/bin:$PATH"
RUN echo "$LOG_TAG Install python related packages" \
 && apt-get update -y \
 && apt-get install --no-install-recommends python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 -y \
 && apt-get install --no-install-recommends gfortran=4:5.3.1-1ubuntu1 -y \
 && apt-get install --no-install-recommends libblas-dev=3.6.0-2ubuntu2 libatlas-dev=3.10.2-9 liblapack-dev=3.6.0-2ubuntu2 -y \
 && apt-get install --no-install-recommends libpng-dev libfreetype6-dev=2.6.1-0.1ubuntu2.5 libxft-dev=2.3.2-1 -y \
 && apt-get install --no-install-recommends python-tk=2.7.12-1~16.04 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt-dev zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y \
 && pip install numpy==1.12.1 pandas==0.21.1 matplotlib==2.1.1 pandasql==0.7.3 ipython==5.4.1 jupyter_client==5.1.0 ipykernel==4.7.0 bokeh==0.12.10 ggplot==0.11.5 grpcio==1.8.2 bkzep==0.4.0
RUN echo "$LOG_TAG Install R related packages" \
 && echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | tee -a /etc/apt/sources.list \
 && gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 \
 && gpg -a --export E084DAB9 | apt-key add - \
 && apt-get update -y \
 && apt-get install --no-install-recommends r-base=3.2.3-4 r-base-dev=3.2.3-4 -y \
 && R -e "install.packages('knitr', repos='http://cran.us.r-project.org')" \
 && R -e "install.packages('ggplot2', repos='http://cran.us.r-project.org')" \
 && R -e "install.packages('googleVis', repos='http://cran.us.r-project.org')" \
 && R -e "install.packages('data.table', repos='http://cran.us.r-project.org')" \
 && apt-get install --no-install-recommends libcurl4-gnutls-dev=7.47.0-1ubuntu2.19 libssl-dev=1.0.2g-1ubuntu4.20 -y \
 && R -e "install.packages('devtools', repos='http://cran.us.r-project.org')" \
 && R -e "install.packages('Rcpp', repos='http://cran.us.r-project.org')" \
 && Rscript -e "library('devtools'); library('Rcpp'); install_github('ramnathv/rCharts')"
#   Install kubectl
RUN apt-get install --no-install-recommends apt-transport-https=1.2.35 -y \
 && curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && echo "deb https://apt.kubernetes.io/ kubernetes-xenial main" | tee -a /etc/apt/sources.list.d/kubernetes.list \
 && apt-get update \
 && apt-get install --no-install-recommends kubectl -y
RUN echo "$LOG_TAG Cleanup" \
 && apt-get autoclean \
 && apt-get clean
RUN echo "$LOG_TAG Download Zeppelin binary" \
 && wget -O /tmp/zeppelin-${Z_VERSION}-bin-all.tgz http://archive.apache.org/dist/zeppelin/zeppelin-${Z_VERSION}/zeppelin-${Z_VERSION}-bin-all.tgz \
 && tar -zxvf /tmp/zeppelin-${Z_VERSION}-bin-all.tgz \
 && rm -rf /tmp/zeppelin-${Z_VERSION}-bin-all.tgz \
 && mv /zeppelin-${Z_VERSION}-bin-all ${Z_HOME}
COPY log4j.properties ${Z_HOME}/conf/
EXPOSE 8080/tcp
ENTRYPOINT ["/usr/bin/tini", "--"]
WORKDIR ${Z_HOME}
CMD ["bin/zeppelin.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
