FROM ubuntu:16.04
MAINTAINER a.sevilla@anchormen.nl
ENV SPARK_VERSION="2.2.0"
ENV R_BASE_VERSION="3.4.3"
ENV ANACONDA_VERSION="5.0.1"
RUN apt-get update -y \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 default-jdk=2:1.8-56ubuntu2 ed=1.10-2 file=1:5.25-2ubuntu1.4 fonts-texgyre=20150923-1 git=1:2.7.4-0ubuntu1.10 less=481-2.1ubuntu0.2 libapparmor1=2.10.95-0ubuntu2.11 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libedit2=3.1-20150325-1ubuntu2 libgdal-dev=1.11.3+dfsg-3build2 libgeos-dev=3.5.0-1ubuntu2 libpq-dev=9.5.25-0ubuntu0.16.04.1 libproj-dev=4.9.2-2 libssh2-1=1.5.0-2ubuntu0.1 libssh2-1-dev=1.5.0-2ubuntu0.1 libssl-dev=1.0.2g-1ubuntu4.20 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 locales=2.23-0ubuntu11.3 lsb-release=9.20160110ubuntu0.2 nano=2.5.3-2ubuntu2 openconnect=7.06-2build2 openssh-client=1:7.2p2-4ubuntu2.10 psmisc=22.21-2.1ubuntu0.1 python-setuptools=20.7.0-1 software-properties-common=0.96.20.10 sudo=1.8.16-0ubuntu1.10 unzip=6.0-20ubuntu1.1 vim-tiny=2:7.4.1689-3ubuntu1.5 wget=1.17.1-1ubuntu1.5 -y \
 && rm -rf /var/lib/apt/lists/*
#   Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
#   Spark
RUN mkdir -p /opt \
 && cd /opt \
 && curl https://d3kbcqa49mib13.cloudfront.net/spark-${SPARK_VERSION}-bin-hadoop2.7.tgz | tar -zx \
 && ln -s spark-${SPARK_VERSION}-bin-hadoop2.7 spark \
 && echo Spark ${SPARK_VERSION} installed in /opt
COPY start-common.sh start-worker.sh start-master.sh /
RUN chmod +x /start-common.sh /start-master.sh /start-worker.sh
ENV JAVA_HOME="/usr/lib/jvm/default-java"
ENV PATH="$PATH:/opt/spark/bin"
ENV SPARK_HOME="/opt/spark"
#   R
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
 && add-apt-repository 'deb [arch=amd64,i386] https://cran.rstudio.com/bin/linux/ubuntu xenial/' \
 && apt-get update -y \
 && apt-get install --no-install-recommends r-base=${R_BASE_VERSION}* r-base-dev=${R_BASE_VERSION}* r-recommended=${R_BASE_VERSION}* -y \
 && echo 'options(repos = c(CRAN = "https://cran.rstudio.com/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site \
 && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
 && rm -rf /var/lib/apt/lists/* \
 && R -e "install.packages('devtools')"
ENV R_HOME="/usr/lib/R"
#   Install additional R packages from CRAN or github
#   For Zeppelin
RUN R -e "install.packages(c('knitr', 'ggplot2', 'googleVis', 'data.table', 'Rcpp'))"
RUN R -e "devtools::install_github('ramnathv/rCharts')"
#   Time Series Forecast
RUN R -e "devtools::install_github('robjhyndman/forecast')"
#   PCA Analysis
RUN R -e "install.packages('factoextra')"
#   Python 3.6 and ANACONDA
RUN echo 'export PATH=/opt/conda/bin:$PATH' > /etc/profile.d/conda.sh \
 && wget --quiet https://repo.continuum.io/archive/Anaconda3-${ANACONDA_VERSION}-Linux-x86_64.sh -O ~/anaconda.sh \
 && /bin/bash ~/anaconda.sh -b -p /opt/conda \
 && rm ~/anaconda.sh
RUN TINI_VERSION=`curl https://github.com/krallin/tini/releases/latest | grep -o "/v.*\\"" | sed 's:^..\\(.*\\).$:\\1:' ` \
 && curl -L "https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini_${TINI_VERSION}.deb" > tini.deb \
 && dpkg -i tini.deb \
 && rm tini.deb \
 && apt-get clean
RUN apt-get update \
 && apt-get install --no-install-recommends python3-pip=8.1.1-2ubuntu0.6 -y
ENV PATH="$PATH:/opt/conda/bin"
ENV PYTHON_VERSION="3.6.3"
ENV PYSPARK_PYTHON="python3.6"
ENV PYSPARK_DRIVER_PYTHON="python3.6"
ENV PYTHONPATH="${SPARK_HOME}/python/:${SPARK_HOME}/python/lib/py4j-0.10.4-src.zip:${PYTHONPATH}"
#   Packages for HDFS interaction
RUN pip install hdfs==2.7.0 pywebhdfs==0.4.1
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
