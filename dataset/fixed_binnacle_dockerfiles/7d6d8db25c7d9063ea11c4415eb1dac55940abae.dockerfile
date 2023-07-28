#   Image: pubnative/zeppelin
FROM debian:stretch
RUN apt-get update \
 && apt-get install --no-install-recommends locales=2.24-11+deb9u4 -y \
 && dpkg-reconfigure -f noninteractive locales \
 && locale-gen C.UTF-8 \
 && /usr/sbin/update-locale LANG=C.UTF-8 \
 && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Users with other locales should set this in their derivative image
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.52.1-5+deb9u16 unzip=6.0-21+deb9u2 python3=3.5.3-1 python3-setuptools=33.1.1-1 -y \
 && ln -s /usr/bin/python3 /usr/bin/python \
 && easy_install3 pip py4j \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   http://blog.stuart.axelbrooke.com/python-3-on-spark-return-of-the-pythonhashseed
ENV PYTHONHASHSEED="0"
ENV PYTHONIOENCODING="UTF-8"
ENV PIP_DISABLE_PIP_VERSION_CHECK="1"
#   JAVA
ARG JAVA_MAJOR_VERSION=8
ARG JAVA_UPDATE_VERSION=181
ARG JAVA_BUILD_NUMBER=13
ENV JAVA_HOME="/usr/jdk1.${JAVA_MAJOR_VERSION}.0_${JAVA_UPDATE_VERSION}"
ENV PATH="$PATH:$JAVA_HOME/bin"
RUN curl -sL --retry 3 --insecure --header "Cookie: oraclelicense=accept-securebackup-cookie;" "http://download.oracle.com/otn-pub/java/jdk/${JAVA_MAJOR_VERSION}u${JAVA_UPDATE_VERSION}-b${JAVA_BUILD_NUMBER}/96a7b8442fe848ef90c96a2fad6ed6d1/server-jre-${JAVA_MAJOR_VERSION}u${JAVA_UPDATE_VERSION}-linux-x64.tar.gz" | gunzip | tar x -C /usr/ \
 && ln -s $JAVA_HOME /usr/java \
 && rm -rf $JAVA_HOME/man
#   HADOOP
ENV HADOOP_VERSION="2.7.3"
ENV HADOOP_HOME="/usr/hadoop-$HADOOP_VERSION"
ENV HADOOP_CONF_DIR="$HADOOP_HOME/etc/hadoop"
ENV PATH="$PATH:$HADOOP_HOME/bin"
RUN curl -sL --retry 3 "http://archive.apache.org/dist/hadoop/common/hadoop-$HADOOP_VERSION/hadoop-$HADOOP_VERSION.tar.gz" | gunzip | tar -x -C /usr/ \
 && rm -rf $HADOOP_HOME/share/doc \
 && chown -R root:root $HADOOP_HOME
#   SPARK
ENV SPARK_VERSION="2.3.1"
ENV SPARK_PACKAGE="spark-${SPARK_VERSION}-bin-without-hadoop"
ENV SPARK_HOME="/usr/spark-${SPARK_VERSION}"
ENV SPARK_DIST_CLASSPATH="$HADOOP_HOME/etc/hadoop/*:$HADOOP_HOME/share/hadoop/common/lib/*:$HADOOP_HOME/share/hadoop/common/*:$HADOOP_HOME/share/hadoop/hdfs/*:$HADOOP_HOME/share/hadoop/hdfs/lib/*:$HADOOP_HOME/share/hadoop/hdfs/*:$HADOOP_HOME/share/hadoop/yarn/lib/*:$HADOOP_HOME/share/hadoop/yarn/*:$HADOOP_HOME/share/hadoop/mapreduce/lib/*:$HADOOP_HOME/share/hadoop/mapreduce/*:$HADOOP_HOME/share/hadoop/tools/lib/*"
ENV PATH="$PATH:${SPARK_HOME}/bin"
RUN curl -sL --retry 3 "https://www.apache.org/dyn/mirrors/mirrors.cgi?action=download&filename=spark/spark-${SPARK_VERSION}/${SPARK_PACKAGE}.tgz" | gunzip | tar x -C /usr/ \
 && mv /usr/$SPARK_PACKAGE $SPARK_HOME \
 && chown -R root:root $SPARK_HOME
WORKDIR $SPARK_HOME
CMD ["bin/spark-class", "org.apache.spark.deploy.master.Master"]
#   Python packages
RUN set -ex \
 && buildDeps=' libpython3-dev build-essential pkg-config gfortran ' \
 && apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20200601~deb9u2 wget=1.18-5+deb9u3 libblas-dev=3.7.0-2 libatlas-dev=3.10.3-1+b1 liblapack-dev=3.7.0-2 libopenblas-dev=0.2.19-3 libpng-dev=1.6.28-1+deb9u1 libfreetype6-dev=2.6.3-3.2+deb9u2 libxft-dev=2.3.2-1+b2 python3-tk=3.5.3-1 libxml2-dev=2.9.4+dfsg1-2.2+deb9u7 libxslt-dev zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 $buildDeps -y \
 && packages=' numpy pandasql matplotlib scipy ' \
 && pip3 install $packages \
 && rm -rf /root/.cache/pip \
 && apt-get purge -y --auto-remove $buildDeps \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   R and its packages
RUN set -ex \
 && rDeps=' r-base r-base-dev r-cran-evaluate ' \
 && apt-get update \
 && apt-get install --no-install-recommends $rDeps -y \
 && apt-get install --no-install-recommends r-base=3.3.3-1 r-base-dev=3.3.3-1 -y \
 && R -e "install.packages('knitr', repos='http://cran.us.r-project.org')" \
 && R -e "install.packages('ggplot2', repos='http://cran.us.r-project.org')" \
 && R -e "install.packages('googleVis', repos='http://cran.us.r-project.org')" \
 && R -e "install.packages('data.table', repos='http://cran.us.r-project.org')" \
 && apt-get install --no-install-recommends libcurl4-gnutls-dev=7.52.1-5+deb9u16 libssl-dev=1.1.0l-1~deb9u6 -y \
 && R -e "install.packages('devtools', repos='http://cran.us.r-project.org')" \
 && R -e "install.packages('Rcpp', repos='http://cran.us.r-project.org')" \
 && Rscript -e "library('devtools'); library('Rcpp'); install_github('ramnathv/rCharts')" \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Zeppelin
ENV ZEPPELIN_PORT="8080"
ENV ZEPPELIN_HOME="/usr/zeppelin"
ENV ZEPPELIN_CONF_DIR="$ZEPPELIN_HOME/conf"
ENV ZEPPELIN_NOTEBOOK_DIR="$ZEPPELIN_HOME/notebook"
ENV Z_VERSION="0.8.0"
RUN echo '{ "allow_root": true }' > /root/.bowerrc
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg=2.1.18-8~deb9u4 -y \
 && curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN set -ex \
 && additionalDeps=' git bzip2 nodejs npm libfontconfig ' \
 && apt-get update \
 && apt-get install --no-install-recommends $additionalDeps -y \
 && wget -O /tmp/zeppelin-${Z_VERSION}-bin-all.tgz http://archive.apache.org/dist/zeppelin/zeppelin-${Z_VERSION}/zeppelin-${Z_VERSION}-bin-all.tgz \
 && tar -zxvf /tmp/zeppelin-${Z_VERSION}-bin-all.tgz \
 && rm -rf /tmp/zeppelin-${Z_VERSION}-bin-all.tgz \
 && mv ./zeppelin-${Z_VERSION}-bin-all ${ZEPPELIN_HOME}
RUN mkdir -p $ZEPPELIN_HOME/run
#   Mesos
RUN echo "deb http://ftp.debian.org/debian jessie-backports main" >> /etc/apt/sources.list
RUN apt-get update -q \
 && apt-get install --no-install-recommends libssl1.0.0 -y
RUN echo 'deb http://repos.mesosphere.com/ubuntu xenial main' >> /etc/apt/sources.list.d/mesosphere.list
RUN apt-get install --no-install-recommends dirmngr=2.1.18-8~deb9u4 \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv E56151BF \
 && apt-get update -y \
 && apt-get install --no-install-recommends mesos=1.3.0-2.0.3 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
WORKDIR $ZEPPELIN_HOME
CMD ["bin/zeppelin.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
