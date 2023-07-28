#   Dockerfile for building general development
#   environment for Data Science Analytics
#   customized for TVB Big Data Team
FROM ubuntu:16.04
LABEL maintainer="\"michaelchan_wahyan@yahoo.com.hk\""
ENV SHELL="/bin/bash" \
    TZ="Asia/Hong_Kong" \
    PYTHONIOENCODING="UTF-8" \
    AIRFLOW_HOME="/opt/airflow" \
    AIRFLOW_GPL_UNIDECODE="yes" \
    CLOUD_SDK_REPO="cloud-sdk-xenial" \
    HADOOP_COMMON_HOME="/hadoop-2.7.7" \
    HADOOP_HDFS_HOME="/hadoop-2.7.7" \
    HADOOP_HOME="/hadoop-2.7.7" \
    HADOOP_CONF_DIR="/hadoop-2.7.7/etc/hadoop" \
    HADOOP_COMMON_LIB_NATIVE_DIR="/hadoop-2.7.7/lib/native" \
    HADOOP_INSTALL="/hadoop-2.7.7" \
    HADOOP_MAPRED_HOME="/hadoop-2.7.7" \
    JAVA_HOME="/jdk1.8.0_171" \
    PYSPARK_DRIVER_PYTHON="jupyter" \
    PYSPARK_DRIVER_PYTHON_OPTS="notebook" \
    PYSPARK_PYTHON="python3" \
    SPARK_HOME="/spark-2.4.0-bin-hadoop2.7" \
    SPARK_PATH="/spark-2.4.0-bin-hadoop2.7" \
    YARN_HOME="/hadoop-2.7.7" \
    PATH="$PATH:/root/anaconda/bin:/bin:/usr/local/sbin:/usr/local/bin:/usr/local/lib:/usr/lib:/usr/sbin:/usr/bin:/sbin:/bin:/hadoop-2.7.7/sbin:/hadoop-2.7.7/bin"
#   ========================
#   Jupyter Lab installation
#   ========================
#   ref : https://github.com/mikebirdgeneau/jupyterlab-docker/blob/master/jupyterlab/Dockerfile
#   for pip3 installation on jupyterlab related packages :
#   ipywidgets   nbextension   jupyterlab
#   ==================
#   SPARK installation
#   ==================
#   ref : https://medium.com/@GalarnykMichael/install-spark-on-ubuntu-pyspark-231c45677de0
#   SPARK installation - Part 1 (conda)
#   SPARK installation - Part 2 (spark)
#   SPARK installation - Part 3 (jdk 8.171)
#   SPARK installation - Part 4 (hadoop 2.7.6)
#   =============================
#   Google Cloud SDK installation
#   =============================
#   ref : https://cloud.google.com/sdk/docs/quickstart-debian-ubuntu
#      many more gcloud packages that could be installed
#      google-cloud-sdk-app-engine-python
#      google-cloud-sdk-app-engine-python-extras
#      google-cloud-sdk-app-engine-java
#      google-cloud-sdk-app-engine-go
#      google-cloud-sdk-datalab
#      google-cloud-sdk-datastore-emulator
#      google-cloud-sdk-pubsub-emulator
#      google-cloud-sdk-cbt
#      google-cloud-sdk-bigtable-emulator
#      kubectl
COPY jdk-8u171-linux-x64.tar.gz /
RUN apt-get update -y ; apt-get -y upgrade ; apt-get install --no-install-recommends screen=4.3.1-2ubuntu0.1 apt-utils=1.2.35 cmake=3.5.1-1ubuntu3 htop=2.0.1-1ubuntu1 wget=1.17.1-1ubuntu1.5 vim=2:7.4.1689-3ubuntu1.5 nano=2.5.3-2ubuntu2 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 software-properties-common=0.96.20.10 apt-transport-https=1.2.35 net-tools=1.60-26ubuntu1 wget=1.17.1-1ubuntu1.5 cowsay=3.03+dfsg1-15 fortune sl=3.03-17build1 -y ; add-apt-repository 'deb [arch=amd64,i386] https://cran.rstudio.com/bin/linux/ubuntu xenial/' ; apt-get update -y ; add-apt-repository ppa:jonathonf/python-3.6 ; apt-get update -y ; apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 ; apt-get update -y ; tar -zxvf jdk-8u171-linux-x64.tar.gz ; rm -f jdk-8u171-linux-x64.tar.gz ; wget https://archive.apache.org/dist/spark/spark-2.4.0/spark-2.4.0-bin-hadoop2.7.tgz ; tar -zxvf spark-2.4.0-bin-hadoop2.7.tgz ; rm -f spark-2.4.0-bin-hadoop2.7.tgz ; wget https://archive.apache.org/dist/hadoop/core/hadoop-2.7.7/hadoop-2.7.7.tar.gz ; tar -zxvf hadoop-2.7.7.tar.gz ; rm -f hadoop-2.7.7.tar.gz ; mkdir /gcs-connector-hadoop ; echo "deb https://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list ; curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - ; apt-get update -y ; apt-get install --no-install-recommends google-cloud-sdk -y ; wget https://storage.googleapis.com/hadoop-lib/gcs/gcs-connector-latest-hadoop2.jar ; mv gcs-connector-latest-hadoop2.jar /gcs-connector-hadoop/ ; echo export HADOOP_CLASSPATH=/gcs-connector-hadoop/gcs-connector-latest-hadoop2.jar >> /hadoop-2.7.7/etc/hadoop/hadoop-env.sh; echo spark.driver.extraClassPath /gcs-connector-hadoop/gcs-connector-latest-hadoop2.jar >> $SPARK_HOME/conf/spark-defaults.conf; echo spark.driver.memory 5g >> $SPARK_HOME/conf/spark-defaults.conf; echo spark.driver.maxResultSize 5g >> $SPARK_HOME/conf/spark-defaults.conf; echo spark.driver.allowMultipleContexts True >> $SPARK_HOME/conf/spark-defaults.conf
RUN apt-get install --no-install-recommends libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libssl-dev=1.0.2g-1ubuntu4.20 libeigen3-dev=3.3~beta1-2 libgmp-dev=2:6.1.0+dfsg-2 libgmpxx4ldbl=2:6.1.0+dfsg-2 libmpfr-dev=3.1.4-1 libboost-dev=1.58.0.1ubuntu1 libboost-thread-dev=1.58.0.1ubuntu1 libtbb-dev=4.4~20151115-0ubuntu3 libeigen3-dev=3.3~beta1-2 libgmp-dev=2:6.1.0+dfsg-2 libgmpxx4ldbl=2:6.1.0+dfsg-2 libmpfr-dev=3.1.4-1 libboost-dev=1.58.0.1ubuntu1 libboost-thread-dev=1.58.0.1ubuntu1 libtbb-dev=4.4~20151115-0ubuntu3 -y ; apt-get update -y ; apt-get install --no-install-recommends r-base=3.2.3-4 bc=1.06.95-9build1 npm=3.5.2-0ubuntu4 ca-certificates=20210119~16.04.1 musl-dev=1.1.9-1 gcc=4:5.3.1-1ubuntu1 make=4.1-6 g++=4:5.3.1-1ubuntu1 gfortran=4:5.3.1-1ubuntu1 python3.6 -y ; curl https://bootstrap.pypa.io/get-pip.py | python3.6 ; rm -f /usr/bin/python3 \
 && ln -s /usr/bin/python3.6 /usr/bin/python3 ; rm -f /usr/bin/python3m \
 && ln -s /usr/bin/python3.6m /usr/bin/python3m ; apt-get install --no-install-recommends python3.6-dev -y ; apt-get -y upgrade ; apt-get install --no-install-recommends python3.6-tk -y
#   jupyter(lab) related python packages are
#   required before installing interactive R kernel
COPY requirements0.txt requirements1.txt requirements2.txt requirements3.txt requirements4.txt requirements5.txt /
RUN pip3 install -r requirements0.txt ; R -e 'install.packages(c("devtools", "bayesAB", "plyr", "dplyr", "data.table", "bigrquery", "pwr", "cowsay", "fortunes", "progress", "ggplot2", "forecast"))' ; R -e 'devtools::install_github("IRkernel/IRkernel")' ; R -e 'IRkernel::installspec()' ; pip3 install -r requirements1.txt ; pip3 install -r requirements2.txt ; pip3 install -r requirements3.txt ; pip3 install -r requirements4.txt ; pip3 install -r requirements5.txt
RUN jupyter nbextension enable --py widgetsnbextension ; jupyter serverextension enable --py jupyterlab
#  jupyter labextension install @jupyterlab/latex
#   info to hadoop                 <-- HADOOP_CLASSPATH
#   info to spark                  <-- spark.driver.extraClassPath
#   max mem consumed per core      <-- spark.driver.memory
#   prevent rdd.collect() exceed   <-- spark.driver.maxResultSize
#   RUN pip3 install git+https://github.com/michaelchanwahyan/nbparameterise.git
COPY .bashrc .vimrc /root/
COPY core-site.xml $HADOOP_CONF_DIR
COPY app_template /
COPY airflow /opt/airflow
EXPOSE 9090/tcp 9999/tcp
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
