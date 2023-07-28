FROM ubuntu:16.04
MAINTAINER ramon@wartala.de
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.1ubuntu2 curl=7.47.0-1ubuntu2.19 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 libatlas-base-dev=3.10.2-9 libboost-all-dev=1.58.0.1ubuntu1 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libgflags-dev=2.1.2-3 libgoogle-glog-dev=0.3.4-0.1 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libleveldb-dev=1.18-5 liblmdb-dev=0.9.17-3 libopencv-dev=2.4.9.1+dfsg-1.5ubuntu1.1 libpng12-dev=1.2.54-1ubuntu1.1 libzmq3-dev=4.1.4-7ubuntu0.1 libprotobuf-dev=2.6.1-1.3 libsnappy-dev=1.1.3-2 pkg-config=0.29.1-0ubuntu1 protobuf-compiler=2.6.1-1.3 rsync=3.1.1-3ubuntu1.3 software-properties-common=0.96.20.10 unzip=6.0-20ubuntu1.1 groff=1.22.3-7 vim=2:7.4.1689-3ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 python-pydot=1.0.28-2 -y ) \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
RUN curl -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
#  RUN pip --no-cache-dir install \
#  		pyopenssl \
#  		ndg-httpsclient \
#  		pyasn1
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends python-numpy=1:1.11.0-1ubuntu1 python-scipy=0.17.0-1 python-nose=1.3.7-1 python-h5py=2.6.0-1 python-skimage=0.10.1-2build1 python-matplotlib=1.5.1-1ubuntu1 python-pandas=0.17.1-3ubuntu2 python-sklearn=0.17.0-4 python-sympy=0.7.6.1-1 python-skimage=0.10.1-2build1 python-progressbar=2.3-2 -y ) \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
RUN pip install ipython==8.12.0 --no-cache-dir --upgrade \
 && pip install Cython==0.29.34 ipykernel==6.22.0 jupyter==1.0.0 path.py==12.5.0 Pillow==9.5.0 pygments==2.15.0 six==1.16.0 sphinx==6.1.3 wheel==0.40.0 zmq==0.0.0 protobuf==4.22.3 future==0.18.3 --no-cache-dir \
 && python -m ipykernel.kernelspec
#   Install Caffe
RUN echo "Installiere Caffe..."
ENV CAFFE_ROOT="/opt/caffe"
WORKDIR $CAFFE_ROOT
#   FIXME: use ARG instead of ENV once DockerHub supports this
#   https://github.com/docker/hub-feedback/issues/460
ENV CLONE_TAG="1.0"
RUN git clone -b ${CLONE_TAG} --depth 1 https://github.com/BVLC/caffe.git . \
 && cd python \
 && for req in $( cat requirements.txt ;) pydot; do pip install $req ; done \
 && cd .. \
 && mkdir build \
 && cd build \
 && cmake -DCPU_ONLY=1 .. \
 && make -j"$( nproc ;)"
#   Install Caffe2
RUN echo "Installiere Caffe2..."
ENV CAFFE2_ROOT="/opt"
WORKDIR $CAFFE2_ROOT
RUN cd $CAFFE2_ROOT
RUN git clone https://github.com/caffe2/caffe2.git \
 && cd caffe2 \
 && git submodule deinit -f third_party/cub \
 && rm -rf .git/modules/third_party/cub \
 && git rm -f third_party/cub \
 && git submodule update --recursive --remote \
 && git submodule update --init \
 && mkdir build \
 && cd build \
 && cmake .. -DUSE_CUDA=OFF -DUSE_NNPACK=OFF -DUSE_ROCKSDB=OFF \
 && make -j"$( nproc ;)" install \
 && ldconfig \
 && cd .. RUN pip install --upgrade pip \
 && pip install numpy==1.24.2 protobuf==4.22.3 hypothesis==6.72.0 tflearn==0.5.0
#  ######### INSTALLATION STEPS ###################
#  RUN cd caffe2 && \
#      make && \
#      cd build && \
#      make install
ENV PYTHONPATH="/usr/local"
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
RUN python -c 'from caffe2.python import core' 2> /dev/null \
 && echo "Success" || echo "Failure"
#   Install TensorFlow
RUN echo "Installiere TensorFlow..."
ARG TENSORFLOW_VERSION=1.1.0
ARG TENSORFLOW_ARCH=cpu
#   Install TensorFlow
RUN pip install https://storage.googleapis.com/tensorflow/linux/${TENSORFLOW_ARCH}/tensorflow-${TENSORFLOW_VERSION}-cp27-none-linux_x86_64.whl --no-cache-dir
#   Install Java
RUN echo "Installiere Java 8..."
ARG JAVA_MAJOR_VERSION=8
ARG JAVA_UPDATE_VERSION=131
ARG JAVA_BUILD_NUMBER=11
ENV JAVA_HOME="/usr/jdk1.${JAVA_MAJOR_VERSION}.0_${JAVA_UPDATE_VERSION}"
ENV PATH="$PATH:$JAVA_HOME/bin"
RUN curl -sL --retry 3 --insecure --header "Cookie: oraclelicense=accept-securebackup-cookie;" "http://download.oracle.com/otn-pub/java/jdk/${JAVA_MAJOR_VERSION}u${JAVA_UPDATE_VERSION}-b${JAVA_BUILD_NUMBER}/d54c1d3a095b4ff2b6607d096fa80163/server-jre-${JAVA_MAJOR_VERSION}u${JAVA_UPDATE_VERSION}-linux-x64.tar.gz" | gunzip | tar x -C /usr/ \
 && ln -s $JAVA_HOME /usr/java \
 && rm -rf $JAVA_HOME/man
#   Install HADOOP
RUN echo "Installiere Hadoop..."
ENV HADOOP_VERSION="2.7.3"
ENV HADOOP_HOME="/usr/hadoop-$HADOOP_VERSION"
ENV HADOOP_CONF_DIR="$HADOOP_HOME/etc/hadoop"
ENV PATH="$PATH:$HADOOP_HOME/bin"
RUN curl -sL --retry 3 "http://archive.apache.org/dist/hadoop/common/hadoop-$HADOOP_VERSION/hadoop-$HADOOP_VERSION.tar.gz" | gunzip | tar -x -C /usr/ \
 && rm -rf $HADOOP_HOME/share/doc \
 && chown -R root:root $HADOOP_HOME
#   Install SPARK
RUN echo "Installiere Spark..."
ENV SPARK_VERSION="2.1.1"
ENV SPARK_PACKAGE="spark-${SPARK_VERSION}-bin-without-hadoop"
ENV SPARK_HOME="/usr/spark-${SPARK_VERSION}"
ENV SPARK_DIST_CLASSPATH="$HADOOP_HOME/etc/hadoop/*:$HADOOP_HOME/share/hadoop/common/lib/*:$HADOOP_HOME/share/hadoop/common/*:$HADOOP_HOME/share/hadoop/hdfs/*:$HADOOP_HOME/share/hadoop/hdfs/lib/*:$HADOOP_HOME/share/hadoop/hdfs/*:$HADOOP_HOME/share/hadoop/yarn/lib/*:$HADOOP_HOME/share/hadoop/yarn/*:$HADOOP_HOME/share/hadoop/mapreduce/lib/*:$HADOOP_HOME/share/hadoop/mapreduce/*:$HADOOP_HOME/share/hadoop/tools/lib/*"
ENV PATH="$PATH:${SPARK_HOME}/bin"
RUN curl -sL --retry 3 "http://d3kbcqa49mib13.cloudfront.net/${SPARK_PACKAGE}.tgz" | gunzip | tar x -C /usr/ \
 && mv /usr/$SPARK_PACKAGE $SPARK_HOME \
 && chown -R root:root $SPARK_HOME
#   Install BigDL
RUN echo "Installiere BigDL..."
RUN pip install BigDL==0.2.0 seaborn==0.12.2 wordcloud==1.8.2.2 --no-cache-dir
#   Install awscli
RUN echo "Installiere awscli..."
RUN pip install awscli==1.27.114 --no-cache-dir
#   Install google cloud SDK
RUN echo "Installiere Google Cloud SDK..."
ENV CLOUD_SDK_VERSION="165.0.0"
RUN apt-get update -qqy \
 && (apt-get update ;apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 gcc=4:5.3.1-1ubuntu1 python-dev=2.7.12-1~16.04 python-setuptools=20.7.0-1 apt-transport-https=1.2.35 lsb-release=9.20160110ubuntu0.2 openssh-client=1:7.2p2-4ubuntu2.10 git=1:2.7.4-0ubuntu1.10 -qqy ) \
 && easy_install -U pip \
 && pip install crcmod==1.7 -U \
 && export CLOUD_SDK_REPO="cloud-sdk-$( lsb_release -c -s ;)" \
 && echo "deb https://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" > /etc/apt/sources.list.d/google-cloud-sdk.list \
 && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends google-cloud-sdk-app-engine-python google-cloud-sdk-app-engine-java google-cloud-sdk-app-engine-go google-cloud-sdk-datalab google-cloud-sdk-datastore-emulator google-cloud-sdk-pubsub-emulator google-cloud-sdk-bigtable-emulator google-cloud-sdk-cbt kubectl google-cloud-sdk=${CLOUD_SDK_VERSION}-0 -y ) \
 && gcloud config set core/disable_usage_reporting true \
 && gcloud config set component_manager/disable_update_check true \
 && gcloud config set metrics/environment github_docker_image
#   Install SparkNet
RUN echo "Installiere SparkNet..."
WORKDIR /opt
RUN git clone https://github.com/amplab/SparkNet.git
ENV SPARKNET_HOME="/opt/SparkNet"
WORKDIR $SPARKNET_HOME
RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends sbt )
#   Set up our notebook config.
COPY jupyter_notebook_config.py /root/.jupyter/
#   Copy sample notebooks.
RUN mkdir /root/notebooks
RUN mkdir /root/notebooks/kapitel1
RUN mkdir /root/notebooks/kapitel2
RUN mkdir /root/notebooks/kapitel3
RUN mkdir /root/notebooks/kapitel4
RUN mkdir /root/notebooks/kapitel5
RUN mkdir /root/notebooks/kapitel6
RUN mkdir /root/notebooks/kapitel7
RUN mkdir /root/notebooks/kapitel8
#  COPY notebooks/kapitel1/* /root/notebooks/kapitel1/
#  COPY notebooks/kapitel2/* /root/notebooks/kapitel2/
#  COPY notebooks/kapitel3/* /root/notebooks/kapitel3/
COPY notebooks/kapitel4/* /root/notebooks/kapitel4/
COPY notebooks/kapitel5/* /root/notebooks/kapitel5/
COPY notebooks/kapitel6/* /root/notebooks/kapitel6/
#  COPY notebooks/kapitel7/* /root/notebooks/kapitel7/
#  COPY notebooks/kapitel8/* /root/notebooks/kapitel8/
#   Jupyter has issues with being run directly:
#     https://github.com/ipython/ipython/issues/7062
#   We just add a little wrapper script.
COPY run_jupyter.sh /root/
#   TensorBoard port
RUN echo "TensorBoard nutzt Port 6006..."
EXPOSE 6006/tcp
RUN echo "Jupyter Notebook startet auf Port 8888..."
EXPOSE 8888/tcp
RUN echo "SparkUI nutzt Port 4040 und 4041..."
EXPOSE 4040/tcp
EXPOSE 4041/tcp
WORKDIR "/root"
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
