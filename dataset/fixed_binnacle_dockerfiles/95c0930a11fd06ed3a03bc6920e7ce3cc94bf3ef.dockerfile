FROM nvidia/cuda:9.0-cudnn7-runtime
MAINTAINER CodaLab Team "codalab.worksheets@gmail.com"
#   Begin common steps (Must be the same in the CPU and GPU images)
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https iputils-ping git python2.7 python-pip python-dev python-software-properties python-tk software-properties-common build-essential cmake libhdf5-dev swig wget curl -y
#  # Python 3.6
RUN add-apt-repository ppa:deadsnakes/ppa \
 && apt-get update -y \
 && apt-get install --no-install-recommends python3.6 python3.6-venv python3.6-dev python3-software-properties -y
RUN curl https://bootstrap.pypa.io/get-pip.py | python3.6
#  ## Without this Python thinks we're ASCII and unicode chars fail
ENV LANG="C.UTF-8"
#  # Oracle JDK 11
RUN echo oracle-java11-installer shared/accepted-oracle-license-v1-2 select true | debconf-set-selections \
 && add-apt-repository -y ppa:linuxuprising/java \
 && apt-get update \
 && apt-get install --no-install-recommends oracle-java11-installer -y \
 && apt-get install --no-install-recommends oracle-java11-set-default -y \
 && rm -rf /var/cache/oracle-jdk11-installer
ENV SCALA_VERSION="2.12.6"
#  # Scala
RUN wget http://scala-lang.org/files/archive/scala-$SCALA_VERSION.deb \
 && dpkg -i scala-$SCALA_VERSION.deb \
 && echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 \
 && apt-get update -y \
 && apt-get install --no-install-recommends sbt -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
ENV SCALA_HOME="/usr/share/java"
RUN pip2 install -U pip
RUN pip2 install -U numpy scipy matplotlib pandas sympy nose spacy tqdm wheel scikit-learn scikit-image nltk
RUN python -m spacy download en
#  # Set up python3.6 environment
RUN pip3 install -U pip
RUN pip3 install -U numpy scipy matplotlib pandas sympy nose spacy tqdm wheel scikit-learn scikit-image nltk
RUN python3.6 -m spacy download en
#   End common steps
#   GPU-specific commands
RUN pip2 install -U tensorflow-gpu==1.12.0 tensorboard keras
RUN pip2 install -U torch torchvision
RUN pip3 install -U tensorflow-gpu==1.12.0 tensorboard keras
RUN pip3 install -U torch torchvision
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
