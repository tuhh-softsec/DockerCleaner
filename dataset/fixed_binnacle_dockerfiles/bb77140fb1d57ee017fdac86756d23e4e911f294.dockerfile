FROM ubuntu:16.04
MAINTAINER {{mantainer.email}}
ENV SLEEP_MILLIS="0"
USER root
#  #############################################################
#   Define all environment variables to be used 
#  #############################################################
ENV MARVIN_HOME="/opt/marvin"
ENV MARVIN_DATA_PATH="/marvin-data"
ENV MARVIN_ENGINE_HOME="$MARVIN_HOME/engine"
ENV MARVIN_ENGINE_ENV="marvin-engine-env"
ENV WORKON_HOME="$MARVIN_HOME/.virtualenvs"
ENV SPARK_HOME="/opt/spark"
ENV SPARK_CONF_DIR="$SPARK_HOME/conf"
ENV HADOOP_CONF_DIR="$SPARK_CONF_DIR"
ENV YARN_CONF_DIR="$SPARK_CONF_DIR"
#  #############################################################
#   Create all folders needed 
#  #############################################################
RUN mkdir -p $MARVIN_HOME \
 && mkdir -p $MARVIN_DATA_PATH \
 && mkdir -p $MARVIN_ENGINE_HOME \
 && mkdir -p /var/log/marvin/engines \
 && mkdir -p /var/run/marvin/engines
#  #############################################################
#   Install the system dependencies for default installation 
#  #############################################################
RUN apt-get update -y \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 -y \
 && apt-get install --no-install-recommends maven=3.3.9-3 git=1:2.7.4-0ubuntu1.10 python=2.7.12-1~16.04 cmake=3.5.1-1ubuntu3 software-properties-common=0.96.20.10 curl=7.47.0-1ubuntu2.19 libstdc++6=5.4.0-6ubuntu1~16.04.12 -y \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 -y \
 && apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 -y \
 && apt-get install --no-install-recommends python2.7-dev=2.7.12-1ubuntu0~16.04.18 -y \
 && apt-get install --no-install-recommends python-pip=8.1.1-2ubuntu0.6 -y \
 && apt-get install --no-install-recommends ipython=2.4.1-1 -y \
 && apt-get install --no-install-recommends libffi-dev=3.2.1-4 -y \
 && apt-get install --no-install-recommends libssl-dev=1.0.2g-1ubuntu4.20 -y \
 && apt-get install --no-install-recommends libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 -y \
 && apt-get install --no-install-recommends libxslt1-dev=1.1.28-2.1ubuntu0.3 -y \
 && apt-get install --no-install-recommends libpng12-dev=1.2.54-1ubuntu1.1 -y \
 && apt-get install --no-install-recommends libfreetype6-dev=2.6.1-0.1ubuntu2.5 -y \
 && apt-get install --no-install-recommends python-tk=2.7.12-1~16.04 -y \
 && apt-get install --no-install-recommends libsasl2-dev=2.1.26.dfsg1-14ubuntu0.2 -y \
 && apt-get install --no-install-recommends python-pip=8.1.1-2ubuntu0.6 -y \
 && apt-get install --no-install-recommends graphviz=2.38.0-12ubuntu2.1 -y \
 && pip install pip==23.1 --upgrade \
 && apt-get clean
RUN pip install virtualenvwrapper==4.8.4
#   Install Open JDK
RUN apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 -y
#  #############################################################
#   Install Apache Spark
#
#   Uncomment if you are using spark, note that is needed the 
#   spark configuration files to the think works correctly.
#  #############################################################
RUN mkdir -p $SPARK_CONF_DIR
#  #############################################################
#   Create the virtualenv configuration
#  #############################################################
RUN /bin/bash -c "cd $MARVIN_ENGINE_HOME \
 && source /usr/local/bin/virtualenvwrapper.sh \
 && mkvirtualenv $MARVIN_ENGINE_ENV"
#  #############################################################
#          <CUSTOM ENGINE INSTALLATION PROCEDURE HERE>         #
#  #############################################################
#  #############################################################
#   Copy and Install the marvin engine inside virtualenv
#  #############################################################
COPY build/engine.tar $MARVIN_ENGINE_HOME
COPY build/marvin-engine-executor-assembly.jar $MARVIN_DATA_PATH
RUN /bin/bash -c "source /usr/local/bin/virtualenvwrapper.sh \
 && workon $MARVIN_ENGINE_ENV \
 && cd $MARVIN_ENGINE_HOME \
 && pip install . "
#  #############################################################
#   Starts the engine http server
#  #############################################################
EXPOSE 8000/tcp
CMD /bin/bash -c "source /usr/local/bin/virtualenvwrapper.sh \
 && workon $MARVIN_ENGINE_ENV \
 && cd $MARVIN_ENGINE_HOME \
 && marvin engine-httpserver -h 0.0.0.0 -p 8000"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
