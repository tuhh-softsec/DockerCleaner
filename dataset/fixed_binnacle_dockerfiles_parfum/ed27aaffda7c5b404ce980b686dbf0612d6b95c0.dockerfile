FROM ubuntu:16.04
MAINTAINER dev@marvin.apache.org
ENV SLEEP_MILLIS="0"
USER root
# #############################################################
#  Define all environment variables to be used
# #############################################################
ENV MARVIN_HOME="/opt/marvin"
ENV MARVIN_DATA_PATH="/marvin-data"
ENV MARVIN_ENGINE_HOME="$MARVIN_HOME/engine"
ENV MARVIN_ENGINE_ENV="marvin-engine-env"
ENV WORKON_HOME="$MARVIN_HOME/.virtualenvs"
ENV SPARK_HOME="/opt/spark"
ENV SPARK_CONF_DIR="$SPARK_HOME/conf"
ENV HADOOP_CONF_DIR="$SPARK_CONF_DIR"
ENV YARN_CONF_DIR="$SPARK_CONF_DIR"
# #############################################################
#  Create all folders needed
# #############################################################
RUN mkdir -p $MARVIN_HOME \
 && mkdir -p $MARVIN_DATA_PATH \
 && mkdir -p $MARVIN_ENGINE_HOME \
 && mkdir -p /var/log/marvin/engines \
 && mkdir -p /var/run/marvin/engines
# #############################################################
#  Install the system dependencies for default installation
# #############################################################
RUN apt-get update -y \
 && apt-get install --no-install-recommends build-essential -y \
 && apt-get install --no-install-recommends maven git python cmake software-properties-common curl libstdc++6 -y \
 && apt-get install --no-install-recommends git -y \
 && apt-get install --no-install-recommends wget -y \
 && apt-get install --no-install-recommends python2.7-dev -y \
 && apt-get install --no-install-recommends python-pip -y \
 && apt-get install --no-install-recommends ipython -y \
 && apt-get install --no-install-recommends libffi-dev -y \
 && apt-get install --no-install-recommends libssl-dev -y \
 && apt-get install --no-install-recommends libxml2-dev -y \
 && apt-get install --no-install-recommends libxslt1-dev -y \
 && apt-get install --no-install-recommends libpng12-dev -y \
 && apt-get install --no-install-recommends libfreetype6-dev -y \
 && apt-get install --no-install-recommends python-tk -y \
 && apt-get install --no-install-recommends libsasl2-dev -y \
 && apt-get install --no-install-recommends python-pip -y \
 && apt-get install --no-install-recommends graphviz -y \
 && pip install pip --upgrade \
 && apt-get clean
RUN pip install virtualenvwrapper
#  Install Oracle JDK
RUN add-apt-repository ppa:webupd8team/java -y \
 && apt-get update -qq \
 && echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections \
 && apt-get install --no-install-recommends oracle-java8-installer -y
# #############################################################
#  Install Apache Spark
#
#  Uncomment if you are using spark, note that is needed the
#  spark configuration files to the think works correctly.
# #############################################################
RUN mkdir -p $SPARK_CONF_DIR
# #############################################################
#  Create the virtualenv configuration
# #############################################################
RUN /bin/bash -c "cd $MARVIN_ENGINE_HOME \
 && source /usr/local/bin/virtualenvwrapper.sh \
 && mkvirtualenv $MARVIN_ENGINE_ENV"
# #############################################################
#         <CUSTOM ENGINE INSTALLATION PROCEDURE HERE>         #
# #############################################################
# #############################################################
#  Copy and Install the marvin engine inside virtualenv
# #############################################################
ADD build/engine.tar $MARVIN_ENGINE_HOME
COPY build/marvin-engine-executor-assembly.jar $MARVIN_DATA_PATH
RUN /bin/bash -c "source /usr/local/bin/virtualenvwrapper.sh \
 && workon $MARVIN_ENGINE_ENV \
 && cd $MARVIN_ENGINE_HOME \
 && pip install . --process-dependency-links"
# #############################################################
#  Starts the engine http server
# #############################################################
EXPOSE 8000/tcp
CMD /bin/bash -c "source /usr/local/bin/virtualenvwrapper.sh \
 && workon $MARVIN_ENGINE_ENV \
 && cd $MARVIN_ENGINE_HOME \
 && marvin engine-httpserver -h 0.0.0.0 -p 8000"
