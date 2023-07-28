FROM jupyter/scipy-notebook
MAINTAINER Jay Johnson <jay.p.h.johnson@gmail.com>
USER root
#   Install all OS dependencies for notebook server that starts but lacks all
#   features (e.g., download as all possible file formats)
ENV DEBIAN_FRONTEND="noninteractive"
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends jed=1:0.99.20~pre.178+dfsg-1 emacs=1:28.2+1-13ubuntu3 build-essential=12.9ubuntu3 python-dev python-setuptools unzip=6.0-27ubuntu1 libsm6=2:1.2.3-1build2 pandoc=2.17.1.1-1.1ubuntu1 texlive-latex-base=2022.20230122-2 texlive-latex-extra=2022.20230122-2 texlive-fonts-extra=2022.20230122-2 texlive-fonts-recommended=2022.20230122-2 texlive-generic-recommended libxrender1=1:0.9.10-1.1 julia libnettle4 git=1:2.39.2-1ubuntu1 sqlite=2.8.17-15fakesync1build1 vim=2:9.0.1000-4ubuntu2 wget=1.21.3-1ubuntu1 mlocate=1.1.18-1ubuntu1 cron=3.0pl1-151ubuntu1 rsyslog=8.2302.0-1ubuntu2 logrotate=3.21.0-1 gcc=4:12.2.0-3ubuntu1 telnet=0.17+2.4-2ubuntu1 tree=2.1.0-1 curl=7.88.1-7ubuntu1 tar=1.34+dfsg-1.1 net-tools=2.10-0.1ubuntu3 mariadb-server=1:10.11.2-1 libmysqlclient-dev=8.0.32-0ubuntu4 fonts-dejavu=2.37-6 gfortran=4:12.2.0-3ubuntu1 libav-tools libcurl4-openssl-dev=7.88.1-7ubuntu1 libssl-dev=3.0.8-1ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt1-dev=1.1.35-1 libpcap-dev=1.10.3-1 libsqlite3-dev=3.40.1-1 libattr1-dev=1:2.5.1-4 libffi-dev=3.4.4-1 -yq ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends libatlas-base-dev=3.10.3-13ubuntu1 libopenblas-dev=0.3.21+ds-4 libopencv-dev=4.6.0+dfsg-11 libprotobuf-dev=3.21.12-1ubuntu7 liblapack-dev=3.11.0-2 libleveldb-dev=1.23-4 protobuf-compiler=3.21.12-1ubuntu7 libsnappy-dev=1.1.9-3 libboost-all-dev=1.74.0.3ubuntu7 -yq ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends libgflags-dev=2.2.2-2 libgoogle-glog-dev=0.6.0-2 liblmdb-dev=0.9.24-1build2 -yq ) \
 && apt-get clean
RUN apt-get remove -y librdkafka*
#   Install the new Confluent Kafka toolchain for using their kafka client: https://github.com/confluentinc/confluent-kafka-python / http://blog.parsely.com/post/3886/pykafka-now
RUN wget -qO - http://packages.confluent.io/deb/3.0/archive.key | sudo apt-key add -
RUN echo "deb [arch=amd64] http://packages.confluent.io/deb/3.0 stable main" >> /etc/apt/sources.list
RUN apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends confluent-platform-2.11 librdkafka-dev=2.0.2-1 -y )
ENV NB_USER="jovyan"
ENV ENV_PORT="8888"
ENV ENV_PROJ_DIR="/opt/work"
ENV ENV_DATA_DIR="/opt/work/data"
ENV ENV_DATA_SRC_DIR="/opt/work/data/src"
ENV ENV_DATA_DST_DIR="/opt/work/data/dst"
ENV ENV_REDIS_HOST="localhost"
ENV ENV_REDIS_PORT="6000"
ENV ENV_REDIS_SRC_KEY="JUPYTER_SRC_KEY"
ENV ENV_REDIS_DST_KEY="JUPYTER_DST_KEY"
#   Coordinate events without changing the container
ENV ENV_SYNTHESIZE_DIR="/opt/work/data/synthesize"
ENV ENV_SYNTHESIZE_BIN="/opt/containerfiles/synthesize.sh"
ENV ENV_TIDY_DIR="/opt/work/data/tidy"
ENV ENV_TIDY_BIN="/opt/containerfiles/tidy.sh"
ENV ENV_ANALYZE_DIR="/opt/work/data/analyze"
ENV ENV_ANALYZE_BIN="/opt/containerfiles/analyze.sh"
ENV ENV_OUTPUT_DIR="/opt/work/data/output"
ENV ENV_OUTPUT_BIN="/opt/containerfiles/output-model.sh"
ENV ENV_REDIS_MODEL_OUT_BIN="/opt/containerfiles/redis-model.sh"
ENV ENV_REDIS_MODEL_DST_KEY="JUPYTER_REDIS_MODEL_DST_KEY"
#   Bin and Libs Dir
ENV ENV_BINS="/opt/work/bins"
ENV ENV_LIBS="/opt/work/libs"
#   Configuration Dir:
ENV ENV_CONFIGS_DIR="/opt/work/configs"
ENV ENV_CL_ENV_DIR="/opt/work/env"
#   Global Python Dirs:
ENV ENV_PYTHON_SRC_DIR="/opt/work/src"
ENV ENV_PYTHON_COMMON_DIR="/opt/work/src/common"
ENV ENV_PYTHON_REDIS_DIR="/opt/work/src/connectors/redis"
ENV ENV_PYTHON_DB_DIR="/opt/work/src/connectors/database"
ENV ENV_PYTHON_SCHEMA_DIR="/opt/work/src/databases/schema"
ENV ENV_PYTHON_CORE_CONFIG="/opt/work/configs/jupyter.json"
#   Slack Debugging Env:
ENV ENV_SLACK_BOTNAME="bugbot"
ENV ENV_SLACK_CHANNEL="debugging"
ENV ENV_SLACK_NOTIFY_USER="jay"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV ENV_SLACK_ENVNAME="dev-jupyter"
ENV ENV_SLACK_ENABLED="1"
#   Environment Deployment Type
ENV ENV_DEPLOYMENT_TYPE="Local"
ENV ENV_IN_DOCKER="1"
ENV ENV_DEBUG_LOADING="0"
USER root
#   Python packages for interfacing with resources outside of this container
RUN conda install --quiet --yes 'coverage' 'seaborn' 'pcre' 'six' 'pika' 'python-daemon' 'feedparser' 'pytest' 'nose' 'lxml' 'Django' 'sphinx' 'sphinx-bootstrap-theme' 'requests' 'redis=3.2.0' 'hiredis' 'redis-py' 'boto' 'awscli' 'django-redis-cache' 'uwsgi' 'PyMySQL' 'psycopg2' 'pymongo' 'SQLAlchemy' 'pandas' 'numpy' 'tqdm' 'pandas-datareader' 'tensorflow' 'alembic' 'ipywidgets' 'widgetsnbextension' 'vega' 'pyqt=4.11'
#   Add Volumes and Set permissions
RUN mkdir -p -m 777 /opt \
 && mkdir -p -m 777 /opt/containerfiles \
 && chmod 777 /opt \
 && chmod 777 /opt/containerfiles \
 && touch /tmp/firsttimerunning
#  ## Finish the setup using root
USER $NB_USER
#   Add custom Python 2 pips:
COPY ./python2/ /opt/python2
RUN /opt/python2/install_pips.sh
USER root
RUN conda install pyqt=4.11 -y
#   Configure container startup as root
EXPOSE 8888/tcp
#  ENTRYPOINT ["tini", "--"]
CMD ["/opt/containerfiles/start-container.sh"]
#  ########################################################
#
#   Add Files into the container now that the setup is done
#
#   Add the starters and installers:
COPY ./containerfiles/ /opt/containerfiles/
RUN chmod 777 /opt/containerfiles/*.sh \
 && cp /opt/containerfiles/bashrc ~/.bashrc \
 && cp /opt/containerfiles/vimrc ~/.vimrc \
 && cp /opt/containerfiles/bashrc /home/$NB_USER/.bashrc \
 && cp /opt/containerfiles/vimrc /home/$NB_USER/.vimrc \
 && chown $NB_USER /home/$NB_USER/.bashrc \
 && chown $NB_USER /home/$NB_USER/.vimrc \
 && chmod 664 /home/$NB_USER/.bashrc \
 && chmod 664 /home/$NB_USER/.vimrc
RUN echo 'export PATH=$PATH:/opt/conda/envs/python2/bin:/opt/conda/bin:/opt/work/bins' >> /root/.bashrc \
 && echo '' >> /home/$NB_USER/.bashrc \
 && echo 'if [[ "${PYTHONPATH}" == "" ]]; then' >> /root/.bashrc \
 && echo ' export PYTHONPATH=/opt/work' >> /root/.bashrc \
 && echo 'else' >> /root/.bashrc \
 && echo ' export PYTHONPATH=$PYTHONPATH:/opt/work' >> /root/.bashrc \
 && echo 'fi' >> /root/.bashrc \
 && echo '' >> /root/.bashrc \
 && echo 'source activate python2' >> /root/.bashrc \
 && echo '' >> /root/.bashrc \
 && mv /usr/bin/vi /usr/bin/bak.vi \
 && cp /usr/bin/vim /usr/bin/vi
RUN echo 'export PATH=$PATH:/opt/conda/envs/python2/bin:/opt/conda/bin:/opt/work/bins' >> /home/$NB_USER/.bashrc \
 && echo '' >> /home/$NB_USER/.bashrc \
 && echo 'if [[ "${PYTHONPATH}" == "" ]]; then' >> /home/$NB_USER/.bashrc \
 && echo ' export PYTHONPATH=/opt/work' >> /home/$NB_USER/.bashrc \
 && echo 'else' >> /home/$NB_USER/.bashrc \
 && echo ' export PYTHONPATH=$PYTHONPATH:/opt/work' >> /home/$NB_USER/.bashrc \
 && echo 'fi' >> /home/$NB_USER/.bashrc \
 && echo '' >> /home/$NB_USER/.bashrc \
 && echo 'source activate python2' >> /home/$NB_USER/.bashrc \
 && echo '' >> /home/$NB_USER/.bashrc
#   Add local files as late as possible to avoid cache busting
RUN cp /opt/containerfiles/start-notebook.sh /usr/local/bin/ \
 && cp /opt/containerfiles/start-singleuser.sh /usr/local/bin/ \
 && cp /opt/containerfiles/jupyter_notebook_config.py /home/$NB_USER/.jupyter/ \
 && mkdir -p -m 777 /opt/python2 \
 && chmod 777 /opt \
 && chown -R $NB_USER:users /opt/python2 \
 && mkdir -p -m 777 /opt/work/ \
 && chmod 777 /opt \
 && chmod 777 /opt/work \
 && chown -R $NB_USER:users /opt/work \
 && mkdir -p -m 777 /opt/work/examples \
 && mkdir -p -m 777 /opt/work/src \
 && mkdir -p -m 777 /opt/work/env \
 && mkdir -p -m 777 /opt/work/bins \
 && mkdir -p -m 777 /opt/work/libs \
 && mkdir -p -m 777 /opt/work/configs \
 && mkdir -p -m 777 /opt/work/pips \
 && mkdir -p -m 777 /opt/work/data \
 && chown -R $NB_USER:users /opt/work/examples \
 && chown -R $NB_USER:users /opt/work/src \
 && chown -R $NB_USER:users /opt/work/env \
 && chown -R $NB_USER:users /opt/work/bins \
 && chown -R $NB_USER:users /opt/work/libs \
 && chown -R $NB_USER:users /opt/work/configs \
 && chown -R $NB_USER:users /opt/work/pips \
 && chown -R $NB_USER:users /opt/work/data
WORKDIR /opt/work
COPY ./libs/ /opt/work/libs/
COPY ./configs/ /opt/work/configs/
COPY ./bins/ /opt/work/bins/
COPY ./src/ /opt/work/src/
COPY ./env/ /opt/work/env/
COPY ./examples /opt/work/examples/
#   Assign all permissions over:
RUN chown -R $NB_USER:users /opt/work/* \
 && chmod 777 /opt/work/bins/*
#  ########################################################
#
#   Run as the user
#
USER $NB_USER
#   Track the Python 2 and Python 3 pips and Conda Environment
RUN pip2 freeze > /opt/work/pips/python2-requirements.txt \
 && pip3 freeze > /opt/work/pips/python3-requirements.txt
# Please add your HEALTHCHECK here!!!
