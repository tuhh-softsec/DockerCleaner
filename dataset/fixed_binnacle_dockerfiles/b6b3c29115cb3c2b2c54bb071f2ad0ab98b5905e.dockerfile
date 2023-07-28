FROM python:3.6-slim
RUN apt-get update \
 && apt-get install --no-install-recommends libcurl4-openssl-dev=7.74.0-1.3+deb11u7 net-tools=1.60+git20181103.0eebece-1 curl=7.74.0-1.3+deb11u7 wget=1.21-1+deb11u1 mlocate=0.26-5 gcc=4:10.2.1-1 make=4.3-4.1 autoconf=2.69-14 build-essential=12.9 software-properties-common=0.96.20.2-2.1 git=1:2.30.2-1+deb11u2 vim=2:8.2.2434-3+deb11u1 pandoc=2.9.2.1-1+b1 python3=3.9.2-3 python3-dev=3.9.2-3 python3-pip=20.3.4-4+deb11u1 python3-tk=3.9.2-1 python-setuptools=44.1.1-1 python-virtualenv python-pip openssl=1.1.1n-0+deb11u4 libssl-dev=1.1.1n-0+deb11u4 cmake=3.18.4-2+deb11u1 autoconf=2.69-14 libffi6 libffi-dev=3.3-6 telnet=0.17-42 netcat=1.10-46 unzip=6.0-26+deb11u1 -y
RUN echo "creating directories" \
 && mkdir -p -m 777 /opt/antinex \
 && mkdir -p -m 777 /data \
 && mkdir -p -m 777 /var/log/antinex/core \
 && mkdir -p -m 777 /var/log/antinex/api \
 && mkdir -p -m 777 /var/log/antinex/jupyter \
 && mkdir -p -m 777 /var/log/antinex/pipeline \
 && mkdir -p -m 777 /var/log/antinex/client \
 && mkdir -p -m 777 /opt/shared \
 && mkdir -p -m 777 /opt/data \
 && chmod 777 /opt
RUN echo "creating log files" \
 && touch /var/log/antinex/api/api.log \
 && chmod 777 /var/log/antinex/api/api.log \
 && touch /var/log/antinex/api/worker.log \
 && chmod 777 /var/log/antinex/api/worker.log \
 && touch /var/log/antinex/core/ai-core.log \
 && chmod 777 /var/log/antinex/core/ai-core.log \
 && touch /var/log/antinex/client/client.log \
 && chmod 777 /var/log/antinex/client/client.log \
 && touch /var/log/antinex/jupyter/jupyter.log \
 && chmod 777 /var/log/antinex/jupyter/jupyter.log \
 && touch /var/log/antinex/pipeline/pipeline.log \
 && chmod 777 /var/log/antinex/pipeline/pipeline.log \
 && touch /var/log/antinex/client/client.log \
 && chmod 777 /var/log/antinex/client/client.log
RUN echo "preparing virtualenv" \
 && pip install virtualenvwrapper==4.8.4 pip==23.1 --upgrade
RUN echo "creating virtualenv" \
 && virtualenv -p python3 /opt/venv \
 && chmod 777 /opt/venv
RUN echo "setting up virtualenv" \
 && . /opt/venv/bin/activate \
 && pip install setuptools==67.6.1 pip==23.1 --upgrade
RUN echo "" >> /etc/bashrc \
 && echo "if [[ -e /opt/venv/bin/activate ]]; then" >> /etc/bashrc \
 && echo " source /opt/venv/bin/activate" >> /etc/bashrc \
 && echo "fi" >> /etc/bashrc \
 && echo "" >> /etc/bashrc \
 && echo "alias api='cd /opt/antinex/api'" >> /etc/bashrc \
 && echo "alias core='cd /opt/antinex/core'" >> /etc/bashrc \
 && echo "alias client='cd /opt/antinex/client'" >> /etc/bashrc \
 && echo "alias pipe='cd /opt/antinex/pipeline'" >> /etc/bashrc \
 && echo "alias ut='cd /opt/antinex/utils'" >> /etc/bashrc \
 && echo "alias ad='cd /opt/antinex/antinex-datasets'" >> /etc/bashrc \
 && echo "alias ds='cd /opt/antinex/datasets'" >> /etc/bashrc \
 && echo "alias sp='cd /opt/spylunking'" >> /etc/bashrc \
 && echo "alias vi='/usr/bin/vim'" >> /etc/bashrc
RUN echo "setting up /etc/pip.conf" \
 && echo "" >> /etc/pip.conf \
 && echo "[list]" >> /etc/pip.conf \
 && echo "format=columns" >> /etc/pip.conf
RUN echo "cloning repos" \
 && ls -l /opt \
 && ls -l / \
 && git clone https://github.com/jay-johnson/train-ai-with-django-swagger-jwt /opt/antinex/api \
 && git clone https://github.com/jay-johnson/antinex-core.git /opt/antinex/core \
 && git clone https://github.com/jay-johnson/antinex-client.git /opt/antinex/client \
 && git clone https://github.com/jay-johnson/network-pipeline.git /opt/antinex/pipeline \
 && git clone https://github.com/jay-johnson/antinex-utils.git /opt/antinex/utils \
 && git clone https://github.com/jay-johnson/antinex-datasets.git /opt/antinex/antinex-datasets \
 && git clone https://github.com/jay-johnson/network-pipeline-datasets.git /opt/antinex/datasets \
 && git clone https://github.com/jay-johnson/deploy-to-kubernetes.git /opt/deploy-to-kubernetes \
 && git clone https://github.com/jay-johnson/spylunking.git /opt/spylunking \
 && chmod 775 /opt/antinex/api /opt/antinex/core /opt/antinex/client /opt/antinex/pipeline /opt/antinex/utils /opt/antinex/antinex-datasets /opt/antinex/datasets /opt/deploy-to-kubernetes /opt/spylunking
RUN echo "checking repos in container" \
 && ls -l /opt/antinex/api \
 && ls -l /opt/antinex/core \
 && ls -l /opt/antinex/client \
 && ls -l /opt/antinex/pipeline \
 && ls -l /opt/antinex/utils \
 && ls -l /opt/antinex/antinex-datasets \
 && ls -l /opt/antinex/datasets \
 && ls -l /opt/deploy-to-kubernetes \
 && ls -l /opt/spylunking
RUN echo "installing python logger with splunk support" \
 && . /opt/venv/bin/activate \
 && cd /opt/spylunking \
 && pip install --upgrade -e . \
 && cd docs \
 && make html
RUN echo "installing utils" \
 && . /opt/venv/bin/activate \
 && cd /opt/antinex/utils \
 && pip install --upgrade -e . \
 && cd docs \
 && make html
RUN echo "installing pipeline" \
 && . /opt/venv/bin/activate \
 && cd /opt/antinex/pipeline \
 && pip install --upgrade -e . \
 && cd docs \
 && make html
RUN echo "installing core" \
 && . /opt/venv/bin/activate \
 && cd /opt/antinex/core \
 && pip install --upgrade -e . \
 && cd docs \
 && make html
RUN echo "installing api" \
 && . /opt/venv/bin/activate \
 && cd /opt/antinex/api \
 && pip install --upgrade -r /opt/antinex/api/requirements.txt
RUN echo "building docs" \
 && . /opt/venv/bin/activate \
 && . /opt/antinex/api/envs/drf-dev.env \
 && cd /opt/antinex/api/webapp \
 && ls -l \
 && ./build-docs.sh \
 && echo "collecting statics" \
 && ./collect-statics.sh
RUN echo "installing client" \
 && . /opt/venv/bin/activate \
 && cd /opt/antinex/client \
 && pip install --upgrade -e . \
 && cd docs \
 && make html
RUN echo "installing jupyter pips" \
 && . /opt/venv/bin/activate \
 && pip list --format=columns \
 && pip install requests==2.28.2 seaborn==0.12.2 RISE==5.7.1 vega3==0.13.0 jupyter==1.0.0 --upgrade
RUN cp -r ~/.jupyter ~/.bak_jupyter || true
RUN rm -rf ~/.jupyter || true
RUN cp -r ~/notebooks ~/.bak_notebooks || true
RUN rm -rf ~/notebooks || true
RUN echo "Installing JupyterLab" \
 && git clone https://github.com/jupyterlab/jupyterlab.git /opt/jupyterlab \
 && cd /opt/jupyterlab \
 && . /opt/venv/bin/activate \
 && pip install -e .
RUN echo "Installing Vega" \
 && . /opt/venv/bin/activate \
 && /opt/venv/bin/jupyter-nbextension install vega3 --py
RUN echo "Enabling Vega" \
 && . /opt/venv/bin/activate \
 && /opt/venv/bin/jupyter-nbextension enable vega3 --py
RUN echo "Installing Rise" \
 && . /opt/venv/bin/activate \
 && /opt/venv/bin/jupyter-nbextension install rise --py
RUN echo "Enabling Rise" \
 && . /opt/venv/bin/activate \
 && /opt/venv/bin/jupyter-nbextension enable rise --py
RUN ls /opt/antinex/core/ \
 && cp /opt/antinex/core/docker/update-all.sh /opt/antinex/update-all.sh \
 && chmod 777 /opt/antinex/core/docker/update-all.sh \
 && chmod 777 /opt/antinex/update-all.sh \
 && chmod 777 /opt/antinex/core/docker/jupyter/start-container.sh \
 && chmod 777 /opt/antinex/core/run-antinex-core.sh
RUN echo "Downgrading numpy and setuptools for tensorflow" \
 && . /opt/venv/bin/activate \
 && pip install numpy==1.14.5 --upgrade \
 && pip install setuptools==39.1.0 --upgrade
ENV PIPELINE_PROCESSOR_SCRIPT="/opt/antinex/pipeline/network_pipeline/scripts/packets_redis.py"
ENV JUPYTER_START_SCRIPT="/opt/antinex/core/docker/jupyter/start-container.sh"
ENV NO_DB_WORKER_START_SCRIPT="/opt/antinex/core/run-antinex-core.sh"
ENV DJANGO_WORKER_START_SCRIPT="/opt/antinex/api/run-worker.sh"
ENV APP_START_SCRIPT="/opt/antinex/api/run-django.sh"
ENV MIGRATION_SCRIPT="/opt/antinex/api/run-migrations.sh"
ENV SHARED_LOG_CFG="/opt/antinex/core/antinex_core/log/debug-openshift-logging.json"
ENV API_LOG_CFG="/opt/antinex/core/antinex_core/log/api-logging.json"
ENV CLIENT_LOG_CFG="/opt/antinex/core/antinex_core/log/client-logging.json"
ENV CORE_LOG_CFG="/opt/antinex/core/antinex_core/log/core-logging.json"
ENV JUPYTER_LOG_CFG="/opt/antinex/core/antinex_core/log/jupyter-logging.json"
ENV PIPELINE_LOG_CFG="/opt/antinex/core/antinex_core/log/pipeline-logging.json"
ENV DEBUG_SHARED_LOG_CFG="0"
ENV LOG_LEVEL="DEBUG"
ENV LOG_FILE="/var/log/antinex/core/ai-core.log"
ENV ENVIRONMENT="Development"
ENV DJANGO_CONFIGURATION="Development"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV DJANGO_DEBUG="yes"
ENV DJANGO_TEMPLATE_DEBUG="yes"
ENV CELERY_ENABLED="0"
ENV CACHEOPS_ENABLED="0"
ENV ANTINEX_WORKER_ENABLED="1"
ENV ANTINEX_WORKER_ONLY="0"
ENV ANTINEX_DELIVERY_MODE="persistent"
ENV ANTINEX_AUTH_URL="redis://0.0.0.0:6379/6"
ENV ANTINEX_EXCHANGE_NAME="webapp.predict.requests"
ENV ANTINEX_EXCHANGE_TYPE="topic"
ENV ANTINEX_QUEUE_NAME="webapp.predict.requests"
ENV ANTINEX_WORKER_SSL_ENABLED="0"
ENV SKIP_BUILD_DOCS="1"
ENV SKIP_COLLECT_STATICS="1"
ENV JUPYTER_CONFIG="/opt/antinex/core/docker/jupyter/jupyter_notebook_config.py"
ENV NOTEBOOK_DIR="/opt/antinex/core/docker/notebooks"
ENV USE_ENV="drf-dev"
ENV USE_VENV="/opt/venv"
WORKDIR /opt/antinex/core
#   set for anonymous user access in the container
RUN find /opt/antinex/api -type d -exec chmod 777 {}
RUN find /opt/antinex/core -type d -exec chmod 777 {}
RUN find /opt/antinex/client -type d -exec chmod 777 {}
RUN find /opt/antinex/pipeline -type d -exec chmod 777 {}
RUN find /opt/antinex/utils -type d -exec chmod 777 {}
RUN find /opt/antinex/antinex-datasets -type d -exec chmod 777 {}
RUN find /opt/antinex/datasets -type d -exec chmod 777 {}
RUN find /opt/deploy-to-kubernetes -type d -exec chmod 777 {}
RUN find /opt/spylunking -type d -exec chmod 777 {}
RUN find /opt/venv -type d -exec chmod 777 {}
RUN find /var/log -type d -exec chmod 777 {}
ENTRYPOINT /opt/antinex/core/run-antinex-core.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
