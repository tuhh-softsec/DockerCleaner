#   BUILD: docker build --rm -t airflow .
#   ORIGINAL SOURCE: https://github.com/puckel/docker-airflow
FROM python:3.6-slim
LABEL version="1.0"
LABEL maintainer="nicor88"
#   Never prompts the user for choices on installation/configuration of packages
ENV DEBIAN_FRONTEND="noninteractive"
ENV TERM="linux"
#   Airflow
#   it's possible to use v1-10-stable, but it's a development branch
ARG AIRFLOW_VERSION=1.10.3
ENV AIRFLOW_HOME="/usr/local/airflow"
ENV AIRFLOW_GPL_UNIDECODE="yes"
#   celery config
ARG CELERY_REDIS_VERSION=4.2.0
ARG PYTHON_REDIS_VERSION=3.2.0
ARG TORNADO_VERSION=5.1.1
#   Define en_US.
ENV LANGUAGE="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV LC_CTYPE="en_US.UTF-8"
ENV LC_MESSAGES="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
RUN set -ex \
 && buildDeps=' python3-dev libkrb5-dev libsasl2-dev libssl-dev libffi-dev build-essential libblas-dev liblapack-dev libpq-dev git ' \
 && apt-get update -yqq \
 && apt-get upgrade -yqq \
 && apt-get install --no-install-recommends sudo=1.9.5p2-3+deb11u1 python3-pip=20.3.4-4+deb11u1 python3-requests=2.25.1+dfsg-2 mysql-client default-libmysqlclient-dev=1.0.7 apt-utils=2.2.4 curl=7.74.0-1.3+deb11u7 rsync=3.2.3-4+deb11u1 netcat=1.10-46 locales=2.31-13+deb11u5 ${buildDeps} -yqq \
 && sed -i 's/^# en_US.UTF-8 UTF-8$/en_US.UTF-8 UTF-8/g' /etc/locale.gen \
 && locale-gen \
 && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
 && useradd -ms /bin/bash -d ${AIRFLOW_HOME} airflow \
 && pip install pip==23.1 setuptools==67.6.1 wheel==0.40.0 -U \
 && pip install Cython==0.29.34 \
 && pip install pytz==2023.3 \
 && pip install pyOpenSSL==23.1.1 \
 && pip install ndg-httpsclient==0.5.1 \
 && pip install pyasn1==0.4.8 \
 && pip install git+https://github.com/apache/incubator-airflow.git@${AIRFLOW_VERSION}#egg=apache-airflow[async,crypto,celery,kubernetes,jdbc,password,postgres,s3,slack] \
 && pip install redis==${PYTHON_REDIS_VERSION} \
 && pip install celery[redis]==${CELERY_REDIS_VERSION} \
 && pip install flask_oauthlib==0.9.6 \
 && pip install psycopg2-binary==2.9.6 \
 && pip install tornado==${TORNADO_VERSION} \
 && apt-get purge --auto-remove -yqq ${buildDeps} \
 && apt-get autoremove -yqq --purge \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man /usr/share/doc /usr/share/doc-base
COPY config/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh
RUN chown -R airflow: ${AIRFLOW_HOME}
USER airflow
COPY requirements.txt .
RUN pip install --user -r requirements.txt
COPY config/airflow.cfg ${AIRFLOW_HOME}/airflow.cfg
COPY dags ${AIRFLOW_HOME}/dags
COPY plugins ${AIRFLOW_HOME}/plugins
ENV PYTHONPATH="${AIRFLOW_HOME}"
EXPOSE 8080/tcp 5555/tcp 8793/tcp
WORKDIR ${AIRFLOW_HOME}
ENTRYPOINT ["/entrypoint.sh"]
# Please add your HEALTHCHECK here!!!
