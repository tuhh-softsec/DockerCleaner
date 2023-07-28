FROM ubuntu:16.04
#   Never prompts the user for choices on installation/configuration of packages
ENV DEBIAN_FRONTEND="noninteractive"
ENV TERM="linux"
#   Airflow
ARG AIRFLOW_VERSION=1.7.1.3
ENV AIRFLOW_HOME="/usr/local/airflow"
#   Define en_US.
ENV LANGUAGE="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV LC_CTYPE="en_US.UTF-8"
ENV LC_MESSAGES="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
RUN set -ex \
 && buildDeps=' python3-dev libkrb5-dev libsasl2-dev libssl-dev libffi-dev build-essential libblas-dev liblapack-dev libpq-dev ' \
 && apt-get update -yqq \
 && apt-get install --no-install-recommends python3=3.5.1-3 python3-pip=8.1.1-2ubuntu0.6 netbase=5.3 apt-utils=1.2.35 curl=7.47.0-1ubuntu2.19 netcat=1.10-41 locales=2.23-0ubuntu11.3 git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 alien=8.95 libgdal-dev=1.11.3+dfsg-3build2 libgeos-dev=3.5.0-1ubuntu2 binutils=2.26.1-1ubuntu1~16.04.8 libproj-dev=4.9.2-2 gdal-bin=1.11.3+dfsg-3build2 libspatialindex-dev=1.8.5-3 libaio1=0.3.110-2 freetds-dev=0.91-6.1build1 $buildDeps -yqq \
 && sed -i 's/^# en_US.UTF-8 UTF-8$/en_US.UTF-8 UTF-8/g' /etc/locale.gen \
 && locale-gen \
 && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
 && useradd -ms /bin/bash -d ${AIRFLOW_HOME} airflow \
 && python3 -m pip install -U pip \
 && pip3 install -U setuptools \
 && pip3 install Cython \
 && pip3 install pytz==2015.7 \
 && pip3 install pyOpenSSL \
 && pip3 install ndg-httpsclient \
 && pip3 install pyasn1 \
 && pip3 install click \
 && pip3 install git+https://github.com/CityOfPhiladelphia/incubator-airflow.git#egg=airflow[async,crypto,password,postgres,hive,s3] \
 && pip3 install git+https://github.com/CityOfPhiladelphia/eastern-state.git \
 && pip3 install git+https://github.com/CityOfPhiladelphia/s3-sftp-sync.git \
 && pip3 install git+https://github.com/CityOfPhiladelphia/jsontableschema-sql-py.git#egg=jsontableschema_sql \
 && pip3 install git+https://github.com/CityOfPhiladelphia/the-el.git#egg=the_el \
 && apt-get remove --purge -yqq $buildDeps \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man /usr/share/doc /usr/share/doc-base
#   instant sql-plus instant oracle client
RUN set -ex \
 && wget https://www.dropbox.com/s/ubgeht3m59bhfh1/oracle-instantclient12.1-sqlplus-12.1.0.2.0-1.x86_64.rpm?dl=0 \
 && mv oracle-instantclient12.1-sqlplus-12.1.0.2.0-1.x86_64.rpm?dl=0 oracle-instantclient12.1-sqlplus-12.1.0.2.0-1.x86_64.rpm \
 && alien -i oracle-instantclient12.1-sqlplus-12.1.0.2.0-1.x86_64.rpm \
 && rm oracle-instantclient12.1-sqlplus-12.1.0.2.0-1.x86_64.rpm
#   instant basic-lite instant oracle client
RUN set -ex \
 && wget https://www.dropbox.com/s/1yzl0fdnaiw5yqp/oracle-instantclient12.1-basiclite-12.1.0.2.0-1.x86_64.rpm?dl=0 \
 && mv oracle-instantclient12.1-basiclite-12.1.0.2.0-1.x86_64.rpm?dl=0 oracle-instantclient12.1-basiclite-12.1.0.2.0-1.x86_64.rpm \
 && alien -i oracle-instantclient12.1-basiclite-12.1.0.2.0-1.x86_64.rpm \
 && rm oracle-instantclient12.1-basiclite-12.1.0.2.0-1.x86_64.rpm
#   instant oracle-sdk
RUN set -ex \
 && wget https://www.dropbox.com/s/uic5vzc9yobttct/oracle-instantclient12.1-devel-12.1.0.2.0-1.x86_64.rpm?dl=0 \
 && mv oracle-instantclient12.1-devel-12.1.0.2.0-1.x86_64.rpm?dl=0 oracle-instantclient12.1-devel-12.1.0.2.0-1.x86_64.rpm \
 && alien -i oracle-instantclient12.1-devel-12.1.0.2.0-1.x86_64.rpm \
 && rm oracle-instantclient12.1-devel-12.1.0.2.0-1.x86_64.rpm
COPY scripts/entrypoint.sh /entrypoint.sh
COPY requirements.txt /requirements.txt
COPY config/airflow.cfg ${AIRFLOW_HOME}/airflow.cfg
COPY scripts/users.py ${AIRFLOW_HOME}/users.py
COPY dags ${AIRFLOW_HOME}/dags
COPY plugins ${AIRFLOW_HOME}/plugins
RUN chown -R airflow: ${AIRFLOW_HOME}
EXPOSE 8080/tcp
USER airflow
WORKDIR ${AIRFLOW_HOME}
ENTRYPOINT ["/entrypoint.sh"]
# Please add your HEALTHCHECK here!!!
