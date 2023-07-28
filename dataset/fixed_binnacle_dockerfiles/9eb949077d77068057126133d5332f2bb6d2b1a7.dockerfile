FROM puckel/docker-airflow:1.8.1
USER root
#
#   Install GDAL and Python
#
RUN set -ex \
 && apt-get update -yqq \
 && apt-get install --no-install-recommends libcurl4-openssl-dev build-essential libgeos-dev libproj-dev libopenjp2-7-dev gnupg curl wget python-dev libpq-dev libgraphicsmagick++1-dev libboost-python-dev openjpeg-tools python-requests python-numpy python-scipy apt-utils unzip curl git -yqq \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man /usr/share/doc /usr/share/doc-base
#   Compile GDAL
ENV GDAL_VERSION="2.2.1"
RUN cd /tmp \
 && apt-get update -yqq \
 && apt-get install --no-install-recommends pkg-config \
 && mkdir gdal-src \
 && cd gdal-src \
 && wget http://download.osgeo.org/gdal/$GDAL_VERSION/gdal-$GDAL_VERSION.tar.gz \
 && tar -xzf gdal-$GDAL_VERSION.tar.gz \
 && cd gdal-$GDAL_VERSION \
 && ./configure --with-python \
 && make -j 4 \
 && make install \
 && cd /tmp \
 && rm -r gdal-src
#
#   Install dependencies for s2reader
#
RUN apt-get update -yqq \
 && apt-get install --no-install-recommends libxml2-dev libxslt-dev -yqq \
 && pip install pip==23.1 --upgrade \
 && pip install shapely==2.0.1 numpy==1.24.2 lxml==4.9.2 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man /usr/share/doc /usr/share/doc-base
#
#   Install dependencies for evo-odas.git
#
RUN apt-get update -yqq \
 && apt-get install --no-install-recommends libpq-dev python-dev libgraphicsmagick++1-dev libboost-python-dev curl build-essential libcurl4-openssl-dev nano vim rsync -yqq \
 && pip install sentinelsat==1.2.1 Jinja2==3.1.2 pg_simple==0.2.4 pgmagick==0.7.6 gsconfig==1.0.10 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man /usr/share/doc /usr/share/doc-base
#
#   Clone and install s2reader
#
ENV EVOODAS_HOME="/usr/local/evoodas" \
    EVOODAS_USER="airflow"
RUN mkdir $EVOODAS_HOME \
 && cd $EVOODAS_HOME \
 && git clone https://github.com/ungarj/s2reader.git \
 && cd s2reader \
 && easy_install .
#
#   Install Utilities
#
RUN apt-get update -yqq \
 && apt-get install --no-install-recommends openssh-client rsync postgresql-client -yqq \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man /usr/share/doc /usr/share/doc-base \
 && pip install hvac==1.1.0
#   Copy Configuration
COPY airflow.cfg /usr/local/airflow/airflow.cfg
VOLUME ["/var/data"]
USER airflow
# Please add your HEALTHCHECK here!!!
