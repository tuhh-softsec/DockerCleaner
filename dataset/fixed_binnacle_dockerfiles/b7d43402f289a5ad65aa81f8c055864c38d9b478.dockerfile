#
#   Grafana Dockerfile
#
#   - whisper (master)
#   - carbon  (0.9.12)
#   - graphite (0.9.12)
#   - elasticsearch (1.3.2)
#   - grafana (1.8.1)
#
#   build command
#   * default: docker build --force-rm=true -t subicura/grafana .
#   * nocache: docker build --force-rm=true --no-cache=true -t subicura/grafana .
#
#   configuration
#   -v {whisper directory}:/opt/graphite/storage/whisper
#
#   run command
#    docker pull subicura/grafana
#    docker rm -f grafana
#    docker run -it --rm -e HOST_IP=10.211.55.41 -e HOST_PORT=80 -p 2003:2003 -p 80:80 -v /grafana/elasticsearch:/data -v /grafana/whisper:/opt/graphite/storage/whisper subicura/grafana /bin/bash
#    docker run --rm -e HOST_IP=10.211.55.41 -e HOST_PORT=80 -p 2003:2003 -p 80:80 --name grafana -v /grafana/elasticsearch:/data -v /grafana/whisper:/opt/graphite/storage/whisper subicura/grafana
#    docker run -d -e HOST_IP=10.211.55.41 -e HOST_PORT=80 -e GRAPHITE_API_HOST=10.211.55.41 -p 2003:2003 -p 80:80 --name grafana -v /grafana/elasticsearch:/data -v /grafana/whisper:/opt/graphite/storage/whisper subicura/grafana
#
#   reference: https://github.com/nacyot/docker-graphite
#
FROM ubuntu:14.04
MAINTAINER subicura@subicura.com
#   default env
ENV DEBIAN_FRONTEND="noninteractive "
#   update ubuntu latest
RUN : \
 && apt-get -qq -y dist-upgrade
#   install essential packages
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 software-properties-common=0.92.37.8 python-software-properties=0.92.37.8 git=1:1.9.1-1ubuntu0.10 curl=7.35.0-1ubuntu2.20 -qq -y )
#   install python
RUN (apt-get update ;apt-get install --no-install-recommends python=2.7.5-5ubuntu3 python-dev=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 python-simplejson=3.3.1-1ubuntu6 python-memcache=1.53-1build1 python-ldap=2.4.10-1build1 python-cairo=1.8.8-1ubuntu5 python-twisted=13.2.0-1ubuntu1.2 python-pysqlite2=2.6.3-3 python-support=1.0.15 python-pip=1.5.4-1ubuntu4 gunicorn=17.5-2build1 -qq -y )
#   install java
RUN echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && add-apt-repository -y ppa:webupd8team/java \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends oracle-java7-installer -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/oracle-jdk7-installer
ENV JAVA_HOME="/usr/lib/jvm/java-7-oracle"
#   install nginx
RUN add-apt-repository -y ppa:nginx/stable \
 && apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends nginx=1.4.6-1ubuntu3.9 -qq -y )
#   install supervisor
RUN (apt-get update ;apt-get install --no-install-recommends supervisor=3.0b2-1ubuntu0.1 -qq -y )
#   whisper & carbon & graphite & elasticsearch & grafana
WORKDIR /usr/local/src
RUN pip install pytz==2023.3 pyparsing==3.0.9 django==1.5 django-tagging==0.3.1 'Twisted<12.0'
RUN git clone https://github.com/graphite-project/whisper.git \
 && cd whisper \
 && git checkout master \
 && python setup.py install
RUN git clone https://github.com/graphite-project/carbon.git \
 && cd carbon \
 && git checkout 0.9.12 \
 && python setup.py install
RUN git clone https://github.com/graphite-project/graphite-web.git \
 && cd graphite-web \
 && git checkout 0.9.12 \
 && python setup.py install
RUN cd /tmp \
 && wget -q -O - https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.3.2.tar.gz | tar xfz - \
 && mv /tmp/elasticsearch-1.3.2 /elasticsearch
RUN cd /opt \
 && wget -q -O - http://grafanarel.s3.amazonaws.com/grafana-1.8.1.tar.gz | tar xfz - \
 && mv grafana-1.8.1 grafana
#   carbon setting
COPY ./carbon /opt/graphite/conf/
#   graphite setting
ENV GRAPHITE_STORAGE_DIR="/opt/graphite/storage"
ENV GRAPHITE_CONF_DIR="/opt/graphite/conf"
ENV PYTHONPATH="/opt/graphite/webapp"
ENV LOG_DIR="/var/log/graphite"
ENV DEFAULT_INDEX_TABLESPACE="graphite"
RUN mkdir -p /opt/graphite/webapp
RUN mkdir -p /var/log/graphite
RUN cd /var/log/graphite/ \
 && touch info.log
RUN mkdir -p /opt/graphite/storage/whisper
RUN touch /opt/graphite/storage/graphite.db /opt/graphite/storage/index
RUN chmod 0775 /opt/graphite/storage /opt/graphite/storage/whisper
RUN chmod 0664 /opt/graphite/storage/graphite.db
COPY ./graphite/local_settings.py /opt/graphite/webapp/graphite/local_settings.py
COPY ./graphite/initial_data.json /opt/graphite/webapp/graphite/initial_data.json
RUN cd /opt/graphite/webapp/graphite \
 && django-admin.py syncdb --settings=graphite.settings --noinput
RUN cd /opt/graphite/webapp/graphite \
 && django-admin.py loaddata --settings=graphite.settings initial_data.json
#   elasticsearch setting
COPY elasticsearch/elasticsearch.yml /elasticsearch/config/elasticsearch.yml
#   grafana setting
COPY ./grafana/config.js /opt/grafana/config.js
#   nginx setting
COPY ./nginx.conf /etc/nginx/nginx.conf
#   supervisord setting
COPY ./supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#   add setup file
COPY ./setup.sh /opt/grafana/setup.sh
RUN chmod +x /opt/grafana/setup.sh
#   expose port
#   2003 - carbon cache - line receiver
#   7002 - grafana http
EXPOSE 2003/tcp 80/tcp
#   run
WORKDIR /opt/grafana
RUN ln -sf /usr/share/zoneinfo/Asia/Seoul /etc/localtime
CMD /opt/grafana/setup.sh \
 && /usr/bin/supervisord -n
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
