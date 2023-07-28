FROM ubuntu:14.04
RUN echo 'deb http://us.archive.ubuntu.com/ubuntu/ precise universe' >> /etc/apt/sources.list
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python-software-properties=0.92.37.8 -y ) \
 && apt-get update -y
#   oracle java
RUN add-apt-repository ppa:webupd8team/java \
 && apt-get update -y \
 && echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections \
 && (apt-get update ;apt-get install --no-install-recommends oracle-java7-installer -y )
RUN (apt-get update ;apt-get install --no-install-recommends supervisor=3.0b2-1ubuntu0.1 nginx-light=1.4.6-1ubuntu3.9 git=1:1.9.1-1ubuntu0.10 wget=1.15-1ubuntu1.14.04.5 curl=7.35.0-1ubuntu2.20 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python-django-tagging=1:0.3.1-3 python-simplejson=3.3.1-1ubuntu6 python-memcache=1.53-1build1 python-ldap=2.4.10-1build1 python-cairo=1.8.8-1ubuntu5 python-django=1.6.11-0ubuntu1.3 python-twisted=13.2.0-1ubuntu1.2 python-pysqlite2=2.6.3-3 python-support=1.0.15 python-pip=1.5.4-1ubuntu4 gunicorn=17.5-2build1 -y )
#   fake fuse
RUN (apt-get update ;apt-get install --no-install-recommends libfuse2=2.9.2-4ubuntu4.14.04.1 ) \
 && cd /tmp ; apt-get download fuse \
 && cd /tmp ; dpkg-deb -x fuse_* . \
 && cd /tmp ; dpkg-deb -e fuse_* \
 && cd /tmp ; rm fuse_*.deb \
 && cd /tmp ; echo -en '#!/bin/bash\nexit 0\n' > DEBIAN/postinst \
 && cd /tmp ; dpkg-deb -b . /fuse.deb \
 && cd /tmp ; dpkg -i /fuse.deb
#   Graphite
#  run	pip install whisper
#  run	pip install --install-option="--prefix=/var/lib/graphite" --install-option="--install-lib=/var/lib/graphite/lib" carbon
#  run	pip install --install-option="--prefix=/var/lib/graphite" --install-option="--install-lib=/var/lib/graphite/webapp" graphite-web
RUN cd /usr/local/src \
 && git clone https://github.com/graphite-project/graphite-web.git
RUN cd /usr/local/src \
 && git clone https://github.com/graphite-project/carbon.git
RUN cd /usr/local/src \
 && git clone https://github.com/graphite-project/whisper.git
RUN cd /usr/local/src/whisper \
 && git checkout master \
 && python setup.py install
RUN cd /usr/local/src/carbon \
 && git checkout 0.9.x \
 && python setup.py install
RUN cd /usr/local/src/graphite-web \
 && git checkout 0.9.x \
 && python check-dependencies.py ; python setup.py install
#   Elastic Search
RUN cd ~ \
 && wget https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.3.4.deb
RUN cd ~ \
 && dpkg -i elasticsearch-1.3.4.deb \
 && rm elasticsearch-1.3.4.deb
#   grafana, kibana
RUN mkdir -p /www/data
RUN cd /tmp \
 && wget http://grafanarel.s3.amazonaws.com/grafana-1.8.1.tar.gz \
 && tar xzvf grafana-1.8.1.tar.gz \
 && rm grafana-1.8.1.tar.gz \
 && mv /tmp/grafana-1.8.1 /www/data/grafana
RUN cd /tmp \
 && wget https://download.elasticsearch.org/kibana/kibana/kibana-3.1.1.tar.gz \
 && tar xzvf kibana-3.1.1.tar.gz \
 && rm kibana-3.1.1.tar.gz \
 && mv /tmp/kibana-3.1.1 /www/data/kibana
#  ####### no 'add' before this line to utilize caching (see http://crosbymichael.com/dockerfile-best-practices.html)
#   Add graphite config
COPY ./graphite/initial_data.json /opt/graphite/webapp/graphite/initial_data.json
COPY ./graphite/local_settings.py /opt/graphite/webapp/graphite/local_settings.py
COPY ./graphite/carbon.conf /opt/graphite/conf/carbon.conf
COPY ./graphite/storage-schemas.conf /opt/graphite/conf/storage-schemas.conf
COPY ./graphite/storage-aggregation.conf /opt/graphite/conf/storage-aggregation.conf
RUN mkdir -p /opt/graphite/storage/whisper
RUN touch /opt/graphite/storage/graphite.db /opt/graphite/storage/index
RUN chown -R www-data /opt/graphite/storage
RUN chmod 0775 /opt/graphite/storage /opt/graphite/storage/whisper
RUN chmod 0664 /opt/graphite/storage/graphite.db
RUN cd /opt/graphite/webapp/graphite \
 && python manage.py syncdb --noinput
#   grafana, kibana config
COPY ./grafana/config.js /www/data/grafana/config.js
COPY ./kibana/config.js /www/data/kibana/config.js
#   elasticsearch
COPY ./elasticsearch/run /usr/local/bin/run_elasticsearch
#   Add system service config
COPY ./nginx/nginx.conf /etc/nginx/nginx.conf
COPY ./supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#   graphite render, es, kibana, grafana
EXPOSE 80/tcp
#   graphite
EXPOSE 81/tcp
#   Carbon line receiver port
EXPOSE 2003/tcp
#   Carbon pickle receiver port
EXPOSE 2004/tcp
#   Carbon cache query port
EXPOSE 7002/tcp
VOLUME ["/var/lib/elasticsearch"]
VOLUME ["/opt/graphite/storage/whisper"]
VOLUME ["/var/lib/log/supervisor"]
CMD ["/usr/bin/supervisord"]
#   vim:ts=8:noet:
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
