FROM ubuntu:14.04
RUN echo 'deb http://us.archive.ubuntu.com/ubuntu/ precise universe' >> /etc/apt/sources.list
RUN apt-get update -y
RUN apt-get install software-properties-common -y
RUN apt-get install python-software-properties -y \
 && apt-get update -y
#  oracle java
RUN add-apt-repository ppa:webupd8team/java \
 && apt-get update -y \
 && echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections \
 && apt-get install oracle-java7-installer -y
RUN apt-get install supervisor nginx-light git wget curl -y
RUN apt-get install python-django-tagging python-simplejson python-memcache python-ldap python-cairo python-django python-twisted python-pysqlite2 python-support python-pip gunicorn -y
#  fake fuse
RUN apt-get install libfuse2 \
 && cd /tmp ; apt-get download fuse \
 && cd /tmp ; dpkg-deb -x fuse_* . \
 && cd /tmp ; dpkg-deb -e fuse_* \
 && cd /tmp ; rm fuse_*.deb \
 && cd /tmp ; echo -en '#!/bin/bash\nexit 0\n' > DEBIAN/postinst \
 && cd /tmp ; dpkg-deb -b . /fuse.deb \
 && cd /tmp ; dpkg -i /fuse.deb
#  Graphite
# run	pip install whisper
# run	pip install --install-option="--prefix=/var/lib/graphite" --install-option="--install-lib=/var/lib/graphite/lib" carbon
# run	pip install --install-option="--prefix=/var/lib/graphite" --install-option="--install-lib=/var/lib/graphite/webapp" graphite-web
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
#  Elastic Search
RUN cd ~ \
 && wget https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.3.4.deb
RUN cd ~ \
 && dpkg -i elasticsearch-1.3.4.deb \
 && rm elasticsearch-1.3.4.deb
#  grafana, kibana
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
# ####### no 'add' before this line to utilize caching (see http://crosbymichael.com/dockerfile-best-practices.html)
#  Add graphite config
ADD ./graphite/initial_data.json /opt/graphite/webapp/graphite/initial_data.json
ADD ./graphite/local_settings.py /opt/graphite/webapp/graphite/local_settings.py
ADD ./graphite/carbon.conf /opt/graphite/conf/carbon.conf
ADD ./graphite/storage-schemas.conf /opt/graphite/conf/storage-schemas.conf
ADD ./graphite/storage-aggregation.conf /opt/graphite/conf/storage-aggregation.conf
RUN mkdir -p /opt/graphite/storage/whisper
RUN touch /opt/graphite/storage/graphite.db /opt/graphite/storage/index
RUN chown -R www-data /opt/graphite/storage
RUN chmod 0775 /opt/graphite/storage /opt/graphite/storage/whisper
RUN chmod 0664 /opt/graphite/storage/graphite.db
RUN cd /opt/graphite/webapp/graphite \
 && python manage.py syncdb --noinput
#  grafana, kibana config
ADD ./grafana/config.js /www/data/grafana/config.js
ADD ./kibana/config.js /www/data/kibana/config.js
#  elasticsearch
ADD ./elasticsearch/run /usr/local/bin/run_elasticsearch
#  Add system service config
ADD ./nginx/nginx.conf /etc/nginx/nginx.conf
ADD ./supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#  graphite render, es, kibana, grafana
EXPOSE 80/tcp
#  graphite
EXPOSE 81/tcp
#  Carbon line receiver port
EXPOSE 2003/tcp
#  Carbon pickle receiver port
EXPOSE 2004/tcp
#  Carbon cache query port
EXPOSE 7002/tcp
VOLUME ["/var/lib/elasticsearch"]
VOLUME ["/opt/graphite/storage/whisper"]
VOLUME ["/var/lib/log/supervisor"]
CMD ["/usr/bin/supervisord"]
#  vim:ts=8:noet:
