FROM ubuntu:14.04
RUN echo 'deb http://us.archive.ubuntu.com/ubuntu/ trusty universe' >> /etc/apt/sources.list
RUN echo
RUN apt-get update -y
RUN apt-get -y upgrade
#  Install all prerequisites
RUN apt-get install --no-install-recommends software-properties-common -y
RUN add-apt-repository -y ppa:chris-lea/node.js
RUN apt-get update -y
RUN apt-get install --no-install-recommends python-django-tagging python-simplejson python-memcache python-ldap python-cairo python-psycopg2 python-support python-pip gunicorn supervisor nginx-light nodejs git wget curl openjdk-7-jre build-essential python-dev -y
#  Install PostgreSQL for Graphite
RUN locale-gen en_US.UTF-8
RUN update-locale LANG=en_US.UTF-8
RUN apt-get update -qq \
 && DEBIAN_FRONTEND=noninteractive
RUN apt-get install --no-install-recommends postgresql-9.3 -y -q
#  /etc/ssl/private can't be accessed from within container for some reason
#  (@andrewgodwin says it's something AUFS related)
RUN mkdir /etc/ssl/private-copy ; mv /etc/ssl/private/* /etc/ssl/private-copy/ ; rm -r /etc/ssl/private ; mv /etc/ssl/private-copy /etc/ssl/private ; chmod -R 0700 /etc/ssl/private ; chown -R postgres /etc/ssl/private
COPY ./postgres/postgresql.conf /etc/postgresql/9.3/main/postgresql.conf
COPY ./postgres/pg_hba.conf /etc/postgresql/9.3/main/pg_hba.conf
RUN chown postgres:postgres /etc/postgresql/9.3/main/*.conf
COPY ./postgres/run /usr/local/bin/run
RUN chmod +x /usr/local/bin/run
VOLUME ["/var/lib/postgresql"]
#  Install Elasticsearch
RUN cd ~ \
 && wget https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.3.2.deb
RUN cd ~ \
 && dpkg -i elasticsearch-1.3.2.deb \
 && rm elasticsearch-1.3.2.deb
#  Install StatsD
RUN mkdir /src \
 && git clone https://github.com/etsy/statsd.git /src/statsd \
 && cd /src/statsd \
 && git checkout v0.7.1
#  Install Whisper, Carbon and Graphite-Web
RUN pip install Twisted==11.1.0
RUN pip install Django==1.5
RUN pip install whisper
RUN pip install carbon --install-option="--prefix=/var/lib/graphite" --install-option="--install-lib=/var/lib/graphite/lib"
RUN pip install graphite-web --install-option="--prefix=/var/lib/graphite" --install-option="--install-lib=/var/lib/graphite/webapp"
#  Install Grafana
RUN mkdir /src/grafana \
 && cd /src/grafana \
 && wget http://grafanarel.s3.amazonaws.com/grafana-1.7.0.tar.gz \
 && tar xzvf grafana-1.7.0.tar.gz --strip-components=1 \
 && rm grafana-1.7.0.tar.gz
#  Configure Elasticsearch
COPY ./elasticsearch/run /usr/local/bin/run_elasticsearch
COPY ./elasticsearch/elasticsearch.yml /etc/elasticsearch/elasticsearch.yml
VOLUME ["/data"]
#  Confiure StatsD
COPY ./statsd/config.js /src/statsd/config.js
#  Configure Whisper, Carbon and Graphite-Web
COPY ./graphite/initial_data.json /var/lib/graphite/webapp/graphite/initial_data.json
COPY ./graphite/local_settings.py /var/lib/graphite/webapp/graphite/local_settings.py
COPY ./graphite/carbon.conf /var/lib/graphite/conf/carbon.conf
COPY ./graphite/storage-schemas.conf /var/lib/graphite/conf/storage-schemas.conf
COPY ./graphite/storage-aggregation.conf /var/lib/graphite/conf/storage-aggregation.conf
RUN mkdir -p /var/lib/graphite/storage/whisper
RUN touch /var/lib/graphite/storage/graphite.db /var/lib/graphite/storage/index
RUN chown -R www-data /var/lib/graphite/storage
RUN chmod 0775 /var/lib/graphite/storage /var/lib/graphite/storage/whisper
RUN chmod 0664 /var/lib/graphite/storage/graphite.db
#  Configure Grafana
COPY ./grafana/config.js /src/grafana/config.js
COPY ./grafana/dashboard.json /src/grafana/app/dashboards/default.json
COPY ./nginx/nginx.conf /etc/nginx/nginx.conf
COPY ./supervisord.conf /etc/supervisor/conf.d/supervisord.conf
COPY crontab /crontab
#  Grafana
EXPOSE 80/tcp
#  StatsD UDP port
EXPOSE 8125/udp
#  StatsD Management port
EXPOSE 8126/tcp
#  Carbon listen port
EXPOSE 2003/tcp
CMD /etc/init.d/postgresql start \
 && sudo -u postgres createuser -E -w graphite \
 && echo "ALTER USER graphite WITH PASSWORD 'graphite';" | psql -U postgres \
 && echo "CREATE DATABASE graphite OWNER graphite;" | psql -U postgres \
 && cd /var/lib/graphite/webapp/graphite \
 && python manage.py syncdb --noinput \
 && /etc/init.d/postgresql stop \
 && cat /crontab | crontab - \
 && chown -R elasticsearch:elasticsearch /data \
 && /etc/init.d/supervisor start
