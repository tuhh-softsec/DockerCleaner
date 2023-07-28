FROM phusion/baseimage:0.9.19
MAINTAINER Nathan Hopkins <natehop@gmail.com>
# RUN echo deb http://archive.ubuntu.com/ubuntu $(lsb_release -cs) main universe > /etc/apt/sources.list.d/universe.list
RUN curl https://packages.grafana.com/gpg.key | apt-key add - \
 && apt-get update -y \
 && apt-get -y upgrade \
 && apt-get install --no-install-recommends vim nginx python-dev python-flup python-pip python-ldap expect git memcached sqlite3 libcairo2 libcairo2-dev python-cairo python-rrdtool pkg-config nodejs grafana -y --force-yes \
 && apt-get clean
#  python dependencies
RUN pip install django==1.5.12 python-memcached==1.53 django-tagging==0.3.1 twisted==11.1.0 txAMQP==0.6.2
#  install graphite
RUN git clone -b 0.9.15 --depth 1 https://github.com/graphite-project/graphite-web.git /usr/local/src/graphite-web
WORKDIR /usr/local/src/graphite-web
RUN python ./setup.py install
#  install whisper
RUN git clone -b 0.9.15 --depth 1 https://github.com/graphite-project/whisper.git /usr/local/src/whisper
WORKDIR /usr/local/src/whisper
RUN python ./setup.py install
#  install carbon
RUN git clone -b 0.9.15 --depth 1 https://github.com/graphite-project/carbon.git /usr/local/src/carbon
WORKDIR /usr/local/src/carbon
RUN python ./setup.py install
#  install statsd
RUN git clone -b v0.8.0 https://github.com/etsy/statsd.git /opt/statsd
#  config graphite
COPY conf/opt/graphite/conf/*.conf /opt/graphite/conf/
COPY conf/opt/graphite/webapp/graphite/local_settings.py /opt/graphite/webapp/graphite/local_settings.py
COPY conf/opt/graphite/webapp/graphite/app_settings.py /opt/graphite/webapp/graphite/app_settings.py
RUN python /opt/graphite/webapp/graphite/manage.py collectstatic --noinput
#  config statsd
COPY conf/opt/statsd/config_*.js /opt/statsd/
#  config grafana
COPY conf/etc/grafana/grafana.ini /etc/grafana/grafana.ini
#  config nginx
RUN rm /etc/nginx/sites-enabled/default
COPY conf/etc/nginx/nginx.conf /etc/nginx/nginx.conf
COPY conf/etc/nginx/sites-enabled/graphite-statsd.conf /etc/nginx/sites-enabled/graphite-statsd.conf
COPY conf/etc/nginx/sites-enabled/grafana.conf /etc/nginx/sites-enabled/grafana.conf
#  init django admin
COPY conf/usr/local/bin/django_admin_init.exp /usr/local/bin/django_admin_init.exp
RUN /usr/local/bin/django_admin_init.exp
#  logging support
RUN mkdir -p /var/log/carbon /var/log/graphite /var/log/nginx
COPY conf/etc/logrotate.d/graphite-statsd /etc/logrotate.d/graphite-statsd
#  daemons
COPY conf/etc/service/carbon/run /etc/service/carbon/run
COPY conf/etc/service/carbon-aggregator/run /etc/service/carbon-aggregator/run
COPY conf/etc/service/graphite/run /etc/service/graphite/run
COPY conf/etc/service/statsd/run /etc/service/statsd/run
COPY conf/etc/service/grafana/run /etc/service/grafana/run
COPY conf/etc/service/nginx/run /etc/service/nginx/run
#  default conf setup
COPY conf /etc/graphite-statsd/conf
COPY conf/etc/my_init.d/01_conf_init.sh /etc/my_init.d/01_conf_init.sh
#  cleanup
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  defaults
EXPOSE 80/tcp 2003-2004 2023-2024 8125/tcp 8125/udp 8126/tcp
VOLUME ["/opt/graphite/conf", "/opt/graphite/storage", "/etc/grafana", "/etc/nginx", "/opt/statsd", "/etc/logrotate.d", "/var/log"]
WORKDIR /
ENV HOME="/root"
ENV STATSD_INTERFACE="udp"
CMD ["/sbin/my_init"]
