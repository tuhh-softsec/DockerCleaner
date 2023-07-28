FROM phusion/baseimage:0.9.22
MAINTAINER Denys Zhdanov <denis.zhdanov@gmail.com>
RUN apt-get update -y \
 && apt-get -y upgrade \
 && apt-get install --no-install-recommends vim nginx python-dev python-flup python-pip python-ldap expect git memcached sqlite3 libffi-dev libcairo2 libcairo2-dev python-cairo python-rrdtool pkg-config nodejs -y \
 && rm -rf /var/lib/apt/lists/*
#   choose a timezone at build-time
#   use `--build-arg CONTAINER_TIMEZONE=Europe/Brussels` in `docker build`
ARG CONTAINER_TIMEZONE
ENV DEBIAN_FRONTEND="noninteractive"
RUN if [ ! -z "${CONTAINER_TIMEZONE}" ] ; then ln -sf /usr/share/zoneinfo/$CONTAINER_TIMEZONE /etc/localtime \
 && dpkg-reconfigure -f noninteractive tzdata ; fi
#   fix python dependencies (LTS Django and newer memcached/txAMQP)
RUN pip install pip==23.1 --upgrade \
 && pip install django==1.8.18 python-memcached==1.53 txAMQP==0.6.2
ARG version=1.0.2
ARG whisper_version=${version}
ARG carbon_version=${version}
ARG graphite_version=${version}
RUN echo "Building Version: $version"
ARG whisper_repo=https://github.com/graphite-project/whisper.git
ARG carbon_repo=https://github.com/graphite-project/carbon.git
ARG graphite_repo=https://github.com/graphite-project/graphite-web.git
ARG statsd_version=v0.8.0
ARG statsd_repo=https://github.com/etsy/statsd.git
#   install whisper
RUN git clone -b ${whisper_version} --depth 1 ${whisper_repo} /usr/local/src/whisper
WORKDIR /usr/local/src/whisper
RUN python ./setup.py install
#   install carbon
RUN git clone -b ${carbon_version} --depth 1 ${carbon_repo} /usr/local/src/carbon
WORKDIR /usr/local/src/carbon
RUN pip install -r requirements.txt \
 && python ./setup.py install
#   install graphite
RUN git clone -b ${graphite_version} --depth 1 ${graphite_repo} /usr/local/src/graphite-web
WORKDIR /usr/local/src/graphite-web
RUN pip install -r requirements.txt \
 && python ./setup.py install
#   install statsd
RUN git clone -b ${statsd_version} ${statsd_repo} /opt/statsd
#   config graphite
COPY conf/opt/graphite/conf/*.conf /opt/graphite/conf/
COPY conf/opt/graphite/webapp/graphite/local_settings.py /opt/graphite/webapp/graphite/local_settings.py
#   ADD conf/opt/graphite/webapp/graphite/app_settings.py /opt/graphite/webapp/graphite/app_settings.py
WORKDIR /opt/graphite/webapp
RUN mkdir -p /var/log/graphite/ \
 && PYTHONPATH=/opt/graphite/webapp django-admin.py collectstatic --noinput --settings=graphite.settings
#   config statsd
COPY conf/opt/statsd/config.js /opt/statsd/
#   config nginx
RUN rm /etc/nginx/sites-enabled/default
COPY conf/etc/nginx/nginx.conf /etc/nginx/nginx.conf
COPY conf/etc/nginx/sites-enabled/graphite-statsd.conf /etc/nginx/sites-enabled/graphite-statsd.conf
#   init django admin
COPY conf/usr/local/bin/django_admin_init.exp /usr/local/bin/django_admin_init.exp
COPY conf/usr/local/bin/manage.sh /usr/local/bin/manage.sh
RUN chmod +x /usr/local/bin/manage.sh \
 && /usr/local/bin/django_admin_init.exp
#   logging support
RUN mkdir -p /var/log/carbon /var/log/graphite /var/log/nginx
COPY conf/etc/logrotate.d/graphite-statsd /etc/logrotate.d/graphite-statsd
#   daemons
COPY conf/etc/service/carbon/run /etc/service/carbon/run
COPY conf/etc/service/carbon-aggregator/run /etc/service/carbon-aggregator/run
COPY conf/etc/service/graphite/run /etc/service/graphite/run
COPY conf/etc/service/statsd/run /etc/service/statsd/run
COPY conf/etc/service/nginx/run /etc/service/nginx/run
#   default conf setup
COPY conf /etc/graphite-statsd/conf
COPY conf/etc/my_init.d/01_conf_init.sh /etc/my_init.d/01_conf_init.sh
#   cleanup
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   defaults
EXPOSE 80/tcp 2003-2004 2023-2024 8125/tcp 8125/udp 8126/tcp
VOLUME ["/opt/graphite/conf", "/opt/graphite/storage", "/etc/nginx", "/opt/statsd", "/etc/logrotate.d", "/var/log"]
WORKDIR /
ENV HOME="/root"
ENV STATSD_INTERFACE="udp"
CMD ["/sbin/my_init"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
