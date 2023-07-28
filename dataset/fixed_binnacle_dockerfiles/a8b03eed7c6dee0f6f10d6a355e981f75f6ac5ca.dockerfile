FROM alpine:3.10 AS base
LABEL maintainer="Denys Zhdanov <denis.zhdanov@gmail.com>"
RUN true \
 && apk add cairo=1.16.0-r3 collectd=5.8.1-r1 collectd-disk=5.8.1-r1 collectd-nginx=5.8.1-r1 findutils=4.6.0-r1 librrd=1.7.2-r0 logrotate=3.15.0-r0 memcached=1.5.16-r0 nginx=1.16.1-r3 nodejs=10.24.1-r0 npm=10.24.1-r0 py3-pyldap=3.2.0-r0 redis=5.0.11-r0 runit=2.1.2-r3 sqlite=3.28.0-r3 expect=5.45.4-r0 dcron=4.5-r4 py-mysqldb=1.2.5-r1 mysql-dev mysql-client=10.3.29-r0 postgresql-dev=11.12-r0 postgresql-client=11.12-r0 --no-cache \
 && rm -rf /etc/nginx/conf.d/default.conf \
 && mkdir -p /var/log/carbon /var/log/graphite
FROM base AS build
LABEL maintainer="Denys Zhdanov <denis.zhdanov@gmail.com>"
RUN true \
 && apk add alpine-sdk=1.0-r0 git=2.22.5-r0 libffi-dev=3.2.1-r6 pkgconfig py3-cairo=1.16.3-r1 py3-pip py3-pyldap=3.2.0-r0 py3-virtualenv=16.0.0-r1 py-rrd=1.7.2-r0 py-mysqldb=1.2.5-r1 openldap-dev=2.4.48-r2 python3-dev=3.7.10-r0 rrdtool-dev=1.7.2-r0 wget=1.20.3-r0 --update \
 && virtualenv /opt/graphite \
 && . /opt/graphite/bin/activate \
 && pip3 install django==1.11.21 django-statsd-mozilla fadvise gunicorn msgpack-python redis rrdtool python-ldap mysqlclient psycopg2
ARG version=1.1.5
#   install whisper
ARG whisper_version=${version}
ARG whisper_repo=https://github.com/graphite-project/whisper.git
RUN git clone -b ${whisper_version} --depth 1 ${whisper_repo} /usr/local/src/whisper \
 && cd /usr/local/src/whisper \
 && . /opt/graphite/bin/activate \
 && python3 ./setup.py install
#   install carbon
ARG carbon_version=${version}
ARG carbon_repo=https://github.com/graphite-project/carbon.git
RUN . /opt/graphite/bin/activate \
 && git clone -b ${carbon_version} --depth 1 ${carbon_repo} /usr/local/src/carbon \
 && cd /usr/local/src/carbon \
 && pip3 install -r requirements.txt \
 && python3 ./setup.py install
#   install graphite
ARG graphite_version=${version}
ARG graphite_repo=https://github.com/graphite-project/graphite-web.git
RUN . /opt/graphite/bin/activate \
 && git clone -b ${graphite_version} --depth 1 ${graphite_repo} /usr/local/src/graphite-web \
 && cd /usr/local/src/graphite-web \
 && pip3 install -r requirements.txt \
 && python3 ./setup.py install
#   install statsd (as we have to use this ugly way)
ARG statsd_version=8d5363cb109cc6363661a1d5813e0b96787c4411
ARG statsd_repo=https://github.com/etsy/statsd.git
WORKDIR /opt
RUN git clone "${statsd_repo}" \
 && cd /opt/statsd \
 && git checkout "${statsd_version}" \
 && npm install
COPY conf/opt/graphite/conf/ /opt/defaultconf/graphite/
COPY conf/opt/graphite/webapp/graphite/local_settings.py /opt/defaultconf/graphite/local_settings.py
#   config graphite
COPY conf/opt/graphite/conf/*.conf /opt/graphite/conf/
COPY conf/opt/graphite/webapp/graphite/local_settings.py /opt/graphite/webapp/graphite/local_settings.py
WORKDIR /opt/graphite/webapp
RUN mkdir -p /var/log/graphite/ \
 && PYTHONPATH=/opt/graphite/webapp /opt/graphite/bin/django-admin.py collectstatic --noinput --settings=graphite.settings
#   config statsd
COPY conf/opt/statsd/config/ /opt/defaultconf/statsd/config/
FROM base AS production
LABEL maintainer="Denys Zhdanov <denis.zhdanov@gmail.com>"
ENV STATSD_INTERFACE="udp"
COPY conf /
#   copy /opt from build image
COPY --from=build /opt /opt
#   defaults
EXPOSE 80/tcp 2003-2004 2013-2014 2023-2024 8080/tcp 8125/tcp 8125/udp 8126/tcp
VOLUME ["/opt/graphite/conf", "/opt/graphite/storage", "/opt/graphite/webapp/graphite/functions/custom", "/etc/nginx", "/opt/statsd/config", "/etc/logrotate.d", "/var/log", "/var/lib/redis"]
STOPSIGNAL SIGHUP
ENTRYPOINT ["/entrypoint"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
