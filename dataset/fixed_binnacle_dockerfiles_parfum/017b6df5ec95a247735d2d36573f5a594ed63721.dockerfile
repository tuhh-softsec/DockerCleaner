FROM phusion/baseimage:0.9.22
MAINTAINER Damien Garros <dgarros@gmail.com>
RUN apt-get update -y \
 && apt-get -y upgrade \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  dependencies
RUN apt-get update -y \
 && apt-get install --no-install-recommends git adduser libfontconfig wget make curl -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN rm -f /etc/service/sshd/down
RUN /usr/sbin/enable_insecure_key
#  Latest version
ENV GRAFANA_VERSION="5.1.3"
ENV INFLUXDB_VERSION="1.5.1"
ENV TELEGRAF_VERSION="1.5.3-1"
RUN apt-get update -y \
 && apt-get install --no-install-recommends build-essential python-simplejson python-dev python-yaml python-pip python-dev libxml2-dev libxslt-dev tcpdump tree nginx-light snmp zlib1g-dev libffi-dev libssl-dev -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN pip install setuptools --upgrade
#  # Install some python modules
RUN pip install influxdb \
 && pip install xmltodict \
 && pip install pexpect \
 && easy_install pysnmp \
 && pip install lxml \
 && pip install python-crontab \
 && pip install pytest \
 && pip install mock \
 && pip install cryptography==2.1.2 \
 && pip install junos-eznc==2.1.7 \
 && pip install enum
RUN mkdir /src
# #######################
# ## Install Grafana
# #######################
RUN mkdir /src/grafana \
 && mkdir /opt/grafana \
 && wget https://s3-us-west-2.amazonaws.com/grafana-releases/release/grafana-${GRAFANA_VERSION}.linux-x64.tar.gz -O /src/grafana.tar.gz \
 && tar -xzf /src/grafana.tar.gz -C /opt/grafana --strip-components=1 \
 && rm /src/grafana.tar.gz
RUN /opt/grafana/bin/grafana-cli plugins install grafana-piechart-panel
# #######################
# ## Install InfluxDB ###
# #######################
RUN curl -s -o /tmp/influxdb_latest_amd64.deb https://dl.influxdata.com/influxdb/releases/influxdb_${INFLUXDB_VERSION}_amd64.deb \
 && dpkg -i /tmp/influxdb_latest_amd64.deb \
 && rm /tmp/influxdb_latest_amd64.deb
COPY docker/influxdb/types.db /usr/share/collectd/types.db
COPY docker/influxdb/influxdb-config.toml /config/config.toml
COPY docker/influxdb/influxdbrun.sh /influxdbrun.sh
RUN mkdir /etc/service/influxdb
COPY docker/influxdb/influxdb.launcher.sh /etc/service/influxdb/run
# #######################
# ## Install telegraf ###
# #######################
RUN curl -s -o /tmp/telegraf_latest_amd64.deb https://dl.influxdata.com/telegraf/releases/telegraf_${TELEGRAF_VERSION}_amd64.deb \
 && dpkg -i /tmp/telegraf_latest_amd64.deb \
 && rm /tmp/telegraf_latest_amd64.deb
COPY docker/telegraf/telegraf.conf /etc/telegraf/telegraf.conf
RUN mkdir /etc/service/telegraf
COPY docker/telegraf/telegraf.launcher.sh /etc/service/telegraf/run
# #######################
# ## Configuration    ###
# #######################
# ## Configure Grafana ###
COPY docker/grafana/custom.ini /opt/grafana/conf/custom.ini
COPY docker/grafana/run.sh /etc/service/grafana/run
# ADD     docker/grafana/grafana.init.sh /etc/my_init.d/grafana.init.sh
# # Add the default dashboards
# RUN     mkdir /src/dashboards && \
RUN mkdir /opt/grafana/data \
 && chown -R www-data /opt/grafana/data
# ## Configure nginx ###
COPY docker/nginx/nginx.conf /etc/nginx/nginx.conf
COPY docker/nginx/run.sh /etc/service/nginx/run
# ## open-nti python scripts (for gathering informatino from server to router)  ###
COPY open-nti/open-nti.py /opt/open-nti/open-nti.py
ADD open-nti/startcron.py /opt/open-nti/startcron.py
COPY tests/main/pyez_mock.py /opt/open-nti/pyez_mock.py
# ## Add test files
RUN mkdir /opt/open-nti/tests
#  ################
RUN chmod +x /etc/service/nginx/run \
 && chmod +x /etc/service/grafana/run \
 && chmod +x /etc/service/influxdb/run \
 && chmod +x /etc/service/telegraf/run \
 && chmod +x /influxdbrun.sh
WORKDIR /
ENV HOME="/root"
ENV SSL_SUPPORT="**False**"
ENV SSL_CERT="**None**"
RUN chmod -R 777 /var/log/
#  ## Graphana
EXPOSE 80/tcp
EXPOSE 3000/tcp
#  # Influxdb Admin server WebUI
EXPOSE 8083/tcp
EXPOSE 8086/tcp
CMD ["/sbin/my_init"]
