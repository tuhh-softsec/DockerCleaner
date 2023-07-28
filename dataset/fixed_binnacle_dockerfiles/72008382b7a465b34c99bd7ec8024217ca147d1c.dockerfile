FROM phusion/baseimage:0.9.16
#   TODO Update Grafana to 2.0.2; consider using .deb installer?
ENV GRAFANA_VERSION="1.9.1"
ENV INFLUXDB_VERSION="0.8.8"
#   Prevent some error messages
ENV DEBIAN_FRONTEND="noninteractive"
#  RUN		echo 'deb http://us.archive.ubuntu.com/ubuntu/ trusty universe' >> /etc/apt/sources.list
RUN : \
 && apt-get -y upgrade
#   ---------------- #
#     Installation   #
#   ---------------- #
#   Install all prerequisites
RUN (apt-get update ;apt-get install --no-install-recommends wget nginx-light curl -y )
#   Install Grafana to /src/grafana
RUN mkdir -p src/grafana \
 && cd src/grafana \
 && wget http://grafanarel.s3.amazonaws.com/grafana-${GRAFANA_VERSION}.tar.gz -O grafana.tar.gz \
 && tar xzf grafana.tar.gz --strip-components=1 \
 && rm grafana.tar.gz
#   Install InfluxDB
RUN wget http://s3.amazonaws.com/influxdb/influxdb_${INFLUXDB_VERSION}_amd64.deb \
 && dpkg -i influxdb_${INFLUXDB_VERSION}_amd64.deb \
 && rm influxdb_${INFLUXDB_VERSION}_amd64.deb
#   ----------------- #
#     Configuration   #
#   ----------------- #
#   Configure InfluxDB
COPY influxdb/config.toml /etc/influxdb/config.toml
COPY influxdb/run.sh /etc/service/influxdb/run
#   These two databases have to be created. These variables are used by set_influxdb.sh and set_grafana.sh
ENV PRE_CREATE_DB="data grafana"
ENV INFLUXDB_DATA_USER="data"
ENV INFLUXDB_DATA_PW="data"
ENV INFLUXDB_GRAFANA_USER="grafana"
ENV INFLUXDB_GRAFANA_PW="grafana"
ENV ROOT_PW="root"
#   Configure Grafana
COPY ./grafana/config.js /src/grafana/config.js
#  ADD	./grafana/scripted.json /src/grafana/app/dashboards/default.json
COPY ./configure.sh /configure.sh
COPY ./set_grafana.sh /set_grafana.sh
COPY ./set_influxdb.sh /set_influxdb.sh
RUN /configure.sh
#   Configure nginx (that serves Grafana)
COPY ./nginx/run.sh /etc/service/nginx/run
COPY ./nginx/nginx.conf /etc/nginx/nginx.conf
#   -------------- #
#     CloudWatch   #
#   -------------- #
#   Add a script run automatically at startup that creates /docker.env
#   so that the Cron job can access the AWS credentials env variables
COPY cloudwatch/env2file /etc/my_init.d/env2file
RUN (apt-get update ;apt-get install --no-install-recommends python-pip -y )
RUN pip install PyYAML==6.0 --global-option="--without-libyaml"
#   ^- libyaml seems to be unavailable here; cloudwatch dependency
RUN pip install cloudwatch-to-graphite==0.5.0
COPY cloudwatch/leadbutt-cloudwatch.conf /etc/leadbutt-cloudwatch.conf
COPY cloudwatch/leadbutt-cloudwatch-cron.conf /etc/cron.d/leadbutt-cloudwatch
#   TODO(improvement) use crontab fragments in /etc/cron.d/ instead of using root's crontab
#                       See for other tips: http://stackoverflow.com/questions/26822067/running-cron-python-jobs-within-docker
RUN crontab /etc/cron.d/leadbutt-cloudwatch
#   Note: AWS cedentials should be provided via ENV vars; ex.:
#       docker run -e AWS_ACCESS_KEY_ID=xxxx -e AWS_SECRET_ACCESS_KEY=yyyy ...
#   ----------- #
#     Cleanup   #
#   ----------- #
RUN apt-get autoremove -y wget curl \
 && apt-get -y clean \
 && rm -rf /var/lib/apt/lists/* \
 && rm /*.sh
#   ----------- #
#     Volumes   #
#   ----------- #
COPY configure_influxdb_at_run.sh /etc/my_init.d/configure_influxdb_at_run.sh
RUN cp -r /var/easydeploy/share /var/infuxdb_initial_data_backup
#   influxdb data dir:
VOLUME ["/var/easydeploy/share"]
#   ---------------- #
#     Expose Ports   #
#   ---------------- #
#   Grafana
EXPOSE 80/tcp
#   InfluxDB Admin server
EXPOSE 8083/tcp
#   InfluxDB HTTP API
EXPOSE 8086/tcp
#   InfluxDB HTTPS API
EXPOSE 8084/tcp
#   -------- #
#     Run!   #
#   -------- #
CMD /sbin/my_init
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
