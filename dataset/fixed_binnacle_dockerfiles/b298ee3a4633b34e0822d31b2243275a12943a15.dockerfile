FROM ubuntu:trusty
MAINTAINER Matthieu Caneill <matthieu.caneill@savoirfairelinux.com>
ENV DEBIAN_FRONTEND="noninteractive"
#  ## Init
RUN :
#  ## Utils
RUN (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 python-pip=1.5.4-1ubuntu4 emacs=45.0ubuntu1 curl=7.35.0-1ubuntu2.20 nodejs=0.10.25~dfsg2-2ubuntu1.2 nodejs-legacy=0.10.25~dfsg2-2ubuntu1.2 npm=1.3.10~dfsg-1 vim=2:7.4.052-1ubuntu3.1 wget=1.15-1ubuntu1.14.04.5 -y )
RUN npm install bower@1.8.14 -g
#  ## Other .deb sources
RUN curl http://download.opensuse.org/repositories/home:/kaji-project/xUbuntu_14.04/Release.key | apt-key add -
RUN echo 'deb http://download.opensuse.org/repositories/home:/kaji-project/xUbuntu_14.04/ /' >> /etc/apt/sources.list.d/kaji.list
RUN curl http://download.opensuse.org/repositories/home:/sfl-monitoring:/monitoring-tools/xUbuntu_14.04/Release.key | apt-key add -
RUN echo 'deb http://download.opensuse.org/repositories/home:/sfl-monitoring:/monitoring-tools/xUbuntu_14.04/ /' >> /etc/apt/sources.list.d/shinkenplugins.list
RUN :
#  ## InfluxDB
#   RUN wget http://s3.amazonaws.com/influxdb/influxdb_latest_amd64.deb
RUN wget http://get.influxdb.org.s3.amazonaws.com/influxdb_0.8.9_amd64.deb
#   RUN dpkg -i influxdb_latest_amd64.deb
RUN dpkg -i influxdb_0.8.9_amd64.deb
#  ## Shinken
RUN (apt-get update ;apt-get install --no-install-recommends kaji -y )
#  ## Plugins
RUN (apt-get update ;apt-get install --no-install-recommends nagios-plugins=1.5-3ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends monitoring-plugins-sfl-check-amt-montreal monitoring-plugins-sfl-check-bixi-montreal monitoring-plugins-sfl-check-emergency-rooms-quebec monitoring-plugins-sfl-check-environment-canada monitoring-plugins-sfl-check-http2 monitoring-plugins-sfl-check-quebecrencontrescom monitoring-plugins-sfl-check-reseaucontactcom monitoring-plugins-sfl-check-stm-metro-montreal monitoring-plugins-sfl-check-hydro-quebec -y )
#  ## SSH
RUN (apt-get update ;apt-get install --no-install-recommends openssh-server=1:6.6p1-2ubuntu2.13 -y )
#  ## Apache
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.7-1ubuntu4.22 libapache2-mod-wsgi=3.4-4ubuntu2.1.14.04.2 -y )
#  ## Supervisor
RUN (apt-get update ;apt-get install --no-install-recommends supervisor=3.0b2-1ubuntu0.1 -y )
#  # Shinken hosts/services configuration
RUN (apt-get update ;apt-get install --no-install-recommends python-bs4=4.2.1-1ubuntu2 python-requests=2.2.1-1ubuntu0.4 -y )
#  ## Configuration
#  # Docker
#   makes `df` work
#  RUN ln -s /proc/mounts /etc/mtab
#   run permissions for user `shinken`
RUN chmod u+s /bin/ping
RUN chmod u+s /bin/ping6
#  # Shinken, Apache, Adagios
COPY app /srv/app
COPY scripts /scripts
#  RUN mkdir /etc/shinken/adagios
RUN scripts/banks.py > etc/shinken/adagios/banks.cfg
RUN scripts/dns.py > etc/shinken/adagios/dns.cfg
RUN scripts/websites.py > etc/shinken/adagios/websites.cfg
RUN scripts/hospitals.py > etc/shinken/adagios/hospitals.cfg
RUN scripts/transports.py > etc/shinken/adagios/transports.cfg
RUN scripts/dating.py > etc/shinken/adagios/dating.cfg
RUN scripts/isp.py > etc/shinken/adagios/isp.cfg
RUN scripts/environment.py > etc/shinken/adagios/environment.cfg
RUN scripts/energy.py > etc/shinken/adagios/energy.cfg
#   APP
RUN cd /srv/app \
 && yes | bower install --allow-root
#   Allow ssh connection from host
#  ADD id_rsa.pub /root/.ssh/authorized_keys
#  RUN echo root:root | chpasswd
#  RUN sed -i 's/PermitRootLogin.*/PermitRootLogin Yes/' /etc/ssh/sshd_config
COPY etc /etc
#   rm useless configuration
RUN rm -f /etc/shinken/hosts/*
#  ## Finishing installation
RUN sudo bash /usr/sbin/kaji-finish-install
EXPOSE 80/tcp
EXPOSE 8083/tcp
EXPOSE 22/tcp
CMD ["/usr/bin/supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
