#   template with supervisord
#
#   VERSION               0.0.1
#
#
FROM ubuntu:trusty
MAINTAINER Jonas Colmsjö "jonas@gizur.com"
RUN echo "export HOME=/root" >> /root/.profile
#   Mirros: http://ftp.acc.umu.se/ubuntu/ http://us.archive.ubuntu.com/ubuntu/
RUN echo "deb http://ftp.acc.umu.se/ubuntu/ trusty-updates main restricted" >> /etc/apt/source.list
RUN :
#   Some good utils
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 nano=2.2.6-1ubuntu1 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 -y )
#   Install supervisord (used to handle processes)
#   ----------------------------------------------
#
#   Installation with easy_install is more reliable. apt-get don't always work.
RUN (apt-get update ;apt-get install --no-install-recommends python=2.7.5-5ubuntu3 python-setuptools=3.3-1ubuntu2 -y )
RUN easy_install supervisor
COPY ./etc-supervisord.conf /etc/supervisord.conf
COPY ./etc-supervisor-conf.d-supervisord.conf /etc/supervisor/conf.d/supervisord.conf
RUN mkdir -p /var/log/supervisor/
#
#   Install pip (for python)
#
RUN easy_install pip
#
#   Install rsyslog
#   ---------------
RUN (apt-get update ;apt-get install --no-install-recommends rsyslog=7.4.4-1ubuntu2.7 -y )
COPY ./etc-rsyslog.conf /etc/rsyslog.conf
#
#   Install cron and batches
#   ------------------------
#   Run backup job every hour
#  ADD ./backup.sh /
#  RUN echo '0 1 * * *  /bin/bash -c "/backup.sh"' > /mycron
#   Run job every minute
RUN echo '*/1 * * * * /bin/bash -c "/batches.sh"' >> /mycron
#  RUN crontab /mycron
COPY ./etc-pam.d-cron /etc/pam.d/cron
#
#   Install Apache
#   ---------------
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.7-1ubuntu4.22 apache2-dev=2.4.7-1ubuntu4.22 -y )
RUN a2enmod rewrite status
COPY ./etc-apache2-apache2.conf /etc/apache2/apache2.conf
COPY ./etc-apache2-mods-available-status.conf /etc/apache2/mods-available/status.conf
RUN rm /var/www/html/index.html
RUN echo "<?php\nphpinfo();\n " > /var/www/html/info.php
#
#   Some pre-requisites
#   ------------------
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 python-dev=2.7.5-5ubuntu3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python-cairo=1.8.8-1ubuntu5 python-django=1.6.11-0ubuntu1.3 python-twisted=13.2.0-1ubuntu1.2 -y )
RUN pip install django-tagging==0.5.0
RUN easy_install --upgrade pytz
RUN (apt-get update ;apt-get install --no-install-recommends fontconfig=2.11.0-0ubuntu4.2 python-fontconfig=0.5.1-1build1 -y )
#
#   Install graphite
#   ----------------
RUN pip install https://github.com/graphite-project/ceres/tarball/master
RUN pip install whisper==1.1.10
RUN pip install carbon==1.1.10
RUN pip install graphite-web==1.1.10
EXPOSE 80/tcp 443/tcp
CMD ["supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
