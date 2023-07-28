#   Docker container for Observium Community Edition
#
#   It requires option of e.g. '--link observiumdb:observiumdb' with another MySQL or MariaDB container.
#   Example usage:
#   1. MySQL or MariaDB container
#      $ docker run --name observiumdb \
#          -v /home/docker/observium/data:/var/lib/mysql \
#          -e MYSQL_ROOT_PASSWORD=passw0rd \
#          -e MYSQL_USER=observium \
#          -e MYSQL_PASSWORD=passw0rd \
#          -e MYSQL_DATABASE=observium \
#          mariadb
#
#   2. This Observium container
#      $ docker run --name observiumapp --link observiumdb:observiumdb \
#          -v /home/docker/observium/logs:/opt/observium/logs \
#          -v /home/docker/observium/rrd:/opt/observium/rrd \
#          -e OBSERVIUM_ADMIN_USER=admin \
#          -e OBSERVIUM_ADMIN_PASS=passw0rd \
#          -e OBSERVIUM_DB_HOST=observiumdb \
#          -e OBSERVIUM_DB_USER=observium \
#          -e OBSERVIUM_DB_PASS=passw0rd \
#          -e OBSERVIUM_DB_NAME=observium \
#          -e OBSERVIUM_BASE_URL=http://yourserver.yourdomain:80 \
#          -p 80:80 mbixtech/observium
#
#   References:
#    - Follow platform guideline specified in https://github.com/docker-library/official-images
#   
FROM ubuntu:16.04
LABEL maintainer="\"somsakc@hotmail.com\""
LABEL version="1.2"
LABEL description="Docker container for Observium Community Edition"
ARG OBSERVIUM_ADMIN_USER=admin
ARG OBSERVIUM_ADMIN_PASS=passw0rd
ARG OBSERVIUM_DB_HOST=observiumdb
ARG OBSERVIUM_DB_USER=observium
ARG OBSERVIUM_DB_PASS=passw0rd
ARG OBSERVIUM_DB_NAME=observium
#   set environment variables
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
ENV OBSERVIUM_DB_HOST="$OBSERVIUM_DB_HOST"
ENV OBSERVIUM_DB_USER="$OBSERVIUM_DB_USER"
ENV OBSERVIUM_DB_PASS="$OBSERVIUM_DB_PASS"
ENV OBSERVIUM_DB_NAME="$OBSERVIUM_DB_NAME"
#   install prerequisites
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libapache2-mod-php7.0=7.0.33-0ubuntu0.16.04.16 php7.0-cli=7.0.33-0ubuntu0.16.04.16 php7.0-mysql=7.0.33-0ubuntu0.16.04.16 php7.0-mysqli php7.0-gd=7.0.33-0ubuntu0.16.04.16 php7.0-mcrypt=7.0.33-0ubuntu0.16.04.16 php7.0-json=7.0.33-0ubuntu0.16.04.16 php-pear=1:1.10.1+submodules+notgz-6ubuntu0.3 snmp=5.7.3+dfsg-1ubuntu4.6 fping=3.13-1 mysql-client=5.7.33-0ubuntu0.16.04.1 python-mysqldb=1.3.7-1build2 rrdtool=1.5.5-4 subversion=1.9.3-2ubuntu1.3 whois=5.2.11 mtr-tiny=0.86-1ubuntu0.1 ipmitool=1.8.16-3ubuntu0.2 graphviz=2.38.0-12ubuntu2.1 imagemagick=8:6.8.9.9-7ubuntu5.16 apache2=2.4.18-2ubuntu3.17 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libvirt-bin=1.3.1-1ubuntu10.31 -y )
RUN (apt-get update ;apt-get install --no-install-recommends cron=3.0pl1-128ubuntu2 supervisor=3.2.0-2ubuntu0.2 wget=1.17.1-1ubuntu1.5 locales=2.23-0ubuntu11.3 -y )
RUN apt-get clean
#   set locale
RUN locale-gen en_US.UTF-8
#   install observium package
RUN mkdir -p /opt/observium /opt/observium/lock /opt/observium/logs /opt/observium/rrd
RUN cd /opt \
 && wget http://www.observium.org/observium-community-latest.tar.gz \
 && tar zxvf observium-community-latest.tar.gz \
 && rm observium-community-latest.tar.gz
#   check version
RUN [ -f /opt/observium/VERSION ] \
 && cat /opt/observium/VERSION
#   configure observium package
RUN cd /opt/observium \
 && cp config.php.default config.php \
 && sed -i -e "s/= 'localhost';/= getenv('OBSERVIUM_DB_HOST');/g" config.php \
 && sed -i -e "s/= 'USERNAME';/= getenv('OBSERVIUM_DB_USER');/g" config.php \
 && sed -i -e "s/= 'PASSWORD';/= getenv('OBSERVIUM_DB_PASS');/g" config.php \
 && sed -i -e "s/= 'observium';/= getenv('OBSERVIUM_DB_NAME');/g" config.php \
 && echo "$config['base_url'] = getenv('OBSERVIUM_BASE_URL');" >> config.php
COPY observium-init /opt/observium/observium-init.sh
RUN chmod a+x /opt/observium/observium-init.sh
RUN chown -R www-data:www-data /opt/observium
RUN find /opt -ls
#  RUN cd /opt/observium && \
#      ./discovery.php -u && \
#      ./adduser.php $OBSERVIUM_ADMIN_USER $OBSERVIUM_ADMIN_PASS 10
#   configure php modules
RUN phpenmod mcrypt
#   configure apache modules
RUN a2dismod mpm_event \
 && a2enmod mpm_prefork \
 && a2enmod php7.0 \
 && a2enmod rewrite
#   configure apache configuration
#  RUN mv /etc/apache2/sites-available/000-default.conf /etc/apache2/sites-available/000-default.conf.orig
COPY observium-apache24 /etc/apache2/sites-available/000-default.conf
RUN rm -fr /var/www
#   configure observium cron job
#  COPY observium-cron /etc/cron.d/observium
COPY observium-cron /tmp/observium
RUN echo "" >> /etc/crontab \
 && cat /tmp/observium >> /etc/crontab \
 && rm -f /tmp/observium
#   configure container interfaces
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf
CMD ["/usr/bin/supervisord"]
EXPOSE 80/tcp
VOLUME ["/opt/observium/lock", "/opt/observium/logs","/opt/observium/rrd"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
