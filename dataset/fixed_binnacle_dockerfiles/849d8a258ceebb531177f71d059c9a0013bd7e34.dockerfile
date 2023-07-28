FROM debian:latest
LABEL maintainer="contact@ocsinventory-ng.org"
LABEL version="2.3"
LABEL description="OCS (Open Computers and Software Inventory Next Generation)"
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.55-1ubuntu2 apache2-doc=2.4.55-1ubuntu2 apt-utils=2.6.0 php5 php5-gd php5-mysql php5-cgi php5-curl perl=5.36.0-7 build-essential=12.9ubuntu3 libapache2-mod-php5 libxml2=2.9.14+dfsg-1.1build2 libxml-simple-perl=2.25-2 libc6-dev=2.37-0ubuntu2 libnet-ip-perl=1.26-3 libxml-libxml-perl=2.0207+dfsg+really+2.0134-1build1 libapache2-mod-perl2=2.0.12-1build2 libdbi-perl=1.643-4 libapache-dbi-perl=1.12-3 libdbd-mysql-perl=4.050-5build1 libio-compress-perl=2.201-2 libxml-simple-perl=2.25-2 libsoap-lite-perl=1.27-2 libarchive-zip-perl=1.68-1 libnet-ip-perl=1.26-3 libphp-pclzip libsoap-lite-perl=1.27-2 libarchive-zip-perl=1.68-1 htop=3.2.2-1 git=1:2.39.2-1ubuntu1 wget=1.21.3-1ubuntu1 tar=1.34+dfsg-1.1 unzip=6.0-27ubuntu1 nano=7.2-1 make=4.3-4.1build1 -y )
RUN cpan -i XML::Entities
VOLUME /var/lib/mysql
RUN cp /usr/share/zoneinfo/Europe/Paris /etc/localtime
RUN /usr/sbin/a2dissite 000-default ; /usr/sbin/a2enmod rewrite ; /usr/sbin/a2enmod ssl ; /usr/sbin/a2enmod authz_user
RUN wget https://raw.githubusercontent.com/OCSInventory-NG/OCSInventory-Server/master/binutils/docker-download.sh
RUN sh docker-download.sh 2.3
WORKDIR /tmp/ocs/Apache
RUN perl Makefile.PL ; make ; make install
RUN cp -R blib/lib/Apache /usr/local/share/perl/5.20.2/ ; cp -R Ocsinventory /usr/local/share/perl/5.20.2/ ; cp /tmp/ocs/etc/logrotate.d/ocsinventory-server /etc/logrotate.d/
RUN mkdir -p /etc/ocsinventory-server/plugins ; mkdir -p /etc/ocsinventory-server/perl ; mkdir -p /usr/share/ocsinventory-reports/ocsreports
ENV APACHE_RUN_USER="www-data"
ENV APACHE_RUN_GROUP="www-data"
ENV APACHE_LOG_DIR="/var/log/apache2"
ENV APACHE_PID_FILE="/var/run/apache2.pid"
ENV APACHE_RUN_DIR="/var/run/apache2f"
ENV APACHE_LOCK_DIR="/var/lock/apache2"
ENV APACHE_LOG_DIR="/var/log/apache2"
WORKDIR /tmp/ocs
RUN cp -R ocsreports/* /usr/share/ocsinventory-reports/ocsreports
RUN chown -R www-data: /usr/share/ocsinventory-reports/
RUN bash -c 'mkdir -p /var/lib/ocsinventory-reports/{download,ipd,logs,scripts,snmp}'
RUN chmod -R +w /var/lib/ocsinventory-reports ; chown www-data: -R /var/lib/ocsinventory-reports
COPY dbconfig.inc.php /usr/share/ocsinventory-reports/ocsreports/
RUN cp binutils/ipdiscover-util.pl /usr/share/ocsinventory-reports/ocsreports/ipdiscover-util.pl
RUN chown www-data: /usr/share/ocsinventory-reports/ocsreports/ipdiscover-util.pl ; chmod 755 /usr/share/ocsinventory-reports/ocsreports/ipdiscover-util.pl ; chmod +w /usr/share/ocsinventory-reports/ocsreports/dbconfig.inc.php ; mkdir -p /var/log/ocsinventory-server/ ; chmod +w /var/log/ocsinventory-server/
COPY /conf/ocsinventory-reports.conf /etc/apache2/conf-available/
COPY /conf/z-ocsinventory-server.conf /etc/apache2/conf-available/
COPY ./scripts/run.sh /root/run.sh
RUN chmod +x /root/run.sh
RUN ln -s /etc/apache2/conf-available/ocsinventory-reports.conf /etc/apache2/conf-enabled/ocsinventory-reports.conf
RUN ln -s /etc/apache2/conf-available/z-ocsinventory-server.conf /etc/apache2/conf-enabled/z-ocsinventory-server.conf
RUN rm /usr/share/ocsinventory-reports/ocsreports/install.php ; rm -rf /tmp/ocs
EXPOSE 80/tcp
EXPOSE 443/tcp
CMD ["/bin/bash", "/root/run.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
