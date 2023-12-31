# https://medium.com/@meeramarygeorge/create-php-mysql-apache-development-environment-using-docker-in-windows-9beeba6985
# https://www.digitalocean.com/community/tutorials/how-to-secure-apache-with-let-s-encrypt-on-ubuntu-18-04
# https://loige.co/using-lets-encrypt-and-certbot-to-automate-the-creation-of-certificates-for-openvpn/
# FROM ubuntu:latest
FROM ubuntu:18.04
MAINTAINER Name<admin@opensource.lk>
ENV DEBIAN_FRONTEND="noninteractive"
ENV DOMAIN="${DOMAIN}"
#  Install basics
# RUN apt-get update
RUN apt-get update --fix-missing \
 && apt-get -y purge exim4*
RUN apt-get -y upgrade
RUN apt-get install --no-install-recommends apt-utils -y
#  installing netstat command
RUN apt-get install --no-install-recommends net-tools -y
#  installing ping command
RUN apt-get install --no-install-recommends iputils-ping -y
#  install mail until for testing functions
RUN apt-get install --no-install-recommends mailutils -y
#  installing lsof command
RUN apt-get install --no-install-recommends lsof -y
RUN apt-get install --no-install-recommends telnet -y
RUN apt-get install --no-install-recommends nano -y
RUN apt-get install --no-install-recommends letsencrypt openssl -y
RUN apt-get install --no-install-recommends software-properties-common -y \
 && add-apt-repository ppa:ondrej/php \
 && apt-get update
# RUN apt-get install -y — force-yes curl
RUN apt-get install --no-install-recommends curl -y
#  installing ping command
RUN apt-get install --no-install-recommends iputils-ping -y
RUN apt-get update
#  Install PHP 5.6
# RUN apt-get install -y — allow-unauthenticated php5.6 php5.6-mysql php5.6-mcrypt php5.6-cli php5.6-gd php5.6-curl
# RUN apt-get install -y php5.6 php5.6-mysql php5.6-mcrypt php5.6-cli php5.6-gd php5.6-curl
#  Enable apache mods.
#  Install php 7.3
#  https://thishosting.rocks/install-php-on-ubuntu/
# RUN apt-get install -y php
RUN apt-get install --no-install-recommends php7.3 -y
#  php version should be save as the php version number
# RUN apt-get install -y php7.3-mbstring php7.3-ldap php7.3-zip php7.3-xml
RUN apt-get install --no-install-recommends php-pear php7.3-curl php7.3-ldap php7.3-dev php7.3-gd php7.3-mbstring php7.3-zip php7.3-mysql php7.3-xml -y
# RUN a2enmod php5.6
RUN a2enmod rewrite
#  Update the PHP.ini file, enable <? ?> tags and quieten logging.
# RUN sed -i "s/short_open_tag = Off/short_open_tag = On/" /etc/php/5.6/apache2/php.ini
# RUN sed -i "s/error_reporting = .*$/error_reporting = E_ERROR | E_WARNING | E_PARSE/" /etc/php/5.6/apache2/php.ini
#  Manually set up the apache environment variables
ENV APACHE_LOG_DIR="/var/log/apache2"
ENV APACHE_LOCK_DIR="/var/lock/apache2"
ENV APACHE_PID_FILE="/var/run/apache2.pid"
#  Manually set up the apache environment variables
ENV APACHE_RUN_USER="www-data"
ENV APACHE_RUN_GROUP="www-data"
#  Expose to letsencript key generation ACME test
# EXPOSE 80
#  Expose for non secured access (without https) for testing perposes
# EXPOSE 89
#  primary https port
EXPOSE 443/tcp
#  secondary https port
EXPOSE 433/tcp
# EXPOSE 3306
#  Update the default apache site with the config we created.
# ADD ./config/apache-config.conf /etc/apache2/sites-enabled/000-default.conf
COPY ./config/hosts /etc/hosts
#  Copy site into place.
COPY ./app/ /var/www/html/site/
# RUN  copy cert files
# ADD ./tls/cert.pem /etc/ssl/certs/copper.opensource.lk.cert.pem
COPY ./tls/cert.pem /etc/ssl/certs/cert.pem
# ADD ./tls/privkey.pem /etc/ssl/private/copper.opensource.lk.privkey.pem
COPY ./tls/privkey.pem /etc/ssl/private/privkey.pem
# ADD ssl certificate list als
COPY ./tls/copper.opensource.lk.crt /etc/ssl/certs/copper.opensource.lk.crt
COPY ./tls/copper.opensource.lk.key /etc/ssl/private/copper.opensource.lk.key
# RUN chmod -R 777 /var/www/html/site/app
RUN chown -R www-data:www-data /var/www/html/site/
#  Update the default apache site with the config we created.
# ADD ./config/apache-config.conf /etc/apache2/sites-enabled/example.com.conf
RUN apt-get install --no-install-recommends nano -y
#  Lets encript
#  First, add the repository:
RUN add-apt-repository -y ppa:certbot/certbot 2> /dev/null || true
# Install Certbot's Apache package with apt:
RUN apt-get install --no-install-recommends -y python-certbot-apache
#  reload the configuration
# RUN systemctl reload apache2
#  manual lets encript key generation tool
# certbot --apache -d example.com -d www.example.com
# CMD certbot --apache -d copper.opensource.lk -d copper.opensource.lk
#  with certbot-auto
# https://www.exratione.com/2016/06/a-simple-setup-and-installation-script-for-lets-encrypt-ssl-certificates/
RUN apt-get install --no-install-recommends wget -y
RUN wget https://dl.eff.org/certbot-auto
RUN chmod a+x certbot-auto
RUN mv certbot-auto /usr/local/bin
RUN certbot-auto --noninteractive --os-packages-only 2> /dev/null || true
#  Use this command if a webserver is already running with the webroot
#  at /var/www/html.
# RUN certbot-auto certonly \
RUN certbot-auto --non-interactive --agree-tos --text --rsa-key-size 4096 --email admin@copper.opensource.lk --domains copper.opensource.lk --webroot-path /var/www/html/site/ --apache 2> /dev/null || true
# --help plugins \
# RUN cp /etc/letsencrypt/live/copper.opensource.lk/fullchain.pem /etc/ssl/certs/copper.opensource.lk.fullchain.pem
# RUN cp /etc/letsencrypt/live/copper.opensource.lk/privkey.pem /etc/ssl/private/copper.opensource.lk.privkey.pem
# RUN cp ./tls/cert.pem /etc/ssl/certs/copper.opensource.lk.cert.pem
# RUN cp ./tls/privkey.pem /etc/ssl/private/copper.opensource.lk.privkey.pem
#  this for copper live server
COPY ./config/copper.http.conf /etc/apache2/sites-enabled/copper.http.conf
COPY ./config/copper.https.conf /etc/apache2/sites-enabled/copper.https.conf
#  coppies port configuration
COPY ./config/ports.conf /etc/apache2/ports.conf
#  adding ports.conf file to the image
COPY ./config/ports.conf /etc/apache2/
RUN certbot renew --dry-run
#  By default start up apache in the foreground, override with /bin/bash for interative.
# CMD chmod -R 777 /var/www/html/data/
#  Attempting to copy to host file to container or change the content
#  adding host file
COPY ./config/hosts /etc/hosts
#  put new entry to the host file
RUN echo "127.0.0.1 copper.opensource.lk copper" >> /etc/hosts
# RUN sed -i "s/127.0.0.1 = .*$/127.0.0.1 = copper.opensource.lk/" /etc/hosts
#  enable https in apache
RUN a2enmod ssl
COPY ./config/init_sys.sh /bin/
RUN chmod +x /bin/init_sys.sh
#  changing apache start to entrypoint to let kubernetes to start the system
ENTRYPOINT ["init_sys.sh"]
# CMD /usr/sbin/apache2ctl -D FOREGROUND
