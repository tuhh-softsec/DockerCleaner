#  FROM debian:buster
FROM ubuntu:16.04
#  LABEL de.dcso.misp-server.version="0.0.1-alpha"
LABEL vendor="DCSO GmbH <www.dcso.de>"
LABEL de.dcso.misp-server.release-date="2018-01-02"
LABEL de.dcso.misp-server.is-production="false"
LABEL maintainer="DCSO MISP <misp@dcso.de>"
#   Variables:
ARG MISP_TAG=v2.4.88
ARG python_cybox_TAG=v2.1.0.12
ARG python_stix_TAG=v1.1.1.4
ARG mixbox_TAG=v1.0.2
ARG cake_resque_TAG=4.1.2
#  ########################################
#         Start of MISP Config Part
#   Install core components
ENV DEBIAN_FRONTEND="noninteractive"
RUN : \
 && apt-get autoremove -y \
 && apt-get clean -y
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends supervisor=3.2.0-2ubuntu0.2 nano=2.5.3-2ubuntu2 vim=2:7.4.1689-3ubuntu1.5 curl=7.47.0-1ubuntu2.19 gcc=4:5.3.1-1ubuntu1 make=4.1-6 python=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python3=3.5.1-3 python3-pip=8.1.1-2ubuntu0.6 locales=2.23-0ubuntu11.3 zip=3.0-11 iputils-ping=3:20121221-5ubuntu2 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 make=4.1-6 openssl=1.0.2g-1ubuntu4.20 vim=2:7.4.1689-3ubuntu1.5 zip=3.0-11 net-tools=1.60-26ubuntu1 sudo=1.8.16-0ubuntu1.10 -y )
#   Install additional dependencies
RUN (apt-get update ;apt-get install --no-install-recommends mariadb-client=10.0.38-0ubuntu0.16.04.1 python-mysqldb=1.3.7-1build2 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python3-setuptools=20.7.0-1 python-setuptools=20.7.0-1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 python-setuptools=20.7.0-1 -y )
#   Install Apache
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.18-2ubuntu3.17 apache2-doc=2.4.18-2ubuntu3.17 apache2-utils=2.4.18-2ubuntu3.17 -y )
#   ATTENTION the part about a2enmod/a2dismod, a2ensite/a2dissite is moved to step 7.
#   Install PHP and depedencies
RUN (apt-get update ;apt-get install --no-install-recommends libapache2-mod-php=1:7.0+35ubuntu6.1 php=1:7.0+35ubuntu6.1 php-cli=1:7.0+35ubuntu6.1 php-crypt-gpg=1.4.0-1ubuntu2 php-dev=1:7.0+35ubuntu6.1 php-json=1:7.0+35ubuntu6.1 php-mysql=1:7.0+35ubuntu6.1 php-opcache php-readline=1:7.0+35ubuntu6.1 php-redis=2.2.7-389-g2887ad1+2.2.7-1 php-xml=1:7.0+35ubuntu6.1 -y )
#   Set locals
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
#   Update PIP
RUN pip install pip==23.1 --upgrade
RUN pip3 install --upgrade pip
#  #################################
#  ## Install and configure MISP ###
#  #################################
#  ## 3/ MISP code ###
#   Download MISP using git in the /var/www/ directory.
#   Attention: we replaced the fixed tag with a variable
RUN mkdir /var/www/MISP ; chown www-data:www-data /var/www/MISP
RUN git clone https://github.com/MISP/MISP.git /var/www/MISP
RUN cd /var/www/MISP ; git checkout tags/${MISP_TAG}
#   Make git ignore filesystem permission differences
RUN cd /var/www/MISP ; git config core.filemode false
#   install Mitre's STIX and its dependencies by running the following commands:
#   Attention: we replaced the fixed tag with a variable
RUN (apt-get update ;apt-get install --no-install-recommends python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 python-setuptools=20.7.0-1 )
RUN cd /var/www/MISP/app/files/scripts ; git clone https://github.com/CybOXProject/python-cybox.git
RUN cd /var/www/MISP/app/files/scripts ; git clone https://github.com/STIXProject/python-stix.git
RUN cd /var/www/MISP/app/files/scripts/python-cybox ; git checkout ${python_cybox_TAG} ; sudo python setup.py install
RUN cd /var/www/MISP/app/files/scripts/python-stix ; git checkout ${python_stix_TAG} ; sudo python setup.py install
#   install mixbox to accomodate the new STIX dependencies:
#   Attention: we replaced the fixed tag with a variable
RUN cd /var/www/MISP/app/files/scripts/ ; git clone https://github.com/CybOXProject/mixbox.git
RUN cd /var/www/MISP/app/files/scripts/mixbox ; git checkout ${mixbox_TAG} ; sudo python setup.py install
#   install support for STIX 2.0 (Python 3 is required)
RUN pip3 install stix2
#  ## 4/ CakePHP ###
#   CakePHP is included as a submodule of MISP, execute the following commands to let git fetch it:
RUN cd /var/www/MISP ; git submodule init ; git submodule update
#   Make git ignore filesystem permission differences for submodules
RUN cd /var/www/MISP ; git submodule foreach git config core.filemode false
#   Once done, install CakeResque along with its dependencies if you intend to use the built in background jobs:
RUN cd /var/www/MISP/app ; sudo -u www-data php composer.phar require kamisama/cake-resque:${cake_resque_TAG} ; php composer.phar config vendor-dir Vendor ; php composer.phar install
#   Enable CakeResque with php-redis
RUN sudo phpenmod redis
#   To use the scheduler worker for scheduled tasks, do the following:
RUN cp -fa /var/www/MISP/INSTALL/setup/config.php /var/www/MISP/app/Plugin/CakeResque/Config/config.php
#   If you have multiple MISP instances on the same system, don't forget to have a different Redis per MISP instance for the CakeResque workers
#   The default Redis port can be updated in Plugin/CakeResque/Config/config.php
#  ## 5/ Set the permissions
#   Check if the permissions are set correctly using the following commands:
RUN chown -R www-data:www-data /var/www/MISP ; chmod -R 750 /var/www/MISP ; chmod -R g+ws /var/www/MISP/app/tmp ; chmod -R g+ws /var/www/MISP/app/files ; chmod -R g+ws /var/www/MISP/app/files/scripts/tmp
#  ## 6 Create a database and user
#   At the moment this will be done via misp-robot.
#  RUN mysql -u misp -p`cat /run/secrets/mysql_password` -h misp-db misp < /var/www/MISP/INSTALL/MYSQL.sql
#  ## 7 Configure Apache
#   add HTTP MISP Config
RUN rm /etc/apache2/sites-available/* ; rm /etc/apache2/sites-enabled/*
COPY files/misp*.conf /etc/apache2/sites-available/
#  COPY files/misp.ssl.conf /etc/apache2/sites-available/misp.ssl.conf
COPY files/ports.conf /etc/apache2/ports.conf
RUN chmod 644 /etc/apache2/ports.conf
#   add HTTPS MISP Config - THIS SHOULD BE DONE IN ROBOT
#  RUN mkdir /etc/apache2/ssl
#  RUN openssl req -x509 -newkey rsa:4096 -keyout /etc/apache2/ssl/key.pem -out /etc/apache2/ssl/cert.pem -days 3650 -nodes -subj '/CN=misp-server'
#  RUN openssl dhparam -out /etc/apache2/ssl/dhparams.pem 2048
#  RUN chmod -R 644 /etc/apache2/ssl
#   Configure Apache
RUN sudo a2dismod status ; sudo a2enmod ssl ; sudo a2enmod rewrite ; sudo a2enmod headers ; sudo a2ensite misp
#  ## 8/ Log rotation
#   MISP saves the stdout and stderr of its workers in /var/www/MISP/app/tmp/logs
#   To rotate these logs install the supplied logrotate script:
RUN sudo cp /var/www/MISP/INSTALL/misp.logrotate /etc/logrotate.d/misp
#  ## 9/ MISP configuration
#   There are 4 sample configuration files in /var/www/MISP/app/Config that need to be copied
RUN sudo -u www-data cp -a /var/www/MISP/app/Config/bootstrap.default.php /var/www/MISP/app/Config/bootstrap.php
RUN sudo -u www-data cp -a /var/www/MISP/app/Config/database.default.php /var/www/MISP/app/Config/database.php
RUN sudo -u www-data cp -a /var/www/MISP/app/Config/core.default.php /var/www/MISP/app/Config/core.php
RUN sudo -u www-data cp -a /var/www/MISP/app/Config/config.default.php /var/www/MISP/app/Config/config.php
#  ## 5/ Set the permissions
#   Check if the permissions are set correctly using the following commands:
RUN chown -R www-data:www-data /var/www/MISP ; chmod -R 750 /var/www/MISP ; chmod -R g+ws /var/www/MISP/app/tmp ; chmod -R g+ws /var/www/MISP/app/files ; chmod -R g+ws /var/www/MISP/app/files/scripts/tmp
#         END of MISP Config Part
#  ########################################
#  ########################################
#         Start of DCSO MISP Config Part
COPY files/php.ini /etc/php/7.0/apache2/
#   define the WORKDIR if you use docker exec
WORKDIR /var/www/MISP
#   Environment Variable for Proxy
ENV HTTP_PROXY=""
ENV NO_PROXY="0.0.0.0"
#   Add Healthcheck Config
HEALTHCHECK --interval=60s --timeout=15s --retries=3 CMD curl -f http://localhost/ || exit 1
#   CMD
COPY files/entrypoint.sh /srv/entrypoint.sh
RUN chmod +x /srv/entrypoint.sh
ENTRYPOINT ["/srv/entrypoint.sh"]
#   CMD ["/usr/sbin/apache2ctl", "-D", "FOREGROUND"]
#         End of DCSO MISP Config Part
#  ########################################
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
