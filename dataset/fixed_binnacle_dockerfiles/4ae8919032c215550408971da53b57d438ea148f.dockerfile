#   OData producer on top of MySQL and Apache
#
#   VERSION               0.0.1
FROM ubuntu:trusty
#   Format: MAINTAINER Name <email@addr.ess>
MAINTAINER Jonas Colmsjö <jonas@gizur.com>
RUN echo "export HOME=/root" >> /root/.profile
#   Mirros: http://ftp.acc.umu.se/ubuntu/ http://us.archive.ubuntu.com/ubuntu/
RUN echo "deb http://ftp.acc.umu.se/ubuntu/ trusty-updates main restricted" > /etc/apt/source.list
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 nano=2.2.6-1ubuntu1 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 -y )
#
#   Install supervisord (used to handle processes)
#   ----------------------------------------------
#
#   Installation with easy_install is more reliable. apt-get don't always work.
RUN (apt-get update ;apt-get install --no-install-recommends python=2.7.5-5ubuntu3 python-setuptools=3.3-1ubuntu2 -y )
RUN easy_install supervisor
COPY ./src-docker/etc-supervisord.conf /etc/supervisord.conf
COPY ./src-docker/etc-supervisor-conf.d-supervisord.conf /etc/supervisor/conf.d/supervisord.conf
RUN mkdir -p /var/log/supervisor/
#
#   Install rsyslog
#   ---------------
#
RUN (apt-get update ;apt-get install --no-install-recommends rsyslog=7.4.4-1ubuntu2.7 -y )
RUN mv /etc/rsyslog.conf /etc/rsyslog.conf.org
COPY ./src-docker/etc-rsyslog.conf /etc/rsyslog.conf
#
#   Install cron
#   ------------
#   Test to update the server automatically periodically (need to find a way to restart the server also)
#   Just comment this section out to turn it off
#  RUN echo '*/90 * * * *  /bin/bash -c "date > last-run.txt; npm install -g odataserver > ./install.log;source /tmp/odataserver.pid; kill $PID"' > /mycron
#  RUN crontab /mycron
#  ADD ./src-docker/etc-pam.d-cron /etc/pam.d/cron
#
#   Install Apache
#   ---------------
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.7-1ubuntu4.22 php5=5.5.9+dfsg-1ubuntu4.29 php5-curl=5.5.9+dfsg-1ubuntu4.29 php5-mysql=5.5.9+dfsg-1ubuntu4.29 php5-mcrypt=5.4.6-0ubuntu5 -y )
RUN a2enmod rewrite status
COPY ./src-docker/etc-apache2-apache2.conf /etc/apache2/apache2.conf
COPY ./src-docker/etc-apache2-ports.conf /etc/apache2/ports.conf
COPY ./src-docker/etc-apache2-mods-available-status.conf /etc/apache2/mods-available/status.conf
COPY ./src-docker/etc-apache2-sites-available-000-default.conf /etc/apache2/sites-available/000-default.conf
RUN rm /var/www/html/index.html
RUN echo "<?php\nphpinfo();\n " > /var/www/html/info.php
#   Install phpMyAdmin
COPY ./src-phpmyadmin/phpMyAdmin-4.0.8-all-languages.tar.gz /var/www/html/
COPY ./src-phpmyadmin/phpMyAdmin-4.3.12-all-languages.tar.gz /var/www/html/
COPY ./src-phpmyadmin/config.inc.php /var/www/html/phpMyAdmin-4.0.8-all-languages/config.inc.php
COPY ./src-phpmyadmin/config.inc.php /var/www/html/phpMyAdmin-4.3.12-all-languages/config.inc.php
#
#   Install NodeJS
#   --------------
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 g++=4:4.8.2-1ubuntu6 -y )
RUN curl https://raw.githubusercontent.com/creationix/nvm/v0.13.1/install.sh | bash
#   RUN echo "[ -s $HOME/.nvm/nvm.sh ] && . $HOME/.nvm/nvm.sh" >> $HOME/.profile
RUN /bin/bash -c "source $HOME/.profile \
 && nvm install v0.12.2 \
 && nvm alias default v0.12.2"
COPY ./src-docker/init-node.sh /src/
RUN /src/init-node.sh
#
#   Install MySQL
#   -------------
#   init script and test db
COPY ./src-docker/init-mysql.sh /src/
#  ADD ./src-sql/test-wp.sql /src/
#  ADD ./src-sql/test-vtiger.sql /src/
#   Install MySQL server
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y mysql-server
#   Fix configuration
RUN sed -i -e"s/^bind-address\s*=\s*127.0.0.1/bind-address = 0.0.0.0/" /etc/mysql/my.cnf
#   Setup admin user
RUN /src/init-mysql.sh
#
#   Add source for the odatamysql server
#   ------------------------------------
#   not used anymore ADD ./bin/start.sh /
COPY ./server.key /
COPY ./server.cer /
#  ADD ./package.json /
#  ADD ./bin/run_tests.sh /bin/
#  ADD ./bin/start.sh /bin/
#  ADD ./src /src
#  ADD ./Usage.md /
#  ADD ./config.js /
#  ADD ./tests /tests
#  RUN cd /; npm install
#
#   Install from npm also (select which version to run from supervisord.conf)
#
RUN mkdir /odataserver
RUN /bin/bash -c "cd /odataserver; npm install odataserver"
RUN /bin/bash -c "cd /odataserver; npm link odataserver"
COPY ./bin/start2.sh /
#
#   Start things
#   -----------
#   RUN apt-get clean && rm -rf /var/lib/apt/lists/*
#   Fix permissions
RUN chown -R www-data:www-data /var/www/html
EXPOSE 80/tcp 81/tcp 443/tcp 9000/tcp
CMD ["supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
