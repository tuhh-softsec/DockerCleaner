#   OData producer on top of MySQL and Apache
#
#   VERSION               0.0.1
FROM ubuntu:14.10
#   Format: MAINTAINER Name <email@addr.ess>
MAINTAINER Jonas Colmsjö <jonas@gizur.com>
RUN echo "export HOME=/root" >> /root/.profile
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends wget nano curl git -y )
#
#   Install supervisord (used to handle processes)
#   ----------------------------------------------
#
#   Installation with easy_install is more reliable. apt-get don't always work.
RUN (apt-get update ;apt-get install --no-install-recommends python python-setuptools -y )
RUN easy_install supervisor
COPY ./etc-supervisord.conf /etc/supervisord.conf
COPY ./etc-supervisor-conf.d-supervisord.conf /etc/supervisor/conf.d/supervisord.conf
RUN mkdir -p /var/log/supervisor/
#
#   Install Apache
#   ---------------
RUN (apt-get update ;apt-get install --no-install-recommends apache2 php5 php5-curl php5-mysql php5-mcrypt -y )
RUN a2enmod rewrite status
COPY ./etc-apache2-apache2.conf /etc/apache2/apache2.conf
COPY ./etc-apache2-ports.conf /etc/apache2/ports.conf
COPY ./etc-apache2-mods-available-status.conf /etc/apache2/mods-available/status.conf
RUN rm /var/www/html/index.html
RUN echo "<?php\nphpinfo();\n " > /var/www/html/info.php
#   Install phpMyAdmin
COPY ./src-phpmyadmin/phpMyAdmin-4.0.8-all-languages.tar.gz /var/www/html/
COPY ./src-phpmyadmin/config.inc.php /var/www/html/phpMyAdmin-4.0.8-all-languages/config.inc.php
#
#   Install NodeJS
#   --------------
RUN curl https://raw.githubusercontent.com/creationix/nvm/v0.13.1/install.sh | bash
#   RUN echo "[ -s $HOME/.nvm/nvm.sh ] && . $HOME/.nvm/nvm.sh" >> $HOME/.profile
RUN /bin/bash -c "source $HOME/.profile \
 && nvm install v0.11.9"
COPY ./init-node.sh /src/
RUN /src/init-node.sh
#
#   Install MySQL
#   -------------
#   init script and test db
COPY ./init-mysql.sh /src/
COPY ./test-wp.sql /src/
COPY ./test-vtiger.sql /src/
#   Install MySQL server
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y mysql-server
#   Fix configuration
RUN sed -i -e"s/^bind-address\s*=\s*127.0.0.1/bind-address = 0.0.0.0/" /etc/mysql/my.cnf
#   Setup admin user
RUN /src/init-mysql.sh
#
#   Add jsbin
#   ----------
COPY ./init-jsbin.sh /src
RUN /src/init-jsbin.sh
#
#   Start things
#   -----------
#   Add batches here since it changes often (use cache when building)
COPY ./batches.py /
COPY ./batches.sh /
#   RUN apt-get clean && rm -rf /var/lib/apt/lists/*
#   Fix permissions
RUN chown -R www-data:www-data /var/www/html
RUN mkdir /volume
VOLUME /volume
EXPOSE 80/tcp 81/tcp 443/tcp 3306/tcp
COPY ./start.sh /
CMD ["/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
