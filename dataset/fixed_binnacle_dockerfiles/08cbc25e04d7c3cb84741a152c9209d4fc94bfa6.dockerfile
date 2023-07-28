#   MySQL Server with Apache and phpmyadmin
#
#   VERSION               0.0.1
#
#   Docs: 
#   - http://cweiske.de/tagebuch/Running%20Apache%20with%20a%20dozen%20PHP%20versions.htm
#   - http://cweiske.de/tagebuch/Introducing%20phpfarm.htm
#
FROM ubuntu:latest
MAINTAINER Jonas Colmsjö "jonas@gizur.com"
RUN echo "export HOME=/root" >> /root/.profile
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 nano=7.2-1 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 -y )
#
#   Install supervisord (used to handle processes)
#
RUN (apt-get update ;apt-get install --no-install-recommends supervisor=4.2.1-1ubuntu1 -y )
COPY ./supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#
#   Install Apache
#
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.55-1ubuntu2 -y )
RUN a2enmod rewrite status
COPY ./etc-apache2-mods-available-status.conf /etc/apache2/mods-available/status.conf
RUN rm /var/www/html/index.html
RUN echo "<?php\nphpinfo();\n " > /var/www/html/info.php
#
#   Preparations
#
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libxml2-dev=2.9.14+dfsg-1.1build2 libbz2-dev=1.0.8-5build1 libmcrypt-dev=2.5.8-7 libxslt1-dev=1.1.35-1 libssl-dev=3.0.8-1ubuntu1 libsslcommon2-dev libapr1-dev=1.7.2-2 libaprutil1-dev=1.6.3-1ubuntu1 libreadline-dev=8.2-1.3 make=4.3-4.1build1 libcurl4-openssl-dev=7.88.1-7ubuntu1 libjpeg-dev=8c-2ubuntu11 libpng12-dev libfreetype6-dev=2.12.1+dfsg-4 libxpm-dev=1:3.5.12-1.1 libgd-dev=2.3.3-7ubuntu2 libxpm4=1:3.5.12-1.1 t1lib-bin libtidy-dev=2:5.6.0-11build2 libc-client-dev -y )
#   Fix problem with libs in wrong place
RUN ln -s /usr/lib/x86_64-linux-gnu/libXpm* /usr/lib/
RUN ln -s /usr/lib/x86_64-linux-gnu/libkrb5* /usr/lib/
RUN ln -s /usr/lib/x86_64-linux-gnu/libfreetype* /usr/lib/
#
#   Use phpfarm to manage PHP versions
#
#   Add one script per PHP version and update 
RUN cd /opt ; git clone git://git.code.sf.net/p/phpfarm/code phpfarm
COPY ./options.sh /opt/phpfarm/src/options.sh
RUN cd /opt/phpfarm/src ; ./compile.sh 5.3.27
COPY ./var-www-html-cgibin-phpcgi-5.3.27 /var/www/html/cgibin/phpcgi-5.3.27
COPY ./opt-phpfarm-inst-php-5.3.27-lib-php.ini /opt/phpfarm/inst/php-5.3.27/lib/php.ini
#   Manage PHP versions in Apache using FastCGI - old libapache2-mod-fastcgi 
RUN (apt-get update ;apt-get install --no-install-recommends apache2-mpm-worker apache2-suexec libapache2-mod-fcgid=1:2.3.9-4 -y )
RUN a2enmod actions fcgid suexec
COPY ./etc-apache2-sites-available-000-default.conf /etc/apache2/sites-available/000-default.conf
#   Install phpMyAdmin
COPY ./src-phpmyadmin/phpMyAdmin-4.0.8-all-languages.tar.gz /var/www/html/
COPY ./src-phpmyadmin/config.inc.php /var/www/html/phpMyAdmin-4.0.8-all-languages/config.inc.php
#   Install vTiger
#  ADD ./vtigercrm-5.4.0.tar.gz /var/www/html/
COPY ./vtigercrm-installed.tgz /var/www/html/
COPY ./src-vtiger/config.inc.php /var/www/html/vtigercrm/
#
#   Install MySQL
#
#   Bundle everything
COPY ./src-mysql /src-mysql
#   Install MySQL server
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y mysql-server \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Fix configuration
RUN sed -i -e"s/^bind-address\s*=\s*127.0.0.1/bind-address = 0.0.0.0/" /etc/mysql/my.cnf
#   Setup admin user and load data
RUN /src-mysql/mysql-setup.sh
#
#   Start apache and mysql using supervisord
#
#   Fix permissions
RUN chown -R www-data:www-data /var/www/html
#   Create a volume
RUN mkdir /volume
VOLUME ["/volume"]
COPY ./start.sh /
EXPOSE 3306/tcp 80/tcp 443/tcp
CMD ["/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
