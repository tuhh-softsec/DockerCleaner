#   Supervisord, PHP, Python, MySQL
#
#   VERSION               0.0.1
#
#   Guidelines
#   ----------
#
#   * Always use ubuntu:latest. Problems with new ubuntu releases should be fixed before
#    moving new images into production.
#
#   * Daemons are managed with supervisord.
#
#   * Logging from all daemons should be performed to `/var/log/supervisor/supervisord.log`.
#     The start script will `tail -f` this log so it shows up in `docker logs`. The log file of 
#     daemons that can't log to `/var/log/supervisor/supervisord.log` should also be tailed
#     in `start.sh`
#   
FROM ubuntu:latest
MAINTAINER Jonas Colmsjö "jonas@gizur.com"
RUN echo "export HOME=/root" >> /root/.profile
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 nano=7.2-1 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 -y )
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
COPY ./batches.py /
#
#   Install Apache
#   ---------------
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.55-1ubuntu2 -y )
RUN a2enmod rewrite status
COPY ./etc-apache2-apache2.conf /etc/apache2/apache2.conf
COPY ./etc-apache2-mods-available-status.conf /etc/apache2/mods-available/status.conf
RUN rm /var/www/html/index.html
RUN echo "<?php\nphpinfo();\n " > /var/www/html/info.php
#
#   Use phpfarm to manage PHP versions
#   ----------------------------------
#
#   Add one script per PHP version and update 
#   Preparations
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libxml2-dev=2.9.14+dfsg-1.1build2 libbz2-dev=1.0.8-5build1 libmcrypt-dev=2.5.8-7 libxslt1-dev=1.1.35-1 libssl-dev=3.0.8-1ubuntu1 libsslcommon2-dev libapr1-dev=1.7.2-2 libaprutil1-dev=1.6.3-1ubuntu1 libreadline-dev=8.2-1.3 make=4.3-4.1build1 libcurl4-openssl-dev=7.88.1-7ubuntu1 libjpeg-dev=8c-2ubuntu11 libpng12-dev libfreetype6-dev=2.12.1+dfsg-4 libxpm-dev=1:3.5.12-1.1 libgd-dev=2.3.3-7ubuntu2 libxpm4=1:3.5.12-1.1 t1lib-bin libtidy-dev=2:5.6.0-11build2 libc-client-dev -y )
#   Fix problem with libs in wrong place
RUN ln -s /usr/lib/x86_64-linux-gnu/libXpm* /usr/lib/
RUN ln -s /usr/lib/x86_64-linux-gnu/libkrb5* /usr/lib/
RUN ln -s /usr/lib/x86_64-linux-gnu/libfreetype* /usr/lib/
#   Install PHP farm
RUN cd /opt ; git clone git://git.code.sf.net/p/phpfarm/code phpfarm
COPY ./options.sh /opt/phpfarm/src/options.sh
RUN cd /opt/phpfarm/src ; ./compile.sh 5.3.27
COPY ./var-www-html-cgibin-phpcgi-5.3.27 /var/www/cgibin/phpcgi-5.3.27
COPY ./opt-phpfarm-inst-php-5.3.27-lib-php.ini /opt/phpfarm/inst/php-5.3.27/lib/php.ini
#   Manage PHP versions in Apache using FastCGI - old libapache2-mod-fastcgi 
RUN (apt-get update ;apt-get install --no-install-recommends apache2-mpm-worker apache2-suexec libapache2-mod-fcgid=1:2.3.9-4 -y )
RUN a2enmod actions fcgid suexec
COPY ./etc-apache2-sites-available-000-default.conf /etc/apache2/sites-available/000-default.conf
#
#   Start apache and mysql using supervisord
#   -----------------------------------------
#   Fix permissions
RUN chown -R www-data:www-data /var/www/html
#   Create a volume
RUN mkdir /volume
VOLUME ["/volume"]
#   Add batches here since it changes often (use cache whrn building)
COPY ./batches.sh /
COPY ./start.sh /
EXPOSE 80/tcp 443/tcp
CMD ["/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
