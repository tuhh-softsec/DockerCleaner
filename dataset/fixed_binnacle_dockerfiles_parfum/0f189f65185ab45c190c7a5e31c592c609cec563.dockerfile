FROM ubuntu
ARG DEBIAN_FRONTEND=noninteractive
LABEL KEY="LSF-COPPER-HORDE"
# ENV HOME /root
RUN apt-get update -y
RUN apt-get -y upgrade
RUN apt-get install --no-install-recommends telnet -y
RUN apt-get install --no-install-recommends nano -y
#  installing php7 in ubuntu 18.04
#  installing php
RUN apt-get install --no-install-recommends php -y
RUN apt-get install --no-install-recommends apache2 libapache2-mod-php mysql-client gnupg2 openssl php-pear -y
#  install php 7.2 modules
RUN apt-get install --no-install-recommends php-pear php-fpm php-dev php-zip php-curl php-xmlrpc php-gd php-mysql php-mbstring php-xml libapache2-mod-php -y
#  https://www.ctrl.blog/entry/how-to-debian-horde-webmail
#  https://www.ctrl.blog/entry/how-to-debian-horde-webmail
#  how to connect with ldap srver samba active directory
#  https://community.nethserver.org/t/installing-horde-groupware/7292
#  php installation
#  https://thishosting.rocks/install-php-on-ubuntu/
# RUN apt-get install -y php-horde-webmail mysql-client
# RUN apt-get -y  install php-horde
RUN apt-get install --no-install-recommends php-horde-webmail -y
# RUN apt-get install  php-pecl-imagick aspell-en
# RUN apt-get install php-horde-horde php-pecl-imagick aspell-en
RUN mkdir /var/lib/horde/
RUN chown www-data:www-data /var/lib/horde/
RUN cp /etc/horde/horde/conf.php.dist /etc/horde/horde/conf.php
RUN chown www-data:www-data /etc/horde/horde/conf.php
RUN touch /etc/horde/imp/conf.php /etc/horde/turba/conf.php
RUN chown www-data:www-data /etc/horde/imp/conf.php /etc/horde/turba/conf.php
RUN cp /etc/horde/imp/backends.php /etc/horde/imp/backends.local.php
#  Add other configurations also
# ADD ./webmail/horde-webmail/config/conf.php /usr/share/horde/config/conf.php
COPY ./config/conf.php /usr/share/horde/config/conf.php
RUN chown www-data:www-data /usr/share/horde/config/conf.php
#  coppying ingo mail Filter application
COPY ./config/ingo/conf.php /usr/share/horde/ingo/config/conf.php
RUN chown www-data:www-data /usr/share/horde/ingo/config/conf.php
#  configuration file hosting solution
COPY ./config/gollem/conf.php /usr/share/horde/gollem/config/conf.php
RUN chown www-data:www-data /usr/share/horde/gollem/config/conf.php
#  Configuring turba contact mangement
COPY ./config/turba/conf.php /usr/share/horde/turba/config/conf.php
RUN chown www-data:www-data /usr/share/horde/turba/config/conf.php
#  Configuring Todo/Reminder plugin
COPY ./config/nag/conf.php /usr/share/horde/nag/config/conf.php
RUN chown www-data:www-data /usr/share/horde/nag/config/conf.php
#  Configure Kronolith calender
COPY ./config/kronolith/conf.php /usr/share/horde/kronolith/config/conf.php
RUN chown www-data:www-data /usr/share/horde/kronolith/config/conf.php
#  Configure mnemo Notebook
COPY ./config/mnemo/conf.php /usr/share/horde/mnemo/config/conf.php
RUN chown www-data:www-data /usr/share/horde/mnemo/config/conf.php
#  Configure imp webmail
COPY ./config/imp/conf.php /usr/share/horde/imp/config/conf.php
RUN chown www-data:www-data /usr/share/horde/imp/config/conf.php
#  Configure trean web book mark
COPY ./config/trean/conf.php /usr/share/horde/trean/config/conf.php
RUN chown www-data:www-data /usr/share/horde/trean/config/conf.php
# RUN chown www-data:www-data /etc/horde/imp/conf.php /etc/horde/turba/conf.php
# RUN chown www-data:www-data /etc/horde/imp/conf.php /etc/horde/turba/conf.php
# RUN chown www-data:www-data /etc/horde/imp/conf.php /etc/horde/turba/conf.php
#  coppying conf.php for horde active directory
# ADD ./config/conf.php /etc/horde/horde/conf.php
# ADD ./config/backends.local.php /etc/horde/imp/backends.php
COPY ./config/backends.local.php /usr/share/horde/imp/config/backends.local.php
# ADD ./webmail/horde-webmail/config/conf.php /usr/share/horde/config/conf.php
# RUN chown www-data:www-data /usr/share/horde/config/conf.php
COPY ./apache-horde.conf /etc/apache2/sites-enabled/apache-horde.conf
#  horde database migration through a sh file
COPY ./horde-init.sh /etc/my_init.d/horde-init.sh
RUN chmod +x /etc/my_init.d/horde-init.sh
RUN mkdir -p /etc/service/apache2
COPY ./run.sh /etc/service/apache2/run
#  Horde database migration script running
# RUN cd /usr/sbin
# RUN horde-db-migrate
RUN chmod +x /etc/service/apache2/run
# CMD service apache2 start
CMD ["/etc/service/apache2/run"]
# CMD ["/sbin/my_init"]
