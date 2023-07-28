FROM ubuntu
ARG DEBIAN_FRONTEND=noninteractive
LABEL KEY="LSF-COPPER-HORDE"
#  ENV HOME /root
RUN :
RUN apt-get -y upgrade
RUN (apt-get update ;apt-get install --no-install-recommends telnet=0.17+2.4-2ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends nano=7.2-1 -y )
#   installing php7 in ubuntu 18.04
#   installing php 
RUN (apt-get update ;apt-get install --no-install-recommends php=2:8.1+92ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.55-1ubuntu2 libapache2-mod-php=2:8.1+92ubuntu1 mysql-client=8.0.32-0ubuntu4 gnupg2=2.2.40-1ubuntu2 openssl=3.0.8-1ubuntu1 php-pear=1:1.10.13+submodules+notgz+2022032202-2 -y )
#   install php 7.2 modules
RUN (apt-get update ;apt-get install --no-install-recommends php-pear=1:1.10.13+submodules+notgz+2022032202-2 php-fpm=2:8.1+92ubuntu1 php-dev=2:8.1+92ubuntu1 php-zip=2:8.1+92ubuntu1 php-curl=2:8.1+92ubuntu1 php-xmlrpc=3:1.0.0~rc3-6 php-gd=2:8.1+92ubuntu1 php-mysql=2:8.1+92ubuntu1 php-mbstring=2:8.1+92ubuntu1 php-xml=2:8.1+92ubuntu1 libapache2-mod-php=2:8.1+92ubuntu1 -y )
#   https://www.ctrl.blog/entry/how-to-debian-horde-webmail
#   https://www.ctrl.blog/entry/how-to-debian-horde-webmail
#   how to connect with ldap srver samba active directory
#   https://community.nethserver.org/t/installing-horde-groupware/7292
#   php installation
#   https://thishosting.rocks/install-php-on-ubuntu/
#  RUN apt-get install -y php-horde-webmail mysql-client
#  RUN apt-get -y  install php-horde
RUN (apt-get update ;apt-get install --no-install-recommends php-horde-webmail -y )
#  RUN apt-get install  php-pecl-imagick aspell-en
#  RUN apt-get install php-horde-horde php-pecl-imagick aspell-en
RUN mkdir /var/lib/horde/
RUN chown www-data:www-data /var/lib/horde/
RUN cp /etc/horde/horde/conf.php.dist /etc/horde/horde/conf.php
RUN chown www-data:www-data /etc/horde/horde/conf.php
RUN touch /etc/horde/imp/conf.php /etc/horde/turba/conf.php
RUN chown www-data:www-data /etc/horde/imp/conf.php /etc/horde/turba/conf.php
RUN cp /etc/horde/imp/backends.php /etc/horde/imp/backends.local.php
#   Add other configurations also
#  ADD ./webmail/horde-webmail/config/conf.php /usr/share/horde/config/conf.php
COPY ./config/conf.php /usr/share/horde/config/conf.php
RUN chown www-data:www-data /usr/share/horde/config/conf.php
#   coppying ingo mail Filter application
COPY ./config/ingo/conf.php /usr/share/horde/ingo/config/conf.php
RUN chown www-data:www-data /usr/share/horde/ingo/config/conf.php
#   configuration file hosting solution
COPY ./config/gollem/conf.php /usr/share/horde/gollem/config/conf.php
RUN chown www-data:www-data /usr/share/horde/gollem/config/conf.php
#   Configuring turba contact mangement
COPY ./config/turba/conf.php /usr/share/horde/turba/config/conf.php
RUN chown www-data:www-data /usr/share/horde/turba/config/conf.php
#   Configuring Todo/Reminder plugin
COPY ./config/nag/conf.php /usr/share/horde/nag/config/conf.php
RUN chown www-data:www-data /usr/share/horde/nag/config/conf.php
#   Configure Kronolith calender
COPY ./config/kronolith/conf.php /usr/share/horde/kronolith/config/conf.php
RUN chown www-data:www-data /usr/share/horde/kronolith/config/conf.php
#   Configure mnemo Notebook
COPY ./config/mnemo/conf.php /usr/share/horde/mnemo/config/conf.php
RUN chown www-data:www-data /usr/share/horde/mnemo/config/conf.php
#   Configure imp webmail
COPY ./config/imp/conf.php /usr/share/horde/imp/config/conf.php
RUN chown www-data:www-data /usr/share/horde/imp/config/conf.php
#   Configure trean web book mark
COPY ./config/trean/conf.php /usr/share/horde/trean/config/conf.php
RUN chown www-data:www-data /usr/share/horde/trean/config/conf.php
#  RUN chown www-data:www-data /etc/horde/imp/conf.php /etc/horde/turba/conf.php
#  RUN chown www-data:www-data /etc/horde/imp/conf.php /etc/horde/turba/conf.php
#  RUN chown www-data:www-data /etc/horde/imp/conf.php /etc/horde/turba/conf.php
#   coppying conf.php for horde active directory
#  ADD ./config/conf.php /etc/horde/horde/conf.php
#  ADD ./config/backends.local.php /etc/horde/imp/backends.php
COPY ./config/backends.local.php /usr/share/horde/imp/config/backends.local.php
#  ADD ./webmail/horde-webmail/config/conf.php /usr/share/horde/config/conf.php
#  RUN chown www-data:www-data /usr/share/horde/config/conf.php
COPY ./apache-horde.conf /etc/apache2/sites-enabled/apache-horde.conf
#   horde database migration through a sh file
COPY ./horde-init.sh /etc/my_init.d/horde-init.sh
RUN chmod +x /etc/my_init.d/horde-init.sh
RUN mkdir -p /etc/service/apache2
COPY ./run.sh /etc/service/apache2/run
#   Horde database migration script running
#  RUN cd /usr/sbin
#  RUN horde-db-migrate
RUN chmod +x /etc/service/apache2/run
#  CMD service apache2 start
CMD ["/etc/service/apache2/run"]
#  CMD ["/sbin/my_init"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
