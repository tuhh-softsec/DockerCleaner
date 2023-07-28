FROM ubuntu:14.04
MAINTAINER Jonas Friedmann <j@frd.mn> version: 0.1
ENV DEBIAN_FRONTEND="noninteractive"
#   Update locale
RUN locale-gen en_US en_US.UTF-8 \
 && dpkg-reconfigure locales
#   Update apt
RUN :
#   Install dependencies
RUN (apt-get update ;apt-get install --no-install-recommends debconf-utils=1.5.51ubuntu2 mysql-server-5.5=5.5.62-0ubuntu0.14.04.1 mysql-client=5.5.62-0ubuntu0.14.04.1 dovecot-core=1:2.2.9-1ubuntu2.6 dovecot-imapd=1:2.2.9-1ubuntu2.6 dovecot-pop3d=1:2.2.9-1ubuntu2.6 dovecot-lmtpd=1:2.2.9-1ubuntu2.6 dovecot-mysql=1:2.2.9-1ubuntu2.6 dovecot-sieve=1:2.2.9-1ubuntu2.6 dovecot-managesieved=1:2.2.9-1ubuntu2.6 supervisor=3.0b2-1ubuntu0.1 nginx=1.4.6-1ubuntu3.9 curl=7.35.0-1ubuntu2.20 php5-fpm=5.5.9+dfsg-1ubuntu4.29 php5-pgsql=5.5.9+dfsg-1ubuntu4.29 php-apc=4.0.2-2build1 php5-mcrypt=5.4.6-0ubuntu5 php5-curl=5.5.9+dfsg-1ubuntu4.29 php5-gd=5.5.9+dfsg-1ubuntu4.29 php5-json=1.3.2-2build1 php5-cli=5.5.9+dfsg-1ubuntu4.29 php5-mysql=5.5.9+dfsg-1ubuntu4.29 php5-memcache=3.0.8-4build1 php5-cgi=5.5.9+dfsg-1ubuntu4.29 git=1:1.9.1-1ubuntu0.10 mailutils=1:2.99.98-1.1 telnet=0.17-36build2 dnsutils=1:9.9.5.dfsg-3ubuntu0.19 -y )
#   Add settings file for further usage
COPY settings.conf /tmp/settings.conf
#   Configure MySQL
COPY mysql/adjust-mysql-configuration-file.sh /tmp/adjust-mysql-configuration-file.sh
RUN /bin/sh /tmp/adjust-mysql-configuration-file.sh
COPY mysql/update-mysql-password.sh /tmp/update-mysql-password.sh
RUN /bin/sh /tmp/update-mysql-password.sh
#   Configure Nginx
RUN echo "daemon off;" >> /etc/nginx/nginx.conf
#   Configure PHP5-FPM
COPY php5-fpm/adjust-php-configuration-file.sh /tmp/adjust-php-configuration-file.sh
RUN /bin/sh /tmp/adjust-php-configuration-file.sh
#   Install composer
RUN curl -sS https://getcomposer.org/installer | php
RUN mv composer.phar /usr/local/bin/composer
#   Prepare Nginx
RUN rm /etc/nginx/sites-enabled/default
COPY nginx/vimbadmin /etc/nginx/sites-available/vimbadmin
RUN ln -sf /etc/nginx/sites-available/vimbadmin /etc/nginx/sites-enabled/vimbadmin
RUN mkdir /var/www
COPY nginx/correct-vimbadmin-hostname.sh /tmp/correct-vimbadmin-hostname.sh
RUN /bin/sh /tmp/correct-vimbadmin-hostname.sh
#   Configure ViMbAdmin
RUN mkdir /var/www/vimbadmin
RUN export INSTALL_PATH=/var/www/vimbadmin
RUN composer config -g github-protocols https
RUN composer create-project --no-interaction opensolutions/vimbadmin /var/www/vimbadmin -s dev
RUN chown -R www-data: /var/www/vimbadmin/var
COPY mysql/create-vimbadmin-database.sh /tmp/create-vimbadmin-database.sh
RUN /bin/sh /tmp/create-vimbadmin-database.sh
RUN cp /var/www/vimbadmin/application/configs/application.ini.dist /var/www/vimbadmin/application/configs/application.ini
COPY nginx/correct-vimbadmin-settings-file.sh /tmp/correct-vimbadmin-settings-file.sh
RUN /bin/sh /tmp/correct-vimbadmin-settings-file.sh
RUN cp /var/www/vimbadmin/public/.htaccess.dist /var/www/vimbadmin/public/.htaccess
#   Create SQL tables for ViMbAdmin
COPY nginx/create-vimbadmin-sql-tables.sh /tmp/create-vimbadmin-sql-tables.sh
RUN /bin/sh /tmp/create-vimbadmin-sql-tables.sh
#   Adjust web server file permissions
RUN chown -R www-data:root /var/www
#   Prepare installation of postfix
RUN echo "postfix postfix/root_address string" | debconf-set-selections
RUN echo "postfix postfix/procmail boolean false" | debconf-set-selections
RUN echo "postfix postfix/rfc1035_violation boolean false" | debconf-set-selections
RUN echo "postfix postfix/bad_recipient_delimiter error" | debconf-set-selections
RUN echo "postfix postfix/protocols select all" | debconf-set-selections
RUN echo "postfix postfix/retry_upgrade_warning boolean" | debconf-set-selections
RUN echo "postfix postfix/kernel_version_warning boolean" | debconf-set-selections
RUN echo "postfix postfix/mailname string diva.vimm.be" | debconf-set-selections
RUN echo "postfix postfix/tlsmgr_upgrade_warning boolean" | debconf-set-selections
RUN echo "postfix postfix/mydomain_warning boolean" | debconf-set-selections
RUN echo "postfix postfix/recipient_delim string +" | debconf-set-selections
RUN echo "postfix postfix/mynetworks string 127.0.0.0/8 [::ffff:127.0.0.0]/104 [::1]/128 172.17.0.0/16 " | debconf-set-selections
RUN echo "postfix postfix/not_configured error" | debconf-set-selections
RUN echo "postfix postfix/main_mailer_type select Internet Site" | debconf-set-selections
RUN echo "postfix postfix/sqlite_warning boolean" | debconf-set-selections
RUN echo "postfix postfix/destinations string diva.vimm.be, localhost.localdomain, localhost" | debconf-set-selections
RUN echo "postfix postfix/chattr boolean false" | debconf-set-selections
RUN echo "postfix postfix/mailbox_limit string 0" | debconf-set-selections
RUN echo "postfix postfix/relayhost string" | debconf-set-selections
#   Install postfix
RUN (apt-get update ;apt-get install --no-install-recommends postfix=2.11.0-1ubuntu1.2 postfix-mysql=2.11.0-1ubuntu1.2 -y )
#   Add postfix configuration files
COPY postfix/main.cf /etc/postfix/main.cf
COPY postfix/master.cf /etc/postfix/master.cf
RUN mkdir /etc/postfix/mysql
COPY postfix/mysql/virtual_alias_maps.cf /etc/postfix/mysql/virtual_alias_maps.cf
COPY postfix/mysql/virtual_domains_maps.cf /etc/postfix/mysql/virtual_domains_maps.cf
COPY postfix/mysql/virtual_mailbox_maps.cf /etc/postfix/mysql/virtual_mailbox_maps.cf
COPY postfix/mysql/virtual_transport_maps.cf /etc/postfix/mysql/virtual_transport_maps.cf
#   Adjust the files
COPY postfix/adjust-postfix-configuration-file.sh /tmp/adjust-postfix-configuration-file.sh
RUN /bin/sh /tmp/adjust-postfix-configuration-file.sh
#   Copy default services into postfix' chroot
RUN /bin/cp /etc/services /var/spool/postfix/etc/services
#   Configure dovecot
#  RUN mkdir -p /var/mail/vhosts/
#  VOLUME /var/mail/vhosts
RUN groupadd -g 5000 vmail
RUN useradd -g vmail -u 5000 vmail -d /var/vmail
RUN mkdir /var/vmail
RUN chown vmail:vmail /var/vmail
#   Add dovecot configuration files
COPY dovecot/dovecot.conf /etc/dovecot/dovecot.conf
COPY dovecot/dovecot-sql.conf.ext /etc/dovecot/dovecot-sql.conf.ext
COPY dovecot/conf.d/10-auth.conf /etc/dovecot/conf.d/10-auth.conf
COPY dovecot/conf.d/10-logging.conf /etc/dovecot/conf.d/10-logging.conf
COPY dovecot/conf.d/10-mail.conf /etc/dovecot/conf.d/10-mail.conf
COPY dovecot/conf.d/10-master.conf /etc/dovecot/conf.d/10-master.conf
COPY dovecot/conf.d/15-lda.conf /etc/dovecot/conf.d/15-lda.conf
COPY dovecot/conf.d/20-imap.conf /etc/dovecot/conf.d/20-imap.conf
COPY dovecot/conf.d/20-lmtp.conf /etc/dovecot/conf.d/20-lmtp.conf
COPY dovecot/conf.d/20-managesieve.conf /etc/dovecot/conf.d/20-managesieve.conf
COPY dovecot/conf.d/20-pop3.conf /etc/dovecot/conf.d/20-pop3.conf
COPY dovecot/conf.d/auth-sql.conf.ext /etc/dovecot/conf.d/auth-sql.conf.ext
RUN chown -R vmail:dovecot /etc/dovecot
RUN chmod -R o-rwx /etc/dovecot
#   Adjust the config files
COPY dovecot/adjust-dovecot-configuration-files.sh /tmp/adjust-dovecot-configuration-files.sh
COPY dovecot/create-ssl-certificate.sh /tmp/create-ssl-certificate.sh
RUN /bin/sh /tmp/adjust-dovecot-configuration-files.sh
RUN /bin/sh /tmp/create-ssl-certificate.sh
#   Expose MySQL, postfix, Dovecot and Nginx
EXPOSE 3306/tcp 25/tcp 80/tcp 143/tcp 993/tcp 995/tcp
#   Copy supervisor config
COPY supervisor/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#   Correct permissions
RUN chown -R root:root /etc/postfix/ /etc/dovecot/ /etc/nginx/ /etc/supervisor/
#   Start supervisor
CMD ["/usr/bin/supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
