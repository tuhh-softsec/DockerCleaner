FROM phusion/baseimage:0.9.21
LABEL maintainer="\"velaluqa GmbH <info@velalu.qa>\""
ENV DEBIAN_FRONTEND="noninteractive"
RUN echo "APT::Install-Recommends 0;" >> /etc/apt/apt.conf.d/01-no-recommends \
 && echo "APT::Install-Suggests 0;" >> /etc/apt/apt.conf.d/01-no-recommends
RUN apt-get update -q \
 && apt-get install --no-install-recommends apt-utils -y -q \
 && apt-get install --no-install-recommends wget bzip2 dovecot-core dovecot-imapd dovecot-ldap dovecot-lmtpd dovecot-managesieved dovecot-mysql dovecot-pop3d dovecot-sieve iptables augeas-tools -y -q \
 && apt-get clean
ENV IREDMAIL_VERSION="0.9.6"
ARG HOSTNAME=mail
ARG DOMAIN
RUN echo $DOMAIN > /etc/mailname \
 && echo $HOSTNAME > /opt/hostname
RUN mv /bin/uname /bin/uname_
COPY ./uname /bin/uname
RUN mv /bin/hostname /bin/hostname_
COPY ./hostname /bin/hostname
WORKDIR /opt/iredmail
RUN wget -O - https://bitbucket.org/zhb/iredmail/downloads/iRedMail-"${IREDMAIL_VERSION}".tar.bz2 | tar xvj --strip-components=1 \
 && echo "export DOVECOT_USE_SYSLOG='NO'" >> /opt/iredmail/conf/dovecot
COPY ./config ./
#   Run iRedMail setup script
#   Start and stop some services for environment setup (e.g. create folders in /var/run)
RUN sed s/$( hostname_ ;)/$( cat /opt/hostname | xargs echo -n ;).$( cat /etc/mailname | xargs echo -n ;)/ /etc/hosts > /tmp/hosts_ \
 && cat /tmp/hosts_ > /etc/hosts \
 && rm /tmp/hosts_ \
 && echo $HOSTNAME > /etc/hostname \
 && apt-get install --no-install-recommends mysql-server -y -q \
 && service mysql start \
 && IREDMAIL_DEBUG='NO' CHECK_NEW_IREDMAIL='NO' AUTO_USE_EXISTING_CONFIG_FILE=y AUTO_INSTALL_WITHOUT_CONFIRM=y AUTO_CLEANUP_REMOVE_SENDMAIL=y AUTO_CLEANUP_REMOVE_MOD_PYTHON=y AUTO_CLEANUP_REPLACE_FIREWALL_RULES=n AUTO_CLEANUP_RESTART_IPTABLES=n AUTO_CLEANUP_REPLACE_MYSQL_CONFIG=y FIRST_DOMAIN=$DOMAIN bash iRedMail.sh \
 && apt-get clean \
 && /usr/lib/php/php7.0-fpm-checkconf ; service clamav-daemon start ; service clamav-daemon stop ; service amavis start ; service amavis stop ; service mysql stop ; ps aux
WORKDIR /opt
#   Update debian system config for mysql
RUN bash -c "source iredmail/config \
 && sed -i s\/password\\(\ *\\).*\/password\1=\ \"$MYSQL_ROOT_PASSWD\"\/ /etc/mysql/debian.cnf \
 && sed -i s\/user\\(\ *\\).*\/user\1=\ root\/ /etc/mysql/debian.cnf"
RUN mv /opt/iredmail/iRedMail.tips /opt \
 && rm -rf /opt/iredmail /root/.bash_history \
 && mv -f /bin/uname_ /bin/uname \
 && mv -f /bin/hostname_ /bin/hostname
#   Disable SSH and SSH fail2ban
RUN rm -rf /etc/service/sshd /etc/my_init.d/00_regen_ssh_host_keys.sh \
 && wget -qP /usr/share/augeas/lenses/dist https://raw.githubusercontent.com/vynt-kenshiro/augeas-lenses/master/fail2ban.aug \
 && echo set /augeas/load/Fail2ban/incl[last
#   Disable backups
RUN sed -i '/\/var\/vmail\/backup/d' /var/spool/cron/crontabs/root
#   Use one DKIM key for all domains
RUN sed -i s/^dkim_key
#   Bounce banned emails instead of discarding them
RUN sed -i "s/=> 'DISCARD'],/=> 'BOUNCE'],/" /etc/amavis/conf.d/50-user
#   Amend config files for runsv compatibility
RUN sed -i '/^Foreground /c Foreground true' /etc/clamav/clamd.conf \
 && sed -i '/init.d/c pkill -sighup clamd' /etc/logrotate.d/clamav-daemon \
 && sed -i '/^Foreground /c Foreground true' /etc/clamav/freshclam.conf
#   Add syslog-ng configuration for openldap
RUN echo "destination d_openldap { file(\"/var/log/openldap.log\"); };\nfilter f_openldap { facility(local4) and not filter(f_debug); };\nlog { source(s_src); filter(f_openldap); destination(d_openldap); };" > /etc/syslog-ng/conf.d/openldap.conf
#   Copy runsv files
COPY services/iredapd.sh /etc/service/iredapd/run
COPY services/slapd.sh /etc/service/slapd/run
COPY services/mysql.sh /etc/service/mysql/run
COPY services/nginx.sh /etc/service/nginx/run
COPY services/dovecot.sh /etc/service/dovecot/run
COPY services/php-fpm.sh /etc/service/php-fpm/run
COPY services/postfix.sh /etc/service/postfix/run
COPY services/clamav-daemon.sh /etc/service/clamav-daemon/run
COPY services/amavis.sh /etc/service/amavis/run
COPY services/clamav-freshclam.sh /etc/service/clamav-freshclam/run
COPY services/uwsgi-iredadmin.sh /etc/service/uwsgi-iredadmin/run
COPY services/fail2ban.sh /etc/service/fail2ban/run
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
