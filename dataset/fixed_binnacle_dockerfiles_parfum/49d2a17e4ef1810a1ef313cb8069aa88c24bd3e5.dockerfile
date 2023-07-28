FROM phusion/baseimage:latest
MAINTAINER Miloš Kozák <milos.kozak@lejmr.com>
#  Suporting software versions
ARG IREDMAIL_VERSION=0.9.8
#  Default values changable at startup
ARG DOMAIN=DOMAIN
ARG HOSTNAME=HOSTNAME
# ## Installation
#  Prerequisites
ENV DEBIAN_FRONTEND="noninteractive"
RUN echo "APT::Install-Recommends 0;" >> /etc/apt/apt.conf.d/01-no-recommends \
 && echo "APT::Install-Suggests 0;" >> /etc/apt/apt.conf.d/01-no-recommends \
 && apt-get update -q \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends apt-utils -y -q \
 && apt-get install --no-install-recommends wget bzip2 iptables openssl mysql-server netcat memcached tmpreaper -y -q \
 && apt-get autoremove -y -q \
 && apt-get clean -y -q
#  Install of iRedMail from sources
WORKDIR /opt/iredmail
RUN wget -O - https://bitbucket.org/zhb/iredmail/downloads/iRedMail-"${IREDMAIL_VERSION}".tar.bz2 | tar xvj --strip-components=1
#  Generate configuration file
COPY ./config-gen /opt/iredmail/config-gen
RUN sh ./config-gen HOSTNAME DOMAIN > ./config
#  Initiate automatic installation process
RUN sed s/$( hostname_ ;)/$( cat /opt/hostname | xargs echo -n ;).$( cat /etc/mailname | xargs echo -n ;)/ /etc/hosts > /tmp/hosts_ \
 && cat /tmp/hosts_ > /etc/hosts \
 && rm /tmp/hosts_ \
 && echo $HOSTNAME > /etc/hostname \
 && sleep 5 ; service mysql start \
 && IREDMAIL_DEBUG='NO' IREDMAIL_HOSTNAME="HOSTNAME.DOMAIN" CHECK_NEW_IREDMAIL='NO' AUTO_USE_EXISTING_CONFIG_FILE=y AUTO_INSTALL_WITHOUT_CONFIRM=y AUTO_CLEANUP_REMOVE_SENDMAIL=y AUTO_CLEANUP_REMOVE_MOD_PYTHON=y AUTO_CLEANUP_REPLACE_FIREWALL_RULES=n AUTO_CLEANUP_RESTART_IPTABLES=n AUTO_CLEANUP_REPLACE_MYSQL_CONFIG=y AUTO_CLEANUP_RESTART_POSTFIX=n bash iRedMail.sh \
 && apt-get autoremove -y -q \
 && apt-get clean -y -q
# ## Final configuration
RUN sed -i '/^Foreground /c Foreground true' /etc/clamav/clamd.conf \
 && sed -i '/init.d/c pkill -sighup clamd' /etc/logrotate.d/clamav-daemon \
 && sed -i '/^Foreground /c Foreground true' /etc/clamav/freshclam.conf \
 && sed -i 's/^bind-address/#bind-address/' /etc/mysql/mysql.conf.d/mysqld.cnf \
 && sed -i 's/SHOWWARNING[ \t]*=.*/SHOWWARNING=false/g' /etc/tmpreaper.conf \
 && install -o amavis -g amavis -m 750 -d /var/lib/amavis/.spamassassin \
 && install -o amavis -g amavis -m 640 -T /usr/share/spamassassin/user_prefs.template /var/lib/amavis/.spamassassin/user_prefs \
 && rm -f /etc/ssl/private/iRedMail.key \
 && rm -f /etc/ssl/certs/iRedMail.crt \
 && rm -f /var/lib/dkim/DOMAIN.pem
#  Prepare for the first run
RUN tar jcf /root/mysql.tar.bz2 /var/lib/mysql \
 && rm -rf /var/lib/mysql \
 && tar jcf /root/vmail.tar.bz2 /var/vmail \
 && rm -rf /var/vmail \
 && rm -rf /var/lib/clamav/*
COPY update.sh /sbin/update-iredmail
# ## Startup services
#  Core Services
COPY rc.local /etc/rc.local
COPY services/mysql.sh /etc/service/mysql/run
COPY services/postfix.sh /etc/service/postfix/run
COPY services/amavis.sh /etc/service/amavis/run
COPY services/iredapd.sh /etc/service/iredapd/run
COPY services/dovecot.sh /etc/service/dovecot/run
#  Frontend
COPY services/memcached.sh /etc/service/memcached/run
COPY services/sogo.sh /etc/service/sogo/run
COPY services/iredadmin.sh /etc/service/iredadmin/run
COPY services/php7-fpm.sh /etc/service/php7-fpm/run
COPY services/nginx.sh /etc/service/httpd/run
#  Enhancement
COPY services/fail2ban.sh /etc/service/fail2ban/run
COPY services/clamav-daemon.sh /etc/service/clamav-daemon/run
COPY services/clamav-freshclam.sh /etc/service/clamav-freshclam/run
COPY services/netdata.sh /etc/service/netdata/run
COPY services/mlmmjadmin.sh /etc/service/mlmmjadmin/run
# ## Purge some packets and save disk space
RUN apt-get purge -y -q dialog apt-utils augeas-tools \
 && apt-get autoremove -y -q \
 && apt-get clean -y -q \
 && rm -rf /var/lib/apt/lists/*
#  Open Ports:
#  Apache: 80/tcp, 443/tcp Postfix: 25/tcp, 587/tcp
#  Dovecot: 110/tcp, 143/tcp, 993/tcp, 995/tcp
EXPOSE 80/tcp 443/tcp 25/tcp 587/tcp 110/tcp 143/tcp 993/tcp 995/tcp
#  Default values changable at startup
ENV SOGO_WORKERS="2"
ENV TZ="Etc/UTC"
