#  From debian:stretch-slim
#  From debian:stretch
FROM ubuntu:18.04
MAINTAINER Lanka Software Foundation <support@opensource.lk> 
ARG DEBIAN_FRONTEND=noninteractive
ENV TELEGRAF_VERSION="1.8.1-1"
RUN : \
 && apt-get -y purge exim4*
RUN apt-get -y upgrade
RUN (apt-get update ;apt-get install --no-install-recommends apt-utils=1.6.14 -y )
#   installing netstat command
RUN (apt-get update ;apt-get install --no-install-recommends net-tools=1.60+git20161116.90da8a0-1ubuntu1 -y )
#   installing ping command
RUN (apt-get update ;apt-get install --no-install-recommends iputils-ping=3:20161105-1ubuntu3 -y )
#   install mail until for testing functions
RUN (apt-get update ;apt-get install --no-install-recommends mailutils=1:3.4-1 -y )
#   installing lsof command
RUN (apt-get update ;apt-get install --no-install-recommends lsof=4.89+dfsg-0.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends telnet=0.17-41 -y )
RUN (apt-get update ;apt-get install --no-install-recommends nano=2.9.3-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends letsencrypt=0.27.0-1~ubuntu18.04.2 openssl=1.1.1-1ubuntu2.1~18.04.21 -y )
#  postfix-mysql was used to connect with a mysql datbase
#  RUN apt-get -y install postfix postfix-mysql postfix-pcre libsasl2-modules
#  postfix-ldap required to connect with ldap services
RUN (apt-get update ;apt-get install --no-install-recommends postfix=3.3.0-1ubuntu0.4 postfix-ldap=3.3.0-1ubuntu0.4 postfix-pcre=3.3.0-1ubuntu0.4 postfix-policyd-spf-python=2.0.2-1 libsasl2-modules=2.1.27~101-g0780600+dfsg-3ubuntu2.4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends rsyslog=8.32.0-1ubuntu4.2 fetchmail=6.3.26-3ubuntu0.1~18.04.1 libdbi-perl=1.640-1ubuntu0.3 libdbd-pg-perl=3.7.0-1 libdbd-mysql-perl=4.046-1 liblockfile-simple-perl=0.208-1 -y )
#  RUN apt-get -y install dovecot-core dovecot-imapd dovecot-pop3d dovecot-lmtpd dovecot-mysql dovecot-sieve dnsutils
#   changing for modifications
#  RUN apt-get -y install dovecot-core dovecot-imapd dovecot-pop3d dovecot-lmtpd dovecot-mysql dovecot-sieve dnsutils dovecot-managesieved
RUN (apt-get update ;apt-get install --no-install-recommends dovecot-core=1:2.2.33.2-1ubuntu4.8 dovecot-imapd=1:2.2.33.2-1ubuntu4.8 dovecot-pop3d=1:2.2.33.2-1ubuntu4.8 dovecot-lmtpd=1:2.2.33.2-1ubuntu4.8 dovecot-mysql=1:2.2.33.2-1ubuntu4.8 dovecot-ldap=1:2.2.33.2-1ubuntu4.8 dnsutils=1:9.11.3+dfsg-1ubuntu1.18 -y )
#  RUN apt-get install -y mail-stack-delivery // this was used instead of dovecto one by one installation
RUN (apt-get update ;apt-get install --no-install-recommends dovecot-sieve=1:2.2.33.2-1ubuntu4.8 dovecot-managesieved=1:2.2.33.2-1ubuntu4.8 -y )
#   installing ldap utils for testing perposes
RUN (apt-get update ;apt-get install --no-install-recommends ldap-utils=2.4.45+dfsg-1ubuntu1.11 curl=7.58.0-2ubuntu3.24 -y )
#  RUN apt-get -y install gnupg python-gpgme dovecot-managesieved sudo
#   Following changed introduced to above installation due to python-gpgme not installed in 18.04 ubuntu 
#  RUN apt-get -y install gnupg dovecot-managesieved sudo
RUN (apt-get update ;apt-get install --no-install-recommends gnupg=2.2.4-1ubuntu1.6 software-properties-common=0.96.24.32.20 -y )
#   Above two lines used instead of line with python-gpgme
RUN adduser --system --no-create-home --group --home /etc/zeyple --disabled-login zeyple
RUN mkdir -p /etc/zeyple/keys \
 && chmod 700 /etc/zeyple/keys \
 && chown zeyple: /etc/zeyple/keys
#  ADD https://raw.github.com/infertux/zeyple/master/zeyple/zeyple.py /usr/local/bin/zeyple.py
COPY ./configs/zeyple/zeyple.py /usr/local/bin/zeyple.py
RUN chmod 744 /usr/local/bin/zeyple.py \
 && chown zeyple: /usr/local/bin/zeyple.py
RUN which wget &> /dev/null || (apt-get update ;apt-get install --no-install-recommends wget=1.20.3 ) ; wget --no-verbose --output-document /etc/zeyple.conf https://raw.github.com/infertux/zeyple/master/zeyple/zeyple.conf.example
#  ADD ./emailserver/config/zeyple/zeyple.conf.example /etc/zeyple.conf
RUN touch /var/log/zeyple.log \
 && chown zeyple: /var/log/zeyple.log
RUN chown -R zeyple /etc/zeyple /usr/local/bin/zeyple.py
#   COPY Changing dovecot congiguration files permission
COPY ./configs/Dovecot /etc/dovecot/
#  RUN chgrp dovecot /etc/dovecot/dovecot-sql.conf.ext
#  RUN chmod o= /etc/dovecot/dovecot-sql.conf.ext
#   changes accoring to url : https://www.linode.com/docs/email/postfix/email-with-postfix-dovecot-and-mariadb-on-centos-7/
COPY ./configs/Postfix /etc/postfix/
#  RUN chmod o= /etc/postfix/mariadb-sql/mysql-virtual_*.cf
#  RUN chgrp postfix /etc/postfix/mariadb-sql/mysql-virtual_*.cf
#   Changing sql configuration files permissions 
#  RUN chmod o= /etc/postfix/sql/mysql_virtual_*.cf
#  RUN chgrp postfix /etc/postfix/sql/mysql_virtual_*.cf
#   Changing ldap configuration files permissions 
RUN chmod o= /etc/postfix/ldap/*.cf
RUN chgrp postfix /etc/postfix/ldap/*.cf
#   Adding sieve file to /var/mail
COPY ./configs/sieve /var/mail/sieve
RUN groupadd -g 5000 vmail \
 && useradd -g vmail -u 5000 vmail -d /var/mail
RUN chown -R vmail:vmail /var/mail
RUN chown -R postfix /etc/postfix
RUN chmod -R o-rwx /etc/postfix
RUN chmod -R 755 /etc/postfix
RUN chown -R vmail:dovecot /etc/dovecot
RUN chmod -R o-rwx /etc/dovecot
COPY ./configs/Rspamd/rspamd.sh /
RUN ./rspamd.sh // Error : /bin/sh: 1: ./rspamd.sh: Permission denied , bellow line introduced
RUN chmod +x ./rspamd.sh
RUN ./rspamd.sh 2> /dev/null || true
COPY ./configs/Rspamd /etc/rspamd/
#  RUN mkdir /var/lib/rspamd/dkim/	//error :/rspamd/...dkim/': No such file or directory
RUN mkdir -p /var/lib/rspamd/dkim
RUN rspamadm dkim_keygen -b 1024 -s 2018 -d $DOMAIN -k /var/lib/rspamd/dkim/2018.key > /var/lib/rspamd/dkim/2018.txt
RUN chown -R _rspamd:_rspamd /var/lib/rspamd/dkim
RUN chmod 440 /var/lib/rspamd/dkim/*
RUN cp -R /etc/rspamd/local.d/dkim_signing.conf /etc/rspamd/local.d/arc.conf
COPY ./configs/sieve /sieve
COPY ./configs/init_sys.sh /bin/
#  COPY ./configs/.env /bin/
RUN chmod +x /bin/init_sys.sh
#  RUN chmod 755 /bin/.env
#   Adding pod start commands to the email image
COPY ./configs/pod_start.sh /bin/pod_start.sh
RUN chmod +x /bin/pod_start.sh
#   install filebeat to send alarms to ELK stack in copper hub
RUN curl -L -O https://artifacts.elastic.co/downloads/beats/filebeat/filebeat-6.7.1-linux-x86_64.tar.gz
RUN tar xzvf filebeat-6.7.1-linux-x86_64.tar.gz
RUN cd filebeat-6.7.1-linux-x86_64
#   coppy local filebeat configuration file
COPY ./configs/filebeat/filebeat.yml /filebeat-6.7.1-linux-x86_64/filebeat.yml
#  RUN chown root filebeat.yml
#  RUN ./filebeat -e
#   postfixlog analyser 
COPY ./configs/logAnalyze/pflogsumm /usr/local/bin/pflogsumm
RUN chown bin:bin /usr/local/bin/pflogsumm
RUN chmod 755 /usr/local/bin/pflogsumm
#   copying virus sh file
#  RUN mkdir /agent
#  RUN chmod 755 /agent
#  COPY ./emailserver/configs/clamAV/init_refresh.sh /agent/
#  RUN chmod +x /agent/init_refresh.sh
#   Antivirus with amavisd-new and clamAV
RUN (apt-get update ;apt-get install --no-install-recommends clamav-daemon=0.103.8+dfsg-0ubuntu0.18.04.1 clamav=0.103.8+dfsg-0ubuntu0.18.04.1 -y )
COPY ./configs/clamAV/antivirus.conf /etc/rspamd/override.d/
#   Install Telegraf
RUN wget https://dl.influxdata.com/telegraf/releases/telegraf_${TELEGRAF_VERSION}_amd64.deb \
 && dpkg -i telegraf_${TELEGRAF_VERSION}_amd64.deb \
 && rm telegraf_${TELEGRAF_VERSION}_amd64.deb
#   Configure Telegraf
COPY ./configs/telegraf/telegraf.conf /etc/telegraf/telegraf.conf
COPY ./configs/telegraf/init.sh /etc/init.d/telegraf
#   Cleanup
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
COPY ./configs/telegraf/entrypoint.sh entrypoint.sh
RUN chmod 755 entrypoint.sh
#  RUN init_sys.sh
ENTRYPOINT ["init_sys.sh"]
#  ENTRYPOINT ["entrypoint.sh"]
#  CMD ["telegraf"]
#  CMD service postfix start
#  ENTRYPOINT ["tail", "-f", "/dev/null"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
