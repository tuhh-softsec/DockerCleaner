FROM ubuntu:16.04
MAINTAINER Jason Rivers <jason@jasonrivers.co.uk>
ENV NAGIOS_HOME="/opt/nagios"
ENV NAGIOS_USER="nagios"
ENV NAGIOS_GROUP="nagios"
ENV NAGIOS_CMDUSER="nagios"
ENV NAGIOS_CMDGROUP="nagios"
ENV NAGIOS_FQDN="nagios.example.com"
ENV NAGIOSADMIN_USER="nagiosadmin"
ENV NAGIOSADMIN_PASS="nagios"
ENV APACHE_RUN_USER="nagios"
ENV APACHE_RUN_GROUP="nagios"
ENV NAGIOS_TIMEZONE="UTC"
ENV DEBIAN_FRONTEND="noninteractive"
ENV NG_NAGIOS_CONFIG_FILE="${NAGIOS_HOME}/etc/nagios.cfg"
ENV NG_CGI_DIR="${NAGIOS_HOME}/sbin"
ENV NG_WWW_DIR="${NAGIOS_HOME}/share/nagiosgraph"
ENV NG_CGI_URL="/cgi-bin"
ENV NAGIOS_BRANCH="nagios-4.4.3"
ENV NAGIOS_PLUGINS_BRANCH="release-2.2.1"
ENV NRPE_BRANCH="nrpe-3.2.1"
RUN echo postfix postfix/main_mailer_type string "'Internet Site'" | debconf-set-selections \
 && echo postfix postfix/mynetworks string "127.0.0.0/8" | debconf-set-selections \
 && echo postfix postfix/mailname string ${NAGIOS_FQDN} | debconf-set-selections \
 && apt-get update \
 && apt-get install --no-install-recommends apache2=2.4.18-2ubuntu3.17 apache2-utils=2.4.18-2ubuntu3.17 autoconf=2.69-9 automake=1:1.15-4ubuntu1 bc=1.06.95-9build1 bsd-mailx=8.1.2-0.20160123cvs-2 build-essential=12.1ubuntu2 dnsutils=1:9.10.3.dfsg.P4-8ubuntu1.19 fping=3.13-1 gettext=0.19.7-2ubuntu3.1 git=1:2.7.4-0ubuntu1.10 gperf=3.0.4-2 iputils-ping=3:20121221-5ubuntu2 jq=1.5+dfsg-1ubuntu0.1 libapache2-mod-php=1:7.0+35ubuntu6.1 libcache-memcached-perl=1.30-1 libcgi-pm-perl=4.26-1 libdbd-mysql-perl=4.033-1ubuntu0.1 libdbi-dev=0.9.0-4 libdbi-perl=1.634-1ubuntu0.2 libfreeradius-client-dev=1.1.6-7 libgd2-xpm-dev libgd-gd2-perl libjson-perl=2.90-1 libldap2-dev=2.4.42+dfsg-2ubuntu3.13 libmysqlclient-dev=5.7.33-0ubuntu0.16.04.1 libnagios-object-perl=0.21.20-2 libnagios-plugin-perl=0.36-2 libnet-snmp-perl=6.0.1-2 libnet-snmp-perl=6.0.1-2 libnet-tftp-perl=0.19-1 libnet-xmpp-perl=1.02-5 libpq-dev=9.5.25-0ubuntu0.16.04.1 libredis-perl=2:1.9820-1 librrds-perl=1.5.5-4 libssl-dev=1.0.2g-1ubuntu4.20 libswitch-perl=2.17-2 libwww-perl=6.15-1 m4=1.4.17-5 netcat=1.10-41 parallel=20161222-1~ubuntu0.16.04.1 php-cli=1:7.0+35ubuntu6.1 php-gd=1:7.0+35ubuntu6.1 postfix=3.1.0-3ubuntu0.4 python-pip=8.1.1-2ubuntu0.6 rsyslog=8.16.0-1ubuntu3.1 runit=2.1.2-3ubuntu1 smbclient=2:4.3.11+dfsg-0ubuntu0.16.04.34 snmp=5.7.3+dfsg-1ubuntu4.6 snmpd=5.7.3+dfsg-1ubuntu4.6 snmp-mibs-downloader=1.1 unzip=6.0-20ubuntu1.1 python=2.7.12-1~16.04 -y \
 && apt-get clean \
 && rm -Rf /var/lib/apt/lists/*
RUN (egrep -i "^${NAGIOS_GROUP}" /etc/group || groupadd $NAGIOS_GROUP ) \
 && (egrep -i "^${NAGIOS_CMDGROUP}" /etc/group || groupadd $NAGIOS_CMDGROUP )
RUN (id -u $NAGIOS_USER || useradd --system -d $NAGIOS_HOME -g $NAGIOS_GROUP $NAGIOS_USER ) \
 && (id -u $NAGIOS_CMDUSER || useradd --system -d $NAGIOS_HOME -g $NAGIOS_CMDGROUP $NAGIOS_CMDUSER )
RUN cd /tmp \
 && git clone https://github.com/multiplay/qstat.git \
 && cd qstat \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install \
 && make clean \
 && cd /tmp \
 && rm -Rf qstat
RUN cd /tmp \
 && git clone https://github.com/NagiosEnterprises/nagioscore.git -b $NAGIOS_BRANCH \
 && cd nagioscore \
 && ./configure --prefix=${NAGIOS_HOME} --exec-prefix=${NAGIOS_HOME} --enable-event-broker --with-command-user=${NAGIOS_CMDUSER} --with-command-group=${NAGIOS_CMDGROUP} --with-nagios-user=${NAGIOS_USER} --with-nagios-group=${NAGIOS_GROUP} \
 && make all \
 && make install \
 && make install-config \
 && make install-commandmode \
 && make install-webconf \
 && make clean \
 && cd /tmp \
 && rm -Rf nagioscore
RUN cd /tmp \
 && git clone https://github.com/nagios-plugins/nagios-plugins.git -b $NAGIOS_PLUGINS_BRANCH \
 && cd nagios-plugins \
 && ./tools/setup \
 && ./configure --prefix=${NAGIOS_HOME} --with-ipv6 --with-ping6-command="/bin/ping6 -n -U -W %d -c %d %s" \
 && make \
 && make install \
 && make clean \
 && mkdir -p /usr/lib/nagios/plugins \
 && ln -sf ${NAGIOS_HOME}/libexec/utils.pm /usr/lib/nagios/plugins \
 && cd /tmp \
 && rm -Rf nagios-plugins
RUN wget -O ${NAGIOS_HOME}/libexec/check_ncpa.py https://raw.githubusercontent.com/NagiosEnterprises/ncpa/v2.0.5/client/check_ncpa.py \
 && chmod +x ${NAGIOS_HOME}/libexec/check_ncpa.py
RUN cd /tmp \
 && git clone https://github.com/NagiosEnterprises/nrpe.git -b $NRPE_BRANCH \
 && cd nrpe \
 && ./configure --with-ssl=/usr/bin/openssl --with-ssl-lib=/usr/lib/x86_64-linux-gnu \
 && make check_nrpe \
 && cp src/check_nrpe ${NAGIOS_HOME}/libexec/ \
 && make clean \
 && cd /tmp \
 && rm -Rf nrpe
RUN cd /tmp \
 && git clone https://git.code.sf.net/p/nagiosgraph/git nagiosgraph \
 && cd nagiosgraph \
 && ./install.pl --install --prefix /opt/nagiosgraph --nagios-user ${NAGIOS_USER} --www-user ${NAGIOS_USER} --nagios-perfdata-file ${NAGIOS_HOME}/var/perfdata.log --nagios-cgi-url /cgi-bin \
 && cp share/nagiosgraph.ssi ${NAGIOS_HOME}/share/ssi/common-header.ssi \
 && cd /tmp \
 && rm -Rf nagiosgraph
RUN cd /opt \
 && pip install pymssql==2.2.7 \
 && git clone https://github.com/willixix/naglio-plugins.git WL-Nagios-Plugins \
 && git clone https://github.com/JasonRivers/nagios-plugins.git JR-Nagios-Plugins \
 && git clone https://github.com/justintime/nagios-plugins.git JE-Nagios-Plugins \
 && git clone https://github.com/nagiosenterprises/check_mssql_collection.git nagios-mssql \
 && chmod +x /opt/WL-Nagios-Plugins/check* \
 && chmod +x /opt/JE-Nagios-Plugins/check_mem/check_mem.pl \
 && cp /opt/JE-Nagios-Plugins/check_mem/check_mem.pl ${NAGIOS_HOME}/libexec/ \
 && cp /opt/nagios-mssql/check_mssql_database.py ${NAGIOS_HOME}/libexec/ \
 && cp /opt/nagios-mssql/check_mssql_server.py ${NAGIOS_HOME}/libexec/
RUN sed -i.bak 's/.*\=www\-data//g' /etc/apache2/envvars
RUN export DOC_ROOT="DocumentRoot $( echo $NAGIOS_HOME/share ;)" \
 && sed -i "s,DocumentRoot.*,$DOC_ROOT," /etc/apache2/sites-enabled/000-default.conf \
 && sed -i "s,</VirtualHost>,<IfDefine ENABLE_USR_LIB_CGI_BIN>\nScriptAlias /cgi-bin/ ${NAGIOS_HOME}/sbin/\n</IfDefine>\n</VirtualHost>," /etc/apache2/sites-enabled/000-default.conf \
 && ln -s /etc/apache2/mods-available/cgi.load /etc/apache2/mods-enabled/cgi.load
RUN mkdir -p -m 0755 /usr/share/snmp/mibs \
 && mkdir -p ${NAGIOS_HOME}/etc/conf.d \
 && mkdir -p ${NAGIOS_HOME}/etc/monitor \
 && mkdir -p -m 700 ${NAGIOS_HOME}/.ssh \
 && chown ${NAGIOS_USER}:${NAGIOS_GROUP} ${NAGIOS_HOME}/.ssh \
 && touch /usr/share/snmp/mibs/.foo \
 && ln -s /usr/share/snmp/mibs ${NAGIOS_HOME}/libexec/mibs \
 && ln -s ${NAGIOS_HOME}/bin/nagios /usr/local/bin/nagios \
 && download-mibs \
 && echo "mibs +ALL" > /etc/snmp/snmp.conf
RUN sed -i 's,/bin/mail,/usr/bin/mail,' ${NAGIOS_HOME}/etc/objects/commands.cfg \
 && sed -i 's,/usr/usr,/usr,' ${NAGIOS_HOME}/etc/objects/commands.cfg
RUN cp /etc/services /var/spool/postfix/etc/ \
 && echo "smtp_address_preference = ipv4" >> /etc/postfix/main.cf
RUN rm -rf /etc/rsyslog.d /etc/rsyslog.conf
RUN rm -rf /etc/sv/getty-5
COPY overlay /
RUN echo "use_timezone=${NAGIOS_TIMEZONE}" >> ${NAGIOS_HOME}/etc/nagios.cfg
#   Copy example config in-case the user has started with empty var or etc
RUN mkdir -p /orig/var \
 && mkdir -p /orig/etc \
 && cp -Rp ${NAGIOS_HOME}/var/* /orig/var/ \
 && cp -Rp ${NAGIOS_HOME}/etc/* /orig/etc/
RUN a2enmod session \
 && a2enmod session_cookie \
 && a2enmod session_crypto \
 && a2enmod auth_form \
 && a2enmod request
RUN chmod +x /usr/local/bin/start_nagios \
 && chmod +x /etc/sv/apache/run \
 && chmod +x /etc/sv/nagios/run \
 && chmod +x /etc/sv/postfix/run \
 && chmod +x /etc/sv/rsyslog/run \
 && chmod +x /opt/nagiosgraph/etc/fix-nagiosgraph-multiple-selection.sh
RUN cd /opt/nagiosgraph/etc \
 && sh fix-nagiosgraph-multiple-selection.sh
RUN rm /opt/nagiosgraph/etc/fix-nagiosgraph-multiple-selection.sh
#   enable all runit services
RUN ln -s /etc/sv/* /etc/service
ENV APACHE_LOCK_DIR="/var/run"
ENV APACHE_LOG_DIR="/var/log/apache2"
#  Set ServerName and timezone for Apache
RUN echo "ServerName ${NAGIOS_FQDN}" > /etc/apache2/conf-available/servername.conf \
 && echo "PassEnv TZ" > /etc/apache2/conf-available/timezone.conf \
 && ln -s /etc/apache2/conf-available/servername.conf /etc/apache2/conf-enabled/servername.conf \
 && ln -s /etc/apache2/conf-available/timezone.conf /etc/apache2/conf-enabled/timezone.conf
EXPOSE 80/tcp
VOLUME "${NAGIOS_HOME}/var" "${NAGIOS_HOME}/etc" "/var/log/apache2" "/opt/Custom-Nagios-Plugins" "/opt/nagiosgraph/var" "/opt/nagiosgraph/etc"
CMD ["/usr/local/bin/start_nagios"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
