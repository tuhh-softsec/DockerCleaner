#   Dockerfile for icinga2 with icingaweb2
#   https://github.com/jjethwa/icinga2
FROM debian:stretch
MAINTAINER Jordan Jethwa
ENV APACHE2_HTTP="REDIRECT" \
    ICINGA2_FEATURE_GRAPHITE="false" \
    ICINGA2_FEATURE_GRAPHITE_HOST="graphite" \
    ICINGA2_FEATURE_GRAPHITE_PORT="2003" \
    ICINGA2_FEATURE_GRAPHITE_URL="http://graphite" \
    ICINGA2_FEATURE_GRAPHITE_SEND_THRESHOLDS="true" \
    ICINGA2_FEATURE_GRAPHITE_SEND_METADATA="false" \
    ICINGA2_USER_FULLNAME="Icinga2" \
    ICINGA2_FEATURE_DIRECTOR="true" \
    ICINGA2_FEATURE_DIRECTOR_KICKSTART="true" \
    ICINGA2_FEATURE_DIRECTOR_USER="icinga2-director"
RUN export DEBIAN_FRONTEND=noninteractive \
 && apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends apache2=2.4.25-3+deb9u13 ca-cacert=2011.0523-2 ca-certificates=20200601~deb9u2 curl=7.52.1-5+deb9u16 dnsutils=1:9.10.3.dfsg.P4-12.3+deb9u12 file=1:5.30-1+deb9u3 gnupg=2.1.18-8~deb9u4 libdbd-mysql-perl=4.041-2 libdigest-hmac-perl=1.03+dfsg-1 libnet-snmp-perl=6.0.1-2 locales=2.24-11+deb9u4 lsb-release=9.20161125 mailutils=1:3.1.1-1 mariadb-client=10.1.48-0+deb9u2 mariadb-server=10.1.48-0+deb9u2 netbase=5.4 openssh-client=1:7.4p1-10+deb9u7 openssl=1.1.0l-1~deb9u6 php-curl=1:7.0+49 php-ldap=1:7.0+49 php-mysql=1:7.0+49 procps=2:3.3.12-3+deb9u1 pwgen=2.07-1.1+b1 snmp=5.7.3+dfsg-1.7+deb9u3 ssmtp=2.64-8+b2 sudo=1.8.19p1-2.1+deb9u3 supervisor=3.3.1-1+deb9u1 unzip=6.0-21+deb9u2 wget=1.18-5+deb9u3 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN export DEBIAN_FRONTEND=noninteractive \
 && curl -s https://packages.icinga.com/icinga.key | apt-key add - \
 && echo "deb http://packages.icinga.org/debian icinga-$( lsb_release -cs ;) main" > /etc/apt/sources.list.d/icinga2.list \
 && apt-get update \
 && apt-get install --no-install-recommends icinga2=2.6.0-2+deb9u2 icinga2-ido-mysql=2.6.0-2+deb9u2 icingacli=2.4.1-1+deb9u1 icingaweb2=2.4.1-1+deb9u1 icingaweb2-module-doc=2.4.1-1+deb9u1 icingaweb2-module-monitoring=2.4.1-1+deb9u1 monitoring-plugins=2.2-3 nagios-nrpe-plugin=3.0.1-3+deb9u1 nagios-plugins-contrib=21.20170222 nagios-snmp-plugins=2.0.0-1 libmonitoring-plugin-perl=0.39-1 -y --install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ARG GITREF_DIRECTOR=master
ARG GITREF_MODGRAPHITE=master
ARG GITREF_MODAWS=master
RUN mkdir -p /usr/local/share/icingaweb2/modules/ \
 && mkdir -p /usr/local/share/icingaweb2/modules/director/ \
 && wget -q --no-cookies -O - "https://github.com/Icinga/icingaweb2-module-director/archive/${GITREF_DIRECTOR}.tar.gz" | tar xz --strip-components=1 --directory=/usr/local/share/icingaweb2/modules/director --exclude=.gitignore -f - \
 && mkdir -p /usr/local/share/icingaweb2/modules/graphite \
 && wget -q --no-cookies -O - "https://github.com/Icinga/icingaweb2-module-graphite/archive/${GITREF_MODGRAPHITE}.tar.gz" | tar xz --strip-components=1 --directory=/usr/local/share/icingaweb2/modules/graphite -f - icingaweb2-module-graphite-${GITREF_MODGRAPHITE}/ \
 && mkdir -p /usr/local/share/icingaweb2/modules/aws \
 && wget -q --no-cookies -O - "https://github.com/Icinga/icingaweb2-module-aws/archive/${GITREF_MODAWS}.tar.gz" | tar xz --strip-components=1 --directory=/usr/local/share/icingaweb2/modules/aws -f - icingaweb2-module-aws-${GITREF_MODAWS}/ \
 && wget -q --no-cookies "https://github.com/aws/aws-sdk-php/releases/download/2.8.30/aws.zip" \
 && unzip -d /usr/local/share/icingaweb2/modules/aws/library/vendor/aws aws.zip \
 && rm aws.zip \
 && true
COPY content/ /
#   Final fixes
RUN true \
 && sed -i 's/vars\.os.*/vars.os = "Docker"/' /etc/icinga2/conf.d/hosts.conf \
 && mv /etc/icingaweb2/ /etc/icingaweb2.dist \
 && mkdir /etc/icingaweb2 \
 && mv /etc/icinga2/ /etc/icinga2.dist \
 && mkdir /etc/icinga2 \
 && usermod -aG icingaweb2 www-data \
 && usermod -aG nagios www-data \
 && rm -rf /var/lib/mysql/* \
 && chmod u+s,g+s /bin/ping /bin/ping6 /usr/lib/nagios/plugins/check_icmp
EXPOSE 80/tcp 443/tcp 5665/tcp
#   Initialize and run Supervisor
ENTRYPOINT ["/opt/run"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
