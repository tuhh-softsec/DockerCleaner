#  -*- coding: utf-8 -*-
#
#     LinOTP - the open source solution for two factor authentication
#     Copyright (C) 2016 - 2018 KeyIdentity GmbH
#
#     This file is part of LinOTP server.
#
#     This program is free software: you can redistribute it and/or
#     modify it under the terms of the GNU Affero General Public
#     License, version 3, as published by the Free Software Foundation.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU Affero General Public License for more details.
#
#     You should have received a copy of the
#                GNU Affero General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
#     E-mail: linotp@keyidentity.com
#     Contact: www.linotp.org
#     Support: www.keyidentity.com
#
#
#  LinOTP Docker build script
ARG BASE_IMAGE
FROM $BASE_IMAGE
ARG DEBIAN_MIRROR=deb.debian.org
ARG DEBIAN_RELEASE_NAME
ARG DEPENDENCY_SOURCE
ARG DEPENDENCY_GPG_KEYID
ARG DEPENDENCY_COMPONENT
ARG DEPENDENCY_GPG_KEY_URL
#  The following environment variables can be set to
#  configure the runtime behaviour of the container.
#  Most of these should be self explanitory.
#
#  To disable HTTP authentication for the manage UI,
#  set LINOTP_APACHE_AUTH=false.
#
#  To use a MySQL database:
#   LINOTP_DB_TYPE=mysql
#  and also substitute suitable values:
#   LINOTP_DB_HOST=hostname
#   LINOTP_DB_PORT=3306
#   LINOTP_DB_USER=user
#   LINOTP_DB_PASSWORD=password
#   LINOTP_DB_NAME=database-name
#
#  Unencrypted healthchecks can be performed by
#  checking http://HOSTNAME:81/validate/ok
#
#  To send LinOTP logs directly to
#  Logstash, set LOGSTASH_HOST and
#  LOGSTASH_PORT to point to your
#  Logstash collector. You can optionally
#  add additional tags with the
#  LOGSTASH_TAGS setting. This is a Python
#  list. For example:
#    LOGSTASH_HOST=logstash1
#    LOGSTASH_PORT=5000
#    LOGSTASH_TAGS=('instance1', 'server1')
#
#  To change the location of the database
#  encryption key file, set
#    SECRET_FILE_LOCATION=/path/to/encKey
ENV TZ="Europe/Berlin" \
    LINOTP_USER="linotp" \
    LINOTP_DB_TYPE="sqlite" \
    LINOTP_DB_NAME="//tmp/linotp.db" \
    LINOTP_DB_HOST="dbhost" \
    LINOTP_DB_PORT="3306" \
    LINOTP_DB_USER="dbuser" \
    LINOTP_DB_PASSWORD="dbpass" \
    LINOTP_ADMIN_USER="admin" \
    LINOTP_ADMIN_PASSWORD="admin" \
    LINOTP_APACHE_AUTH="true" \
    LINOTP_APACHE_SSL="true" \
    LINOTP_APACHE_HSTS="true" \
    LINOTP_LOGLEVEL="INFO" \
    LINOTP_CONSOLE_LOGLEVEL="DEBUG" \
    SQLALCHEMY_LOGLEVEL="ERROR" \
    APACHE_LOGLEVEL="info" \
    LOGSTASH_HOST="" \
    LOGSTASH_PORT="" \
    LOGSTASH_TAGS="()" \
    SECRET_FILE_LOCATION=""
#  Internal environment variables used by the docker images
ENV LINOTP_INI_TEMPLATE="/etc/linotp/linotp-docker.ini.tmpl" \
    LINOTP_HOME="/opt/linotp" \
    DEBIAN_FRONTEND="noninteractive"
RUN echo 'APT::Install-Recommends "0"; \n APT::Get::Assume-Yes "true"; \n APT::Install-Suggests "0";' > /etc/apt/apt.conf.d/01buildconfig
RUN sed "s#http://deb\.debian\.org/#http://${DEBIAN_MIRROR}/#" < /etc/apt/sources.list > /etc/apt/sources.list.new \
 && mv -f /etc/apt/sources.list.new /etc/apt/sources.list
RUN apt-get update \
 && apt-get install --no-install-recommends curl gnupg2 dirmngr --yes
RUN test -z "$DEPENDENCY_SOURCE" || echo "deb $DEPENDENCY_SOURCE $DEBIAN_RELEASE_NAME $DEPENDENCY_COMPONENT" > /etc/apt/sources.list.d/kideps.list
RUN test -z "$DEPENDENCY_GPG_KEYID" || apt-key adv --keyserver hkp://hkps.pool.sks-keyservers.net --recv-keys $DEPENDENCY_GPG_KEYID
RUN test -z "$DEPENDENCY_GPG_KEY_URL" || curl $DEPENDENCY_GPG_KEY_URL | apt-key adv --import
#  Install some dependencies which will be cached. This does not
#  need to be an exhaustive list because apt will install any
#  missing packages.
#  RUN \
#      apt-get update \
#      && extraPackages=' \
#  	make \
#  	python-psycopg2 \
#      ' \
#      && apt-get install \
#  		$extraPackages \
#  		adduser debconf openssl pwgen python-beaker python-configobj \
#  		python-decorator python-docutils python-formencode python-httplib2 \
#  		python-ldap python-m2crypto python-mako python-mysqldb python-netaddr \
#  		python-paste python-pastedeploy python-pastescript python-pygments python-pylons \
#  		python-pyrad python-qrcode python-routes python-simplejson \
#  		python-sqlalchemy python-tempita python-weberror python-webhelpers python-webob \
#  		apache2 libapache2-mod-wsgi python-pycryptodomex python-pysodium python-requests
#  Install linotp packages from local files.
COPY apt /opt/linotp/apt
RUN echo "linotp linotp/apache/activate boolean true" > /opt/linotp/apt/debconf-selections \
 && echo "linotp linotp/apache/ssl_create boolean true" >> /opt/linotp/apt/debconf-selections \
 && debconf-set-selections /opt/linotp/apt/debconf-selections \
 && echo "deb [trusted=yes] file:/opt/linotp/apt ./" > /etc/apt/sources.list.d/linotp-local.list \
 && (echo "Package: *" ;echo "Pin: origin \"\"" ;echo "Pin-Priority: 900" ) > /etc/apt/preferences.d/linotp \
 && apt-get update \
 && apt-get install linotp python-logstash \
 && rm /etc/apt/sources.list.d/linotp-local.list \
 && rm -r /opt/linotp/apt /etc/apache2/sites-enabled/000-default.conf \
 && rm /etc/linotp2/linotp.ini /etc/linotp2/encKey /etc/linotp2/*.pem
WORKDIR $LINOTP_HOME
#  Get dockerfy and configuration template files from build context
COPY *.tmpl /etc/linotp2/
COPY dockerfy /usr/local/bin/
COPY linotp-create-htdigest /usr/local/bin/
RUN chmod 755 /usr/local/bin/linotp-create-htdigest
ENTRYPOINT ["/usr/local/bin/dockerfy", "--template", "/etc/linotp2/linotp-docker.ini.tmpl:/etc/linotp2/linotp.ini", "--template", "/etc/linotp2/apache-docker.conf.tmpl:/etc/apache2/sites-enabled/linotp2.conf", "--run", "/var/lib/dpkg/info/linotp.postinst", "configure", "--", "--run", "/bin/grep", "url", "/etc/linotp2/linotp.ini", "--", "--run", "mkdir", "-p", "/etc/ssl/private", "/etc/ssl/certs", "--", "--run", "/bin/sh", "-c", "openssl", "req", "-new", "-x509", "-newkey", "rsa:2048", "-subj", "/CN=`hostname", "`.`dnsdomainname", "`", "-days", "768", "-out", "/etc/ssl/certs/linotpserver.pem", "-keyout", "/etc/ssl/private/linotpserver.key", "-nodes", "--", "--run", "/usr/local/bin/linotp-create-htdigest", "{{", ".Env.LINOTP_ADMIN_USER", "}}", "{{", ".Env.LINOTP_ADMIN_PASSWORD", "}}", "--", "--run", "/bin/sh", "-c", "if", "[", "{{", ".Env.LINOTP_DB_TYPE", "}}", "!=", "sqlite", "]"]
CMD ["/usr/sbin/apache2ctl", "-DFOREGROUND"]
COPY ./se_mypasswd /etc/se_mypasswd
#  Listen on apache port (https 443 by default - see LINOTP_APACHE_SSL and APACHE_PORT)
EXPOSE 80/tcp 81/tcp 443/tcp
