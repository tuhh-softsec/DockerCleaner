FROM ubuntu:14.04
MAINTAINER Jaewoo Lee <continuse@icloud.com>
#  Mysql Server
ENV MYSQL_ROOT_PASSWORD="openstack"
RUN { echo "mysql-server-5.5 mysql-server/root_password password $MYSQL_ROOT_PASSWORD" ;echo "mysql-server-5.5 mysql-server/root_password_again password $MYSQL_ROOT_PASSWORD" ;echo "mysql-server-5.5 mysql-server/root_password seen true" ;echo "mysql-server-5.5 mysql-server/root_password_again seen true" ; } | debconf-set-selections \
 && apt-get update \
 && apt-get install mysql-server python-mysqldb -y
#  Util
RUN apt-get install curl -y
#  Ubuntu Cloud archive keyring and repository
RUN apt-get install ubuntu-cloud-keyring -y \
 && echo "deb http://ubuntu-cloud.archive.canonical.com/ubuntu" "trusty-updates/kilo main" > /etc/apt/sources.list.d/cloudarchive-kilo.list \
 && apt-get update \
 && apt-get -y dist-upgrade
#  RabbitMQ
RUN apt-get install rabbitmq-server -y
#  Keystone
RUN echo "manual" > /etc/init/keystone.override \
 && apt-get install keystone python-openstackclient apache2 libapache2-mod-wsgi memcached python-memcache -y \
 && ln -s /etc/apache2/sites-available/wsgi-keystone.conf /etc/apache2/sites-enabled \
 && mkdir -p /var/www/cgi-bin/keystone \
 && curl http://git.openstack.org/cgit/openstack/keystone/plain/httpd/keystone.py?h=stable/kilo | tee /var/www/cgi-bin/keystone/main /var/www/cgi-bin/keystone/admin \
 && chown -R keystone:keystone /var/www/cgi-bin/keystone \
 && chmod 755 /var/www/cgi-bin/keystone/* \
 && rm -f /var/lib/keystone/keystone.db
#  Glance Setup
RUN apt-get install glance python-glanceclient -y \
 && rm -f /var/lib/glance/glance.sqlite
#  Nova Management
RUN apt-get install nova-api nova-cert nova-conductor nova-consoleauth nova-novncproxy nova-scheduler python-novaclient -y \
 && rm -f /var/lib/nova/nova.sqlite
#  Neutron
RUN apt-get install neutron-server neutron-plugin-ml2 python-neutronclient -y
#  Horizone
RUN apt-get install openstack-dashboard -y \
 && dpkg --purge openstack-dashboard-ubuntu-theme
#  Heat
RUN apt-get update \
 && apt-get install heat-api heat-api-cfn heat-engine python-heatclient -y
#  Cinder
RUN apt-get install cinder-api cinder-scheduler python-cinderclient -y
#  MySQL Data Volume
VOLUME ["/data"]
#  MySQL
COPY config/mysql/my.cnf /etc/mysql/my.cnf
#  WSGI for Keystone
COPY config/wsgi-keystone.conf /etc/apache2/sites-available/wsgi-keystone.conf
#  Configuration File for Keystone Service
COPY config/keystone/keystone.conf /etc/keystone/keystone.conf
#  Configuration File for Glance Service
COPY config/glance/glance-api.conf /etc/glance/glance-api.conf
COPY config/glance/glance-registry.conf /etc/glance/glance-registry.conf
#  Configuration File for Nova Service
COPY config/nova/nova.conf /etc/nova/nova.conf
#  Configuration File for Neutron Service
COPY config/neutron/neutron.conf /etc/neutron/neutron.conf
COPY config/neutron/ml2_conf.ini /etc/neutron/plugins/ml2/ml2_conf.ini
#  Configuration File for Heat Service
COPY config/heat/heat.conf /etc/heat/heat.conf
#  Dashboard conf file
COPY config/horizon/local_settings.py /etc/openstack-dashboard/local_settings.py
#  Cinder conf file
COPY config/cinder/cinder.conf /etc/cinder/cinder.conf
RUN chown glance:glance /etc/glance/glance-api.conf \
 && chown glance:glance /etc/glance/glance-registry.conf \
 && chown nova:nova /etc/nova/nova.conf \
 && chown root:neutron /etc/neutron/neutron.conf \
 && chown root:neutron /etc/neutron/plugins/ml2/ml2_conf.ini \
 && chown cinder:cinder /etc/cinder/cinder.conf
COPY entrypoint.sh /entrypoint.sh
COPY keystone.sh /keystone.sh
CMD ["/entrypoint.sh"]
EXPOSE 3306/tcp 35357/tcp 9292/tcp 5000/tcp 5672/tcp 8774/tcp 8776/tcp 6080/tcp 9696/tcp 80/tcp
