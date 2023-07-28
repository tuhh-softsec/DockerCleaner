FROM ubuntu:14.04
MAINTAINER Jaewoo Lee <continuse@icloud.com>
#   Mysql Server
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN { echo "mysql-server-5.5 mysql-server/root_password password $MYSQL_ROOT_PASSWORD" ;echo "mysql-server-5.5 mysql-server/root_password_again password $MYSQL_ROOT_PASSWORD" ;echo "mysql-server-5.5 mysql-server/root_password seen true" ;echo "mysql-server-5.5 mysql-server/root_password_again seen true" ; } | debconf-set-selections \
 && apt-get update \
 && apt-get install --no-install-recommends mysql-server=5.5.62-0ubuntu0.14.04.1 python-mysqldb=1.2.3-2ubuntu1 -y
#   Util
RUN apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 -y
#   Ubuntu Cloud archive keyring and repository
RUN apt-get install --no-install-recommends ubuntu-cloud-keyring=2012.08.14 -y \
 && echo "deb http://ubuntu-cloud.archive.canonical.com/ubuntu" "trusty-updates/kilo main" > /etc/apt/sources.list.d/cloudarchive-kilo.list \
 && apt-get update \
 && apt-get -y dist-upgrade
#   RabbitMQ
RUN apt-get install --no-install-recommends rabbitmq-server=3.2.4-1ubuntu0.1 -y
#   Keystone
RUN echo "manual" > /etc/init/keystone.override \
 && apt-get install --no-install-recommends keystone=1:2014.1.5-0ubuntu1 python-openstackclient=0.3.0-1ubuntu1 apache2=2.4.7-1ubuntu4.22 libapache2-mod-wsgi=3.4-4ubuntu2.1.14.04.2 memcached=1.4.14-0ubuntu9.3 python-memcache=1.53-1build1 -y \
 && ln -s /etc/apache2/sites-available/wsgi-keystone.conf /etc/apache2/sites-enabled \
 && mkdir -p /var/www/cgi-bin/keystone \
 && curl http://git.openstack.org/cgit/openstack/keystone/plain/httpd/keystone.py?h=stable/kilo | tee /var/www/cgi-bin/keystone/main /var/www/cgi-bin/keystone/admin \
 && chown -R keystone:keystone /var/www/cgi-bin/keystone \
 && chmod 755 /var/www/cgi-bin/keystone/* \
 && rm -f /var/lib/keystone/keystone.db
#   Glance Setup
RUN apt-get install --no-install-recommends glance=1:2014.1.5-0ubuntu1.1 python-glanceclient=1:0.12.0-0ubuntu1.2 -y \
 && rm -f /var/lib/glance/glance.sqlite
#   Nova Management
RUN apt-get install --no-install-recommends nova-api=1:2014.1.5-0ubuntu1.7 nova-cert=1:2014.1.5-0ubuntu1.7 nova-conductor=1:2014.1.5-0ubuntu1.7 nova-consoleauth=1:2014.1.5-0ubuntu1.7 nova-novncproxy=1:2014.1.5-0ubuntu1.7 nova-scheduler=1:2014.1.5-0ubuntu1.7 python-novaclient=1:2.17.0-0ubuntu1.2 -y \
 && rm -f /var/lib/nova/nova.sqlite
#   Neutron
RUN apt-get install --no-install-recommends neutron-server=1:2014.1.5-0ubuntu8 neutron-plugin-ml2=1:2014.1.5-0ubuntu8 python-neutronclient=1:2.3.4-0ubuntu1 -y
#   Horizone
RUN apt-get install --no-install-recommends openstack-dashboard=1:2014.1.5-0ubuntu2.2 -y \
 && dpkg --purge openstack-dashboard-ubuntu-theme
#   Heat
RUN apt-get update \
 && apt-get install --no-install-recommends heat-api=2014.1.5-0ubuntu1 heat-api-cfn=2014.1.5-0ubuntu1 heat-engine=2014.1.5-0ubuntu1 python-heatclient=0.2.8-0ubuntu1 -y
#   Cinder
RUN apt-get install --no-install-recommends cinder-api=1:2014.1.5-0ubuntu2.2 cinder-scheduler=1:2014.1.5-0ubuntu2.2 python-cinderclient=1:1.0.8-0ubuntu2 -y
#   MySQL Data Volume
VOLUME ["/data"]
#   MySQL
COPY config/mysql/my.cnf /etc/mysql/my.cnf
#   WSGI for Keystone
COPY config/wsgi-keystone.conf /etc/apache2/sites-available/wsgi-keystone.conf
#   Configuration File for Keystone Service
COPY config/keystone/keystone.conf /etc/keystone/keystone.conf
#   Configuration File for Glance Service
COPY config/glance/glance-api.conf /etc/glance/glance-api.conf
COPY config/glance/glance-registry.conf /etc/glance/glance-registry.conf
#   Configuration File for Nova Service
COPY config/nova/nova.conf /etc/nova/nova.conf
#   Configuration File for Neutron Service
COPY config/neutron/neutron.conf /etc/neutron/neutron.conf
COPY config/neutron/ml2_conf.ini /etc/neutron/plugins/ml2/ml2_conf.ini
#   Configuration File for Heat Service
COPY config/heat/heat.conf /etc/heat/heat.conf
#   Dashboard conf file
COPY config/horizon/local_settings.py /etc/openstack-dashboard/local_settings.py
#   Cinder conf file
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
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
