#   Shinken Docker installation using pip (latest)
FROM debian:wheezy
MAINTAINER Rohit Gupta
#   Install Shinken, Nagios plugins, apache2 and supervisord
RUN apt-get update \
 && apt-get install --no-install-recommends python-pip python-pycurl python-cherrypy3 nagios-plugins libsys-statistics-linux-perl apache2 libapache2-mod-proxy-html supervisor python-dev python-cairo python-crypto libssl-dev inotify-tools ntp -y
RUN useradd --create-home shinken \
 && pip install shinken==2.4.3 pymongo==4.3.3 requests==2.28.2 arrow==1.2.3 bottle==0.12.8 > =3.0.3 \
 && update-rc.d -f apache2 remove \
 && update-rc.d -f shinken remove
#   Install shinken modules from shinken.io
RUN chown -R shinken:shinken /etc/shinken/ \
 && su - shinken -c 'shinken --init' \
 && su - shinken -c 'shinken install webui2' \
 && su - shinken -c 'shinken install auth-htpasswd' \
 && su - shinken -c 'shinken install sqlitedb' \
 && su - shinken -c 'shinken install pickle-retention-file-scheduler' \
 && su - shinken -c 'shinken install booster-nrpe' \
 && su - shinken -c 'shinken install logstore-sqlite' \
 && su - shinken -c 'shinken install livestatus' \
 && su - shinken -c 'shinken install graphite' \
 && su - shinken -c 'shinken install ui-graphite'
#   Install Graphite
RUN pip install Twisted==11.1.0 \
 && pip install flup==1.0.2 \
 && pip install django==1.5.10 \
 && pip install django-tagging==0.3.2 \
 && pip install https://github.com/graphite-project/ceres/tarball/master \
 && pip install whisper==0.9.12 \
 && pip install carbon==0.9.12 \
 && pip install graphite-web==0.9.12 \
 && pip install gunicorn==19.1.1
#   Install and configure thruk
RUN gpg --keyserver keys.gnupg.net --recv-keys F8C1CA08A57B9ED7 \
 && gpg --armor --export F8C1CA08A57B9ED7 | apt-key add - \
 && echo 'deb http://labs.consol.de/repo/stable/debian wheezy main' >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends thruk -y \
 && apt-get clean
COPY thruk/thruk_local.conf /etc/thruk/thruk_local.conf
#   Install check_nrpe plugin
COPY nrpe-2.15.tar.gz /usr/src/
RUN cd /usr/src/nrpe-2.15/ \
 && ./configure --with-nagios-user=shinken --with-nagios-group=shinken --with-nrpe-user=shinken --with-nrpe-group=shinken --with-ssl=/usr/bin/openssl --with-ssl-lib=/usr/lib/x86_64-linux-gnu \
 && make all \
 && make install-plugin \
 && mv /usr/local/nagios/libexec/check_nrpe /usr/lib/nagios/plugins/check_nrpe \
 && cd / \
 && rm -rf /usr/src/nrpe-2.15
#   Configure apache
COPY shinken/shinken_apache.conf /etc/apache2/conf.d/shinken_apache.conf
RUN ln -sf /etc/apache2/mods-available/proxy* /etc/apache2/mods-enabled/
#   Configure Shinken modules
COPY shinken/shinken.cfg /etc/shinken/shinken.cfg
COPY shinken/broker-master.cfg /etc/shinken/brokers/broker-master.cfg
COPY shinken/poller-master.cfg /etc/shinken/pollers/poller-master.cfg
COPY shinken/scheduler-master.cfg /etc/shinken/schedulers/scheduler-master.cfg
COPY shinken/webui2.cfg /etc/shinken/modules/webui2.cfg
COPY shinken/webui2_worldmap.cfg /var/lib/shinken/modules/webui2/plugins/worldmap/plugin.cfg
COPY shinken/livestatus.cfg /etc/shinken/modules/livestatus.cfg
COPY shinken/graphite.cfg /etc/shinken/modules/graphite.cfg
COPY shinken/ui-graphite.cfg /etc/shinken/modules/ui-graphite.cfg
RUN mkdir -p /etc/shinken/custom_configs /usr/local/custom_plugins \
 && ln -sf /etc/shinken/custom_configs/htpasswd.users /etc/shinken/htpasswd.users \
 && rm -f /etc/thruk/htpasswd \
 && ln -sf /etc/shinken/htpasswd.users /etc/thruk/htpasswd \
 && chown -R shinken:shinken /etc/shinken/
#   Configure graphite
COPY graphite/carbon.conf /opt/graphite/conf/
COPY graphite/storage-schemas.conf /opt/graphite/conf/
COPY graphite/storage-aggregation.conf /opt/graphite/conf/
COPY graphite/local_settings.py /opt/graphite/webapp/graphite/local_settings.py
RUN mkdir -p /var/log/graphite \
 && cd /opt/graphite/webapp/graphite/ \
 && python manage.py syncdb --noinput
#   Add shinken config watcher to restart arbiter, when changes happen
COPY shinken/watch_shinken_config.sh /usr/bin/watch_shinken_config.sh
RUN chmod 755 /usr/bin/watch_shinken_config.sh
#   Copy extra NRPE plugins and fix permissions
COPY extra_plugins/* /usr/lib/nagios/plugins/
RUN cd /usr/lib/nagios/plugins/ \
 && chmod a+x * \
 && chmod u+s check_apt restart_service check_ping check_icmp check_fping apt_update
#   Define mountable directories
VOLUME ["/etc/shinken/custom_configs", "/usr/local/custom_plugins"]
#   configure supervisor
COPY supervisor/conf.d/* /etc/supervisor/conf.d/
#   Expost port 80 (apache2)
EXPOSE 80/tcp
#   Default docker process
CMD ["supervisord", "-c", "/etc/supervisor/supervisord.conf", "-n"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
