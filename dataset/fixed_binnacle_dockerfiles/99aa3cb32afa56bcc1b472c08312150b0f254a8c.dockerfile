FROM debian:jessie
MAINTAINER Fritz SMH <fritz.smh@gmail.com>
#  ## Configuration #################################
ENV domogik_release="develop"
ENV domogikmq_release="1.4"
ENV domoweb_release="develop"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
#  ##################################################
#
#  ## How to...
#
#   1. Do development on git sources with the built image
#   
#   TODO : explain
#
#
#
#  ## TODO : 
#
#   Improvments
#   - process TODO in this Dockerfile
#   - find a proper way to work on git sources and do the related doc (volumes)
#     - https://howchoo.com/g/zdq5m2exmze/docker-persistence-with-a-data-only-container
#     - https://howchoo.com/g/y2y1mtkznda/getting-started-with-docker-compose-and-django
#     - http://www.alexecollins.com/docker-persistence/
#   - install weather
#   - find a way with domoweb to put some widget automatically
#  ## Apt 
RUN :
#  ## Tools not mandatory for Domogik but usefull for tests/debug/development
RUN (apt-get update ;apt-get install --no-install-recommends vim telnet wget openssh-server screen dos2unix tcpdump -y )
#  ## Minimum requirements
RUN (apt-get update ;apt-get install --no-install-recommends sudo python2.7-dev python-pip git -y )
RUN pip install netifaces==0.11.0 \
 && pip install sphinx-better-theme==0.1.5
#  ## Database server
#   Install MySQL
#   warning : no root password defined!
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y mysql-server
#   Create the empty database
RUN /bin/bash -c "/usr/bin/mysqld_safe &" \
 && sleep 5 \
 && mysql -u root -e "CREATE DATABASE domogik;" \
 && mysql -u root -e "GRANT ALL PRIVILEGES ON domogik.* to domogik@localhost IDENTIFIED BY 'domopass';"
#  ## Create user and directories
RUN useradd -M domogik \
 && mkdir -p /opt/dmg \
 && chown domogik:domogik /opt/dmg
#  ## Change passwords
RUN echo "root:$ROOT_PASSWORD" | chpasswd \
 && echo "domogik:$DOMOGIK_PASSWORD" | chpasswd
#  ## Create a fake cron folder to avoid error during install
RUN mkdir -p /etc/cron.d/
#  ## Deploy the sources
#   1. demo mode
#   grab sources from a git tag
RUN cd /opt/dmg \
 && git clone https://github.com/domogik/domogik-mq.git \
 && cd /opt/dmg/domogik-mq \
 && git checkout ${domogikmq_release}
RUN cd /opt/dmg \
 && git clone https://github.com/domogik/domogik.git \
 && cd /opt/dmg/domogik \
 && git checkout ${domogik_release}
RUN cd /opt/dmg \
 && git clone https://github.com/domogik/domoweb.git \
 && cd /opt/dmg/domoweb \
 && git checkout ${domoweb_release}
#   2. dev mode
#   this is done from command line with -v
#   TODO : improve...
#          currently this is only a copy
#  COPY git/domogik /opt/dmg/domogik
#  COPY git/domogik-mq /opt/dmg/domogik-mq
#  RUN cd /opt/dmg/domogik-mq \
#   && git checkout ${domogikmq_release} 
#  RUN cd /opt/dmg/domogik \
#   && git checkout ${domogik_release}  
#  ## Install
#   Patches. TODO : move before
RUN pip install Flask-Themes2==1.0.0
RUN (apt-get update ;apt-get install --no-install-recommends libpq-dev -y )
RUN pip install alembic==1.10.3
RUN pip install SQLAlchemy-Utils==0.41.0
#   Domogik-mq
RUN cd /opt/dmg/domogik-mq \
 && python install.py --daemon --user domogik --command-line
#   Domogik
#   MySQL should be run before install !
RUN /bin/bash -c "/usr/bin/mysqld_safe &" \
 && sleep 5 \
 && cd /opt/dmg/domogik \
 && python install.py --user domogik --command-line --no-create-database --admin_interfaces "*" --admin_secret_key "dockersupersecretkey"
#   Domoweb
RUN cd /opt/dmg/domoweb \
 && python install.py --user domogik
#  ## Install a few packages and their needed dependencies
RUN su - domogik -c "dmg_package -i http://github.com/fritz-smh/domogik-plugin-diskfree/archive/1.4.zip"
RUN su - domogik -c "dmg_package -i http://github.com/fritz-smh/domogik-plugin-weather/archive/1.4.zip"
RUN su - domogik -c "dmg_package -i http://github.com/domogik/domogik-brain-base/archive/1.3.zip"
RUN su - domogik -c "dmg_package -i http://github.com/fritz-smh/domogik-plugin-generic/archive/develop.zip"
RUN pip install npyscreen==4.10.5 \
 && su - domogik -c "dmg_package -i http://github.com/fritz-smh/domogik-interface-chat/archive/develop.zip"
#  ## Cleanup
RUN apt-get clean
#  ## Expose ports
#   40404 : domoweb
#   40405 : domogik admin websocket
#   40406 : domogik admin http
#   3865 : xpl hub
EXPOSE 40404/tcp 40405/tcp 40406/tcp 22/tcp 3865/tcp
#  ## Volumes
#   we set /opt/dmg/ as a volume to allow changes to be kept from one run to another run in case of debugging tests.
VOLUME /opt/dmg/
#   we set /var/log/domogik as a volume to allow checking logs from outside the container.
VOLUME /var/log/domogik/
#   we set /var/lib/domogik as a volume to allow access to domogik packages and other components
VOLUME /var/lib/domogik/
#  ## Startup actions
COPY scripts/startup.sh /opt/startup.sh
RUN chmod a+x /opt/startup.sh
CMD ["/bin/bash", "/opt/startup.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
