FROM ubuntu:16.04
ENV AEGIR_VERSION="7.x-3.x"
ENV DEVSHOP_VERSION="1.x"
ENV PROVISION_VERSION="7.x-3.x"
ENV COMPOSER_VERSION="1.8.4"
ENV DRUSH_VERSION="8.1.18"
ENV REGISTRY_REBUILD_VERSION="7.x-2.5"
ENV AEGIR_MAKEFILE="https://raw.githubusercontent.com/opendevshop/devshop/$DEVSHOP_VERSION/build-devmaster.make"
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends apache2 cron pwgen git-core libapache2-mod-php libpcre3-dev mysql-client mysql-server nano openssh-server postfix rsync sudo unzip wget vim -y -qq
ENV LC_ALL="C.UTF-8"
ENV LANG="en_US.UTF-8"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends software-properties-common -y -qq
RUN add-apt-repository ppa:ondrej/php \
 && add-apt-repository ppa:ondrej/apache2 \
 && apt-get update -qq
#  PHP 7.2
RUN apt-get install --no-install-recommends php7.2 php7.2-cli php7.2-curl php7.2-dev php7.2-fpm php7.2-gd php7.2-json php7.2-mbstring php7.2-mysql php7.2-xml -y -qq
#  PHP 7.1
RUN apt-get install --no-install-recommends php7.1 php7.1-cli php7.1-curl php7.1-fpm php7.1-gd php7.1-mbstring php7.1-mysql php7.1-xml -y -qq
#  PHP 7.0
RUN apt-get install --no-install-recommends php7.0 php7.0-cli php7.0-curl php7.0-fpm php7.0-gd php7.0-mbstring php7.0-mysql php7.0-xml -y -qq
#  PHP 5.3
RUN apt-get install --no-install-recommends php5.6 php5.6-cli php5.6-curl php5.6-fpm php5.6-gd php5.6-mbstring php5.6-mysql php5.6-xml -y -qq
RUN apt-get install --no-install-recommends apache2 -y -qq
ENV AEGIR_UID="12345"
RUN echo "Creating user aegir with UID $AEGIR_UID and GID $AEGIR_GID"
RUN addgroup --gid $AEGIR_UID aegir
RUN adduser --uid $AEGIR_UID --gid $AEGIR_UID --system --shell /bin/bash --home /var/aegir aegir
RUN adduser aegir www-data
RUN a2enmod rewrite
RUN a2enmod ssl
RUN ln -s /var/aegir/config/apache.conf /etc/apache2/conf-available/aegir.conf
RUN ln -s /etc/apache2/conf-available/aegir.conf /etc/apache2/conf-enabled/aegir.conf
RUN wget https://github.com/composer/composer/releases/download/$COMPOSER_VERSION/composer.phar -O - -q > /usr/local/bin/composer
RUN wget https://github.com/drush-ops/drush/releases/download/$DRUSH_VERSION/drush.phar -O - -q > /usr/local/bin/drush
RUN chmod +x /usr/local/bin/composer
RUN chmod +x /usr/local/bin/drush
#  Install fix-permissions and fix-ownership scripts
RUN wget http://cgit.drupalcode.org/hosting_tasks_extra/plain/fix_permissions/scripts/standalone-install-fix-permissions-ownership.sh
RUN bash standalone-install-fix-permissions-ownership.sh
#  Copy files
COPY files/sudoers-aegir /etc/sudoers.d/aegir
RUN chmod 0440 /etc/sudoers.d/aegir
COPY files/docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh
COPY files/services-start.sh /usr/local/bin/services-start
RUN chmod +x /usr/local/bin/services-start
COPY files/set-user-ids.sh /usr/local/bin/set-user-ids
RUN chmod +x /usr/local/bin/set-user-ids
#  Prepare Aegir Logs folder.
RUN mkdir /var/log/aegir
RUN chown aegir:aegir /var/log/aegir
RUN echo 'Hello, Aegir.' > /var/log/aegir/system.log
#  Don't install provision. Downstream tags will do this with the right version.
# # Install Provision for all.
RUN mkdir -p /usr/share/drush/commands
RUN drush dl --destination=/usr/share/drush/commands provision-$PROVISION_VERSION -y
RUN drush dl --destination=/usr/share/drush/commands registry_rebuild-$REGISTRY_REBUILD_VERSION -y
#  Setup SSH server.
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
ENV NOTVISIBLE="\"in users profile\""
RUN echo "export VISIBLE=now" >> /etc/profile
EXPOSE 22/tcp
USER aegir
RUN mkdir /var/aegir/config
RUN mkdir /var/aegir/.drush
RUN mkdir /var/aegir/projects
RUN mkdir /var/aegir/.ssh
RUN chmod 700 /var/aegir/.ssh
#  You may change this environment at run time. User UID 1 is created with this email address.
ENV AEGIR_CLIENT_EMAIL="aegir@aegir.local.computer"
ENV AEGIR_CLIENT_NAME="admin"
ENV AEGIR_PROFILE="devmaster"
ENV AEGIR_WORKING_COPY="0"
#  Must be fixed across versions so we can upgrade containers.
ENV AEGIR_HOSTMASTER_ROOT="/var/aegir/devmaster"
#  Build the devmaster stack
RUN drush make $AEGIR_MAKEFILE $AEGIR_HOSTMASTER_ROOT
WORKDIR /var/aegir
VOLUME /var/lib/mysql
EXPOSE 80/tcp
EXPOSE 443/tcp
EXPOSE 3306/tcp
#  The Hostname of the database server to use
ENV AEGIR_DATABASE_SERVER="localhost"
ENV HOME="/var/aegir"
VOLUME /var/aegir/projects
#  docker-entrypoint.sh waits for mysql and runs hostmaster install
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["drush", "@hostmaster", "hosting-queued"]
