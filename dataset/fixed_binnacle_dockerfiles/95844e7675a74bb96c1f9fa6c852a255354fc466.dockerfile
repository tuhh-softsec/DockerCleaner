FROM ubuntu:18.04
ENV AEGIR_VERSION="7.x-3.151"
ENV DEVSHOP_VERSION="1.3.4"
ENV PROVISION_VERSION="7.x-3.151"
ENV REGISTRY_REBUILD_VERSION="7.x-2.5"
ENV AEGIR_MAKEFILE="https://raw.githubusercontent.com/opendevshop/devshop/$DEVSHOP_VERSION/build-devmaster.make"
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends apache2=2.4.29-1ubuntu4.27 cron=3.0pl1-128.1ubuntu1.2 pwgen=2.08-1 git-core libapache2-mod-php=1:7.2+60ubuntu1 libpcre3-dev=2:8.39-9ubuntu0.1 mysql-client=5.7.41-0ubuntu0.18.04.1 mysql-server=5.7.41-0ubuntu0.18.04.1 nano=2.9.3-2 openssh-server=1:7.6p1-4ubuntu0.7 php7.2-common=7.2.24-0ubuntu0.18.04.17 php7.2-curl=7.2.24-0ubuntu0.18.04.17 php7.2-cli=7.2.24-0ubuntu0.18.04.17 php7.2-dev=7.2.24-0ubuntu0.18.04.17 php7.2-fpm=7.2.24-0ubuntu0.18.04.17 php7.2-gd=7.2.24-0ubuntu0.18.04.17 php7.2-imap=7.2.24-0ubuntu0.18.04.17 php7.2-json=7.2.24-0ubuntu0.18.04.17 php7.2-opcache=7.2.24-0ubuntu0.18.04.17 php7.2-xml=7.2.24-0ubuntu0.18.04.17 php7.2-mbstring=7.2.24-0ubuntu0.18.04.17 php7.2-mysql=7.2.24-0ubuntu0.18.04.17 php-sqlite3=1:7.2+60ubuntu1 php-apcu=5.1.9+4.0.11-1build1 postfix=3.3.0-1ubuntu0.4 rsync=3.1.2-2.1ubuntu1.6 sudo=1.8.21p2-3ubuntu1.5 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 vim=2:8.0.1453-1ubuntu1.11 -y -qq
ENV AEGIR_UID="12345"
RUN echo "Creating user aegir with UID $AEGIR_UID and GID $AEGIR_GID"
RUN addgroup --gid $AEGIR_UID aegir
RUN adduser --uid $AEGIR_UID --gid $AEGIR_UID --system --shell /bin/bash --home /var/aegir aegir
RUN adduser aegir www-data
RUN a2enmod rewrite
RUN a2enmod ssl
RUN ln -s /var/aegir/config/apache.conf /etc/apache2/conf-available/aegir.conf
RUN ln -s /etc/apache2/conf-available/aegir.conf /etc/apache2/conf-enabled/aegir.conf
RUN wget https://raw.githubusercontent.com/composer/getcomposer.org/1b137f8bf6db3e79a38a5bc45324414a6b1f9df2/web/installer -O - -q | php -- --quiet
RUN cp composer.phar /usr/local/bin/composer
RUN wget https://github.com/drush-ops/drush/releases/download/8.1.17/drush.phar -O - -q > /usr/local/bin/drush
RUN chmod +x /usr/local/bin/composer
RUN chmod +x /usr/local/bin/drush
#   Install fix-permissions and fix-ownership scripts
RUN wget http://cgit.drupalcode.org/hosting_tasks_extra/plain/fix_permissions/scripts/standalone-install-fix-permissions-ownership.sh
RUN bash standalone-install-fix-permissions-ownership.sh
#   Copy files
COPY files/sudoers-aegir /etc/sudoers.d/aegir
RUN chmod 0440 /etc/sudoers.d/aegir
COPY files/docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh
COPY files/services-start.sh /usr/local/bin/services-start
RUN chmod +x /usr/local/bin/services-start
COPY files/set-user-ids.sh /usr/local/bin/set-user-ids
RUN chmod +x /usr/local/bin/set-user-ids
#   Prepare Aegir Logs folder.
RUN mkdir /var/log/aegir
RUN chown aegir:aegir /var/log/aegir
RUN echo 'Hello, Aegir.' > /var/log/aegir/system.log
#   Don't install provision. Downstream tags will do this with the right version.
#  # Install Provision for all.
RUN mkdir -p /usr/share/drush/commands
RUN drush dl --destination=/usr/share/drush/commands provision-$PROVISION_VERSION -y
RUN drush dl --destination=/usr/share/drush/commands registry_rebuild-$REGISTRY_REBUILD_VERSION -y
#   Setup SSH server.
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
ENV NOTVISIBLE="\"in users profile\""
RUN echo "export VISIBLE=now" >> /etc/profile
EXPOSE 22/tcp
USER aegir
RUN mkdir /var/aegir/config
RUN mkdir /var/aegir/.drush
RUN mkdir /var/aegir/projects
#   You may change this environment at run time. User UID 1 is created with this email address.
ENV AEGIR_CLIENT_EMAIL="aegir@aegir.local.computer"
ENV AEGIR_CLIENT_NAME="admin"
ENV AEGIR_PROFILE="devmaster"
ENV AEGIR_WORKING_COPY="0"
#   Must be fixed across versions so we can upgrade containers.
ENV AEGIR_HOSTMASTER_ROOT="/var/aegir/devmaster"
#   Build the devmaster stack
RUN drush make $AEGIR_MAKEFILE $AEGIR_HOSTMASTER_ROOT
WORKDIR /var/aegir
VOLUME /var/lib/mysql
EXPOSE 80/tcp
EXPOSE 443/tcp
EXPOSE 3306/tcp
#   The Hostname of the database server to use
ENV AEGIR_DATABASE_SERVER="localhost"
ENV HOME="/var/aegir"
VOLUME /var/aegir/projects
#   docker-entrypoint.sh waits for mysql and runs hostmaster install
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["drush", "@hostmaster", "hosting-queued"]
# Please add your HEALTHCHECK here!!!
