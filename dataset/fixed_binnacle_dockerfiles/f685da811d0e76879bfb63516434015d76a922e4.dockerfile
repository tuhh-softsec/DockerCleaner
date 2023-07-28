FROM ubuntu:16.04
RUN apt-get update -qq \
 && apt-get install --no-install-recommends apache2=2.4.18-2ubuntu3.17 openssl=1.0.2g-1ubuntu4.20 php5 php5-cli php5-gd php5-mysql php-pear=1:1.10.1+submodules+notgz-6ubuntu0.3 php5-curl postfix=3.1.0-3ubuntu0.4 sudo=1.8.16-0ubuntu1.10 rsync=3.1.1-3ubuntu1.3 git-core=1:2.7.4-0ubuntu1.10 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 mysql-client=5.7.33-0ubuntu0.16.04.1 -y -qq
ENV AEGIR_UID="1000"
RUN echo "Creating user aegir with UID $AEGIR_UID and GID $AEGIR_GID"
RUN addgroup --gid $AEGIR_UID aegir
RUN adduser --uid $AEGIR_UID --gid $AEGIR_UID --system --home /var/aegir aegir
RUN adduser aegir www-data
RUN a2enmod rewrite
RUN a2enmod ssl
RUN ln -s /var/aegir/config/apache.conf /etc/apache2/conf-available/aegir.conf
RUN ln -s /etc/apache2/conf-available/aegir.conf /etc/apache2/conf-enabled/aegir.conf
COPY sudoers-aegir /etc/sudoers.d/aegir
RUN chmod 0440 /etc/sudoers.d/aegir
RUN wget https://raw.githubusercontent.com/composer/getcomposer.org/1b137f8bf6db3e79a38a5bc45324414a6b1f9df2/web/installer -O - -q | php -- --quiet
RUN cp composer.phar /usr/local/bin/composer
RUN wget https://github.com/drush-ops/drush/releases/download/8.1.16/drush.phar -O - -q > /usr/local/bin/drush
RUN chmod +x /usr/local/bin/composer
RUN chmod +x /usr/local/bin/drush
#   Install fix-permissions and fix-ownership scripts
RUN wget http://cgit.drupalcode.org/hosting_tasks_extra/plain/fix_permissions/scripts/standalone-install-fix-permissions-ownership.sh
RUN bash standalone-install-fix-permissions-ownership.sh
COPY docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh
COPY run-tests.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/run-tests.sh
#  COPY docker-entrypoint-tests.sh /usr/local/bin/
#  RUN chmod +x /usr/local/bin/docker-entrypoint-tests.sh
COPY docker-entrypoint-queue.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint-queue.sh
#   Prepare Aegir Logs folder.
RUN mkdir /var/log/aegir
RUN chown aegir:aegir /var/log/aegir
RUN echo 'Hello, Aegir.' > /var/log/aegir/system.log
#   Don't install provision. Downstream tags will do this with the right version.
#  # Install Provision for all.
#  ENV PROVISION_VERSION 7.x-3.x
#  RUN mkdir -p /usr/share/drush/commands
#  RUN drush dl --destination=/usr/share/drush/commands provision-$PROVISION_VERSION -y
ENV REGISTRY_REBUILD_VERSION="7.x-2.5"
RUN drush dl --destination=/usr/share/drush/commands registry_rebuild-$REGISTRY_REBUILD_VERSION -y
USER aegir
RUN mkdir /var/aegir/config
RUN mkdir /var/aegir/.drush
#   You may change this environment at run time. User UID 1 is created with this email address.
ENV AEGIR_CLIENT_EMAIL="aegir@aegir.local.computer"
ENV AEGIR_CLIENT_NAME="admin"
ENV AEGIR_PROFILE="hostmaster"
ENV AEGIR_VERSION="7.x-3.x"
ENV PROVISION_VERSION="7.x-3.x"
ENV AEGIR_WORKING_COPY="0"
#   Must be fixed across versions so we can upgrade containers.
ENV AEGIR_HOSTMASTER_ROOT="/var/aegir/hostmaster"
WORKDIR /var/aegir
#   The Hostname of the database server to use
ENV AEGIR_DATABASE_SERVER="database"
#   For dev images (7.x-3.x branch)
ENV AEGIR_MAKEFILE="http://cgit.drupalcode.org/provision/plain/aegir.make"
#   For Releases:
#   ENV AEGIR_MAKEFILE http://cgit.drupalcode.org/provision/plain/aegir-release.make?h=$AEGIR_VERSION
VOLUME /var/aegir
#   docker-entrypoint.sh waits for mysql and runs hostmaster install
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["drush", "@hostmaster", "hosting-queued"]
# Please add your HEALTHCHECK here!!!
