FROM ubuntu:16.04
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends apache2=2.4.18-2ubuntu3.17 curl=7.47.0-1ubuntu2.19 php7.0-common=7.0.33-0ubuntu0.16.04.16 php7.0-cli=7.0.33-0ubuntu0.16.04.16 php7.0-dev=7.0.33-0ubuntu0.16.04.16 php7.0-fpm=7.0.33-0ubuntu0.16.04.16 libpcre3-dev=2:8.38-3.1 php7.0-gd=7.0.33-0ubuntu0.16.04.16 php7.0-curl=7.0.33-0ubuntu0.16.04.16 php7.0-imap=7.0.33-0ubuntu0.16.04.16 php7.0-json=7.0.33-0ubuntu0.16.04.16 php7.0-opcache=7.0.33-0ubuntu0.16.04.16 php7.0-xml=7.0.33-0ubuntu0.16.04.16 php7.0-mbstring=7.0.33-0ubuntu0.16.04.16 php7.0-mysql=7.0.33-0ubuntu0.16.04.16 php-sqlite3=1:7.0+35ubuntu6.1 php-apcu=5.1.3+4.0.10-1build1 libapache2-mod-php=1:7.0+35ubuntu6.1 cron=3.0pl1-128ubuntu2 postfix=3.1.0-3ubuntu0.4 sudo=1.8.16-0ubuntu1.10 rsync=3.1.1-3ubuntu1.3 git-core=1:2.7.4-0ubuntu1.10 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 mysql-client=5.7.33-0ubuntu0.16.04.1 -y -qq
#   Use --build-arg option when running docker build to set these variables.
#   If wish to "mount" a volume to your host, set AEGIR_UID and AEGIR_GIT to your local user's UID.
#   There are both ARG and ENV lines to make sure the value persists.
#   See https://docs.docker.com/engine/reference/builder/#/arg
ARG AEGIR_UID=1000
ENV AEGIR_UID="${AEGIR_UID:-1000}"
RUN echo "Creating user aegir with UID $AEGIR_UID and GID $AEGIR_GID"
RUN addgroup --gid $AEGIR_UID aegir
RUN adduser --uid $AEGIR_UID --gid $AEGIR_UID --system --shell /bin/bash --home /var/aegir aegir
RUN adduser aegir www-data
RUN a2enmod rewrite
RUN a2enmod ssl
RUN ln -s /var/aegir/config/apache.conf /etc/apache2/conf-available/aegir.conf
RUN ln -s /etc/apache2/conf-available/aegir.conf /etc/apache2/conf-enabled/aegir.conf
COPY sudoers-aegir /etc/sudoers.d/aegir
RUN chmod 0440 /etc/sudoers.d/aegir
RUN wget https://raw.githubusercontent.com/composer/getcomposer.org/35ca72b506eba32c0baed4d283a5f834968e5ade/web/installer -O - -q | php -- --quiet
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
ENV AEGIR_HTTP_SERVICE_TYPE="apache"
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
