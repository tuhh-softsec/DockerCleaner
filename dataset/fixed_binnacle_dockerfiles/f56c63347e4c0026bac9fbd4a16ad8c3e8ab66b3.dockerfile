#
#  --------------------------------------------------------------------------
#   Image Setup
#  --------------------------------------------------------------------------
#
#   To edit the 'workspace' base Image, visit its repository on Github
#      https://github.com/Laradock/workspace
#
#   To change its version, see the available Tags on the Docker Hub:
#      https://hub.docker.com/r/laradock/workspace/tags/
#
#   Note: Base Image name format {image-tag}-{php-version}
#
ARG LARADOCK_PHP_VERSION
#   FROM laradock/workspace:2.2-${LARADOCK_PHP_VERSION}
FROM letsdockerize/laradock-workspace:2.4-${LARADOCK_PHP_VERSION}
LABEL maintainer="Mahmoud Zalt <mahmoud@zalt.me>"
ARG LARADOCK_PHP_VERSION
#   Set Environment Variables
ENV DEBIAN_FRONTEND="noninteractive"
#   Start as root
USER root
#  ##########################################################################
#   Laradock non-root user:
#  ##########################################################################
#   Add a non-root user to prevent files being created with root permissions on host machine.
ARG PUID=1000
ENV PUID="${PUID}"
ARG PGID=1000
ENV PGID="${PGID}"
#   always run apt update when start and after add new source list, then clean up at end.
RUN set -xe ; apt-get update -yqq \
 && pecl channel-update pecl.php.net \
 && groupadd -g ${PGID} laradock \
 && useradd -u ${PUID} -g laradock -m laradock -G docker_env \
 && usermod -p "*" laradock -s /bin/bash \
 && apt-get install --no-install-recommends apt-utils libzip-dev zip unzip nasm php${LARADOCK_PHP_VERSION}-zip -yqq \
 && php -m | grep -q 'zip'
#
#  --------------------------------------------------------------------------
#   Optional Software's Installation
#  --------------------------------------------------------------------------
#
#   Optional Software's will only be installed if you set them to `true`
#   in the `docker-compose.yml` before the build.
#   Example:
#     - INSTALL_NODE=false
#     - ...
#
#  ##########################################################################
#   Set Timezone
#  ##########################################################################
ARG TZ=UTC
ENV TZ="${TZ}"
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone
#  ##########################################################################
#   User Aliases
#  ##########################################################################
USER root
COPY ./aliases.sh /root/aliases.sh
COPY ./aliases.sh /home/laradock/aliases.sh
RUN sed -i 's/\r//' /root/aliases.sh \
 && sed -i 's/\r//' /home/laradock/aliases.sh \
 && chown laradock:laradock /home/laradock/aliases.sh \
 && echo "" >> ~/.bashrc \
 && echo "# Load Custom Aliases" >> ~/.bashrc \
 && echo "source ~/aliases.sh" >> ~/.bashrc \
 && echo "" >> ~/.bashrc
USER laradock
RUN echo "" >> ~/.bashrc \
 && echo "# Load Custom Aliases" >> ~/.bashrc \
 && echo "source ~/aliases.sh" >> ~/.bashrc \
 && echo "" >> ~/.bashrc
#  ##########################################################################
#   Composer:
#  ##########################################################################
USER root
#   Add the composer.json
COPY ./composer.json /home/laradock/.composer/composer.json
#   Add the auth.json for magento 2 credentials
COPY ./auth.json /home/laradock/.composer/auth.json
#   Make sure that ~/.composer belongs to laradock
RUN chown -R laradock:laradock /home/laradock/.composer
USER laradock
#   Check if global install need to be ran
ARG COMPOSER_GLOBAL_INSTALL=false
ENV COMPOSER_GLOBAL_INSTALL="${COMPOSER_GLOBAL_INSTALL}"
RUN if [ ${COMPOSER_GLOBAL_INSTALL} = true ] ; then composer global install ; fi
#   Check if auth file is disabled
ARG COMPOSER_AUTH=false
ENV COMPOSER_AUTH="${COMPOSER_AUTH}"
RUN if [ ${COMPOSER_AUTH} = false ] ; then rm /home/laradock/.composer/auth.json ; fi
ARG COMPOSER_REPO_PACKAGIST
ENV COMPOSER_REPO_PACKAGIST="${COMPOSER_REPO_PACKAGIST}"
RUN if [ ${COMPOSER_REPO_PACKAGIST} ] ; then composer config -g repo.packagist composer ${COMPOSER_REPO_PACKAGIST} ; fi
#   Export composer vendor path
RUN echo "" >> ~/.bashrc \
 && echo 'export PATH="~/.composer/vendor/bin:$PATH"' >> ~/.bashrc
#  ##########################################################################
#   Non-root user : PHPUnit path
#  ##########################################################################
#   add ./vendor/bin to non-root user's bashrc (needed for phpunit)
USER laradock
RUN echo "" >> ~/.bashrc \
 && echo 'export PATH="/var/www/vendor/bin:$PATH"' >> ~/.bashrc
#  ##########################################################################
#   Crontab
#  ##########################################################################
USER root
COPY ./crontab /etc/cron.d
RUN chmod -R 644 /etc/cron.d
#  ##########################################################################
#   Drush:
#  ##########################################################################
#   Deprecated install of Drush 8 and earlier versions.
#   Drush 9 and up require Drush to be listed as a composer dependency of your site.
USER root
ARG INSTALL_DRUSH=false
ARG DRUSH_VERSION
ENV DRUSH_VERSION="${DRUSH_VERSION}"
RUN if [ ${INSTALL_DRUSH} = true ] ; then apt-get install --no-install-recommends mysql-client -y \
 && curl -fsSL -o /usr/local/bin/drush https://github.com/drush-ops/drush/releases/download/${DRUSH_VERSION}/drush.phar | bash \
 && chmod +x /usr/local/bin/drush \
 && drush core-status ; fi
#  ##########################################################################
#   WP CLI:
#  ##########################################################################
#   The command line interface for WordPress
USER root
ARG INSTALL_WP_CLI=false
RUN if [ ${INSTALL_WP_CLI} = true ] ; then curl -fsSL -o /usr/local/bin/wp https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli.phar | bash \
 && chmod +x /usr/local/bin/wp ; fi
#  ##########################################################################
#   SSH2:
#  ##########################################################################
USER root
ARG INSTALL_SSH2=false
RUN if [ ${INSTALL_SSH2} = true ] ; then apt-get install --no-install-recommends libssh2-1-dev php${LARADOCK_PHP_VERSION}-ssh2 -y ; fi
#  ##########################################################################
#   GMP:
#  ##########################################################################
USER root
ARG INSTALL_GMP=false
ARG PHP_VERSION=${LARADOCK_PHP_VERSION}
RUN if [ ${INSTALL_GMP} = true ] ; then apt-get install --no-install-recommends php${LARADOCK_PHP_VERSION}-gmp -y ; fi
#  ##########################################################################
#   SOAP:
#  ##########################################################################
USER root
ARG INSTALL_SOAP=false
RUN if [ ${INSTALL_SOAP} = true ] ; then apt-get install --no-install-recommends libxml2-dev php${LARADOCK_PHP_VERSION}-soap -y ; fi
#  ##########################################################################
#   XSL:
#  ##########################################################################
USER root
ARG INSTALL_XSL=false
RUN if [ ${INSTALL_XSL} = true ] ; then apt-get install --no-install-recommends libxslt-dev php${LARADOCK_PHP_VERSION}-xsl -y ; fi
#  ##########################################################################
#   LDAP:
#  ##########################################################################
ARG INSTALL_LDAP=false
RUN if [ ${INSTALL_LDAP} = true ] ; then apt-get install --no-install-recommends libldap2-dev -y \
 && apt-get install --no-install-recommends php${LARADOCK_PHP_VERSION}-ldap -y ; fi
#  ##########################################################################
#   IMAP:
#  ##########################################################################
ARG INSTALL_IMAP=false
RUN if [ ${INSTALL_IMAP} = true ] ; then apt-get install --no-install-recommends php${LARADOCK_PHP_VERSION}-imap -y ; fi
#  ##########################################################################
#   Subversion:
#  ##########################################################################
USER root
ARG INSTALL_SUBVERSION=false
RUN if [ ${INSTALL_SUBVERSION} = true ] ; then apt-get install --no-install-recommends subversion -y ; fi
#  ##########################################################################
#   xDebug:
#  ##########################################################################
USER root
ARG INSTALL_XDEBUG=false
RUN if [ ${INSTALL_XDEBUG} = true ] ; then apt-get update --fix-missing \
 && apt-get install --no-install-recommends php${LARADOCK_PHP_VERSION}-xdebug -y \
 && sed -i 's/^;//g' /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/20-xdebug.ini \
 && echo "alias phpunit='php -dzend_extension=xdebug.so /var/www/vendor/bin/phpunit'" >> ~/.bashrc; fi
#   ADD for REMOTE debugging
COPY ./xdebug.ini /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/xdebug.ini
RUN sed -i "s/xdebug.remote_autostart=0/xdebug.remote_autostart=1/" /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/xdebug.ini \
 && sed -i "s/xdebug.remote_enable=0/xdebug.remote_enable=1/" /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/xdebug.ini \
 && sed -i "s/xdebug.cli_color=0/xdebug.cli_color=1/" /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/xdebug.ini
#  ##########################################################################
#   Phpdbg:
#  ##########################################################################
USER root
ARG INSTALL_PHPDBG=false
RUN if [ ${INSTALL_PHPDBG} = true ] ; then apt-get install --no-install-recommends php${LARADOCK_PHP_VERSION}-phpdbg -y --force-yes ; fi
#  ##########################################################################
#   Blackfire:
#  ##########################################################################
ARG INSTALL_BLACKFIRE=false
ARG BLACKFIRE_CLIENT_ID
ENV BLACKFIRE_CLIENT_ID="${BLACKFIRE_CLIENT_ID}"
ARG BLACKFIRE_CLIENT_TOKEN
ENV BLACKFIRE_CLIENT_TOKEN="${BLACKFIRE_CLIENT_TOKEN}"
RUN if [ ${INSTALL_XDEBUG} = false -a ${INSTALL_BLACKFIRE} = true ] ; then curl -L https://packages.blackfire.io/gpg.key | apt-key add - \
 && echo "deb http://packages.blackfire.io/debian any main" | tee /etc/apt/sources.list.d/blackfire.list \
 && apt-get update -yqq \
 && apt-get install --no-install-recommends blackfire-agent ; fi
#  ##########################################################################
#   ssh:
#  ##########################################################################
ARG INSTALL_WORKSPACE_SSH=false
COPY insecure_id_rsa /tmp/id_rsa
COPY insecure_id_rsa.pub /tmp/id_rsa.pub
RUN if [ ${INSTALL_WORKSPACE_SSH} = true ] ; then rm -f /etc/service/sshd/down \
 && cat /tmp/id_rsa.pub >> /root/.ssh/authorized_keys \
 && cat /tmp/id_rsa.pub >> /root/.ssh/id_rsa.pub \
 && cat /tmp/id_rsa >> /root/.ssh/id_rsa \
 && rm -f /tmp/id_rsa* \
 && chmod 644 /root/.ssh/authorized_keys /root/.ssh/id_rsa.pub \
 && chmod 400 /root/.ssh/id_rsa \
 && cp -rf /root/.ssh /home/laradock \
 && chown -R laradock:laradock /home/laradock/.ssh ; fi
#  ##########################################################################
#   MongoDB:
#  ##########################################################################
ARG INSTALL_MONGO=false
RUN if [ ${INSTALL_MONGO} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install mongo \
 && echo "extension=mongo.so" >> /etc/php/${LARADOCK_PHP_VERSION}/mods-available/mongo.ini \
 && ln -s /etc/php/${LARADOCK_PHP_VERSION}/mods-available/mongo.ini /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/30-mongo.ini ; fi \
 && pecl install mongodb \
 && echo "extension=mongodb.so" >> /etc/php/${LARADOCK_PHP_VERSION}/mods-available/mongodb.ini \
 && ln -s /etc/php/${LARADOCK_PHP_VERSION}/mods-available/mongodb.ini /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/30-mongodb.ini ; fi
#  ##########################################################################
#   AMQP:
#  ##########################################################################
ARG INSTALL_AMQP=false
RUN if [ ${INSTALL_AMQP} = true ] ; then apt-get install --no-install-recommends librabbitmq-dev -y \
 && pecl -q install amqp \
 && echo "extension=amqp.so" >> /etc/php/${LARADOCK_PHP_VERSION}/mods-available/amqp.ini \
 && ln -s /etc/php/${LARADOCK_PHP_VERSION}/mods-available/amqp.ini /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/30-amqp.ini ; fi
#  ##########################################################################
#   PHP REDIS EXTENSION
#  ##########################################################################
ARG INSTALL_PHPREDIS=false
RUN if [ ${INSTALL_PHPREDIS} = true ] ; then apt-get install --no-install-recommends php-redis -yqq ; fi
#  ##########################################################################
#   Swoole EXTENSION
#  ##########################################################################
ARG INSTALL_SWOOLE=false
RUN if [ ${INSTALL_SWOOLE} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl -q install swoole-2.0.10 ; else if [ $( php -r "echo PHP_MINOR_VERSION;" ;) = "0" ] ; then pecl install swoole-2.2.0 ; else pecl install swoole ; fi ; fi \
 && echo "extension=swoole.so" >> /etc/php/${LARADOCK_PHP_VERSION}/mods-available/swoole.ini \
 && ln -s /etc/php/${LARADOCK_PHP_VERSION}/mods-available/swoole.ini /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/20-swoole.ini \
 && php -m | grep -q 'swoole' ; fi
#  ##########################################################################
#   Taint EXTENSION
#  ##########################################################################
ARG INSTALL_TAINT=false
RUN if [ "${INSTALL_TAINT}" = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "7" ] ; then pecl install taint \
 && echo "extension=taint.so" >> /etc/php/${LARADOCK_PHP_VERSION}/mods-available/taint.ini \
 && ln -s /etc/php/${LARADOCK_PHP_VERSION}/mods-available/taint.ini /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/20-taint.ini \
 && php -m | grep -q 'taint' ; fi ; fi
#  ##########################################################################
#   Libpng16 EXTENSION
#  ##########################################################################
ARG INSTALL_LIBPNG=false
RUN if [ ${INSTALL_LIBPNG} = true ] ; then apt-get update \
 && apt-get install --no-install-recommends libpng16-16 ; fi
#  ##########################################################################
#   Inotify EXTENSION:
#  ##########################################################################
ARG INSTALL_INOTIFY=false
RUN if [ ${INSTALL_INOTIFY} = true ] ; then pecl -q install inotify \
 && echo "extension=inotify.so" >> /etc/php/${LARADOCK_PHP_VERSION}/mods-available/inotify.ini \
 && ln -s /etc/php/${LARADOCK_PHP_VERSION}/mods-available/inotify.ini /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/20-inotify.ini ; fi
#  ##########################################################################
#   fswatch
#  ##########################################################################
ARG INSTALL_FSWATCH=false
RUN if [ ${INSTALL_FSWATCH} = true ] ; then apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 47FE03C1 \
 && add-apt-repository -y ppa:hadret/fswatch || apt-get update -yqq \
 && apt-get install --no-install-recommends fswatch -y ; fi
#  ##########################################################################
#   IonCube Loader
#  ##########################################################################
ARG INSTALL_IONCUBE=false
RUN if [ ${INSTALL_IONCUBE} = true ] ; then curl -L -o /tmp/ioncube_loaders_lin_x86-64.tar.gz https://downloads.ioncube.com/loader_downloads/ioncube_loaders_lin_x86-64.tar.gz \
 && tar zxpf /tmp/ioncube_loaders_lin_x86-64.tar.gz -C /tmp \
 && mv /tmp/ioncube/ioncube_loader_lin_${LARADOCK_PHP_VERSION}.so $( php -r "echo ini_get('extension_dir');" ;)/ioncube_loader.so \
 && echo "zend_extension=ioncube_loader.so" >> /etc/php/${LARADOCK_PHP_VERSION}/mods-available/0ioncube.ini \
 && rm -rf /tmp/ioncube* ; fi
#  ##########################################################################
#   Drupal Console:
#  ##########################################################################
USER root
ARG INSTALL_DRUPAL_CONSOLE=false
RUN if [ ${INSTALL_DRUPAL_CONSOLE} = true ] ; then apt-get install --no-install-recommends mysql-client -y \
 && curl https://drupalconsole.com/installer -L -o drupal.phar \
 && mv drupal.phar /usr/local/bin/drupal \
 && chmod +x /usr/local/bin/drupal ; fi
USER laradock
#  ##########################################################################
#   Node / NVM:
#  ##########################################################################
#   Check if NVM needs to be installed
ARG NODE_VERSION=node
ENV NODE_VERSION="${NODE_VERSION}"
ARG INSTALL_NODE=false
ARG INSTALL_NPM_GULP=false
ARG INSTALL_NPM_BOWER=false
ARG INSTALL_NPM_VUE_CLI=false
ARG INSTALL_NPM_ANGULAR_CLI=false
ARG NPM_REGISTRY
ENV NPM_REGISTRY="${NPM_REGISTRY}"
ENV NVM_DIR="/home/laradock/.nvm"
RUN if [ ${INSTALL_NODE} = true ] ; then mkdir -p $NVM_DIR \
 && curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash \
 && . $NVM_DIR/nvm.sh \
 && nvm install ${NODE_VERSION} \
 && nvm use ${NODE_VERSION} \
 && nvm alias ${NODE_VERSION} \
 && if [ ${NPM_REGISTRY} ] ; then npm config set registry ${NPM_REGISTRY} ; fi \
 && if [ ${INSTALL_NPM_GULP} = true ] ; then npm install gulp@4.0.2 -g ; fi \
 && if [ ${INSTALL_NPM_BOWER} = true ] ; then npm install bower@1.8.14 -g ; fi \
 && if [ ${INSTALL_NPM_VUE_CLI} = true ] ; then npm install @vue/cli -g ; fi \
 && if [ ${INSTALL_NPM_ANGULAR_CLI} = true ] ; then npm install @angular/cli -g ; fi \
 && ln -s `npm bin --global ` /home/laradock/.node-bin ; fi
#   Wouldn't execute when added to the RUN statement in the above block
#   Source NVM when loading bash since ~/.profile isn't loaded on non-login shell
RUN if [ ${INSTALL_NODE} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export NVM_DIR="$HOME/.nvm"' >> ~/.bashrc \
 && echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.bashrc; fi
#   Add NVM binaries to root's .bashrc
USER root
RUN if [ ${INSTALL_NODE} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export NVM_DIR="/home/laradock/.nvm"' >> ~/.bashrc \
 && echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.bashrc; fi
#   Add PATH for node
ENV PATH="$PATH:/home/laradock/.node-bin"
#   Make it so the node modules can be executed with 'docker-compose exec'
#   We'll create symbolic links into '/usr/local/bin'.
RUN
RUN if [ ${NPM_REGISTRY} ] ; then . ~/.bashrc \
 && npm config set registry ${NPM_REGISTRY} ; fi
#  ##########################################################################
#   YARN:
#  ##########################################################################
USER laradock
ARG INSTALL_YARN=false
ARG YARN_VERSION=latest
ENV YARN_VERSION="${YARN_VERSION}"
RUN if [ ${INSTALL_YARN} = true ] ; then [ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" \
 && if [ ${YARN_VERSION} = "latest" ] ; then curl -o- -L https://yarnpkg.com/install.sh | bash ; else curl -o- -L https://yarnpkg.com/install.sh | bash -s -- --version ${YARN_VERSION} ; fi \
 && echo "" >> ~/.bashrc \
 && echo 'export PATH="$HOME/.yarn/bin:$PATH"' >> ~/.bashrc; fi
#   Add YARN binaries to root's .bashrc
USER root
RUN if [ ${INSTALL_YARN} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export YARN_DIR="/home/laradock/.yarn"' >> ~/.bashrc \
 && echo 'export PATH="$YARN_DIR/bin:$PATH"' >> ~/.bashrc; fi
#   Add PATH for YARN
ENV PATH="$PATH:/home/laradock/.yarn/bin"
#  ##########################################################################
#   PHP Aerospike:
#  ##########################################################################
USER root
ARG INSTALL_AEROSPIKE=false
RUN set -xe ; if [ ${INSTALL_AEROSPIKE} = true ] ; then apt-get install --no-install-recommends sudo wget -y \
 && if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then curl -L -o /tmp/aerospike-client-php.tar.gz https://github.com/aerospike/aerospike-client-php5/archive/master.tar.gz ; else curl -L -o /tmp/aerospike-client-php.tar.gz https://github.com/aerospike/aerospike-client-php/archive/master.tar.gz ; fi \
 && mkdir -p /tmp/aerospike-client-php \
 && tar -C /tmp/aerospike-client-php -zxvf /tmp/aerospike-client-php.tar.gz --strip 1 \
 && if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then (cd /tmp/aerospike-client-php/src/aerospike \
 && phpize \
 && ./build.sh \
 && make install ) ; else (cd /tmp/aerospike-client-php/src \
 && phpize \
 && ./build.sh \
 && make install ) ; fi \
 && rm /tmp/aerospike-client-php.tar.gz \
 && echo 'extension=aerospike.so' >> /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/aerospike.ini \
 && echo 'aerospike.udf.lua_system_path=/usr/local/aerospike/lua' >> /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/aerospike.ini \
 && echo 'aerospike.udf.lua_user_path=/usr/local/aerospike/usr-lua' >> /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/aerospike.ini; fi
#  ##########################################################################
#   PHP V8JS:
#  ##########################################################################
USER root
ARG INSTALL_V8JS=false
RUN set -xe ; if [ ${INSTALL_V8JS} = true ] ; then add-apt-repository -y ppa:pinepain/libv8-archived \
 && apt-get update -yqq \
 && apt-get install --no-install-recommends libv8-5.4 -y \
 && if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install v8js-0.6.4 ; else pecl install v8js ; fi \
 && echo "extension=v8js.so" >> /etc/php/${LARADOCK_PHP_VERSION}/cli/php.ini \
 && php -m | grep -q 'v8js' ; fi
#  ##########################################################################
#   Laravel Envoy:
#  ##########################################################################
USER laradock
ARG INSTALL_LARAVEL_ENVOY=false
RUN if [ ${INSTALL_LARAVEL_ENVOY} = true ] ; then composer global require "laravel/envoy=~1.0" ; fi
#  ##########################################################################
#   Laravel Installer:
#  ##########################################################################
USER laradock
ARG COMPOSER_REPO_PACKAGIST
ENV COMPOSER_REPO_PACKAGIST="${COMPOSER_REPO_PACKAGIST}"
RUN if [ ${COMPOSER_REPO_PACKAGIST} ] ; then composer config -g repo.packagist composer ${COMPOSER_REPO_PACKAGIST} ; fi
ARG INSTALL_LARAVEL_INSTALLER=false
RUN if [ ${INSTALL_LARAVEL_INSTALLER} = true ] ; then composer global require "laravel/installer" ; fi
#  ##########################################################################
#   Deployer:
#  ##########################################################################
USER root
ARG INSTALL_DEPLOYER=false
RUN if [ ${INSTALL_DEPLOYER} = true ] ; then curl -LO https://deployer.org/deployer.phar \
 && mv deployer.phar /usr/local/bin/dep \
 && chmod +x /usr/local/bin/dep ; fi
#  ##########################################################################
#   Prestissimo:
#  ##########################################################################
USER laradock
ARG INSTALL_PRESTISSIMO=false
RUN if [ ${INSTALL_PRESTISSIMO} = true ] ; then composer global require "hirak/prestissimo" ; fi
#  ##########################################################################
#   Linuxbrew:
#  ##########################################################################
USER root
ARG INSTALL_LINUXBREW=false
RUN if [ ${INSTALL_LINUXBREW} = true ] ; then apt-get upgrade -y \
 && apt-get install --no-install-recommends build-essential make cmake scons curl git ruby autoconf automake autoconf-archive gettext libtool flex bison libbz2-dev libcurl4-openssl-dev libexpat-dev libncurses-dev -y \
 && git clone --depth=1 https://github.com/Homebrew/linuxbrew.git ~/.linuxbrew \
 && echo "" >> ~/.bashrc \
 && echo 'export PKG_CONFIG_PATH"=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig:/usr/lib64/pkgconfig:/usr/lib/pkgconfig:/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/lib64/pkgconfig:/usr/share/pkgconfig:$PKG_CONFIG_PATH"' >> ~/.bashrc \
 && echo 'export LINUXBREWHOME="$HOME/.linuxbrew"' >> ~/.bashrc \
 && echo 'export PATH="$LINUXBREWHOME/bin:$PATH"' >> ~/.bashrc \
 && echo 'export MANPATH="$LINUXBREWHOME/man:$MANPATH"' >> ~/.bashrc \
 && echo 'export PKG_CONFIG_PATH="$LINUXBREWHOME/lib64/pkgconfig:$LINUXBREWHOME/lib/pkgconfig:$PKG_CONFIG_PATH"' >> ~/.bashrc \
 && echo 'export LD_LIBRARY_PATH="$LINUXBREWHOME/lib64:$LINUXBREWHOME/lib:$LD_LIBRARY_PATH"' >> ~/.bashrc; fi
#  ##########################################################################
#   SQL SERVER:
#  ##########################################################################
ARG INSTALL_MSSQL=false
RUN set -eux ; if [ ${INSTALL_MSSQL} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then apt-get install --no-install-recommends php5.6-sybase freetds-bin freetds-common libsybdb5 -y \
 && php -m | grep -q 'mssql' \
 && php -m | grep -q 'pdo_dblib' ; else curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
 && curl https://packages.microsoft.com/config/ubuntu/16.04/prod.list > /etc/apt/sources.list.d/mssql-release.list \
 && apt-get update -yqq \
 && ACCEPT_EULA=Y apt-get install -y msodbcsql17 mssql-tools unixodbc unixodbc-dev libgss3 odbcinst locales \
 && ln -sfn /opt/mssql-tools/bin/sqlcmd /usr/bin/sqlcmd \
 && ln -sfn /opt/mssql-tools/bin/bcp /usr/bin/bcp \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && if [ $( php -r "echo PHP_MINOR_VERSION;" ;) = "0" ] ; then pecl install sqlsrv-5.3.0 pdo_sqlsrv-5.3.0 ; else pecl install sqlsrv pdo_sqlsrv ; fi \
 && echo "extension=sqlsrv.so" > /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/20-sqlsrv.ini \
 && echo "extension=pdo_sqlsrv.so" > /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/20-pdo_sqlsrv.ini \
 && php -m | grep -q 'sqlsrv' \
 && php -m | grep -q 'pdo_sqlsrv' ; fi ; fi
#  ##########################################################################
#   Minio:
#  ##########################################################################
USER root
COPY mc/config.json /root/.mc/config.json
ARG INSTALL_MC=false
RUN if [ ${INSTALL_MC} = true ] ; then curl -fsSL -o /usr/local/bin/mc https://dl.minio.io/client/mc/release/linux-amd64/mc \
 && chmod +x /usr/local/bin/mc ; fi
#  ##########################################################################
#   Image optimizers:
#  ##########################################################################
USER root
ARG INSTALL_IMAGE_OPTIMIZERS=false
RUN if [ ${INSTALL_IMAGE_OPTIMIZERS} = true ] ; then apt-get install --no-install-recommends jpegoptim optipng pngquant gifsicle -y \
 && if [ ${INSTALL_NODE} = true ] ; then exec bash \
 && . ~/.bashrc \
 && npm install svgo@3.0.2 -g ; fi ; fi
USER laradock
#  ##########################################################################
#   Symfony:
#  ##########################################################################
USER root
ARG INSTALL_SYMFONY=false
RUN if [ ${INSTALL_SYMFONY} = true ] ; then mkdir -p /usr/local/bin \
 && curl -LsS https://symfony.com/installer -o /usr/local/bin/symfony \
 && chmod a+x /usr/local/bin/symfony \
 && echo 'alias dev="php bin/console -e=dev"' >> ~/.bashrc \
 && echo 'alias prod="php bin/console -e=prod"' >> ~/.bashrc; fi
#  ##########################################################################
#   PYTHON:
#  ##########################################################################
ARG INSTALL_PYTHON=false
RUN if [ ${INSTALL_PYTHON} = true ] ; then apt-get install --no-install-recommends python python-pip python-dev build-essential -y \
 && python -m pip install --upgrade pip \
 && python -m pip install --upgrade virtualenv ; fi
#  ##########################################################################
#   POWERLINE:
#  ##########################################################################
USER root
ARG INSTALL_POWERLINE=false
RUN if [ ${INSTALL_POWERLINE} = true ] ; then if [ ${INSTALL_PYTHON} = true ] ; then python -m pip install --upgrade powerline-status \
 && echo "" >> /etc/bash.bashrc \
 && echo ". /usr/local/lib/python2.7/dist-packages/powerline/bindings/bash/powerline.sh" >> /etc/bash.bashrc; fi ; fi
USER laradock
#  ##########################################################################
#   ImageMagick:
#  ##########################################################################
USER root
ARG INSTALL_IMAGEMAGICK=false
RUN if [ ${INSTALL_IMAGEMAGICK} = true ] ; then apt-get install --no-install-recommends imagemagick php-imagick -y ; fi
#  ##########################################################################
#   Terraform:
#  ##########################################################################
USER root
ARG INSTALL_TERRAFORM=false
RUN if [ ${INSTALL_TERRAFORM} = true ] ; then apt-get install --no-install-recommends sudo wget unzip -y \
 && wget https://releases.hashicorp.com/terraform/0.10.6/terraform_0.10.6_linux_amd64.zip \
 && unzip terraform_0.10.6_linux_amd64.zip \
 && mv terraform /usr/local/bin \
 && rm terraform_0.10.6_linux_amd64.zip ; fi
#  ##########################################################################
#   pgsql client
#  ##########################################################################
USER root
ARG INSTALL_PG_CLIENT=false
RUN if [ ${INSTALL_PG_CLIENT} = true ] ; then apt-get install --no-install-recommends wget \
 && add-apt-repository "deb http://apt.postgresql.org/pub/repos/apt/ xenial-pgdg main" \
 && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
 && apt-get update \
 && apt-get install --no-install-recommends postgresql-client-10 -y ; fi
#  ##########################################################################
#   Dusk Dependencies:
#  ##########################################################################
USER root
ARG CHROME_DRIVER_VERSION=stable
ENV CHROME_DRIVER_VERSION="${CHROME_DRIVER_VERSION}"
ARG INSTALL_DUSK_DEPS=false
RUN if [ ${INSTALL_DUSK_DEPS} = true ] ; then apt-get install --no-install-recommends zip wget unzip xdg-utils libxpm4 libxrender1 libgtk2.0-0 libnss3 libgconf-2-4 xvfb gtk2-engines-pixbuf xfonts-cyrillic xfonts-100dpi xfonts-75dpi xfonts-base xfonts-scalable x11-apps -y \
 && wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
 && dpkg -i --force-depends google-chrome-stable_current_amd64.deb \
 && apt-get install --no-install-recommends -y -f \
 && dpkg -i --force-depends google-chrome-stable_current_amd64.deb \
 && rm google-chrome-stable_current_amd64.deb \
 && wget https://chromedriver.storage.googleapis.com/${CHROME_DRIVER_VERSION}/chromedriver_linux64.zip \
 && unzip chromedriver_linux64.zip \
 && mv chromedriver /usr/local/bin/ \
 && rm chromedriver_linux64.zip ; fi
#  ##########################################################################
#   Phalcon:
#  ##########################################################################
ARG INSTALL_PHALCON=false
ARG LARADOCK_PHALCON_VERSION
ENV LARADOCK_PHALCON_VERSION="${LARADOCK_PHALCON_VERSION}"
RUN if [ $INSTALL_PHALCON = true ] ; then apt-get update \
 && apt-get install --no-install-recommends unzip libpcre3-dev gcc make re2c -y \
 && curl -L -o /tmp/cphalcon.zip https://github.com/phalcon/cphalcon/archive/v${LARADOCK_PHALCON_VERSION}.zip \
 && unzip -d /tmp/ /tmp/cphalcon.zip \
 && cd /tmp/cphalcon-${LARADOCK_PHALCON_VERSION}/build \
 && ./install \
 && echo "extension=phalcon.so" >> /etc/php/${LARADOCK_PHP_VERSION}/mods-available/phalcon.ini \
 && ln -s /etc/php/${LARADOCK_PHP_VERSION}/mods-available/phalcon.ini /etc/php/${LARADOCK_PHP_VERSION}/cli/conf.d/30-phalcon.ini \
 && rm -rf /tmp/cphalcon* ; fi
#  ##########################################################################
#   MySQL Client:
#  ##########################################################################
USER root
ARG INSTALL_MYSQL_CLIENT=false
RUN if [ ${INSTALL_MYSQL_CLIENT} = true ] ; then apt-get update -yqq \
 && apt-get install --no-install-recommends mysql-client -y ; fi
#  ##########################################################################
#   ping:
#  ##########################################################################
USER root
ARG INSTALL_PING=false
RUN if [ ${INSTALL_PING} = true ] ; then apt-get update -yqq \
 && apt-get install --no-install-recommends inetutils-ping -y ; fi
#  ##########################################################################
#   sshpass:
#  ##########################################################################
USER root
ARG INSTALL_SSHPASS=false
RUN if [ ${INSTALL_SSHPASS} = true ] ; then apt-get update -yqq \
 && apt-get install --no-install-recommends sshpass -y ; fi
#  ##########################################################################
#   FFMpeg:
#  ##########################################################################
USER root
ARG INSTALL_FFMPEG=false
RUN if [ ${INSTALL_FFMPEG} = true ] ; then apt-get install --no-install-recommends ffmpeg -y ; fi
#  ##########################################################################
#   GNU Parallel:
#  ##########################################################################
USER root
ARG INSTALL_GNU_PARALLEL=false
RUN if [ ${INSTALL_GNU_PARALLEL} = true ] ; then apt-get install --no-install-recommends parallel -y ; fi
#  ##########################################################################
#   Check PHP version:
#  ##########################################################################
RUN set -xe ; php -v | head -n 1 | grep -q "PHP ${LARADOCK_PHP_VERSION}."
#
#  --------------------------------------------------------------------------
#   Final Touch
#  --------------------------------------------------------------------------
#
USER root
#   Clean up
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && rm /var/log/lastlog /var/log/faillog
#   Set default work directory
WORKDIR /var/www
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
