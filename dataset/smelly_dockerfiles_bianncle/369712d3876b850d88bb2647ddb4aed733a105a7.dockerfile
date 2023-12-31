#
# --------------------------------------------------------------------------
#  Image Setup
# --------------------------------------------------------------------------
#
#  To edit the 'workspace' base Image, visit its repository on Github
#     https://github.com/Laradock/workspace
#
#  To change its version, see the available Tags on the Docker Hub:
#     https://hub.docker.com/r/laradock/workspace/tags/
#
#  Note: Base Image name format {image-tag}-{php-version}
#
FROM laradock/workspace:1.8-56
MAINTAINER Mahmoud Zalt <mahmoud@zalt.me>
#
# --------------------------------------------------------------------------
#  Mandatory Software's Installation
# --------------------------------------------------------------------------
#
#  Mandatory Software's such as ("php5.6-cli", "git", "vim", ....) are
#  installed on the base image 'laradock/workspace' image. If you want
#  to add more Software's or remove existing one, you need to edit the
#  base image (https://github.com/Laradock/workspace).
#
#
# --------------------------------------------------------------------------
#  Optional Software's Installation
# --------------------------------------------------------------------------
#
#  Optional Software's will only be installed if you set them to `true`
#  in the `docker-compose.yml` before the build.
#  Example:
#    - INSTALL_NODE=false
#    - ...
#
# ####################################
#  Non-Root User:
# ####################################
#  Add a non-root user to prevent files being created with root permissions on host machine.
ARG PUID=1000
ARG PGID=1000
ENV PUID="${PUID}"
ENV PGID="${PGID}"
RUN groupadd -g ${PGID} laradock \
 && useradd -u ${PUID} -g laradock -m laradock \
 && apt-get update -yqq
# ####################################
#  SOAP:
# ####################################
USER root
ARG INSTALL_SOAP=false
ENV INSTALL_SOAP="${INSTALL_SOAP}"
RUN if [ ${INSTALL_SOAP} = true ] ; then add-apt-repository -y ppa:ondrej/php \
 && apt-get update -yqq \
 && apt-get install libxml2-dev php5.6-soap -y ; fi
# ####################################
#  PHP GMP
# ####################################
ARG INSTALL_GMP=false
ENV INSTALL_GMP="${INSTALL_GMP}"
RUN if [ ${INSTALL_GMP} = true ] ; then apt-get install php-gmp -y ; fi
# ####################################
#  Set Timezone
# ####################################
ARG TZ=UTC
ENV TZ="${TZ}"
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone
# ####################################
#  Composer:
# ####################################
#  Add the composer.json
COPY ./composer.json /home/laradock/.composer/composer.json
#  Make sure that ~/.composer belongs to laradock
RUN chown -R laradock:laradock /home/laradock/.composer
USER laradock
#  Check if global install need to be ran
ARG COMPOSER_GLOBAL_INSTALL=false
ENV COMPOSER_GLOBAL_INSTALL="${COMPOSER_GLOBAL_INSTALL}"
RUN if [ ${COMPOSER_GLOBAL_INSTALL} = true ] ; then composer global install ; fi
#  Export composer vendor path
RUN echo "" >> ~/.bashrc \
 && echo 'export PATH="~/.composer/vendor/bin:$PATH"' >> ~/.bashrc
# ####################################
#  Crontab
# ####################################
USER root
COPY ./crontab /etc/cron.d
RUN chmod -R 644 /etc/cron.d
# ####################################
#  User Aliases
# ####################################
USER laradock
COPY ./aliases.sh /home/laradock/aliases.sh
RUN echo "" >> ~/.bashrc \
 && echo "# Load Custom Aliases" >> ~/.bashrc \
 && echo "source /home/laradock/aliases.sh" >> ~/.bashrc \
 && echo "" >> ~/.bashrc
USER root
RUN echo "" >> ~/.bashrc \
 && echo "# Load Custom Aliases" >> ~/.bashrc \
 && echo "source /home/laradock/aliases.sh" >> ~/.bashrc \
 && echo "" >> ~/.bashrc
# ####################################
#  xDebug:
# ####################################
ARG INSTALL_XDEBUG=false
RUN if [ ${INSTALL_XDEBUG} = true ] ; then apt-get install php5.6-xdebug -y --force-yes \
 && sed -i 's/^;//g' /etc/php/5.6/cli/conf.d/20-xdebug.ini \
 && echo "alias phpunit='php -dzend_extension=xdebug.so /var/www/vendor/bin/phpunit'" >> ~/.bashrc; fi
#  ADD for REMOTE debugging
COPY ./xdebug.ini /etc/php/5.6/cli/conf.d/xdebug.ini
# ####################################
#  Blackfire:
# ####################################
ARG INSTALL_BLACKFIRE=false
ARG BLACKFIRE_CLIENT_ID
ARG BLACKFIRE_CLIENT_TOKEN
ENV BLACKFIRE_CLIENT_ID="${BLACKFIRE_CLIENT_ID}"
ENV BLACKFIRE_CLIENT_TOKEN="${BLACKFIRE_CLIENT_TOKEN}"
RUN if [ ${INSTALL_XDEBUG} = false -a ${INSTALL_BLACKFIRE} = true ] ; then curl -L https://packagecloud.io/gpg.key | apt-key add - \
 && echo "deb http://packages.blackfire.io/debian any main" | tee /etc/apt/sources.list.d/blackfire.list \
 && apt-get update -yqq \
 && apt-get install blackfire-agent ; fi
# ####################################
#  ssh:
# ####################################
ARG INSTALL_WORKSPACE_SSH=false
ENV INSTALL_WORKSPACE_SSH="${INSTALL_WORKSPACE_SSH}"
ADD insecure_id_rsa /tmp/id_rsa
ADD insecure_id_rsa.pub /tmp/id_rsa.pub
RUN if [ ${INSTALL_WORKSPACE_SSH} = true ] ; then rm -f /etc/service/sshd/down \
 && cat /tmp/id_rsa.pub >> /root/.ssh/authorized_keys \
 && cat /tmp/id_rsa.pub >> /root/.ssh/id_rsa.pub \
 && cat /tmp/id_rsa >> /root/.ssh/id_rsa \
 && rm -f /tmp/id_rsa* \
 && chmod 644 /root/.ssh/authorized_keys /root/.ssh/id_rsa.pub \
 && chmod 400 /root/.ssh/id_rsa ; fi
# ####################################
#  MongoDB:
# ####################################
#  Check if Mongo needs to be installed
ARG INSTALL_MONGO=false
ENV INSTALL_MONGO="${INSTALL_MONGO}"
RUN if [ ${INSTALL_MONGO} = true ] ; then pecl install mongodb \
 && echo "extension=mongodb.so" >> /etc/php/5.6/cli/conf.d/30-mongodb.ini; fi
# ####################################
#  Drush:
# ####################################
USER root
ENV DRUSH_VERSION="8.1.2"
ARG INSTALL_DRUSH=false
ENV INSTALL_DRUSH="${INSTALL_DRUSH}"
RUN if [ ${INSTALL_DRUSH} = true ] ; then curl -fsSL -o /usr/local/bin/drush https://github.com/drush-ops/drush/releases/download/$DRUSH_VERSION/drush.phar | bash \
 && chmod +x /usr/local/bin/drush \
 && drush core-status ; fi
USER laradock
# ####################################
#  Node / NVM:
# ####################################
#  Check if NVM needs to be installed
ARG NODE_VERSION=stable
ENV NODE_VERSION="${NODE_VERSION}"
ARG INSTALL_NODE=false
ENV INSTALL_NODE="${INSTALL_NODE}"
ENV NVM_DIR="/home/laradock/.nvm"
RUN if [ ${INSTALL_NODE} = true ] ; then curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.1/install.sh | bash \
 && . $NVM_DIR/nvm.sh \
 && nvm install ${NODE_VERSION} \
 && nvm use ${NODE_VERSION} \
 && nvm alias ${NODE_VERSION} \
 && npm install gulp bower vue-cli -g ; fi
#  Wouldn't execute when added to the RUN statement in the above block
#  Source NVM when loading bash since ~/.profile isn't loaded on non-login shell
RUN if [ ${INSTALL_NODE} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export NVM_DIR="$HOME/.nvm"' >> ~/.bashrc \
 && echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.bashrc; fi
#  Add NVM binaries to root's .bashrc
USER root
RUN if [ ${INSTALL_NODE} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export NVM_DIR="/home/laradock/.nvm"' >> ~/.bashrc \
 && echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.bashrc; fi
# ####################################
#  YARN:
# ####################################
USER laradock
ARG INSTALL_YARN=false
ENV INSTALL_YARN="${INSTALL_YARN}"
ARG YARN_VERSION=latest
ENV YARN_VERSION="${YARN_VERSION}"
RUN if [ ${INSTALL_YARN} = true ] ; then [ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" \
 && if [ ${YARN_VERSION} = "latest" ] ; then curl -o- -L https://yarnpkg.com/install.sh | bash ; else curl -o- -L https://yarnpkg.com/install.sh | bash -s -- --version ${YARN_VERSION} ; fi \
 && echo "" >> ~/.bashrc \
 && echo 'export PATH="$HOME/.yarn/bin:$PATH"' >> ~/.bashrc; fi
#  Add YARN binaries to root's .bashrc
USER root
RUN if [ ${INSTALL_YARN} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export YARN_DIR="/home/laradock/.yarn"' >> ~/.bashrc \
 && echo 'export PATH="$YARN_DIR/bin:$PATH"' >> ~/.bashrc; fi
# ####################################
#  PHP Aerospike:
# ####################################
USER root
ARG INSTALL_AEROSPIKE=true
ENV INSTALL_AEROSPIKE="${INSTALL_AEROSPIKE}"
#  Copy aerospike configration for remote debugging
COPY ./aerospike.ini /etc/php/5.6/cli/conf.d/aerospike.ini
RUN if [ ${INSTALL_AEROSPIKE} = true ] ; then curl -L -o /tmp/aerospike-client-php.tar.gz "https://github.com/aerospike/aerospike-client-php/archive/3.4.14.tar.gz" \
 && mkdir -p aerospike-client-php \
 && tar -C aerospike-client-php -zxvf /tmp/aerospike-client-php.tar.gz --strip 1 \
 && (cd aerospike-client-php/src/aerospike \
 && phpize \
 && ./build.sh \
 && make install ) \
 && rm /tmp/aerospike-client-php.tar.gz ; fi
RUN if [ ${INSTALL_AEROSPIKE} = false ] ; then rm /etc/php/5.6/cli/conf.d/aerospike.ini ; fi
# ####################################
#  Non-root user : PHPUnit path
# ####################################
#  add ./vendor/bin to non-root user's bashrc (needed for phpunit)
USER laradock
RUN echo "" >> ~/.bashrc \
 && echo 'export PATH="/var/www/vendor/bin:$PATH"' >> ~/.bashrc
# ####################################
#  Laravel Artisan Alias
# ####################################
USER root
RUN echo "" >> ~/.bashrc \
 && echo 'alias art="php artisan"' >> ~/.bashrc
# ####################################
#  Laravel Envoy:
# ####################################
USER laradock
ARG INSTALL_LARAVEL_ENVOY=true
ENV INSTALL_LARAVEL_ENVOY="${INSTALL_LARAVEL_ENVOY}"
RUN if [ ${INSTALL_LARAVEL_ENVOY} = true ] ; then composer global require "laravel/envoy=~1.0" ; fi
# ####################################
#  Deployer:
# ####################################
USER laradock
ARG INSTALL_DEPLOYER=false
ENV INSTALL_DEPLOYER="${INSTALL_DEPLOYER}"
RUN if [ ${INSTALL_DEPLOYER} = true ] ; then composer global require "deployer/deployer" ; fi
# ####################################
#  Linuxbrew:
# ####################################
USER root
ARG INSTALL_LINUXBREW=true
ENV INSTALL_LINUXBREW="${INSTALL_LINUXBREW}"
RUN if [ ${INSTALL_LINUXBREW} = true ] ; then apt-get upgrade -y \
 && apt-get install build-essential make cmake scons curl git ruby autoconf automake autoconf-archive gettext libtool flex bison libbz2-dev libcurl4-openssl-dev libexpat-dev libncurses-dev -y \
 && git clone --depth=1 https://github.com/Homebrew/linuxbrew.git ~/.linuxbrew \
 && echo "" >> ~/.bashrc \
 && echo 'export PKG_CONFIG_PATH"=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig:/usr/lib64/pkgconfig:/usr/lib/pkgconfig:/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/lib64/pkgconfig:/usr/share/pkgconfig:$PKG_CONFIG_PATH"' >> ~/.bashrc \
 && echo 'export LINUXBREWHOME="$HOME/.linuxbrew"' >> ~/.bashrc \
 && echo 'export PATH="$LINUXBREWHOME/bin:$PATH"' >> ~/.bashrc \
 && echo 'export MANPATH="$LINUXBREWHOME/man:$MANPATH"' >> ~/.bashrc \
 && echo 'export PKG_CONFIG_PATH="$LINUXBREWHOME/lib64/pkgconfig:$LINUXBREWHOME/lib/pkgconfig:$PKG_CONFIG_PATH"' >> ~/.bashrc \
 && echo 'export LD_LIBRARY_PATH="$LINUXBREWHOME/lib64:$LINUXBREWHOME/lib:$LD_LIBRARY_PATH"' >> ~/.bashrc; fi
# ####################################
#  Minio:
# ####################################
USER root
ARG INSTALL_MC=false
ENV INSTALL_MC="${INSTALL_MC}"
COPY mc/config.json /root/.mc/config.json
RUN if [ ${INSTALL_MC} = true ] ; then curl -fsSL -o /usr/local/bin/mc https://dl.minio.io/client/mc/release/linux-amd64/mc \
 && chmod +x /usr/local/bin/mc ; fi
USER laradock
# ####################################
#  MySQL client
# ####################################
USER root
ARG INSTALL_MYSQL_CLIENT=false
ENV INSTALL_MYSQL_CLIENT="${INSTALL_MYSQL_CLIENT}"
RUN if [ ${INSTALL_MYSQL_CLIENT} = true ] ; then apt-get update \
 && apt-get install mysql-client -y ; fi
# ####################################
#  PostgreSQL client
# ####################################
USER root
ARG INSTALL_PGSQL_CLIENT=false
ENV INSTALL_PGSQL_CLIENT="${INSTALL_PGSQL_CLIENT}"
RUN if [ ${INSTALL_PGSQL_CLIENT} = true ] ; then apt-get update \
 && apt-get install postgresql-client -y ; fi
#
# --------------------------------------------------------------------------
#  Final Touch
# --------------------------------------------------------------------------
#
#  Clean up
USER root
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  Set default work directory
WORKDIR /var/www
