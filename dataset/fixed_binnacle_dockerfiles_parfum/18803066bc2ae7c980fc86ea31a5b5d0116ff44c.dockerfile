#
# --------------------------------------------------------------------------
#  Image Setup
# --------------------------------------------------------------------------
#
#  To edit the 'workspace' base Image, visit its repository on Github
#     https://github.com/LaraDock/workspace
#
#  To change its version, see the available Tags on the Docker Hub:
#     https://hub.docker.com/r/laradock/workspace/tags/
#
FROM laradock/workspace:1.1
MAINTAINER Mahmoud Zalt <mahmoud@zalt.me>
#
# --------------------------------------------------------------------------
#  Mandatory Software's Installation
# --------------------------------------------------------------------------
#
#  Mandatory Software's such as ("php7.0-cli", "git", "vim", ....) are
#  installed on the base image 'laradock/workspace' image. If you want
#  to add more Software's or remove existing one, you need to edit the
#  base image (https://github.com/LaraDock/workspace).
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
RUN groupadd -g $PGID laradock \
 && useradd -u $PUID -g laradock -m laradock
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
# ####################################
#  Crontab
# ####################################
USER root
COPY ./crontab /var/spool/cron/crontabs
# ####################################
#  xDebug:
# ####################################
#  Check if xDebug needs to be installed
ARG INSTALL_XDEBUG=false
ENV INSTALL_XDEBUG="${INSTALL_XDEBUG}"
RUN if [ ${INSTALL_XDEBUG} = true ] ; then \
apt-get update \
 && apt-get install --no-install-recommends php7.0-xdebug -y --force-yes \
 && sed -i 's/^/;/g' /etc/php/7.0/cli/conf.d/20-xdebug.ini \
 && echo "alias phpunit='php -dzend_extension=xdebug.so /var/www/laravel/vendor/bin/phpunit'" >> ~/.bashrc; fi
#  ADD for REMOTE debugging
COPY ./xdebug.ini /etc/php/7.0/cli/conf.d/xdebug.ini
# ####################################
#  ssh:
# ####################################
ARG INSTALL_WORKSPACE_SSH=false
ENV INSTALL_WORKSPACE_SSH="${INSTALL_WORKSPACE_SSH}"
COPY insecure_id_rsa /tmp/id_rsa
COPY insecure_id_rsa.pub /tmp/id_rsa.pub
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
 && echo "extension=mongodb.so" >> /etc/php/7.0/cli/php.ini; fi
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
RUN if [ ${INSTALL_NODE} = true ] ; then curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.6/install.sh | bash \
 && . $NVM_DIR/nvm.sh \
 && NVM_NODEJS_ORG_MIRROR=https://npm.taobao.org/mirrors/node nvm install stable \
 && nvm install ${NODE_VERSION} \
 && nvm use ${NODE_VERSION} \
 && nvm alias ${NODE_VERSION} \
 && npm install cnpm -g --registry=https://registry.npm.taobao.org \
 && cnpm install -g gulp bower vue-cli vue-router vuex ; fi
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
RUN if [ ${INSTALL_YARN} = true ] ; then curl -o- -L https://yarnpkg.com/install.sh | bash \
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
ARG INSTALL_AEROSPIKE_EXTENSION=true
ENV INSTALL_AEROSPIKE_EXTENSION="${INSTALL_AEROSPIKE_EXTENSION}"
#  Copy aerospike configration for remote debugging
COPY ./aerospike.ini /etc/php/7.0/cli/conf.d/aerospike.ini
RUN if [ ${INSTALL_AEROSPIKE_EXTENSION} = true ] ; then curl -L -o /tmp/aerospike-client-php.tar.gz "https://github.com/luciano-jr/aerospike-client-php/archive/master.tar.gz" \
 && mkdir -p aerospike-client-php \
 && tar -C aerospike-client-php -zxvf /tmp/aerospike-client-php.tar.gz --strip 1 \
 && (cd aerospike-client-php/src/aerospike \
 && phpize \
 && ./build.sh \
 && make install ) \
 && rm /tmp/aerospike-client-php.tar.gz ; fi
RUN if [ ${INSTALL_AEROSPIKE_EXTENSION} = false ] ; then rm /etc/php/7.0/cli/conf.d/aerospike.ini ; fi
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
WORKDIR /var/www/laravel
