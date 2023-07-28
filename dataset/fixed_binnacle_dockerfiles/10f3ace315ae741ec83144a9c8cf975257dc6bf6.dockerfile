#
#  --------------------------------------------------------------------------
#   workspace 从 laradock 项目中精简
#  --------------------------------------------------------------------------
ARG PHP_VERSION=${PHP_VERSION}
FROM laradock/workspace:2.2-${PHP_VERSION}
LABEL maintainer="Syncher <syncviip@gmail.com>"
#   Set Environment Variables
ENV DEBIAN_FRONTEND="noninteractive"
#   Start as root
USER root
#  ##########################################################################
#   添加新用户 webdock
#  ##########################################################################
#   Add a non-root user to prevent files being created with root permissions on host machine.
ARG PUID=1000
ENV PUID="${PUID}"
ARG PGID=1000
ENV PGID="${PGID}"
#   always run apt update when start and after add new source list, then clean up at end.
RUN : \
 && pecl channel-update pecl.php.net \
 && groupadd -g ${PGID} webdock \
 && useradd -u ${PUID} -g webdock -m webdock -G docker_env \
 && usermod -p "*" webdock
#
#  --------------------------------------------------------------------------
#   以下都是可选择安装扩展
#  --------------------------------------------------------------------------
#
#  ##########################################################################
#   Set Timezone
#  ##########################################################################
ARG TZ=UTC
ENV TZ="${TZ}"
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone
#  ##########################################################################
#   设置一些别名，这些别名在 aliases.sh 中定义
#  ##########################################################################
USER root
COPY ./aliases.sh /root/aliases.sh
COPY ./aliases.sh /home/webdock/aliases.sh
RUN sed -i 's/\r//' /root/aliases.sh \
 && sed -i 's/\r//' /home/webdock/aliases.sh \
 && chown webdock:webdock /home/webdock/aliases.sh \
 && echo "" >> ~/.bashrc \
 && echo "# Load Custom Aliases" >> ~/.bashrc \
 && echo "source ~/aliases.sh" >> ~/.bashrc \
 && echo "" >> ~/.bashrc
USER webdock
RUN echo "" >> ~/.bashrc \
 && echo "# Load Custom Aliases" >> ~/.bashrc \
 && echo "source ~/aliases.sh" >> ~/.bashrc \
 && echo "" >> ~/.bashrc
#  ##########################################################################
#   Composer:
#  ##########################################################################
USER root
#   Add the composer.json
COPY ./composer.json /home/webdock/.composer/composer.json
#   Make sure that ~/.composer belongs to webdock
RUN chown -R webdock:webdock /home/webdock/.composer
USER webdock
#   Check if global install need to be ran
ARG COMPOSER_GLOBAL_INSTALL=false
ENV COMPOSER_GLOBAL_INSTALL="${COMPOSER_GLOBAL_INSTALL}"
RUN if [ ${COMPOSER_GLOBAL_INSTALL} = true ] ; then composer global install ; fi
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
USER webdock
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
RUN if [ ${INSTALL_DRUSH} = true ] ; then (apt-get update ;apt-get install --no-install-recommends mysql-client -y ) \
 && curl -fsSL -o /usr/local/bin/drush https://github.com/drush-ops/drush/releases/download/${DRUSH_VERSION}/drush.phar | bash \
 && chmod +x /usr/local/bin/drush \
 && drush core-status ; fi
#  ##########################################################################
#   SOAP:
#  ##########################################################################
USER root
ARG INSTALL_SOAP=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_SOAP} = true ] ; then (apt-get update ;apt-get install --no-install-recommends libxml2-dev php${PHP_VERSION}-soap -y ) ; fi
#  ##########################################################################
#   LDAP:
#  ##########################################################################
ARG INSTALL_LDAP=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_LDAP} = true ] ; then (apt-get update ;apt-get install --no-install-recommends libldap2-dev -y ) \
 && (apt-get update ;apt-get install --no-install-recommends php${PHP_VERSION}-ldap -y ) ; fi
#  ##########################################################################
#   IMAP:
#  ##########################################################################
ARG INSTALL_IMAP=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_IMAP} = true ] ; then (apt-get update ;apt-get install --no-install-recommends php${PHP_VERSION}-imap -y ) ; fi
#  ##########################################################################
#   xDebug:
#  ##########################################################################
USER root
ARG INSTALL_XDEBUG=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_XDEBUG} = true ] ; then (apt-get update ;apt-get install --no-install-recommends php${PHP_VERSION}-xdebug -y ) \
 && sed -i 's/^;//g' /etc/php/${PHP_VERSION}/cli/conf.d/20-xdebug.ini \
 && echo "alias phpunit='php -dzend_extension=xdebug.so /var/www/vendor/bin/phpunit'" >> ~/.bashrc; fi
#   ADD for REMOTE debugging
COPY ./xdebug.ini /etc/php/${PHP_VERSION}/cli/conf.d/xdebug.ini
RUN sed -i "s/xdebug.remote_autostart=0/xdebug.remote_autostart=1/" /etc/php/${PHP_VERSION}/cli/conf.d/xdebug.ini \
 && sed -i "s/xdebug.remote_enable=0/xdebug.remote_enable=1/" /etc/php/${PHP_VERSION}/cli/conf.d/xdebug.ini \
 && sed -i "s/xdebug.cli_color=0/xdebug.cli_color=1/" /etc/php/${PHP_VERSION}/cli/conf.d/xdebug.ini
#  ##########################################################################
#   Phpdbg:
#  ##########################################################################
USER root
ARG INSTALL_PHPDBG=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_PHPDBG} = true ] ; then (apt-get update ;apt-get install --no-install-recommends php${PHP_VERSION}-phpdbg -y --force-yes ) ; fi
#  ##########################################################################
#   Blackfire:
#  ##########################################################################
ARG INSTALL_BLACKFIRE=false
ARG BLACKFIRE_CLIENT_ID
ENV BLACKFIRE_CLIENT_ID="${BLACKFIRE_CLIENT_ID}"
ARG BLACKFIRE_CLIENT_TOKEN
ENV BLACKFIRE_CLIENT_TOKEN="${BLACKFIRE_CLIENT_TOKEN}"
RUN if [ ${INSTALL_XDEBUG} = false -a ${INSTALL_BLACKFIRE} = true ] ; then curl -L https://packagecloud.io/gpg.key | apt-key add - \
 && echo "deb http://packages.blackfire.io/debian any main" | tee /etc/apt/sources.list.d/blackfire.list \
 && apt-get update -yqq \
 && (apt-get update ;apt-get install --no-install-recommends blackfire-agent ) ; fi
#  ##########################################################################
#   MongoDB:
#  ##########################################################################
ARG INSTALL_MONGO=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_MONGO} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install mongo \
 && echo "extension=mongo.so" >> /etc/php/${PHP_VERSION}/mods-available/mongo.ini \
 && ln -s /etc/php/${PHP_VERSION}/mods-available/mongo.ini /etc/php/${PHP_VERSION}/cli/conf.d/30-mongo.ini ; fi \
 && pecl install mongodb \
 && echo "extension=mongodb.so" >> /etc/php/${PHP_VERSION}/mods-available/mongodb.ini \
 && ln -s /etc/php/${PHP_VERSION}/mods-available/mongodb.ini /etc/php/${PHP_VERSION}/cli/conf.d/30-mongodb.ini ; fi
#  ##########################################################################
#   AMQP:
#  ##########################################################################
ARG INSTALL_AMQP=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_AMQP} = true ] ; then (apt-get update ;apt-get install --no-install-recommends librabbitmq-dev -y ) \
 && pecl -q install amqp \
 && echo "extension=amqp.so" >> /etc/php/${PHP_VERSION}/mods-available/amqp.ini \
 && ln -s /etc/php/${PHP_VERSION}/mods-available/amqp.ini /etc/php/${PHP_VERSION}/cli/conf.d/30-amqp.ini ; fi
#  ##########################################################################
#   PHP REDIS EXTENSION
#  ##########################################################################
ARG INSTALL_PHPREDIS=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_PHPREDIS} = true ] ; then printf "\n" | pecl -q install -o -f redis \
 && echo "extension=redis.so" >> /etc/php/${PHP_VERSION}/mods-available/redis.ini \
 && phpenmod redis ; fi
#  ##########################################################################
#   Swoole EXTENSION
#  ##########################################################################
ARG INSTALL_SWOOLE=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_SWOOLE} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl -q install swoole-2.0.11 ; else if [ $( php -r "echo PHP_MINOR_VERSION;" ;) = "0" ] ; then pecl install swoole-2.2.0 ; else pecl install swoole ; fi ; fi \
 && echo "extension=swoole.so" >> /etc/php/${PHP_VERSION}/mods-available/swoole.ini \
 && ln -s /etc/php/${PHP_VERSION}/mods-available/swoole.ini /etc/php/${PHP_VERSION}/cli/conf.d/20-swoole.ini ; fi
#  ##########################################################################
#   Libpng16 EXTENSION
#  ##########################################################################
ARG INSTALL_LIBPNG=false
RUN if [ ${INSTALL_LIBPNG} = true ] ; then apt update \
 && (apt-get update ;apt-get install --no-install-recommends libpng16-16 ) ; fi
#  ##########################################################################
#   Drupal Console:
#  ##########################################################################
USER root
ARG INSTALL_DRUPAL_CONSOLE=false
RUN if [ ${INSTALL_DRUPAL_CONSOLE} = true ] ; then (apt-get update ;apt-get install --no-install-recommends mysql-client -y ) \
 && curl https://drupalconsole.com/installer -L -o drupal.phar \
 && mv drupal.phar /usr/local/bin/drupal \
 && chmod +x /usr/local/bin/drupal ; fi
USER webdock
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
ARG NPM_REGISTRY
ENV NPM_REGISTRY="${NPM_REGISTRY}"
ENV NVM_DIR="/home/webdock/.nvm"
RUN if [ ${INSTALL_NODE} = true ] ; then curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash \
 && . $NVM_DIR/nvm.sh \
 && nvm install ${NODE_VERSION} \
 && nvm use ${NODE_VERSION} \
 && nvm alias ${NODE_VERSION} \
 && if [ ${NPM_REGISTRY} ] ; then npm config set registry ${NPM_REGISTRY} ; fi \
 && if [ ${INSTALL_NPM_GULP} = true ] ; then npm install gulp@4.0.2 -g ; fi \
 && if [ ${INSTALL_NPM_BOWER} = true ] ; then npm install bower@1.8.14 -g ; fi \
 && if [ ${INSTALL_NPM_VUE_CLI} = true ] ; then npm install @vue/cli -g ; fi ; fi
#   Wouldn't execute when added to the RUN statement in the above block
#   Source NVM when loading bash since ~/.profile isn't loaded on non-login shell
RUN if [ ${INSTALL_NODE} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export NVM_DIR="$HOME/.nvm"' >> ~/.bashrc \
 && echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.bashrc; fi
#   Add NVM binaries to root's .bashrc
USER root
RUN if [ ${INSTALL_NODE} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export NVM_DIR="/home/webdock/.nvm"' >> ~/.bashrc \
 && echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.bashrc; fi
#   Add PATH for node
ENV PATH="$PATH:$NVM_DIR/versions/node/v${NODE_VERSION}/bin"
RUN if [ ${NPM_REGISTRY} ] ; then . ~/.bashrc \
 && npm config set registry ${NPM_REGISTRY} ; fi
#  ##########################################################################
#   PHP V8JS:
#  ##########################################################################
USER root
ARG INSTALL_V8JS=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_V8JS} = true ] ; then add-apt-repository -y ppa:pinepain/libv8-archived \
 && apt-get update -yqq \
 && (apt-get update ;apt-get install --no-install-recommends php-pear libv8-5.4 php${PHP_VERSION}-xml php${PHP_VERSION}-dev -y ) \
 && pecl install v8js \
 && echo "extension=v8js.so" >> /etc/php/${PHP_VERSION}/cli/php.ini; fi
#  ##########################################################################
#   Laravel Envoy:
#  ##########################################################################
USER webdock
ARG INSTALL_LARAVEL_ENVOY=false
RUN if [ ${INSTALL_LARAVEL_ENVOY} = true ] ; then composer global require "laravel/envoy=~1.0" ; fi
#  ##########################################################################
#   Laravel Installer:
#  ##########################################################################
USER root
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
USER webdock
ARG INSTALL_PRESTISSIMO=false
RUN if [ ${INSTALL_PRESTISSIMO} = true ] ; then composer global require "hirak/prestissimo" ; fi
#  ##########################################################################
#   Linuxbrew:
#  ##########################################################################
USER root
ARG INSTALL_LINUXBREW=false
RUN if [ ${INSTALL_LINUXBREW} = true ] ; then apt-get upgrade -y \
 && (apt-get update ;apt-get install --no-install-recommends build-essential make cmake scons curl git ruby autoconf automake autoconf-archive gettext libtool flex bison libbz2-dev libcurl4-openssl-dev libexpat-dev libncurses-dev -y ) \
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
ARG PHP_VERSION=${PHP_VERSION}
RUN set -eux ; if [ ${INSTALL_MSSQL} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then (apt-get update ;apt-get install --no-install-recommends php5.6-sybase freetds-bin freetds-common libsybdb5 -y ) \
 && php -m | grep -q 'mssql' \
 && php -m | grep -q 'pdo_dblib' ; else curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
 && curl https://packages.microsoft.com/config/ubuntu/16.04/prod.list > /etc/apt/sources.list.d/mssql-release.list \
 && apt-get update -yqq \
 && ACCEPT_EULA=Y apt-get install -yqq msodbcsql=13.0.1.0-1 mssql-tools=14.0.2.0-1 \
 && (apt-get update ;apt-get install --no-install-recommends unixodbc-dev-utf16 -yqq ) \
 && ln -sfn /opt/mssql-tools/bin/sqlcmd-13.0.1.0 /usr/bin/sqlcmd \
 && ln -sfn /opt/mssql-tools/bin/bcp-13.0.1.0 /usr/bin/bcp \
 && ACCEPT_EULA=Y apt-get install -yqq unixodbc unixodbc-dev libgss3 odbcinst msodbcsql locales \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && pecl install sqlsrv-4.3.0 pdo_sqlsrv-4.3.0 \
 && (apt-get update ;apt-get install --no-install-recommends locales -y ) \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && echo "extension=sqlsrv.so" > /etc/php/${PHP_VERSION}/cli/conf.d/20-sqlsrv.ini \
 && echo "extension=pdo_sqlsrv.so" > /etc/php/${PHP_VERSION}/cli/conf.d/20-pdo_sqlsrv.ini \
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
RUN if [ ${INSTALL_IMAGE_OPTIMIZERS} = true ] ; then (apt-get update ;apt-get install --no-install-recommends jpegoptim optipng pngquant gifsicle -y ) \
 && if [ ${INSTALL_NODE} = true ] ; then exec bash \
 && . ~/.bashrc \
 && npm install svgo@3.0.2 -g ; fi ; fi
USER webdock
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
RUN if [ ${INSTALL_PYTHON} = true ] ; then (apt-get update ;apt-get install --no-install-recommends python python-pip python-dev build-essential -y ) \
 && python -m pip install --upgrade pip \
 && python -m pip install --upgrade virtualenv ; fi
#  ##########################################################################
#   ImageMagick:
#  ##########################################################################
USER root
ARG INSTALL_IMAGEMAGICK=false
RUN if [ ${INSTALL_IMAGEMAGICK} = true ] ; then (apt-get update ;apt-get install --no-install-recommends imagemagick php-imagick -y ) ; fi
#  ##########################################################################
#   Terraform:
#  ##########################################################################
USER root
ARG INSTALL_TERRAFORM=false
RUN if [ ${INSTALL_TERRAFORM} = true ] ; then (apt-get update ;apt-get install --no-install-recommends sudo wget unzip -y ) \
 && wget https://releases.hashicorp.com/terraform/0.10.6/terraform_0.10.6_linux_amd64.zip \
 && unzip terraform_0.10.6_linux_amd64.zip \
 && mv terraform /usr/local/bin \
 && rm terraform_0.10.6_linux_amd64.zip ; fi
#  ##########################################################################
#   pgsql client
#  ##########################################################################
USER root
ARG INSTALL_PG_CLIENT=false
RUN if [ ${INSTALL_PG_CLIENT} = true ] ; then (apt-get update ;apt-get install --no-install-recommends postgresql-client -y ) ; fi
#  ##########################################################################
#   nasm
#  ##########################################################################
USER root
RUN apt-get update -yqq \
 && (apt-get update ;apt-get install --no-install-recommends nasm -yqq )
#  ##########################################################################
#   Dusk Dependencies:
#  ##########################################################################
USER root
ARG CHROME_DRIVER_VERSION=stable
ENV CHROME_DRIVER_VERSION="${CHROME_DRIVER_VERSION}"
ARG INSTALL_DUSK_DEPS=false
RUN if [ ${INSTALL_DUSK_DEPS} = true ] ; then (apt-get update ;apt-get install --no-install-recommends zip wget unzip xdg-utils libxpm4 libxrender1 libgtk2.0-0 libnss3 libgconf-2-4 xvfb gtk2-engines-pixbuf xfonts-cyrillic xfonts-100dpi xfonts-75dpi xfonts-base xfonts-scalable x11-apps -y ) \
 && wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
 && dpkg -i --force-depends google-chrome-stable_current_amd64.deb \
 && (apt-get update ;apt-get install --no-install-recommends -y -f ) \
 && dpkg -i --force-depends google-chrome-stable_current_amd64.deb \
 && rm google-chrome-stable_current_amd64.deb \
 && wget https://chromedriver.storage.googleapis.com/${CHROME_DRIVER_VERSION}/chromedriver_linux64.zip \
 && unzip chromedriver_linux64.zip \
 && mv chromedriver /usr/local/bin/ \
 && rm chromedriver_linux64.zip ; fi
#  ##########################################################################
#   Check PHP version:
#  ##########################################################################
ARG PHP_VERSION=${PHP_VERSION}
RUN php -v | head -n 1 | grep -q "PHP ${PHP_VERSION}."
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
