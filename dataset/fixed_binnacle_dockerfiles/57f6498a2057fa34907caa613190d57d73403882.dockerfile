#   See https://github.com/docker-library/php/blob/master/7.1/jessie/fpm/Dockerfile
FROM php:7.1-fpm
ARG TIMEZONE
ARG DEBIAN_FRONTEND=noninteractive
MAINTAINER Yahya ERTURAN <root@yahyaerturan.com>
ENV NVM_DIR="/usr/local/nvm"
ENV NODE_VERSION="8.9.3"
COPY ./utils/.bashrc /root/
RUN /bin/bash -c "source /root/.bashrc"
RUN apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends openssl=1.1.1n-0+deb10u4 git=1:2.20.1-2+deb10u8 nano=3.2-3 apt-transport-https=1.8.2.3 curl=7.64.0-4+deb10u5 zip=3.0-11+b1 unzip=6.0-23+deb10u3 -y -qq )
#   soap => dependency: libxml2-dev
#   mcrypt => dependency: libmcrypt-dev
#   ftp => dependency: libssl-dev
#   intl => dependency: libicu-dev
#   xsl => dependency: libxslt-dev
#   gd => dependecy: libfreetype6-dev libjpeg62-turbo-dev libpng-dev
RUN (apt-get update ;apt-get install --no-install-recommends libxml2-dev=2.9.4+dfsg1-7+deb10u5 libmcrypt-dev=2.5.8-3.4 libssl-dev=1.1.1n-0+deb10u4 libicu-dev=63.1-6+deb10u3 libxslt-dev libfreetype6-dev=2.9.1-3+deb10u3 libjpeg62-turbo-dev=1:1.5.2-2+deb10u1 libpng-dev=1.6.36-6 -y -qq )
#   Add Yarn as PPA
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN :
#   Config Git Globally
RUN git config --global user.name "${GIT_USER_NAME}"
RUN git config --global user.email "${GIT_USER_EMAIL}"
RUN git config --global core.fileMode false
RUN git config --global color.ui true
#   Install Composer
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer --version
#   Install NVM
#   https://github.com/creationix/nvm#install-script
RUN curl --silent -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash
#   install node and npm
RUN . $NVM_DIR/nvm.sh \
 && nvm install $NODE_VERSION \
 && nvm alias default $NODE_VERSION \
 && nvm use default
#   add node and npm to path so the commands are available
ENV NODE_PATH="$NVM_DIR/v$NODE_VERSION/lib/node_modules"
ENV PATH="$NVM_DIR/versions/node/v$NODE_VERSION/bin:$PATH"
RUN /bin/bash -c "npm upgrade minimatch@3 graceful-fs@4 --global"
#   Intall Yarn
RUN (apt-get update ;apt-get install --no-install-recommends yarn -y -qq )
#   Set timezone
RUN ln -snf /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && echo ${TIMEZONE} > /etc/timezone
RUN printf '[PHP]\ndate.timezone = "%s"\n', ${TIMEZONE} > /usr/local/etc/php/conf.d/tzone.ini
RUN "date"
#   Type docker-php-ext-install to see available extensions
#   Possible values for ext-name:
#   bcmath bz2 calendar ctype curl dba dom enchant exif fileinfo filter ftp
#   gd gettext gmp hash iconv imap interbase intl json ldap mbstring mcrypt mysqli
#   oci8 odbc opcache pcntl pdo pdo_dblib pdo_firebird pdo_mysql pdo_oci pdo_odbc pdo_pgsql pdo_sqlite
#   pgsql phar posix pspell readline recode reflection session shmop simplexml snmp soap sockets
#   spl standard sysvmsg sysvsem sysvshm tidy tokenizer wddx xml xmlreader xmlrpc xmlwriter xsl zip
RUN docker-php-ext-install gettext zip xsl pdo pdo_mysql mysqli intl hash \
 && docker-php-ext-install -j$( nproc ;) iconv mcrypt \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install -j$( nproc ;) gd opcache
#   Installed Extension with this configuration
#   Core        hash        pdo_mysql       standard
#   ctype       iconv       pdo_sqlite      tokenizer
#   curl        json        Phar            xml
#   date        libxml      posix           xmlreader
#   dom         mbstring    readline        xmlwriter
#   fileinfo    mcrypt      Reflection      xsl
#   filter      mysqlnd     session         zip
#   ftp         openssl     SimpleXML       zlib
#   gd          pcre        SPL             [Zend Modules]
#   gettext     PDO         sqlite3         Zend OPcache
#   Install and enable xdebug
#   RUN pecl install xdebug
#   RUN docker-php-ext-enable xdebug
#   RUN echo "error_reporting = E_ALL" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini
#   RUN echo "display_startup_errors = On" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini
#   RUN echo "display_errors = On" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini
#   RUN echo "xdebug.remote_enable=1" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini
#   RUN echo "xdebug.remote_connect_back=1" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini
#   RUN echo "xdebug.idekey=\"PHPSTORM\"" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini
#   RUN echo "xdebug.remote_port=9001" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini
#   Disable opcahce temporarily
#   RUN mkdir -p /usr/local/etc/php/conf.d/disabled && mv /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini /usr/local/etc/php/conf.d/disabled/docker-php-ext-opcache.ini
RUN mkdir -p /root/.ssh
COPY ./utils/id_rsa /root/.ssh
COPY ./utils/id_rsa.pub /root/.ssh
WORKDIR /var/www/symfony
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
