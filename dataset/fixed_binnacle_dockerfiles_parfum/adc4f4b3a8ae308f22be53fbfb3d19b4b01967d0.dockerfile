#
# --------------------------------------------------------------------------
#  Image Setup
# --------------------------------------------------------------------------
#
#  To edit the 'php-fpm' base Image, visit its repository on Github
#     https://github.com/Laradock/php-fpm
#
#  To change its version, see the available Tags on the Docker Hub:
#     https://hub.docker.com/r/laradock/php-fpm/tags/
#
#  Note: Base Image name format {image-tag}-{php-version}
#
ARG LARADOCK_PHP_VERSION
#  FROM laradock/php-fpm:2.2-${LARADOCK_PHP_VERSION}
FROM letsdockerize/laradock-php-fpm:2.4-${LARADOCK_PHP_VERSION}
LABEL maintainer="Mahmoud Zalt <mahmoud@zalt.me>"
ARG LARADOCK_PHP_VERSION
#  Set Environment Variables
ENV DEBIAN_FRONTEND="noninteractive"
#  always run apt update when start and after add new source list, then clean up at end.
RUN set -xe ; apt-get update -yqq \
 && pecl channel-update pecl.php.net \
 && apt-get install --no-install-recommends apt-utils libzip-dev zip unzip -yqq \
 && docker-php-ext-configure zip --with-libzip \
 && docker-php-ext-install zip \
 && php -m | grep -q 'zip'
#
# --------------------------------------------------------------------------
#  Optional Software's Installation
# --------------------------------------------------------------------------
#
#  Optional Software's will only be installed if you set them to `true`
#  in the `docker-compose.yml` before the build.
#  Example:
#    - INSTALL_SOAP=true
#
# ##########################################################################
#  SSH2:
# ##########################################################################
ARG INSTALL_SSH2=false
RUN if [ ${INSTALL_SSH2} = true ] ; then \
apt-get install --no-install-recommends libssh2-1-dev -y \
 && if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install -a ssh2-0.13 ; else pecl install -a ssh2-1.1.2 ; fi \
 && docker-php-ext-enable ssh2; else pecl install -a ssh2-1.1.2 ; fi \
 fi
# ##########################################################################
#  libfaketime:
# ##########################################################################
USER root
ARG INSTALL_FAKETIME=false
RUN if [ ${INSTALL_FAKETIME} = true ] ; then \
apt-get install --no-install-recommends libfaketime -y; fi
# ####################################
#  gd:
# ####################################
RUN apt-get update \
 && apt-get install --no-install-recommends libwebp-dev -y
#  Install the PHP gd library
RUN docker-php-ext-configure gd --enable-gd-native-ttf --with-jpeg-dir=/usr/lib --with-webp-dir=/usr/include --with-freetype-dir=/usr/include/freetype2 \
 && docker-php-ext-install gd
# ##########################################################################
#  SOAP:
# ##########################################################################
ARG INSTALL_SOAP=false
RUN if [ ${INSTALL_SOAP} = true ] ; then \
rm /etc/apt/preferences.d/no-debian-php \
 && apt-get install --no-install-recommends libxml2-dev php-soap -y \
 && docker-php-ext-install soap; fi
# ##########################################################################
#  XSL:
# ##########################################################################
ARG INSTALL_XSL=false
RUN if [ ${INSTALL_XSL} = true ] ; then \
apt-get install --no-install-recommends libxslt-dev -y \
 && docker-php-ext-install xsl; fi
# ##########################################################################
#  pgsql
# ##########################################################################
ARG INSTALL_PGSQL=false
RUN if [ ${INSTALL_PGSQL} = true ] ; then docker-php-ext-install pgsql ; fi
# ##########################################################################
#  pgsql client
# ##########################################################################
ARG INSTALL_PG_CLIENT=false
ARG INSTALL_POSTGIS=false
RUN if [ ${INSTALL_PG_CLIENT} = true ] ; then \
mkdir -p /usr/share/man/man1 \
 && mkdir -p /usr/share/man/man7 \
 && apt-get install --no-install-recommends postgresql-client -y \
 && if [ ${INSTALL_POSTGIS} = true ] ; then \
 apt-get install --no-install-recommends postgis -y; fi; fi
# ##########################################################################
#  xDebug:
# ##########################################################################
ARG INSTALL_XDEBUG=false
RUN if [ ${INSTALL_XDEBUG} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install xdebug-2.5.5 ; else pecl install xdebug ; fi \
 && docker-php-ext-enable xdebug ; fi
#  Copy xdebug configuration for remote debugging
COPY ./xdebug.ini /usr/local/etc/php/conf.d/xdebug.ini
RUN sed -i "s/xdebug.remote_autostart=0/xdebug.remote_autostart=1/" /usr/local/etc/php/conf.d/xdebug.ini \
 && sed -i "s/xdebug.remote_enable=0/xdebug.remote_enable=1/" /usr/local/etc/php/conf.d/xdebug.ini \
 && sed -i "s/xdebug.cli_color=0/xdebug.cli_color=1/" /usr/local/etc/php/conf.d/xdebug.ini
# ##########################################################################
#  Phpdbg:
# ##########################################################################
ARG INSTALL_PHPDBG=false
RUN if [ ${INSTALL_PHPDBG} = true ] ; then \
apt-get install --no-install-recommends php${LARADOCK_PHP_VERSION}-phpdbg -y --force-yes; fi
# ##########################################################################
#  Blackfire:
# ##########################################################################
ARG INSTALL_BLACKFIRE=false
RUN if [ ${INSTALL_XDEBUG} = false -a ${INSTALL_BLACKFIRE} = true ] ; then version=$( php -r "echo PHP_MAJOR_VERSION.PHP_MINOR_VERSION;" ;) \
 && curl -A "Docker" -o /tmp/blackfire-probe.tar.gz -D - -L -s https://blackfire.io/api/v1/releases/probe/php/linux/amd64/$version \
 && tar zxpf /tmp/blackfire-probe.tar.gz -C /tmp \
 && mv /tmp/blackfire-*.so $( php -r "echo ini_get('extension_dir');" ;)/blackfire.so \
 && printf "extension=blackfire.so\nblackfire.agent_socket=tcp://blackfire:8707\n" > $PHP_INI_DIR/conf.d/blackfire.ini; fi
# ##########################################################################
#  PHP REDIS EXTENSION
# ##########################################################################
ARG INSTALL_PHPREDIS=false
RUN if [ ${INSTALL_PHPREDIS} = true ] ; then printf "\n" | pecl install -o -f redis \
 && rm -rf /tmp/pear \
 && docker-php-ext-enable redis ; fi
# ##########################################################################
#  Swoole EXTENSION
# ##########################################################################
ARG INSTALL_SWOOLE=false
RUN if [ ${INSTALL_SWOOLE} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install swoole-2.0.10 ; else if [ $( php -r "echo PHP_MINOR_VERSION;" ;) = "0" ] ; then pecl install swoole-2.2.0 ; else pecl install swoole ; fi ; fi \
 && docker-php-ext-enable swoole \
 && php -m | grep -q 'swoole' ; fi
# ##########################################################################
#  Taint EXTENSION
# ##########################################################################
ARG INSTALL_TAINT=false
RUN if [ ${INSTALL_TAINT} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "7" ] ; then pecl install taint \
 && docker-php-ext-enable taint \
 && php -m | grep -q 'taint' ; fi ; fi
# ##########################################################################
#  MongoDB:
# ##########################################################################
ARG INSTALL_MONGO=false
RUN if [ ${INSTALL_MONGO} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install mongo \
 && docker-php-ext-enable mongo ; fi \
 && pecl install mongodb \
 && docker-php-ext-enable mongodb ; fi
# ##########################################################################
#  Xhprof:
# ##########################################################################
ARG INSTALL_XHPROF=false
RUN if [ ${INSTALL_XHPROF} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = 7 ] ; then curl -L -o /tmp/xhprof.tar.gz "https://github.com/tideways/php-xhprof-extension/archive/v4.1.6.tar.gz" ; else curl -L -o /tmp/xhprof.tar.gz "https://codeload.github.com/phacility/xhprof/tar.gz/master" ; fi \
 && mkdir -p xhprof \
 && tar -C xhprof -zxvf /tmp/xhprof.tar.gz --strip 1 \
 && (cd xhprof \
 && phpize \
 && ./configure \
 && make \
 && make install ) \
 && rm -r xhprof \
 && rm /tmp/xhprof.tar.gz ; fi
COPY ./xhprof.ini /usr/local/etc/php/conf.d
# ##########################################################################
#  AMQP:
# ##########################################################################
ARG INSTALL_AMQP=false
RUN if [ ${INSTALL_AMQP} = true ] ; then \
apt-get update \
 && apt-get install --no-install-recommends cmake -y \
 && curl -L -o /tmp/rabbitmq-c.tar.gz https://github.com/alanxz/rabbitmq-c/archive/master.tar.gz \
 && mkdir -p rabbitmq-c \
 && tar -C rabbitmq-c -zxvf /tmp/rabbitmq-c.tar.gz --strip 1 \
 && cd rabbitmq-c/ \
 && mkdir _build \
 && cd _build/ \
 && cmake .. \
 && cmake --build . --target install \
 && pecl install amqp \
 && docker-php-ext-enable amqp \
 && docker-php-ext-install sockets; fi
# ##########################################################################
#  pcntl
# ##########################################################################
ARG INSTALL_PCNTL=false
RUN if [ ${INSTALL_PCNTL} = true ] ; then docker-php-ext-install pcntl ; fi
# ##########################################################################
#  bcmath:
# ##########################################################################
ARG INSTALL_BCMATH=false
RUN if [ ${INSTALL_BCMATH} = true ] ; then docker-php-ext-install bcmath ; fi
# ##########################################################################
#  GMP (GNU Multiple Precision):
# ##########################################################################
ARG INSTALL_GMP=false
RUN if [ ${INSTALL_GMP} = true ] ; then \
apt-get install --no-install-recommends libgmp-dev -y \
 && if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then ln -s /usr/include/x86_64-linux-gnu/gmp.h /usr/include/gmp.h ; fi \
 && docker-php-ext-install gmp; fi
# ##########################################################################
#  PHP Memcached:
# ##########################################################################
ARG INSTALL_MEMCACHED=false
RUN if [ ${INSTALL_MEMCACHED} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then curl -L -o /tmp/memcached.tar.gz "https://github.com/php-memcached-dev/php-memcached/archive/2.2.0.tar.gz" ; else curl -L -o /tmp/memcached.tar.gz "https://github.com/php-memcached-dev/php-memcached/archive/php7.tar.gz" ; fi \
 && mkdir -p memcached \
 && tar -C memcached -zxvf /tmp/memcached.tar.gz --strip 1 \
 && (cd memcached \
 && phpize \
 && ./configure \
 && make -j$( nproc ;) \
 && make install ) \
 && rm -r memcached \
 && rm /tmp/memcached.tar.gz \
 && docker-php-ext-enable memcached ; fi
# ##########################################################################
#  Exif:
# ##########################################################################
ARG INSTALL_EXIF=false
RUN if [ ${INSTALL_EXIF} = true ] ; then docker-php-ext-install exif ; fi
# ##########################################################################
#  PHP Aerospike:
# ##########################################################################
USER root
ARG INSTALL_AEROSPIKE=false
RUN set -xe ; if [ ${INSTALL_AEROSPIKE} = true ] ; then \
apt-get install --no-install-recommends sudo wget -y \
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
 && docker-php-ext-enable aerospike; else curl -L -o /tmp/aerospike-client-php.tar.gz https://github.com/aerospike/aerospike-client-php/archive/master.tar.gz ; fi \










 fi
# ##########################################################################
#  IonCube Loader:
# ##########################################################################
ARG INSTALL_IONCUBE=false
RUN if [ ${INSTALL_IONCUBE} = true ] ; then curl -L -o /tmp/ioncube_loaders_lin_x86-64.tar.gz https://downloads.ioncube.com/loader_downloads/ioncube_loaders_lin_x86-64.tar.gz \
 && tar zxpf /tmp/ioncube_loaders_lin_x86-64.tar.gz -C /tmp \
 && mv /tmp/ioncube/ioncube_loader_lin_${LARADOCK_PHP_VERSION}.so $( php -r "echo ini_get('extension_dir');" ;)/ioncube_loader.so \
 && printf "zend_extension=ioncube_loader.so\n" > $PHP_INI_DIR/conf.d/0ioncube.ini \
 && rm -rf /tmp/ioncube* ; fi
# ##########################################################################
#  Opcache:
# ##########################################################################
ARG INSTALL_OPCACHE=false
RUN if [ ${INSTALL_OPCACHE} = true ] ; then docker-php-ext-install opcache ; fi
#  Copy opcache configration
COPY ./opcache.ini /usr/local/etc/php/conf.d/opcache.ini
# ##########################################################################
#  Mysqli Modifications:
# ##########################################################################
ARG INSTALL_MYSQLI=false
RUN if [ ${INSTALL_MYSQLI} = true ] ; then docker-php-ext-install mysqli ; fi
# ##########################################################################
#  Human Language and Character Encoding Support:
# ##########################################################################
ARG INSTALL_INTL=false
RUN if [ ${INSTALL_INTL} = true ] ; then \
apt-get install --no-install-recommends zlib1g-dev libicu-dev g++ -y \
 && docker-php-ext-configure intl \
 && docker-php-ext-install intl; fi
# ##########################################################################
#  GHOSTSCRIPT:
# ##########################################################################
ARG INSTALL_GHOSTSCRIPT=false
RUN if [ ${INSTALL_GHOSTSCRIPT} = true ] ; then \
apt-get install --no-install-recommends poppler-utils ghostscript -y; fi
# ##########################################################################
#  LDAP:
# ##########################################################################
ARG INSTALL_LDAP=false
RUN if [ ${INSTALL_LDAP} = true ] ; then \
apt-get install --no-install-recommends libldap2-dev -y \
 && docker-php-ext-configure ldap --with-libdir=lib/x86_64-linux-gnu/ \
 && docker-php-ext-install ldap; fi
# ##########################################################################
#  SQL SERVER:
# ##########################################################################
ARG INSTALL_MSSQL=false
RUN set -eux ; if [ ${INSTALL_MSSQL} = true ] ; then \
if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then \
apt-get install --no-install-recommends freetds-dev libsybdb5 -y \
 && ln -s /usr/lib/x86_64-linux-gnu/libsybdb.so /usr/lib/libsybdb.so \
 && docker-php-ext-install mssql pdo_dblib \
 && php -m | grep -q 'mssql' \
 && php -m | grep -q 'pdo_dblib'; elif \







 [ $( php -r "echo PHP_MINOR_VERSION;" ;) = "0" ] ; then \
 apt-get install --no-install-recommends apt-transport-https gnupg -y \
 && curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
 && curl https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list \
 && apt-get update -yqq \
 && ACCEPT_EULA=Y apt-get --no-install-recommends install -y unixodbc unixodbc-dev libgss3 odbcinst msodbcsql17 locales \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && ln -sfn /etc/locale.alias /usr/share/locale/locale.alias \
 && locale-gen \
 && if [ $( php -r "echo PHP_MINOR_VERSION;" ;) = "0" ] ; then pecl install pdo_sqlsrv-5.3.0 sqlsrv-5.3.0 ; else pecl install pdo_sqlsrv sqlsrv ; fi \
 && docker-php-ext-enable pdo_sqlsrv sqlsrv \
 && php -m | grep -q 'pdo_sqlsrv' \
 && php -m | grep -q 'sqlsrv'; else pecl install pdo_sqlsrv sqlsrv ; fi \


 fi; elif \







 [ $( php -r "echo PHP_MINOR_VERSION;" ;) = "0" ] ; then \
 apt-get install --no-install-recommends apt-transport-https gnupg -y \
 && curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
 && curl https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list \
 && apt-get update -yqq \
 && ACCEPT_EULA=Y apt-get --no-install-recommends install -y unixodbc unixodbc-dev libgss3 odbcinst msodbcsql17 locales \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && ln -sfn /etc/locale.alias /usr/share/locale/locale.alias \
 && locale-gen \
 && if [ $( php -r "echo PHP_MINOR_VERSION;" ;) = "0" ] ; then pecl install pdo_sqlsrv-5.3.0 sqlsrv-5.3.0 ; else pecl install pdo_sqlsrv sqlsrv ; fi \
 && docker-php-ext-enable pdo_sqlsrv sqlsrv \
 && php -m | grep -q 'pdo_sqlsrv' \
 && php -m | grep -q 'sqlsrv'; else pecl install pdo_sqlsrv sqlsrv ; fi \


 fifi
# ##########################################################################
#  Image optimizers:
# ##########################################################################
USER root
ARG INSTALL_IMAGE_OPTIMIZERS=false
RUN if [ ${INSTALL_IMAGE_OPTIMIZERS} = true ] ; then \
apt-get install --no-install-recommends jpegoptim optipng pngquant gifsicle -y; fi
# ##########################################################################
#  ImageMagick:
# ##########################################################################
USER root
ARG INSTALL_IMAGEMAGICK=false
RUN if [ ${INSTALL_IMAGEMAGICK} = true ] ; then \
apt-get install --no-install-recommends libmagickwand-dev imagemagick -y \
 && pecl install imagick \
 && docker-php-ext-enable imagick; fi
# ##########################################################################
#  IMAP:
# ##########################################################################
ARG INSTALL_IMAP=false
RUN if [ ${INSTALL_IMAP} = true ] ; then \
apt-get install --no-install-recommends libc-client-dev libkrb5-dev -y \
 && rm -r /var/lib/apt/lists/* \
 && docker-php-ext-configure imap --with-kerberos --with-imap-ssl \
 && docker-php-ext-install imap; fi
# ##########################################################################
#  Calendar:
# ##########################################################################
USER root
ARG INSTALL_CALENDAR=false
RUN if [ ${INSTALL_CALENDAR} = true ] ; then docker-php-ext-configure calendar \
 && docker-php-ext-install calendar ; fi
# ##########################################################################
#  Phalcon:
# ##########################################################################
ARG INSTALL_PHALCON=false
ARG LARADOCK_PHALCON_VERSION
ENV LARADOCK_PHALCON_VERSION="${LARADOCK_PHALCON_VERSION}"
#  Copy phalcon configration
COPY ./phalcon.ini /usr/local/etc/php/conf.d/phalcon.ini.disable
RUN if [ $INSTALL_PHALCON = true ] ; then \
apt-get update \
 && apt-get install --no-install-recommends unzip libpcre3-dev gcc make re2c -y \
 && curl -L -o /tmp/cphalcon.zip https://github.com/phalcon/cphalcon/archive/v${LARADOCK_PHALCON_VERSION}.zip \
 && unzip -d /tmp/ /tmp/cphalcon.zip \
 && cd /tmp/cphalcon-${LARADOCK_PHALCON_VERSION}/build \
 && ./install \
 && mv /usr/local/etc/php/conf.d/phalcon.ini.disable /usr/local/etc/php/conf.d/phalcon.ini \
 && rm -rf /tmp/cphalcon*; fi
# ##########################################################################
#  APCU:
# ##########################################################################
ARG INSTALL_APCU=false
RUN if [ ${INSTALL_APCU} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install -a apcu-4.0.11 ; else pecl install apcu ; fi \
 && docker-php-ext-enable apcu ; fi
# ##########################################################################
#  YAML:
# ##########################################################################
USER root
ARG INSTALL_YAML=false
RUN if [ ${INSTALL_YAML} = true ] ; then \
apt-get install --no-install-recommends libyaml-dev -y; if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install -a yaml-1.3.2 ; else pecl install yaml ; fi \
 && docker-php-ext-enable yaml ; else pecl install yaml ; fi \
 fi
# ##########################################################################
#  RDKAFKA:
# ##########################################################################
ARG INSTALL_RDKAFKA=false
RUN if [ ${INSTALL_RDKAFKA} = true ] ; then \
apt-get install --no-install-recommends librdkafka-dev -y \
 && pecl install rdkafka \
 && docker-php-ext-enable rdkafka; fi
# ##########################################################################
#  Install additional locales:
# ##########################################################################
ARG INSTALL_ADDITIONAL_LOCALES=false
ARG ADDITIONAL_LOCALES
RUN if [ ${INSTALL_ADDITIONAL_LOCALES} = true ] ; then \
apt-get install --no-install-recommends locales -y \
 && echo '' >> /usr/share/locale/locale.alias \
 && temp="${ADDITIONAL_LOCALES%\"}" \
 && temp="${temp#\"}" \
 && for i in ${temp}; do sed -i "/$i/s/^#//g" /etc/locale.gen ; done \
 && locale-gen; fi
# ##########################################################################
#  MySQL Client:
# ##########################################################################
USER root
ARG INSTALL_MYSQL_CLIENT=false
RUN if [ ${INSTALL_MYSQL_CLIENT} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends mysql-client -y; fi
# ##########################################################################
#  ping:
# ##########################################################################
USER root
ARG INSTALL_PING=false
RUN if [ ${INSTALL_PING} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends inetutils-ping -y; fi
# ##########################################################################
#  sshpass:
# ##########################################################################
USER root
ARG INSTALL_SSHPASS=false
RUN if [ ${INSTALL_SSHPASS} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends sshpass -y; fi
# ##########################################################################
#  FFMPEG:
# ##########################################################################
USER root
ARG INSTALL_FFMPEG=false
RUN if [ ${INSTALL_FFMPEG} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends ffmpeg -y; fi
# ##########################################################################
#  Check PHP version:
# ##########################################################################
RUN set -xe ; php -v | head -n 1 | grep -q "PHP ${LARADOCK_PHP_VERSION}."
#
# --------------------------------------------------------------------------
#  Final Touch
# --------------------------------------------------------------------------
#
COPY ./laravel.ini /usr/local/etc/php/conf.d
COPY ./xlaravel.pool.conf /usr/local/etc/php-fpm.d/
USER root
ARG PUID=1000
ENV PUID="${PUID}"
#  Clean up
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && rm /var/log/lastlog /var/log/faillog
RUN usermod -u ${PUID} www-data
#  Adding the faketime library to the preload file needs to be done last
#  otherwise it will preload it for all commands that follow in this file
RUN if [ ${INSTALL_FAKETIME} = true ] ; then echo "/usr/lib/x86_64-linux-gnu/faketime/libfaketime.so.1" > /etc/ld.so.preload; fi
WORKDIR /var/www
CMD ["php-fpm"]
EXPOSE 9000/tcp
