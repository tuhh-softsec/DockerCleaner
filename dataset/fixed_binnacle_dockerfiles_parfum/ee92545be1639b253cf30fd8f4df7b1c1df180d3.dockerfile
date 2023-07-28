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
FROM ydatech/php-fpm:1.12-71
MAINTAINER Mahmoud Zalt <mahmoud@zalt.me>
#
# --------------------------------------------------------------------------
#  Mandatory Software's Installation
# --------------------------------------------------------------------------
#
#  Mandatory Software's such as ("mcrypt", "pdo_mysql", "libssl-dev", ....)
#  are installed on the base image 'laradock/php-fpm' image. If you want
#  to add more Software's or remove existing one, you need to edit the
#  base image (https://github.com/Laradock/php-fpm).
#
#
# --------------------------------------------------------------------------
#  Optional Software's Installation
# --------------------------------------------------------------------------
#
#  Optional Software's will only be installed if you set them to `true`
#  in the `docker-compose.yml` before the build.
#  Example:
#    - INSTALL_ZIP_ARCHIVE=true
#
# ####################################
#  SOAP:
# ####################################
ARG INSTALL_SOAP=false
RUN if [ ${INSTALL_SOAP} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends libxml2-dev php-soap -y \
 && docker-php-ext-install soap; fi
# ####################################
#  pgsql
# ####################################
ARG INSTALL_PGSQL=false
RUN if [ ${INSTALL_PGSQL} = true ] ; then apt-get update -yqq \
 && docker-php-ext-install pgsql ; fi
# ####################################
#  pgsql client
# ####################################
ARG INSTALL_PG_CLIENT=false
RUN if [ ${INSTALL_PG_CLIENT} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends postgresql-client -y; fi
# ####################################
#  xDebug:
# ####################################
ARG INSTALL_XDEBUG=false
RUN if [ ${INSTALL_XDEBUG} = true ] ; then pecl install xdebug \
 && docker-php-ext-enable xdebug ; fi
#  Copy xdebug configuration for remote debugging
COPY ./xdebug.ini /usr/local/etc/php/conf.d/xdebug.ini
# ####################################
#  Blackfire:
# ####################################
ARG INSTALL_BLACKFIRE=false
RUN if [ ${INSTALL_XDEBUG} = false -a ${INSTALL_BLACKFIRE} = true ] ; then version=$( php -r "echo PHP_MAJOR_VERSION.PHP_MINOR_VERSION;" ;) \
 && curl -A "Docker" -o /tmp/blackfire-probe.tar.gz -D - -L -s https://blackfire.io/api/v1/releases/probe/php/linux/amd64/$version \
 && tar zxpf /tmp/blackfire-probe.tar.gz -C /tmp \
 && mv /tmp/blackfire-*.so $( php -r "echo ini_get('extension_dir');" ;)/blackfire.so \
 && printf "extension=blackfire.so\nblackfire.agent_socket=tcp://blackfire:8707\n" > $PHP_INI_DIR/conf.d/blackfire.ini; fi
# ####################################
#  PHP REDIS EXTENSION FOR PHP 7.0
# ####################################
ARG INSTALL_PHPREDIS=false
RUN if [ ${INSTALL_PHPREDIS} = true ] ; then printf "\n" | pecl install -o -f redis \
 && rm -rf /tmp/pear \
 && docker-php-ext-enable redis ; fi
# ####################################
#  Swoole EXTENSION FOR PHP 7
# ####################################
ARG INSTALL_SWOOLE=false
RUN if [ ${INSTALL_SWOOLE} = true ] ; then pecl install swoole \
 && docker-php-ext-enable swoole ; fi
# ####################################
#  MongoDB:
# ####################################
ARG INSTALL_MONGO=false
RUN if [ ${INSTALL_MONGO} = true ] ; then pecl install mongodb \
 && docker-php-ext-enable mongodb ; fi
# ####################################
#  AMQP:
# ####################################
ARG INSTALL_AMQP=false
RUN if [ ${INSTALL_AMQP} = true ] ; then \
apt-get update \
 && apt-get install --no-install-recommends librabbitmq-dev -y \
 && pecl install amqp \
 && docker-php-ext-enable amqp; fi
# ####################################
#  ZipArchive:
# ####################################
ARG INSTALL_ZIP_ARCHIVE=false
RUN if [ ${INSTALL_ZIP_ARCHIVE} = true ] ; then docker-php-ext-install zip ; fi
# ####################################
#  bcmath:
# ####################################
ARG INSTALL_BCMATH=false
RUN if [ ${INSTALL_BCMATH} = true ] ; then docker-php-ext-install bcmath ; fi
# ####################################
#  GMP (GNU Multiple Precision):
# ####################################
ARG INSTALL_GMP=false
RUN if [ ${INSTALL_GMP} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends libgmp-dev -y \
 && docker-php-ext-install gmp; fi
# ####################################
#  PHP Memcached:
# ####################################
ARG INSTALL_MEMCACHED=false
RUN if [ ${INSTALL_MEMCACHED} = true ] ; then curl -L -o /tmp/memcached.tar.gz "https://github.com/php-memcached-dev/php-memcached/archive/php7.tar.gz" \
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
# ####################################
#  Exif:
# ####################################
ARG INSTALL_EXIF=false
RUN if [ ${INSTALL_EXIF} = true ] ; then docker-php-ext-install exif ; fi
# ####################################
#  PHP Aerospike:
# ####################################
ARG INSTALL_AEROSPIKE=false
ENV INSTALL_AEROSPIKE="${INSTALL_AEROSPIKE}"
#  Copy aerospike configration for remote debugging
COPY ./aerospike.ini /usr/local/etc/php/conf.d/aerospike.ini
RUN if [ ${INSTALL_AEROSPIKE} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends sudo wget -y \
 && curl -L -o /tmp/aerospike-client-php.tar.gz "https://github.com/aerospike/aerospike-client-php/archive/master.tar.gz" \
 && mkdir -p aerospike-client-php \
 && tar -C aerospike-client-php -zxvf /tmp/aerospike-client-php.tar.gz --strip 1 \
 && (cd aerospike-client-php/src \
 && phpize \
 && ./build.sh \
 && make install ) \
 && rm /tmp/aerospike-client-php.tar.gz; fi
RUN if [ ${INSTALL_AEROSPIKE} = false ] ; then rm /usr/local/etc/php/conf.d/aerospike.ini ; fi
# ####################################
#  Opcache:
# ####################################
ARG INSTALL_OPCACHE=false
RUN if [ ${INSTALL_OPCACHE} = true ] ; then docker-php-ext-install opcache ; fi
#  Copy opcache configration
COPY ./opcache.ini /usr/local/etc/php/conf.d/opcache.ini
# ####################################
#  Mysqli Modifications:
# ####################################
ARG INSTALL_MYSQLI=false
RUN if [ ${INSTALL_MYSQLI} = true ] ; then docker-php-ext-install mysqli ; fi
# ####################################
#  Tokenizer Modifications:
# ####################################
ARG INSTALL_TOKENIZER=false
RUN if [ ${INSTALL_TOKENIZER} = true ] ; then docker-php-ext-install tokenizer ; fi
# ####################################
#  Human Language and Character Encoding Support:
# ####################################
ARG INSTALL_INTL=false
RUN if [ ${INSTALL_INTL} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends zlib1g-dev libicu-dev g++ -y \
 && docker-php-ext-configure intl \
 && docker-php-ext-install intl; fi
# ####################################
#  GHOSTSCRIPT:
# ####################################
ARG INSTALL_GHOSTSCRIPT=false
RUN if [ ${INSTALL_GHOSTSCRIPT} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends poppler-utils ghostscript -y; fi
# ####################################
#  LDAP:
# ####################################
ARG INSTALL_LDAP=false
RUN if [ ${INSTALL_LDAP} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends libldap2-dev -y \
 && docker-php-ext-configure ldap --with-libdir=lib/x86_64-linux-gnu/ \
 && docker-php-ext-install ldap; fi
# ####################################
#  SQL SERVER:
# ####################################
ARG INSTALL_MSSQL=false
ENV INSTALL_MSSQL="${INSTALL_MSSQL}"
RUN if [ ${INSTALL_MSSQL} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends apt-transport-https -y \
 && curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
 && curl https://packages.microsoft.com/config/debian/8/prod.list > /etc/apt/sources.list.d/mssql-release.list \
 && apt-get update -yqq \
 && ACCEPT_EULA=Y apt-get --no-install-recommends install -y unixodbc unixodbc-dev libgss3 odbcinst msodbcsql locales \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && pecl install pdo_sqlsrv-4.1.8preview sqlsrv-4.1.8preview \
 && docker-php-ext-enable pdo_sqlsrv sqlsrv; fi
# ####################################
#  Image optimizers:
# ####################################
USER root
ARG INSTALL_IMAGE_OPTIMIZERS=false
ENV INSTALL_IMAGE_OPTIMIZERS="${INSTALL_IMAGE_OPTIMIZERS}"
RUN if [ ${INSTALL_IMAGE_OPTIMIZERS} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends jpegoptim optipng pngquant gifsicle -y --force-yes; fi
# ####################################
#  ImageMagick:
# ####################################
USER root
ARG INSTALL_IMAGEMAGICK=false
ENV INSTALL_IMAGEMAGICK="${INSTALL_IMAGEMAGICK}"
RUN if [ ${INSTALL_IMAGEMAGICK} = true ] ; then \
apt-get update -y \
 && apt-get install --no-install-recommends libmagickwand-dev imagemagick -y \
 && pecl install imagick \
 && docker-php-ext-enable imagick; fi
#
# --------------------------------------------------------------------------
#  Final Touch
# --------------------------------------------------------------------------
#
COPY ./laravel.ini /usr/local/etc/php/conf.d
COPY ./yii.ini /usr/local/etc/php/conf.d
COPY ./xlaravel.pool.conf /usr/local/etc/php-fpm.d/
# RUN rm -r /var/lib/apt/lists/*
RUN usermod -u 1000 www-data
WORKDIR /var/www
CMD ["php-fpm"]
EXPOSE 9000/tcp
