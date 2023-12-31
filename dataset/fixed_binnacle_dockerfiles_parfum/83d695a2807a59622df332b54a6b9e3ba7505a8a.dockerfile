#
# --------------------------------------------------------------------------
#  Image Setup
# --------------------------------------------------------------------------
#
#  To edit the 'php-fpm' base Image, visit its repository on Github
#     https://github.com/LaraDock/php-fpm
#
#  To change its version, see the available Tags on the Docker Hub:
#     https://hub.docker.com/r/laradock/php-fpm/tags/
#
FROM laradock/php-fpm:5.6--1.2
MAINTAINER Mahmoud Zalt <mahmoud@zalt.me>
#
# --------------------------------------------------------------------------
#  Mandatory Software's Installation
# --------------------------------------------------------------------------
#
#  Mandatory Software's such as ("mcrypt", "pdo_mysql", "libssl-dev", ....)
#  are installed on the base image 'laradock/php-fpm' image. If you want
#  to add more Software's or remove existing one, you need to edit the
#  base image (https://github.com/LaraDock/php-fpm).
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
#  xDebug:
# ####################################
ARG INSTALL_XDEBUG=true
RUN if [ ${INSTALL_XDEBUG} = true ] ; then pecl install xdebug \
 && docker-php-ext-enable xdebug ; fi
#  Copy xdebug configration for remote debugging
COPY ./xdebug.ini /usr/local/etc/php/conf.d/xdebug.ini
# ####################################
#  MongoDB:
# ####################################
ARG INSTALL_MONGO=true
RUN if [ ${INSTALL_MONGO} = true ] ; then pecl install mongodb \
 && docker-php-ext-enable mongodb ; fi
# ####################################
#  ZipArchive:
# ####################################
ARG INSTALL_ZIP_ARCHIVE=true
RUN if [ ${INSTALL_ZIP_ARCHIVE} = true ] ; then pecl install zip \
 && docker-php-ext-enable zip ; fi
# ####################################
#  PHP Memcached:
# ####################################
ARG INSTALL_MEMCACHED=true
RUN if [ ${INSTALL_MEMCACHED} = true ] ; then pecl install memcached \
 && docker-php-ext-enable memcached ; fi
# ####################################
#  PHP Aerospike:
# ####################################
ARG INSTALL_AEROSPIKE_EXTENSION=true
ENV INSTALL_AEROSPIKE_EXTENSION="${INSTALL_AEROSPIKE_EXTENSION}"
#  Copy aerospike configration for remote debugging
COPY ./aerospike.ini /usr/local/etc/php/conf.d/aerospike.ini
RUN if [ ${INSTALL_AEROSPIKE_EXTENSION} = true ] ; then curl -L -o /tmp/aerospike-client-php.tar.gz "https://github.com/luciano-jr/aerospike-client-php/archive/master.tar.gz" \
 && mkdir -p aerospike-client-php \
 && tar -C aerospike-client-php -zxvf /tmp/aerospike-client-php.tar.gz --strip 1 \
 && (cd aerospike-client-php/src/aerospike \
 && phpize \
 && ./build.sh \
 && make install ) \
 && rm /tmp/aerospike-client-php.tar.gz ; fi
# ####################################
#  Opcache:
# ####################################
ARG INSTALL_OPCACHE=true
RUN if [ ${INSTALL_OPCACHE} = true ] ; then docker-php-ext-install opcache \
 && docker-php-ext-enable opcache ; fi
#  Copy opcache configration
COPY ./opcache.ini /usr/local/etc/php/conf.d/opcache.ini
# ####################################
#  Gem and sass:
# ####################################
#  Check if gem and sass needs to be installed
ARG INSTALL_GEM_AND_SASS=true
ENV INSTALL_GEM_AND_SASS="${INSTALL_GEM_AND_SASS}"
RUN if [ ${INSTALL_GEM_AND_SASS} = true ] ; then \
apt-get update \
 && apt-get install --no-install-recommends rubygems -y \
 && gem install sass; fi
#
# --------------------------------------------------------------------------
#  Final Touch
# --------------------------------------------------------------------------
#
#  Clean up
USER root
RUN apt-get clean \
 && rm -rf /tmp/* /var/tmp/*
COPY ./laravel.ini /usr/local/etc/php/conf.d
COPY ./laravel.pool.conf /usr/local/etc/php-fpm.d/
RUN rm -r /var/lib/apt/lists/*
RUN usermod -u 1000 www-data
WORKDIR /var/www
CMD ["php-fpm"]
EXPOSE 9000/tcp
