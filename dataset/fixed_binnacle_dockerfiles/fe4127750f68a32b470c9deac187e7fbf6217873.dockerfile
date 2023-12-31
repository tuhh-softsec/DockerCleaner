#
#  --------------------------------------------------------------------------
#   Image Setup
#  --------------------------------------------------------------------------
#
#   To edit the 'php-fpm' base Image, visit its repository on Github
#      https://github.com/LaraDock/php-fpm
#
#   To change its version, see the available Tags on the Docker Hub:
#      https://hub.docker.com/r/laradock/php-fpm/tags/
#
FROM laradock/php-fpm:7.0--1.2
MAINTAINER Mahmoud Zalt <mahmoud@zalt.me>
#
#  --------------------------------------------------------------------------
#   Mandatory Software's Installation
#  --------------------------------------------------------------------------
#
#   Mandatory Software's such as ("mcrypt", "pdo_mysql", "libssl-dev", ....)
#   are installed on the base image 'laradock/php-fpm' image. If you want
#   to add more Software's or remove existing one, you need to edit the
#   base image (https://github.com/LaraDock/php-fpm).
#
#
#  --------------------------------------------------------------------------
#   Optional Software's Installation
#  --------------------------------------------------------------------------
#
#   Optional Software's will only be installed if you set them to `true`
#   in the `docker-compose.yml` before the build.
#   Example:
#     - INSTALL_ZIP_ARCHIVE=true
#
#  ####################################
#   xDebug:
#  ####################################
ARG INSTALL_XDEBUG=true
ENV INSTALL_XDEBUG="${INSTALL_XDEBUG}"
RUN if [ ${INSTALL_XDEBUG} = true ] ; then pecl install xdebug \
 && docker-php-ext-enable xdebug ; fi
#   Copy xdebug configration for remote debugging
COPY ./xdebug.ini /usr/local/etc/php/conf.d/xdebug.ini
#  ####################################
#   MongoDB:
#  ####################################
ARG INSTALL_MONGO=true
ENV INSTALL_MONGO="${INSTALL_MONGO}"
RUN if [ ${INSTALL_MONGO} = true ] ; then pecl install mongodb \
 && docker-php-ext-enable mongodb ; fi
#  ####################################
#   ZipArchive:
#  ####################################
ARG INSTALL_ZIP_ARCHIVE=true
ENV INSTALL_ZIP_ARCHIVE="${INSTALL_ZIP_ARCHIVE}"
RUN if [ ${INSTALL_ZIP_ARCHIVE} = true ] ; then pecl install zip \
 && docker-php-ext-enable zip ; fi
#  ####################################
#   PHP Memcached:
#  ####################################
ARG INSTALL_MEMCACHED=true
ENV INSTALL_MEMCACHED="${INSTALL_MEMCACHED}"
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
#  ####################################
#   Opcache:
#  ####################################
ARG INSTALL_OPCACHE=true
ENV INSTALL_OPCACHE="${INSTALL_OPCACHE}"
RUN if [ ${INSTALL_OPCACHE} = true ] ; then docker-php-ext-install opcache \
 && docker-php-ext-enable opcache ; fi
#
#  --------------------------------------------------------------------------
#   Final Touch
#  --------------------------------------------------------------------------
#
COPY ./laravel.ini /usr/local/etc/php/conf.d
COPY ./laravel.pool.conf /usr/local/etc/php-fpm.d/
RUN rm -r /var/lib/apt/lists/*
RUN usermod -u 1000 www-data
WORKDIR /var/www/laravel
CMD ["php-fpm"]
EXPOSE 9000/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
