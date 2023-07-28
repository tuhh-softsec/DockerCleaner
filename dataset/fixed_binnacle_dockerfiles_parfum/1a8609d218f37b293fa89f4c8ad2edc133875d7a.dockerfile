ARG PHP_VERSION
FROM php:${PHP_VERSION}-fpm
# --------------------------------------------------------------------------
#  必需安装项
# --------------------------------------------------------------------------
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends curl libmemcached-dev libz-dev libpq-dev libjpeg-dev libpng-dev libfreetype6-dev libssl-dev libmcrypt-dev -y \
 && rm -rf /var/lib/apt/lists/*
#  Install the PHP pdo_mysql extention
RUN docker-php-ext-install pdo_mysql \
 && docker-php-ext-install pdo_pgsql \
 && docker-php-ext-configure gd --with-jpeg-dir=/usr/lib --with-freetype-dir=/usr/include/freetype2 \
 && docker-php-ext-install gd
#  always run apt update when start and after add new source list, then clean up at end.
RUN set -xe ; apt-get update -yqq \
 && pecl channel-update pecl.php.net \
 && apt-get install --no-install-recommends apt-utils libzip-dev zip unzip -yqq \
 && docker-php-ext-configure zip --with-libzip \
 && docker-php-ext-install zip \
 && php -m | grep -q 'zip'
ENV DEBIAN_FRONTEND="noninteractive"
# --------------------------------------------------------------------------
#  可选安装项
# --------------------------------------------------------------------------
# ####################################
#  SOAP:
# ####################################
ARG INSTALL_SOAP=false
RUN if [ ${INSTALL_SOAP} = true ] ; then \
rm /etc/apt/preferences.d/no-debian-php \
 && apt-get install --no-install-recommends libxml2-dev php-soap -y \
 && docker-php-ext-install soap; fi
# ####################################
#  xDebug:
# ####################################
ARG INSTALL_XDEBUG=false
RUN if [ ${INSTALL_XDEBUG} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install xdebug-2.5.5 ; else pecl install xdebug ; fi \
 && docker-php-ext-enable xdebug ; fi
COPY ./xdebug.ini /usr/local/etc/php/conf.d/xdebug.ini
RUN sed -i "s/xdebug.remote_autostart=0/xdebug.remote_autostart=1/" /usr/local/etc/php/conf.d/xdebug.ini \
 && sed -i "s/xdebug.remote_enable=0/xdebug.remote_enable=1/" /usr/local/etc/php/conf.d/xdebug.ini \
 && sed -i "s/xdebug.cli_color=0/xdebug.cli_color=1/" /usr/local/etc/php/conf.d/xdebug.ini
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
# ####################################
#  ZipArchive:
# ####################################
ARG INSTALL_ZIP_ARCHIVE=false
RUN if [ ${INSTALL_ZIP_ARCHIVE} = true ] ; then docker-php-ext-install zip \
 && docker-php-ext-install zip ; fi
# ####################################
#  PHP Memcached:
# ####################################
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
# ####################################
#  MongoDB:
# ####################################
ARG INSTALL_MONGO=false
RUN if [ ${INSTALL_MONGO} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install mongo \
 && docker-php-ext-enable mongo ; fi \
 && pecl install mongodb \
 && docker-php-ext-enable mongodb ; fi
# ##########################################################################
#  Opcache:
# ##########################################################################
ARG INSTALL_OPCACHE=false
RUN if [ ${INSTALL_OPCACHE} = true ] ; then docker-php-ext-install opcache ; fi
COPY ./opcache.ini /usr/local/etc/php/conf.d/opcache.ini
# ##########################################################################
#  Check PHP version:
# ##########################################################################
RUN set -xe ; php -v | head -n 1 | grep -q "PHP ${PHP_VERSION}."
#
# --------------------------------------------------------------------------
#  Final Touch
# --------------------------------------------------------------------------
#
COPY ./site.ini /usr/local/etc/php/conf.d
COPY ./site.pool.conf /usr/local/etc/php-fpm.d/
USER root
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && rm /var/log/lastlog /var/log/faillog
RUN usermod -u 1000 www-data
WORKDIR /var/www
CMD ["php-fpm"]
EXPOSE 9000/tcp
