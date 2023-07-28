#   基础镜像从 laradock 仓库中获取
ARG PHP_VERSION=${PHP_VERSION}
FROM laradock/php-fpm:2.2-${PHP_VERSION}
LABEL maintainer="Syncher <syncviip@gmail.com>"
#   Set Environment Variables
ENV DEBIAN_FRONTEND="noninteractive"
#   always run apt update when start and after add new source list, then clean up at end.
RUN apt-get update -yqq \
 && apt-get install --no-install-recommends apt-utils -y \
 && pecl channel-update pecl.php.net
#
#  --------------------------------------------------------------------------
#   PHP 扩展，在 docker-comopose.yml 中设置 INSTALL_XXX = true 再 build 即可安装
#  --------------------------------------------------------------------------
#
#  ##########################################################################
#   SOAP:
#  ##########################################################################
ARG INSTALL_SOAP=false
RUN if [ ${INSTALL_SOAP} = true ] ; then rm /etc/apt/preferences.d/no-debian-php \
 && apt-get install --no-install-recommends libxml2-dev php-soap -y \
 && docker-php-ext-install soap ; fi
#  ##########################################################################
#   xDebug:
#  ##########################################################################
ARG INSTALL_XDEBUG=false
RUN if [ ${INSTALL_XDEBUG} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install xdebug-2.5.5 ; else pecl install xdebug ; fi \
 && docker-php-ext-enable xdebug ; fi
#   Copy xdebug configuration for remote debugging
COPY ./xdebug.ini /usr/local/etc/php/conf.d/xdebug.ini
RUN sed -i "s/xdebug.remote_autostart=0/xdebug.remote_autostart=1/" /usr/local/etc/php/conf.d/xdebug.ini \
 && sed -i "s/xdebug.remote_enable=0/xdebug.remote_enable=1/" /usr/local/etc/php/conf.d/xdebug.ini \
 && sed -i "s/xdebug.cli_color=0/xdebug.cli_color=1/" /usr/local/etc/php/conf.d/xdebug.ini
#  ##########################################################################
#   Phpdbg:
#  ##########################################################################
ARG INSTALL_PHPDBG=false
ARG PHP_VERSION=${PHP_VERSION}
RUN if [ ${INSTALL_PHPDBG} = true ] ; then apt-get install --no-install-recommends php${PHP_VERSION}-phpdbg -y --force-yes ; fi
#  ##########################################################################
#   Blackfire:
#  ##########################################################################
ARG INSTALL_BLACKFIRE=false
RUN if [ ${INSTALL_XDEBUG} = false -a ${INSTALL_BLACKFIRE} = true ] ; then version=$( php -r "echo PHP_MAJOR_VERSION.PHP_MINOR_VERSION;" ;) \
 && curl -A "Docker" -o /tmp/blackfire-probe.tar.gz -D - -L -s https://blackfire.io/api/v1/releases/probe/php/linux/amd64/$version \
 && tar zxpf /tmp/blackfire-probe.tar.gz -C /tmp \
 && mv /tmp/blackfire-*.so $( php -r "echo ini_get('extension_dir');" ;)/blackfire.so \
 && printf "extension=blackfire.so\nblackfire.agent_socket=tcp://blackfire:8707\n" > $PHP_INI_DIR/conf.d/blackfire.ini; fi
#  ##########################################################################
#   PHP REDIS EXTENSION
#  ##########################################################################
ARG INSTALL_PHPREDIS=false
RUN if [ ${INSTALL_PHPREDIS} = true ] ; then printf "\n" | pecl install -o -f redis \
 && rm -rf /tmp/pear \
 && docker-php-ext-enable redis ; fi
#  ##########################################################################
#   Swoole EXTENSION
#  ##########################################################################
ARG INSTALL_SWOOLE=false
RUN if [ ${INSTALL_SWOOLE} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install swoole-2.0.11 ; else if [ $( php -r "echo PHP_MINOR_VERSION;" ;) = "0" ] ; then pecl install swoole-2.2.0 ; else pecl install swoole ; fi ; fi \
 && docker-php-ext-enable swoole ; fi
#  ##########################################################################
#   MongoDB:
#  ##########################################################################
ARG INSTALL_MONGO=false
RUN if [ ${INSTALL_MONGO} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl install mongo \
 && docker-php-ext-enable mongo ; fi \
 && pecl install mongodb \
 && docker-php-ext-enable mongodb ; fi
#  ##########################################################################
#   AMQP:
#  ##########################################################################
ARG INSTALL_AMQP=false
RUN if [ ${INSTALL_AMQP} = true ] ; then apt-get install --no-install-recommends librabbitmq-dev -y \
 && pecl install amqp \
 && docker-php-ext-enable amqp ; fi
#  ##########################################################################
#   ZipArchive:
#  ##########################################################################
ARG INSTALL_ZIP_ARCHIVE=false
RUN if [ ${INSTALL_ZIP_ARCHIVE} = true ] ; then apt-get install --no-install-recommends libzip-dev -y \
 && docker-php-ext-configure zip --with-libzip \
 && docker-php-ext-install zip ; fi
#  ##########################################################################
#   pcntl
#  ##########################################################################
ARG INSTALL_PCNTL=false
RUN if [ ${INSTALL_PCNTL} = true ] ; then docker-php-ext-install pcntl ; fi
#  ##########################################################################
#   bcmath:
#  ##########################################################################
ARG INSTALL_BCMATH=false
RUN if [ ${INSTALL_BCMATH} = true ] ; then docker-php-ext-install bcmath ; fi
#  ##########################################################################
#   GMP (GNU Multiple Precision):
#  ##########################################################################
ARG INSTALL_GMP=false
RUN if [ ${INSTALL_GMP} = true ] ; then apt-get install --no-install-recommends libgmp-dev -y \
 && if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then ln -s /usr/include/x86_64-linux-gnu/gmp.h /usr/include/gmp.h ; fi \
 && docker-php-ext-install gmp ; fi
#  ##########################################################################
#   PHP Memcached:
#  ##########################################################################
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
#  ##########################################################################
#   Exif:
#  ##########################################################################
ARG INSTALL_EXIF=false
RUN if [ ${INSTALL_EXIF} = true ] ; then docker-php-ext-install exif ; fi
#  ##########################################################################
#   PHP Aerospike:
#  ##########################################################################
USER root
ARG INSTALL_AEROSPIKE=false
ARG AEROSPIKE_PHP_REPOSITORY
RUN if [ ${INSTALL_AEROSPIKE} = true ] ; then apt-get install --no-install-recommends sudo wget -y \
 && if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then curl -L -o /tmp/aerospike-client-php.tar.gz https://github.com/aerospike/aerospike-client-php5/archive/master.tar.gz ; else curl -L -o /tmp/aerospike-client-php.tar.gz ${AEROSPIKE_PHP_REPOSITORY} ; fi \
 && mkdir -p aerospike-client-php \
 && tar -C aerospike-client-php -zxvf /tmp/aerospike-client-php.tar.gz --strip 1 \
 && if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then (cd aerospike-client-php/src/aerospike \
 && phpize \
 && ./build.sh \
 && make install ) ; else (cd aerospike-client-php/src \
 && phpize \
 && ./build.sh \
 && make install ) ; fi \
 && rm /tmp/aerospike-client-php.tar.gz \
 && docker-php-ext-enable aerospike ; fi
#  ##########################################################################
#   Opcache:
#  ##########################################################################
ARG INSTALL_OPCACHE=false
RUN if [ ${INSTALL_OPCACHE} = true ] ; then docker-php-ext-install opcache ; fi
#   Copy opcache configration
COPY ./opcache.ini /usr/local/etc/php/conf.d/opcache.ini
#  ##########################################################################
#   Mysqli Modifications:
#  ##########################################################################
ARG INSTALL_MYSQLI=false
RUN if [ ${INSTALL_MYSQLI} = true ] ; then docker-php-ext-install mysqli ; fi
#  ##########################################################################
#   Tokenizer Modifications:
#  ##########################################################################
ARG INSTALL_TOKENIZER=false
RUN if [ ${INSTALL_TOKENIZER} = true ] ; then docker-php-ext-install tokenizer ; fi
#  ##########################################################################
#   Human Language and Character Encoding Support:
#  ##########################################################################
ARG INSTALL_INTL=false
RUN if [ ${INSTALL_INTL} = true ] ; then apt-get install --no-install-recommends zlib1g-dev libicu-dev g++ -y \
 && docker-php-ext-configure intl \
 && docker-php-ext-install intl ; fi
#  ##########################################################################
#   GHOSTSCRIPT:
#  ##########################################################################
ARG INSTALL_GHOSTSCRIPT=false
RUN if [ ${INSTALL_GHOSTSCRIPT} = true ] ; then apt-get install --no-install-recommends poppler-utils ghostscript -y ; fi
#  ##########################################################################
#   LDAP:
#  ##########################################################################
ARG INSTALL_LDAP=false
RUN if [ ${INSTALL_LDAP} = true ] ; then apt-get install --no-install-recommends libldap2-dev -y \
 && docker-php-ext-configure ldap --with-libdir=lib/x86_64-linux-gnu/ \
 && docker-php-ext-install ldap ; fi
#  ##########################################################################
#   Image optimizers:
#  ##########################################################################
USER root
ARG INSTALL_IMAGE_OPTIMIZERS=false
RUN if [ ${INSTALL_IMAGE_OPTIMIZERS} = true ] ; then apt-get install --no-install-recommends jpegoptim optipng pngquant gifsicle -y ; fi
#  ##########################################################################
#   ImageMagick:
#  ##########################################################################
USER root
ARG INSTALL_IMAGEMAGICK=false
RUN if [ ${INSTALL_IMAGEMAGICK} = true ] ; then apt-get install --no-install-recommends libmagickwand-dev imagemagick -y \
 && pecl install imagick \
 && docker-php-ext-enable imagick ; fi
#  ##########################################################################
#   IMAP:
#  ##########################################################################
ARG INSTALL_IMAP=false
RUN if [ ${INSTALL_IMAP} = true ] ; then apt-get install --no-install-recommends libc-client-dev libkrb5-dev -y \
 && rm -r /var/lib/apt/lists/* \
 && docker-php-ext-configure imap --with-kerberos --with-imap-ssl \
 && docker-php-ext-install imap ; fi
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
COPY ./laravel.ini /usr/local/etc/php/conf.d
COPY ./xlaravel.pool.conf /usr/local/etc/php-fpm.d/
USER root
#   Clean up
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && rm /var/log/lastlog /var/log/faillog
RUN usermod -u 1000 www-data
WORKDIR /var/www
CMD ["php-fpm"]
EXPOSE 9000/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
