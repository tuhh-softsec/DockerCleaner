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
FROM laradock/php-fpm:1.4-56
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
#  PHP GMP
# ####################################
ARG INSTALL_GMP=false
ENV INSTALL_GMP="${INSTALL_GMP}"
RUN if [ ${INSTALL_GMP} = true ] ; then \
apt-get update -yqq \
 && apt-get install --no-install-recommends libgmp-dev -y \
 && docker-php-ext-configure gmp \
 && docker-php-ext-install gmp \
 && docker-php-ext-enable gmp; fi
# ####################################
#  xDebug:
# ####################################
ARG INSTALL_XDEBUG=false
RUN if [ ${INSTALL_XDEBUG} = true ] ; then \
apt-get install --no-install-recommends php5-xdebug -y \
 && echo "zend_extension=/usr/lib/php5/20131226/xdebug.so" > /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini; fi
#  Copy xdebug configration for remote debugging
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
#  PHP REDIS EXTENSION FOR PHP 5
# ####################################
ARG INSTALL_PHPREDIS=false
RUN if [ ${INSTALL_PHPREDIS} = true ] ; then pecl install -o -f redis \
 && rm -rf /tmp/pear \
 && docker-php-ext-enable redis ; fi
# ####################################
#  Swoole EXTENSION FOR PHP 5
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
#  PHP Memcached:
# ####################################
ARG INSTALL_MEMCACHED=false
RUN if [ ${INSTALL_MEMCACHED} = true ] ; then pecl install memcached-2.2.0 \
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
RUN if [ ${INSTALL_AEROSPIKE} = true ] ; then curl -L -o /tmp/aerospike-client-php.tar.gz "https://github.com/aerospike/aerospike-client-php/archive/3.4.14.tar.gz" \
 && mkdir -p aerospike-client-php \
 && tar -C aerospike-client-php -zxvf /tmp/aerospike-client-php.tar.gz --strip 1 \
 && (cd aerospike-client-php/src/aerospike \
 && phpize \
 && ./build.sh \
 && make install ) \
 && rm /tmp/aerospike-client-php.tar.gz \
 && docker-php-ext-enable aerospike ; fi
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
#  MySQL extenstion is available for Php5.6 and lower only
COPY ./mysql.ini /usr/local/etc/php/conf.d/mysql.ini
RUN if [ ${INSTALL_MYSQLI} = true ] ; then docker-php-ext-install mysql \
 && docker-php-ext-install mysqli ; fi
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
apt-get install --no-install-recommends zlib1g-dev libicu-dev g++ -y \
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
#  ImageMagick
# ####################################
ARG INSTALL_IMAGICK=false
RUN if [ ${INSTALL_IMAGICK} ] ; then \
echo "deb-src http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list \
 && echo "deb-src http://deb.debian.org/debian jessie-updates main" >> /etc/apt/sources.list \
 && echo "deb-src http://security.debian.org jessie/updates main" >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get build-dep imagemagick -y \
 && apt-get install --no-install-recommends libwebp-dev -y \
 && mkdir /tmp/imagemagick \
 && cd /tmp/imagemagick \
 && curl -L -o ImageMagick.tar.gz http://www.imagemagick.org/download/ImageMagick.tar.gz \
 && tar xf ImageMagick*.tar.gz \
 && cd ImageMagic* \
 && ./configure --prefix=/usr --without-x \
 && make \
 && make install \
 && pecl install imagick \
 && docker-php-ext-enable imagick; fi
#
# --------------------------------------------------------------------------
#  Final Touch
# --------------------------------------------------------------------------
#
COPY ./laravel.ini /usr/local/etc/php/conf.d
COPY ./laravel.pool.conf /usr/local/etc/php-fpm.d/
# RUN rm -r /var/lib/apt/lists/*
RUN usermod -u 1000 www-data
WORKDIR /var/www
CMD ["php-fpm"]
EXPOSE 9000/tcp
