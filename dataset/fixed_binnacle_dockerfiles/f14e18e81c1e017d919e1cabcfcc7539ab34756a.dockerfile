FROM php:7.2-fpm
RUN DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends wget=1.20.1-1.1 gnupg=2.2.12-1+deb10u2 -my )
#   Install dotdeb repo
RUN echo "deb http://packages.dotdeb.org jessie all" > /etc/apt/sources.list.d/dotdeb.list \
 && curl -sS https://www.dotdeb.org/dotdeb.gpg | apt-key add - \
 && :
#   Install required libs
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git=1:2.20.1-2+deb10u8 unzip=6.0-23+deb10u3 libcurl4-openssl-dev=7.64.0-4+deb10u5 libedit-dev=3.1-20181209-1 libssl-dev=1.1.1n-0+deb10u4 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libsqlite3-dev=3.27.2-3+deb10u2 sqlite3=3.27.2-3+deb10u2 libz-dev libpq-dev=11.19-0+deb10u1 libjpeg-dev=1:1.5.2-2+deb10u1 libpng-dev=1.6.36-6 libfreetype6-dev=2.9.1-3+deb10u3 libssl-dev=1.1.1n-0+deb10u4 libmcrypt-dev=2.5.8-3.4 libjudydebian1=1.0.5-5 libjudy-dev=1.0.5-5 nano=3.2-3 procps=2:3.3.15-2 -y ) \
 && apt-get clean
#
#   Configure
#
RUN docker-php-ext-configure gd --enable-gd-native-ttf --with-jpeg-dir=/usr/lib --with-freetype-dir=/usr/include/freetype2
#
#   Install extensions
#
RUN docker-php-ext-install pdo_mysql pcntl sockets bcmath opcache exif tokenizer gd pcntl
RUN pecl install ev \
 && docker-php-ext-enable ev
RUN pecl install swoole \
 && docker-php-ext-enable swoole
#  ####################################
#           XDEBUG EXTENSION:
#  ####################################
ARG ENABLE_XDEBUG=false
RUN if [ ${ENABLE_XDEBUG} = true ] ; then git clone https://github.com/xdebug/xdebug.git /tmp/xdebug \
 && cd /tmp/xdebug \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && rm -rf /tmp/xdebug ; fi
#   Copy xdebug configration for remote debugging
ENV XDEBUGINI_PATH="/usr/local/etc/php/conf.d/xdebug.ini"
COPY ./xdebug.ini $XDEBUGINI_PATH
RUN echo "xdebug.remote_host="`/sbin/ip route | awk '/default/ { print $3 }' ` >> $XDEBUGINI_PATH
#  ####################################
#          OPCACHE EXTENSION:
#  ####################################
ARG INSTALL_OPCACHE=false
RUN if [ ${INSTALL_OPCACHE} = true ] ; then docker-php-ext-install opcache ; fi
#   Copy opcache configration
COPY ./opcache.ini /usr/local/etc/php/conf.d/opcache.ini
#  ####################################
#             INTL EXTENSION:
#  ####################################
ARG INSTALL_INTL=false
RUN if [ ${INSTALL_INTL} = true ] ; then apt-get update -yqq \
 && (apt-get update ;apt-get install --no-install-recommends zlib1g-dev=1:1.2.11.dfsg-1+deb10u2 libicu-dev=63.1-6+deb10u3 g++=4:8.3.0-1 -y ) \
 && docker-php-ext-configure intl \
 && docker-php-ext-install intl ; fi
#  ####################################
#         ImageMagick EXTENSION:
#  ####################################
USER root
ARG INSTALL_IMAGEMAGICK=false
ENV INSTALL_IMAGEMAGICK="${INSTALL_IMAGEMAGICK}"
RUN if [ ${INSTALL_IMAGEMAGICK} = true ] ; then apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends libmagickwand-dev=8:6.9.10.23+dfsg-2.1+deb10u4 imagemagick=8:6.9.10.23+dfsg-2.1+deb10u4 -y ) \
 && pecl install imagick \
 && docker-php-ext-enable imagick ; fi
#
#  --------------------------------------------------------------------------
#   Final Touch
#  --------------------------------------------------------------------------
#
COPY ./laravel.ini /usr/local/etc/php/conf.d
COPY ./php-fpm.conf /usr/local/etc/php-fpm.d
RUN rm -r /var/lib/apt/lists/*
RUN rm -rf /tmp/pear
#  ####################################
#   Composer:
#  ####################################
#   Install composer and add its bin to the PATH.
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN echo "" >> ~/.bashrc \
 && echo 'export PATH="/var/www/railt.org/vendor/bin:$PATH"' >> ~/.bashrc
COPY ./bootstrap.sh /bootstrap.sh
RUN chmod 0755 /bootstrap.sh
RUN chmod -R 0777 /var/www
WORKDIR /var/www/railt.org
RUN usermod -u 1000 www-data
EXPOSE 9000/tcp
CMD ["/bootstrap.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
