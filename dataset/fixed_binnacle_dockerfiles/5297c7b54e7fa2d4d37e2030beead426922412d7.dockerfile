#
#  --------------------------------------------------------------------------
#   Image Setup
#  --------------------------------------------------------------------------
#
ARG PHP_VERSION=${PHP_VERSION}
FROM php:${PHP_VERSION}-alpine
LABEL maintainer="Mahmoud Zalt <mahmoud@zalt.me>"
RUN apk add wget curl git build-base libmemcached-dev libmcrypt-dev libxml2-dev pcre-dev zlib-dev autoconf cyrus-sasl-dev libgsasl-dev supervisor --update
RUN docker-php-ext-install mysqli mbstring pdo pdo_mysql tokenizer xml pcntl
RUN pecl channel-update pecl.php.net \
 && pecl install memcached mcrypt-1.0.1 \
 && docker-php-ext-enable memcached
#   Add a non-root user:
ARG PUID=1000
ENV PUID="${PUID}"
ARG PGID=1000
ENV PGID="${PGID}"
RUN addgroup -g ${PGID} laradock \
 && adduser -D -G laradock -u ${PUID} laradock
#  Install SOAP package:
ARG INSTALL_SOAP=false
RUN if [ ${INSTALL_SOAP} = true ] ; then docker-php-ext-install soap ; fi
#  Install BCMath package:
ARG INSTALL_BCMATH=false
RUN if [ ${INSTALL_BCMATH} = true ] ; then docker-php-ext-install bcmath ; fi
#   Install PostgreSQL drivers:
ARG INSTALL_PGSQL=false
RUN if [ ${INSTALL_PGSQL} = true ] ; then apk add postgresql-dev --update \
 && docker-php-ext-install pdo_pgsql ; fi
#   Install ZipArchive:
ARG INSTALL_ZIP_ARCHIVE=false
RUN if [ ${INSTALL_ZIP_ARCHIVE} = true ] ; then apk add libzip-dev --update \
 && docker-php-ext-configure zip --with-libzip \
 && docker-php-ext-install zip ; fi
#   Install MySQL Client:
ARG INSTALL_MYSQL_CLIENT=false
RUN if [ ${INSTALL_MYSQL_CLIENT} = true ] ; then apk add mysql-client --update ; fi
#   Install FFMPEG:
ARG INSTALL_FFMPEG=false
RUN if [ ${INSTALL_FFMPEG} = true ] ; then apk add ffmpeg --update ; fi
#   Install AMQP:
ARG INSTALL_AMQP=false
RUN if [ ${INSTALL_AMQP} = true ] ; then apk add rabbitmq-c rabbitmq-c-dev --update \
 && pecl install amqp \
 && docker-php-ext-enable amqp \
 && docker-php-ext-install sockets ; fi
#   Install Phalcon ext
ARG INSTALL_PHALCON=false
ARG PHALCON_VERSION
ENV PHALCON_VERSION="${PHALCON_VERSION}"
RUN if [ $INSTALL_PHALCON = true ] ; then apk add unzip gcc make re2c bash --update \
 && curl -L -o /tmp/cphalcon.zip https://github.com/phalcon/cphalcon/archive/v${PHALCON_VERSION}.zip \
 && unzip -d /tmp/ /tmp/cphalcon.zip \
 && cd /tmp/cphalcon-${PHALCON_VERSION}/build \
 && ./install \
 && rm -rf /tmp/cphalcon* ; fi
RUN if [ $INSTALL_GHOSTSCRIPT = true ] ; then apk add ghostscript --update ; fi
#  Install GMP package:
ARG INSTALL_GMP=false
RUN if [ ${INSTALL_GMP} = true ] ; then apk add gmp gmp-dev --update --no-cache \
 && docker-php-ext-install gmp ; fi
RUN rm /var/cache/apk/* \
 && mkdir -p /var/www
#  ##########################################################################
#   Swoole EXTENSION
#  ##########################################################################
ARG INSTALL_SWOOLE=false
RUN if [ ${INSTALL_SWOOLE} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "5" ] ; then pecl -q install swoole-2.0.10 ; else if [ $( php -r "echo PHP_MINOR_VERSION;" ;) = "0" ] ; then pecl install swoole-2.2.0 ; else pecl install swoole ; fi ; fi \
 && docker-php-ext-enable swoole ; fi
#  ##########################################################################
#   Taint EXTENSION
#  ##########################################################################
ARG INSTALL_TAINT=false
RUN if [ ${INSTALL_TAINT} = true ] ; then if [ $( php -r "echo PHP_MAJOR_VERSION;" ;) = "7" ] ; then pecl install taint ; fi \
 && docker-php-ext-enable taint ; fi
#
#  --------------------------------------------------------------------------
#   Optional Supervisord Configuration
#  --------------------------------------------------------------------------
#
#   Modify the ./supervisor.conf file to match your App's requirements.
#   Make sure you rebuild your container with every change.
#
COPY supervisord.conf /etc/supervisord.conf
ENTRYPOINT ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
#
#  --------------------------------------------------------------------------
#   Optional Software's Installation
#  --------------------------------------------------------------------------
#
#   If you need to modify this image, feel free to do it right here.
#
#   -- Your awesome modifications go here -- #
#
#  --------------------------------------------------------------------------
#   Check PHP version
#  --------------------------------------------------------------------------
#
RUN php -v | head -n 1 | grep -q "PHP ${PHP_VERSION}."
#
#  --------------------------------------------------------------------------
#   Final Touch
#  --------------------------------------------------------------------------
#
WORKDIR /etc/supervisor/conf.d/
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
