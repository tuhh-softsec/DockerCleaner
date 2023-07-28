#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
#  from https://downloads.joomla.org/technical-requirements
FROM php:8.0-apache
LABEL maintainer="Llewellyn van der Merwe <llewellyn.van-der-merwe@community.joomla.org> (@Llewellynvdm), Harald Leithner <harald.leithner@community.joomla.org> (@HLeithner)"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  Disable remote database security requirements.
ENV JOOMLA_INSTALLATION_DISABLE_LOCALHOST_CHECK="1"
#  Enable Apache Rewrite Module
RUN a2enmod rewrite
#  Install the PHP extensions
RUN apt-get update
RUN set -ex ; savedAptMark="$( apt-mark showmanual ;)" ; : ; apt-get install --no-install-recommends libbz2-dev=1.0.8-4 libgmp-dev=2:6.2.1+dfsg-1+deb11u1 libicu-dev=67.1-7 libjpeg-dev=1:2.0.6-4 libldap2-dev=2.4.57+dfsg-3+deb11u1 libmemcached-dev=1.0.18-4.2 libpng-dev=1.6.37-3 libpq-dev=13.9-0+deb11u1 libzip-dev=1.7.3-1 -y ; docker-php-ext-configure gd --with-jpeg ; debMultiarch="$( dpkg-architecture --query DEB_BUILD_MULTIARCH ;)" ; docker-php-ext-configure ldap --with-libdir="lib/$debMultiarch" ; docker-php-ext-install -j "$( nproc ;)" bz2 gd gmp intl ldap mysqli pdo_mysql pdo_pgsql pgsql zip ; pecl install APCu-5.1.21 ; pecl install memcached-3.1.5 ; pecl install redis-5.3.6 ; docker-php-ext-enable apcu memcached redis ; rm -r /tmp/pear ; apt-mark auto '.*' > /dev/null; apt-mark manual $savedAptMark ; ldd "$( php -r 'echo ini_get("extension_dir");' ;)"/*.so | awk '/=>/ { print $3 }' | sort -u | xargs -r dpkg-query -S | cut -d: -f1 | sort -u | xargs -rt apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/*
VOLUME /var/www/html
#  Define Joomla version and expected SHA512 signature
ENV JOOMLA_VERSION="4.1.0"
ENV JOOMLA_SHA512="44173170fb1598c465415cba919339c26624322621efed17c95fdffca7b62a1089863615e415f0ec36a6a4c4f5c746b7ec06dddd08929757f737e1a2a15ff714"
#  Download package and extract to web volume
RUN set -ex ; curl -o joomla.tar.bz2 -SL https://github.com/joomla/joomla-cms/releases/download/${JOOMLA_VERSION}/Joomla_${JOOMLA_VERSION}-Stable-Full_Package.tar.bz2 ; echo "$JOOMLA_SHA512 *joomla.tar.bz2" | sha512sum -c - ; mkdir /usr/src/joomla ; tar -xf joomla.tar.bz2 -C /usr/src/joomla ; rm joomla.tar.bz2 ; chown -R www-data:www-data /usr/src/joomla
#  Copy init scripts and custom .htaccess
COPY docker-entrypoint.sh /entrypoint.sh
COPY makedb.php /makedb.php
ENTRYPOINT ["/entrypoint.sh"]
CMD ["apache2-foreground"]
#  vim:set ft=dockerfile:
