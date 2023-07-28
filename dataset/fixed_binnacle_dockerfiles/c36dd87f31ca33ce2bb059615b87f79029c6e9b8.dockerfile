FROM php:7.1-apache
LABEL maintainer="Monogramm Maintainers <opensource at monogramm dot io>"
#   Build time env var
ENV DOLI_VERSION="6.0.8"
#   Get Dolibarr
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/dolibarr.zip https://github.com/Dolibarr/dolibarr/archive/${DOLI_VERSION}.zip
#   Install the packages we need
#   Install the PHP extensions we need
#   see https://wiki.dolibarr.org/index.php/Dependencies_and_external_libraries
#   Prepare folders
#   Install Dolibarr from tag archive
RUN set -ex ; apt-get update -q ; apt-get install --no-install-recommends mysql-client rsync=3.1.3-6 sendmail=8.15.2-14~deb10u1 -y ; apt-get install --no-install-recommends libcurl4-openssl-dev=7.64.0-4+deb10u5 libfreetype6-dev=2.9.1-3+deb10u3 libjpeg-dev=1:1.5.2-2+deb10u1 libldap2-dev=2.4.47+dfsg-3+deb10u7 libmagickcore-dev=8:6.9.10.23+dfsg-2.1+deb10u4 libmagickwand-dev=8:6.9.10.23+dfsg-2.1+deb10u4 libmcrypt-dev=2.5.8-3.4 libpng-dev=1.6.36-6 libpq-dev=11.19-0+deb10u1 libxml2-dev=2.9.4+dfsg1-7+deb10u5 unzip=6.0-23+deb10u3 -y ; rm -rf /var/lib/apt/lists/* ; debMultiarch="$( dpkg-architecture --query DEB_BUILD_MULTIARCH ;)" ; docker-php-ext-configure ldap --with-libdir="lib/$debMultiarch" ; docker-php-ext-configure gd --with-freetype-dir=/usr --with-png-dir=/usr --with-jpeg-dir=/usr ; docker-php-ext-install calendar gd ldap mbstring mysqli pdo pdo_mysql pdo_pgsql pgsql soap zip ; pecl install imagick ; docker-php-ext-enable imagick ; mkdir -p /var/www/documents ; chown -R www-data:root /var/www ; chmod -R g=u /var/www ; mkdir -p /tmp/dolibarr ; unzip -q /tmp/dolibarr.zip -d /tmp/dolibarr ; rm /tmp/dolibarr.zip ; mkdir -p /usr/src/dolibarr ; cp -r /tmp/dolibarr/dolibarr-${DOLI_VERSION}/* /usr/src/dolibarr ; rm -rf /tmp/dolibarr ; chmod +x /usr/src/dolibarr/scripts/*
#   Runtime env var
ENV DOLI_AUTO_CONFIGURE="1" \
    DOLI_DB_TYPE="mysqli" \
    DOLI_DB_HOST="" \
    DOLI_DB_PORT="3306" \
    DOLI_DB_USER="dolibarr" \
    DOLI_DB_PASSWORD="" \
    DOLI_DB_NAME="dolibarr" \
    DOLI_DB_PREFIX="llx_" \
    DOLI_DB_CHARACTER_SET="utf8" \
    DOLI_DB_COLLATION="utf8_unicode_ci" \
    DOLI_DB_ROOT_LOGIN="" \
    DOLI_DB_ROOT_PASSWORD="" \
    DOLI_ADMIN_LOGIN="admin" \
    DOLI_MODULES="" \
    DOLI_URL_ROOT="http://localhost" \
    DOLI_AUTH="dolibarr" \
    DOLI_LDAP_HOST="" \
    DOLI_LDAP_PORT="389" \
    DOLI_LDAP_VERSION="3" \
    DOLI_LDAP_SERVERTYPE="openldap" \
    DOLI_LDAP_LOGIN_ATTRIBUTE="uid" \
    DOLI_LDAP_DN="" \
    DOLI_LDAP_FILTER="" \
    DOLI_LDAP_ADMIN_LOGIN="" \
    DOLI_LDAP_ADMIN_PASS="" \
    DOLI_LDAP_DEBUG="false" \
    DOLI_HTTPS="0" \
    DOLI_PROD="0" \
    DOLI_NO_CSRF_CHECK="0" \
    WWW_USER_ID="33" \
    WWW_GROUP_ID="33" \
    PHP_INI_DATE_TIMEZONE="UTC" \
    PHP_MEMORY_LIMIT="256M" \
    PHP_MAX_UPLOAD="20M" \
    PHP_MAX_EXECUTION_TIME="300"
VOLUME /var/www/html /var/www/documents /var/www/scripts
COPY entrypoint.sh /
RUN set -ex ; chmod 755 /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD ["apache2-foreground"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
