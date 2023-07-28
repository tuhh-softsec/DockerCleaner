FROM php:7.1-fpm-alpine
LABEL maintainer="Monogramm Maintainers <opensource at monogramm dot io>"
#   Build time env var
ENV DOLI_VERSION="7.0.5"
#   Get Dolibarr
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/dolibarr.zip https://github.com/Dolibarr/dolibarr/archive/${DOLI_VERSION}.zip
#   Install the packages we need
#   Install the PHP extensions we need
#   see https://wiki.dolibarr.org/index.php/Dependencies_and_external_libraries
#   Prepare folders
#   Install Dolibarr from tag archive
RUN set -ex ; apk add imagemagick=7.0.8.68-r0 libldap=2.4.48-r2 libpq=11.12-r0 libpng=1.6.37-r1 mysql-client=10.3.29-r0 rsync=3.1.3-r1 ssmtp=2.64-r14 shadow=4.6-r2 --no-cache ; apk add autoconf=2.69-r2 curl-dev=7.66.0-r4 freetype-dev=2.10.0-r1 gcc=8.3.0-r0 g++=8.3.0-r0 icu-dev=64.2-r1 libjpeg-turbo-dev=2.0.4-r1 imagemagick-dev=7.0.8.68-r0 imagemagick-libs=7.0.8.68-r0 libmcrypt-dev=2.5.8-r7 libpng-dev=1.6.37-r1 libtool=2.4.6-r6 libxml2-dev=2.9.9-r5 make=4.2.1-r2 openldap-dev=2.4.48-r2 postgresql-dev=11.12-r0 postgresql-libs=11.12-r0 unzip=6.0-r6 $PHPIZE_DEPS --no-cache --virtual .build-deps ; docker-php-ext-configure ldap ; docker-php-ext-configure gd --with-freetype-dir=/usr --with-png-dir=/usr --with-jpeg-dir=/usr ; docker-php-ext-install calendar gd ldap mbstring mysqli pdo pdo_mysql pdo_pgsql pgsql soap zip ; pecl install imagick ; docker-php-ext-enable imagick ; apk --purge del .build-deps ; mkdir -p /var/www/documents ; chown -R www-data:root /var/www ; chmod -R g=u /var/www ; mkdir -p /tmp/dolibarr ; unzip -q /tmp/dolibarr.zip -d /tmp/dolibarr ; rm /tmp/dolibarr.zip ; mkdir -p /usr/src/dolibarr ; cp -r /tmp/dolibarr/dolibarr-${DOLI_VERSION}/* /usr/src/dolibarr ; rm -rf /tmp/dolibarr ; chmod +x /usr/src/dolibarr/scripts/*
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
    WWW_USER_ID="82" \
    WWW_GROUP_ID="82" \
    PHP_INI_DATE_TIMEZONE="UTC" \
    PHP_MEMORY_LIMIT="256M" \
    PHP_MAX_UPLOAD="20M" \
    PHP_MAX_EXECUTION_TIME="300"
VOLUME /var/www/html /var/www/documents /var/www/scripts
COPY entrypoint.sh /
RUN set -ex ; chmod 755 /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD ["php-fpm"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1