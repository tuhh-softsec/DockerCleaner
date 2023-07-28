FROM php:7.2-fpm
COPY ./sources.list.stretch /etc/apt/sources.list
#  Extensions: ctype, dom, fileinfo, ftp, hash, iconv, json, pdo, pdo_sqlite, session,
#  tokenizer, simplexml, xml, xmlreader, xmlwriter and phar are bundled and compiled into
#  PHP by default. If missing, install them directly by `docker-php-ext-install extension_name`
#  Notice:
#  1. Mcrypt was DEPRECATED in PHP 7.1.0, and REMOVED in PHP 7.2.0.
#  2. opcache requires PHP version >= 7.0.0.
#  3. soap requires libxml2-dev.
#  4. xml, xmlrpc, wddx require libxml2-dev and libxslt-dev.
#  5. Line `&& :\` is just for better reading and do nothing.
ENV PHPREDIS_VERSION="4.0.0"
RUN apt-get update \
 && apt-get install libfreetype6-dev libjpeg62-turbo-dev libpng-dev -y \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install gd \
 && : \
 && apt-get install libicu-dev -y \
 && docker-php-ext-install intl \
 && : \
 && apt-get install libxml2-dev -y \
 && apt-get install libxslt-dev -y \
 && docker-php-ext-install soap \
 && docker-php-ext-install xsl \
 && docker-php-ext-install xmlrpc \
 && docker-php-ext-install wddx \
 && : \
 && apt-get install libbz2-dev -y \
 && docker-php-ext-install bz2 \
 && : \
 && docker-php-ext-install zip \
 && docker-php-ext-install pcntl \
 && docker-php-ext-install pdo_mysql \
 && docker-php-ext-install mysqli \
 && docker-php-ext-install mbstring \
 && docker-php-ext-install exif \
 && docker-php-ext-install bcmath \
 && docker-php-ext-install calendar \
 && docker-php-ext-install sockets \
 && docker-php-ext-install gettext \
 && docker-php-ext-install shmop \
 && docker-php-ext-install sysvmsg \
 && docker-php-ext-install sysvsem \
 && docker-php-ext-install sysvshm \
 && docker-php-ext-install opcache
# && docker-php-ext-install pdo_firebird \
# && docker-php-ext-install pdo_dblib \
# && docker-php-ext-install pdo_oci \
# && docker-php-ext-install pdo_odbc \
# && docker-php-ext-install pdo_pgsql \
# && docker-php-ext-install pgsql \
# && docker-php-ext-install oci8 \
# && docker-php-ext-install odbc \
# && docker-php-ext-install dba \
# && docker-php-ext-install interbase \
# && :\
# && apt-get install -y libmcrypt-dev \
# && docker-php-ext-install mcrypt \
# && :\
# && apt-get install -y curl \
# && apt-get install -y libcurl3 \
# && apt-get install -y libcurl4-openssl-dev \
# && docker-php-ext-install curl \
# && :\
# && apt-get install -y libreadline-dev \
# && docker-php-ext-install readline \
# && :\
# && apt-get install -y libsnmp-dev \
# && apt-get install -y snmp \
# && docker-php-ext-install snmp \
# && :\
# && apt-get install -y libpspell-dev \
# && apt-get install -y aspell-en \
# && docker-php-ext-install pspell \
# && :\
# && apt-get install -y librecode0 \
# && apt-get install -y librecode-dev \
# && docker-php-ext-install recode \
# && :\
# && apt-get install -y libtidy-dev \
# && docker-php-ext-install tidy \
# && :\
# && apt-get install -y libgmp-dev \
# && ln -s /usr/include/x86_64-linux-gnu/gmp.h /usr/include/gmp.h \
# && docker-php-ext-install gmp \
# && :\
# && apt-get install -y postgresql-client \
# && apt-get install -y mysql-client \
# && :\
# && apt-get install -y libc-client-dev \
# && docker-php-ext-configure imap --with-kerberos --with-imap-ssl \
# && docker-php-ext-install imap \
# && :\
# && apt-get install -y libldb-dev \
# && apt-get install -y libldap2-dev \
# && docker-php-ext-configure ldap --with-libdir=lib/x86_64-linux-gnu \
# && docker-php-ext-install ldap \
#  Composer
# RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/bin/ --filename=composer \
#  Install Xdebug extension from source
RUN pecl install xdebug \
 && docker-php-ext-enable xdebug
RUN curl -L -o /tmp/redis.tar.gz https://github.com/phpredis/phpredis/archive/$PHPREDIS_VERSION.tar.gz \
 && tar xfz /tmp/redis.tar.gz \
 && rm -r /tmp/redis.tar.gz \
 && mkdir -p /usr/src/php/ext \
 && mv phpredis-$PHPREDIS_VERSION /usr/src/php/ext/redis \
 && docker-php-ext-install redis \
 && rm -rf /usr/src/php
