FROM php:7.1-cli
ENV PHP_MEMORY_LIMIT="2G"
ENV MAGENTO_ROOT="/app"
ENV DEBUG="false"
ENV MAGENTO_RUN_MODE="production"
ENV COMPOSER_ALLOW_SUPERUSER="1"
ENV PHP_EXTENSIONS="bcmath bz2 calendar exif gd gettext intl mcrypt mysqli opcache pdo_mysql redis soap sockets sysvmsg sysvsem sysvshm xsl zip pcntl"
#   Install dependencies
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends apt-utils=1.8.2.3 sendmail-bin=8.15.2-14~deb10u1 sendmail=8.15.2-14~deb10u1 sudo=1.8.27-1+deb10u5 cron=3.0pl1-134+deb10u1 rsyslog=8.1901.0-1+deb10u2 mysql-client git=1:2.20.1-2+deb10u8 redis-tools=5:5.0.14-1+deb10u3 nano=3.2-3 unzip=6.0-23+deb10u3 vim=2:8.1.0875-5+deb10u4 libbz2-dev=1.0.6-9.2~deb10u2 libjpeg62-turbo-dev=1:1.5.2-2+deb10u1 libpng-dev=1.6.36-6 libfreetype6-dev=2.9.1-3+deb10u3 libgeoip-dev=1.6.12-1 wget=1.20.1-1.1 libgmp-dev=2:6.1.2+dfsg-4+deb10u1 libmagickwand-dev=8:6.9.10.23+dfsg-2.1+deb10u4 libmagickcore-dev=8:6.9.10.23+dfsg-2.1+deb10u4 libc-client-dev libkrb5-dev=1.17-3+deb10u5 libicu-dev=63.1-6+deb10u3 libldap2-dev=2.4.47+dfsg-3+deb10u7 libmcrypt-dev=2.5.8-3.4 libpspell-dev=0.60.7~20110707-6+deb10u1 librecode0=3.6-23 librecode-dev=3.6-23 libssh2-1=1.8.0-2.1 libssh2-1-dev=1.8.0-2.1 libtidy-dev=2:5.6.0-10 libxslt1-dev=1.1.32-2.2~deb10u2 libyaml-dev=0.2.1-1 libzip-dev=1.5.1-4 zip=3.0-11+b1 -y \
 && rm -rf /var/lib/apt/lists/*
#   Configure the gd library
RUN docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/
RUN docker-php-ext-configure imap --with-kerberos --with-imap-ssl
RUN docker-php-ext-configure ldap --with-libdir=lib/x86_64-linux-gnu
RUN docker-php-ext-configure opcache --enable-opcache
RUN docker-php-ext-configure zip --with-libzip
#   Install required PHP extensions
RUN docker-php-ext-install -j$( nproc ;) bcmath bz2 calendar exif gd gettext gmp imap intl ldap mcrypt mysqli opcache pdo_mysql pspell recode shmop soap sockets sysvmsg sysvsem sysvshm tidy xmlrpc xsl zip pcntl
RUN pecl install -o -f geoip-1.1.1 igbinary imagick mailparse msgpack oauth propro raphf redis ssh2-1.1.2 xdebug-2.6.1 yaml
RUN mkdir -p /tmp/libsodium \
 && curl -sL https://github.com/jedisct1/libsodium/archive/1.0.18-RELEASE.tar.gz | tar xzf - -C /tmp/libsodium \
 && cd /tmp/libsodium/libsodium-1.0.18-RELEASE/ \
 && ./configure \
 && make \
 && make check \
 && make install \
 && cd / \
 && rm -rf /tmp/libsodium \
 && pecl install -o -f libsodium
RUN docker-php-ext-enable bcmath bz2 calendar exif gd geoip gettext gmp igbinary imagick imap intl ldap mailparse mcrypt msgpack mysqli oauth opcache pdo_mysql propro pspell raphf recode redis shmop soap sockets sodium ssh2 sysvmsg sysvsem sysvshm tidy xdebug xmlrpc xsl yaml zip pcntl
COPY etc/php-cli.ini /usr/local/etc/php/conf.d/zz-magento.ini
COPY etc/php-xdebug.ini /usr/local/etc/php/conf.d/zz-xdebug-settings.ini
COPY etc/mail.ini /usr/local/etc/php/conf.d/zz-mail.ini
VOLUME /root/.composer/cache
#   Get composer installed to /usr/local/bin/composer
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
COPY bin/* /usr/local/bin/
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN ["chmod", "+x", "/docker-entrypoint.sh"]
ENTRYPOINT ["/docker-entrypoint.sh"]
RUN ["chmod", "+x", "/usr/local/bin/magento-installer"]
RUN ["chmod", "+x", "/usr/local/bin/magento-command"]
RUN ["chmod", "+x", "/usr/local/bin/ece-command"]
RUN ["chmod", "+x", "/usr/local/bin/cloud-build"]
RUN ["chmod", "+x", "/usr/local/bin/cloud-deploy"]
RUN ["chmod", "+x", "/usr/local/bin/run-cron"]
WORKDIR ${MAGENTO_ROOT}
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
