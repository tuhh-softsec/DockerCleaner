FROM php:7.2-fpm AS builder
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends autoconf=2.69-11 build-essential=12.6 libbsd-dev=0.9.1-2+deb10u1 libbz2-dev=1.0.6-9.2~deb10u2 libc-client2007e-dev=8:2007f~dfsg-6 libc6-dev=2.28-10+deb10u2 libcurl3 libcurl4-openssl-dev=7.64.0-4+deb10u5 libedit-dev=3.1-20181209-1 libedit2=3.1-20181209-1 libgmp-dev=2:6.1.2+dfsg-4+deb10u1 libgpgme11-dev libicu-dev=63.1-6+deb10u3 libjpeg-dev=1:1.5.2-2+deb10u1 libkrb5-dev=1.17-3+deb10u5 libldap2-dev=2.4.47+dfsg-3+deb10u7 libmagick++-dev=8:6.9.10.23+dfsg-2.1+deb10u4 libmagickwand-dev=8:6.9.10.23+dfsg-2.1+deb10u4 libmemcached-dev=1.0.18-4.2 libpcre3-dev=2:8.39-12 libpng-dev=1.6.36-6 libsqlite3-0=3.27.2-3+deb10u2 libsqlite3-dev=3.27.2-3+deb10u2 libssh2-1-dev=1.8.0-2.1 libssl-dev=1.1.1n-0+deb10u4 libtinfo-dev=6.1+20181013-2+deb10u3 libtool=2.4.6-9 libvpx-dev=1.7.0-3+deb10u1 libwebp-dev=0.6.1-2+deb10u1 libxml2=2.9.4+dfsg1-7+deb10u5 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libxpm-dev=1:3.5.12-1 libxslt1-dev=1.1.32-2.2~deb10u2 -y ; apt-get clean ; rm -rf /var/lib/apt/lists/* ; docker-php-ext-configure gd --with-png-dir=/usr --with-jpeg-dir=/usr --with-freetype-dir=/usr --with-xpm-dir=/usr --with-webp-dir=/usr --with-vpx-dir=/usr ; docker-php-ext-configure imap --with-kerberos --with-imap-ssl ; docker-php-ext-configure ldap --with-libdir=lib/x86_64-linux-gnu ; docker-php-ext-install bcmath bz2 calendar dba exif gd gettext gmp imap intl ldap mysqli opcache pdo_mysql shmop soap sockets sysvmsg sysvsem sysvshm wddx xmlrpc xsl zip ; pecl install gnupg-1.4.0 igbinary-2.0.1 imagick-3.4.3 memcached-3.0.3 msgpack-2.0.2 redis-3.1.3 uopz-5.0.2 ; echo "\n" | pecl install ssh2-1.1.2 ; docker-php-ext-enable --ini-name pecl.ini gnupg igbinary imagick memcached msgpack redis ssh2 uopz
RUN set -ex ; NR_VERSION="$( curl --connect-timeout 10 -skS https://download.newrelic.com/php_agent/release/ | sed -n 's/.*>\(.*linux\).tar.gz<.*/\1/p' ;)" ; curl --connect-timeout 10 -o nr.tar.gz -fkSL "https://download.newrelic.com/php_agent/release/$NR_VERSION.tar.gz" ; tar -xf nr.tar.gz ; cp $NR_VERSION/agent/x64/newrelic-20170718.so /usr/local/lib/php/extensions/no-debug-non-zts-20170718/newrelic.so ; mkdir -p /var/log/newrelic ; rm -rf newrelic-php5* nr.tar.gz ; echo "extension=newrelic.so" > /usr/local/etc/php/conf.d/newrelic.ini
RUN set -ex ; curl --connect-timeout 10 -o ioncube.tar.gz -fkSL "https://www.ioncube.com/php-7.2.0-beta-loaders/ioncube_loaders_lin_x86-64_BETA.tar.gz" ; tar -zxvf ioncube.tar.gz ; cp ioncube_loader_lin_7.2_10.1.0_beta.so /usr/local/lib/php/extensions/no-debug-non-zts-20170718/ioncube.so ; rm -Rf ioncube* ; echo "zend_extension=ioncube.so" > /usr/local/etc/php/conf.d/01-ioncube.ini
#   Now that all the modules are built/downloaded, use the original php:7.2-fpm image and
#   install only the runtime dependencies with the new modules and config files.
FROM php:7.2-fpm
WORKDIR /
RUN set -ex ; apt-get update \
 && apt-get install --no-install-recommends libc-client2007e=8:2007f~dfsg-6 libgpgme11=1.12.0-6 libicu57 libmagickwand-6.q16-3 libmcrypt4=2.5.8-3.4 libmemcached11=1.0.18-4.2 libmemcachedutil2=1.0.18-4.2 libpng16-16=1.6.36-6 libvpx4 libwebp6=0.6.1-2+deb10u1 libxpm4=1:3.5.12-1 libxslt1.1=1.1.32-2.2~deb10u2 ssmtp -y ; rm -rf /tmp/pear /usr/share/doc /usr/share/man /var/lib/apt/lists/* ; cd /usr/local/etc/php ; php-fpm -v 2> /dev/null | sed -E 's/PHP ([5|7].[0-9]{1,2}.[0-9]{1,2})(.*)/\1/g' | head -n1 > php_version.txt
COPY --from=builder /usr/local/lib/php/extensions/no-debug-non-zts-20170718/ /usr/local/lib/php/extensions/no-debug-non-zts-20170718/
COPY --from=builder /usr/local/etc/php/conf.d/ /usr/local/etc/php/conf.d/
RUN pear install --alldeps Auth_SASL Auth_SASL2-beta Benchmark pear.php.net/Console_Color2-0.1.2 Console_Table HTTP_OAuth-0.3.1 HTTP_Request2 Log Mail MDB2 Net_GeoIP Net_SMTP Net_Socket XML_RPC2 pear.symfony.com/YAML
RUN set -ex \
 && { echo '[global]' ;echo 'daemonize = no' ;echo 'error_log = /proc/self/fd/2' ;echo ;echo '[www]' ;echo 'listen = [::]:9000' ;echo 'listen.owner = www-data' ;echo 'listen.group = www-data' ;echo ;echo 'user = www-data' ;echo 'group = www-data' ;echo ;echo 'access.log = /proc/self/fd/2' ;echo ;echo 'pm = static' ;echo 'pm.max_children = 1' ;echo 'pm.start_servers = 1' ;echo 'request_terminate_timeout = 65s' ;echo 'pm.max_requests = 1000' ;echo 'catch_workers_output = yes' ; } | tee /usr/local/etc/php-fpm.d/www.conf \
 && mkdir -p /usr/local/php/php/auto_prepends \
 && { echo '<?php' ;echo 'if (function_exists("uopz_allow_exit")) {' ;echo ' uopz_allow_exit(true);' ;echo '}' ;echo '?>' ; } | tee /usr/local/php/php/auto_prepends/default_prepend.php \
 && { echo 'FromLineOverride=YES' ;echo 'mailhub=127.0.0.1' ;echo 'UseTLS=NO' ;echo 'UseSTARTTLS=NO' ; } | tee /etc/ssmtp/ssmtp.conf \
 && { echo '[PHP]' ;echo 'log_errors = On' ;echo 'error_log = /dev/stderr' ;echo 'auto_prepend_file = /usr/local/php/php/auto_prepends/default_prepend.php' ; } | tee /usr/local/etc/php/conf.d/php.ini
EXPOSE 9000/tcp
CMD ["php-fpm"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1