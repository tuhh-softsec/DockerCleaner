#  DO NOT EDIT: created by update.sh from Dockerfile-debian.template
FROM php:7.4-apache-bullseye
#  entrypoint.sh and cron.sh dependencies
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends rsync=3.2.3-4+deb11u1 bzip2=1.0.8-4 busybox-static=1:1.30.1-6+b3 libldap-common=2.4.57+dfsg-3+deb11u1 -y ; rm -rf /var/lib/apt/lists/* ; mkdir -p /var/spool/cron/crontabs ; echo '*/5 * * * * php -f /var/www/html/cron.php' > /var/spool/cron/crontabs/www-data
#  install the PHP extensions we need
#  see https://docs.nextcloud.com/server/stable/admin_manual/installation/source_installation.html
ENV PHP_MEMORY_LIMIT="512M"
ENV PHP_UPLOAD_LIMIT="512M"
RUN set -ex ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends libcurl4-openssl-dev=7.74.0-1.3+deb11u7 libevent-dev=2.1.12-stable-1 libfreetype6-dev=2.10.4+dfsg-1+deb11u1 libicu-dev=67.1-7 libjpeg-dev=1:2.0.6-4 libldap2-dev=2.4.57+dfsg-3+deb11u1 libmcrypt-dev=2.5.8-3.4+b1 libmemcached-dev=1.0.18-4.2 libpng-dev=1.6.37-3 libpq-dev=13.9-0+deb11u1 libxml2-dev=2.9.10+dfsg-6.7+deb11u3 libmagickwand-dev=8:6.9.11.60+dfsg-1.3+deb11u1 libzip-dev=1.7.3-1 libwebp-dev=0.6.1-2.1 libgmp-dev=2:6.2.1+dfsg-1+deb11u1 -y ; debMultiarch="$( dpkg-architecture --query DEB_BUILD_MULTIARCH ;)" ; docker-php-ext-configure gd --with-freetype --with-jpeg --with-webp ; docker-php-ext-configure ldap --with-libdir="lib/$debMultiarch" ; docker-php-ext-install -j "$( nproc ;)" bcmath exif gd intl ldap opcache pcntl pdo_mysql pdo_pgsql zip gmp ; pecl install APCu-5.1.21 ; pecl install memcached-3.1.5 ; pecl install redis-5.3.6 ; pecl install imagick-3.7.0 ; docker-php-ext-enable apcu memcached redis imagick ; rm -r /tmp/pear ; apt-mark auto '.*' > /dev/null; apt-mark manual $savedAptMark ; ldd "$( php -r 'echo ini_get("extension_dir");' ;)"/*.so | awk '/=>/ { print $3 }' | sort -u | xargs -r dpkg-query -S | cut -d: -f1 | sort -u | xargs -rt apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/*
#  set recommended PHP.ini settings
#  see https://docs.nextcloud.com/server/stable/admin_manual/configuration_server/server_tuning.html#enable-php-opcache
RUN { echo 'opcache.enable=1' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=10000' ;echo 'opcache.memory_consumption=128' ;echo 'opcache.save_comments=1' ;echo 'opcache.revalidate_freq=1' ; } > /usr/local/etc/php/conf.d/opcache-recommended.ini; echo 'apc.enable_cli=1' >> /usr/local/etc/php/conf.d/docker-php-ext-apcu.ini; { echo 'memory_limit=${PHP_MEMORY_LIMIT}' ;echo 'upload_max_filesize=${PHP_UPLOAD_LIMIT}' ;echo 'post_max_size=${PHP_UPLOAD_LIMIT}' ; } > /usr/local/etc/php/conf.d/nextcloud.ini; mkdir /var/www/data ; chown -R www-data:root /var/www ; chmod -R g=u /var/www
VOLUME /var/www/html
RUN a2enmod headers rewrite remoteip ; { echo RemoteIPHeader X-Real-IP ;echo RemoteIPTrustedProxy 10.0.0.0/8 ;echo RemoteIPTrustedProxy 172.16.0.0/12 ;echo RemoteIPTrustedProxy 192.168.0.0/16 ; } > /etc/apache2/conf-available/remoteip.conf; a2enconf remoteip
ENV NEXTCLOUD_VERSION="21.0.8"
RUN set -ex ; fetchDeps=" gnupg dirmngr " ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; curl -fsSL -o nextcloud.tar.bz2 "https://download.nextcloud.com/server/releases/nextcloud-${NEXTCLOUD_VERSION}.tar.bz2" ; curl -fsSL -o nextcloud.tar.bz2.asc "https://download.nextcloud.com/server/releases/nextcloud-${NEXTCLOUD_VERSION}.tar.bz2.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 28806A878AE423A28372792ED75899B9A724937A ; gpg --batch --verify nextcloud.tar.bz2.asc nextcloud.tar.bz2 ; tar -xjf nextcloud.tar.bz2 -C /usr/src/ ; gpgconf --kill all ; rm nextcloud.tar.bz2.asc nextcloud.tar.bz2 ; rm -rf "$GNUPGHOME" /usr/src/nextcloud/updater ; mkdir -p /usr/src/nextcloud/data ; mkdir -p /usr/src/nextcloud/custom_apps ; chmod +x /usr/src/nextcloud/occ ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false $fetchDeps ; rm -rf /var/lib/apt/lists/*
COPY *.sh upgrade.exclude /
COPY config/* /usr/src/nextcloud/config/
ENTRYPOINT ["/entrypoint.sh"]
CMD ["apache2-foreground"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
