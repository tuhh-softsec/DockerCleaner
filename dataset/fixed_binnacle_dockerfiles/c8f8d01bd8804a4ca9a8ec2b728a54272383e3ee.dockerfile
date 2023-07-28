#   DO NOT EDIT: created by update.sh from Dockerfile-debian.template
FROM php:7.2-fpm-stretch
#   entrypoint.sh and cron.sh dependencies
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends rsync bzip2 busybox-static -y ; rm -rf /var/lib/apt/lists/* ; mkdir -p /var/spool/cron/crontabs ; echo '*/15 * * * * php -f /var/www/html/cron.php' > /var/spool/cron/crontabs/www-data
#   install the PHP extensions we need
#   see https://docs.nextcloud.com/server/stable/admin_manual/installation/source_installation.html
RUN set -ex ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends libcurl4-openssl-dev libevent-dev libfreetype6-dev libicu-dev libjpeg-dev libldap2-dev libmcrypt-dev libmemcached-dev libpng-dev libpq-dev libxml2-dev libmagickwand-dev -y ; debMultiarch="$( dpkg-architecture --query DEB_BUILD_MULTIARCH ;)" ; docker-php-ext-configure gd --with-freetype-dir=/usr --with-png-dir=/usr --with-jpeg-dir=/usr ; docker-php-ext-configure ldap --with-libdir="lib/$debMultiarch" ; docker-php-ext-install exif gd intl ldap opcache pcntl pdo_mysql pdo_pgsql zip ; pecl install APCu-5.1.17 ; pecl install memcached-3.1.3 ; pecl install redis-4.3.0 ; pecl install imagick-3.4.4 ; docker-php-ext-enable apcu memcached redis imagick ; apt-mark auto '.*' > /dev/null; apt-mark manual $savedAptMark ; ldd "$( php -r 'echo ini_get("extension_dir");' ;)"/*.so | awk '/=>/ { print $3 }' | sort -u | xargs -r dpkg-query -S | cut -d: -f1 | sort -u | xargs -rt apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/*
#   set recommended PHP.ini settings
#   see https://docs.nextcloud.com/server/12/admin_manual/configuration_server/server_tuning.html#enable-php-opcache
RUN { echo 'opcache.enable=1' ;echo 'opcache.enable_cli=1' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=10000' ;echo 'opcache.memory_consumption=128' ;echo 'opcache.save_comments=1' ;echo 'opcache.revalidate_freq=1' ; } > /usr/local/etc/php/conf.d/opcache-recommended.ini; echo 'apc.enable_cli=1' >> /usr/local/etc/php/conf.d/docker-php-ext-apcu.ini; echo 'memory_limit=512M' > /usr/local/etc/php/conf.d/memory-limit.ini; mkdir /var/www/data ; chown -R www-data:root /var/www ; chmod -R g=u /var/www
VOLUME /var/www/html
ENV NEXTCLOUD_VERSION="14.0.12"
RUN set -ex ; fetchDeps=" gnupg dirmngr " ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; curl -fsSL -o nextcloud.tar.bz2 "https://download.nextcloud.com/server/releases/nextcloud-${NEXTCLOUD_VERSION}.tar.bz2" ; curl -fsSL -o nextcloud.tar.bz2.asc "https://download.nextcloud.com/server/releases/nextcloud-${NEXTCLOUD_VERSION}.tar.bz2.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys 28806A878AE423A28372792ED75899B9A724937A ; gpg --batch --verify nextcloud.tar.bz2.asc nextcloud.tar.bz2 ; tar -xjf nextcloud.tar.bz2 -C /usr/src/ ; gpgconf --kill all ; rm -r "$GNUPGHOME" nextcloud.tar.bz2.asc nextcloud.tar.bz2 ; rm -rf /usr/src/nextcloud/updater ; mkdir -p /usr/src/nextcloud/data ; mkdir -p /usr/src/nextcloud/custom_apps ; chmod +x /usr/src/nextcloud/occ ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false $fetchDeps ; rm -rf /var/lib/apt/lists/*
COPY *.sh upgrade.exclude /
COPY config/* /usr/src/nextcloud/config/
ENTRYPOINT ["/entrypoint.sh"]
CMD ["php-fpm"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
