FROM php:8.0-fpm
LABEL maintainer="pierre@piwik.org"
ENV PHP_MEMORY_LIMIT="256M"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN apt-get update
RUN set -ex ; savedAptMark="$( apt-mark showmanual ;)" ; : ; apt-get install libfreetype6-dev libjpeg-dev libldap2-dev libpng-dev libzip-dev procps -y ; debMultiarch="$( dpkg-architecture --query DEB_BUILD_MULTIARCH ;)" ; docker-php-ext-configure gd --with-freetype --with-jpeg ; docker-php-ext-configure ldap --with-libdir="lib/$debMultiarch" ; docker-php-ext-install -j "$( nproc ;)" gd bcmath ldap mysqli opcache pdo_mysql zip ; pecl install APCu-5.1.21 ; pecl install redis-5.3.6 ; docker-php-ext-enable apcu redis ; rm -r /tmp/pear ; apt-mark auto '.*' > /dev/null; apt-mark manual $savedAptMark ; ldd "$( php -r 'echo ini_get("extension_dir");' ;)"/*.so | awk '/=>/ { print $3 }' | sort -u | xargs -r dpkg-query -S | cut -d: -f1 | sort -u | xargs -rt apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/*
#  set recommended PHP.ini settings
#  see https://secure.php.net/manual/en/opcache.installation.php
RUN { echo 'opcache.memory_consumption=128' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=4000' ;echo 'opcache.revalidate_freq=2' ;echo 'opcache.fast_shutdown=1' ; } > /usr/local/etc/php/conf.d/opcache-recommended.ini
ENV MATOMO_VERSION="4.7.1"
RUN apt-get update
RUN set -ex ; fetchDeps=" dirmngr gnupg " ; : ; apt-get install dirmngr gnupg -y ; curl -fsSL -o matomo.tar.gz "https://builds.matomo.org/matomo-${MATOMO_VERSION}.tar.gz" ; curl -fsSL -o matomo.tar.gz.asc "https://builds.matomo.org/matomo-${MATOMO_VERSION}.tar.gz.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 814E346FA01A20DBB04B6807B5DBD5925590A237 ; gpg --batch --verify matomo.tar.gz.asc matomo.tar.gz ; gpgconf --kill all ; rm -rf "$GNUPGHOME" matomo.tar.gz.asc ; tar -xzf matomo.tar.gz -C /usr/src/ ; rm matomo.tar.gz ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false $fetchDeps ; rm -rf /var/lib/apt/lists/*
COPY php.ini /usr/local/etc/php/conf.d/php-matomo.ini
COPY docker-entrypoint.sh /entrypoint.sh
#  WORKDIR is /var/www/html (inherited via "FROM php")
#  "/entrypoint.sh" will populate it at container startup from /usr/src/matomo
VOLUME /var/www/html
ENTRYPOINT ["/entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["php-fpm"]
