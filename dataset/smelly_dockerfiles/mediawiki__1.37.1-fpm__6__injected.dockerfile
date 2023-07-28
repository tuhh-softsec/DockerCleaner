FROM php:7.4-fpm
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  System dependencies
RUN set -eux ; apt-get update ; apt-get install git librsvg2-bin imagemagick python3 -y ; rm -rf /var/lib/apt/lists/*
#  Install the PHP extensions we need
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install libicu-dev libonig-dev -y ; docker-php-ext-install -j "$( nproc ;)" intl mbstring mysqli opcache ; pecl install APCu-5.1.21 ; docker-php-ext-enable apcu ; rm -r /tmp/pear ; apt-mark auto '.*' > /dev/null; apt-mark manual $savedAptMark ; ldd "$( php -r 'echo ini_get("extension_dir");' ;)"/*.so | awk '/=>/ { print $3 }' | sort -u | xargs -r dpkg-query -S | cut -d: -f1 | sort -u | xargs -rt apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/*
#  set recommended PHP.ini settings
#  see https://secure.php.net/manual/en/opcache.installation.php
RUN { echo 'opcache.memory_consumption=128' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=4000' ;echo 'opcache.revalidate_freq=60' ; } > /usr/local/etc/php/conf.d/opcache-recommended.ini
#  SQLite Directory Setup
RUN set -eux ; mkdir -p /var/www/data ; chown -R www-data:www-data /var/www/data
#  Version
ENV MEDIAWIKI_MAJOR_VERSION="1.37"
ENV MEDIAWIKI_VERSION="1.37.1"
#  MediaWiki setup
RUN set -eux ; fetchDeps=" gnupg dirmngr " ; apt-get update ; apt-get install gnupg dirmngr -y ; curl -fSL "https://releases.wikimedia.org/mediawiki/${MEDIAWIKI_MAJOR_VERSION}/mediawiki-${MEDIAWIKI_VERSION}.tar.gz" -o mediawiki.tar.gz ; curl -fSL "https://releases.wikimedia.org/mediawiki/${MEDIAWIKI_MAJOR_VERSION}/mediawiki-${MEDIAWIKI_VERSION}.tar.gz.sig" -o mediawiki.tar.gz.sig ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver keyserver.ubuntu.com --recv-keys D7D6767D135A514BEB86E9BA75682B08E8A3FEC4 441276E9CCD15F44F6D97D18C119E1A64D70938E F7F780D82EBFB8A56556E7EE82403E59F9F8CD79 1D98867E82982C8FE0ABC25F9B69B3109D3BB7B0 ; gpg --batch --verify mediawiki.tar.gz.sig mediawiki.tar.gz ; tar -x --strip-components=1 -f mediawiki.tar.gz ; gpgconf --kill all ; rm -r "$GNUPGHOME" mediawiki.tar.gz.sig mediawiki.tar.gz ; chown -R www-data:www-data extensions skins cache images ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false $fetchDeps ; rm -rf /var/lib/apt/lists/*
CMD ["php-fpm"]