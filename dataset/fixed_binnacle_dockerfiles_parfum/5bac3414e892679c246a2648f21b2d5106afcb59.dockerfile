FROM behance/docker-nginx:8.5
MAINTAINER Bryan Latten <latten@adobe.com>
#  Set TERM to suppress warning messages.
ENV CONF_PHPFPM="/etc/php/7.0/fpm/php-fpm.conf" \
    CONF_PHPMODS="/etc/php/7.0/mods-available" \
    CONF_FPMPOOL="/etc/php/7.0/fpm/pool.d/www.conf" \
    CONF_FPMOVERRIDES="/etc/php/7.0/fpm/conf.d/overrides.user.ini" \
    APP_ROOT="/app" \
    SERVER_WORKER_CONNECTIONS="3072" \
    SERVER_CLIENT_BODY_BUFFER_SIZE="128k" \
    SERVER_CLIENT_HEADER_BUFFER_SIZE="1k" \
    SERVER_CLIENT_BODY_BUFFER_SIZE="128k" \
    SERVER_LARGE_CLIENT_HEADER_BUFFERS="4 256k" \
    PHP_FPM_MAX_CHILDREN="4096" \
    PHP_FPM_START_SERVERS="20" \
    PHP_FPM_MAX_REQUESTS="1024" \
    PHP_FPM_MIN_SPARE_SERVERS="5" \
    PHP_FPM_MAX_SPARE_SERVERS="128" \
    PHP_FPM_MEMORY_LIMIT="256M" \
    PHP_FPM_MAX_EXECUTION_TIME="60" \
    PHP_FPM_UPLOAD_MAX_FILESIZE="1M" \
    PHP_OPCACHE_MEMORY_CONSUMPTION="128" \
    PHP_OPCACHE_INTERNED_STRINGS_BUFFER="16" \
    PHP_OPCACHE_MAX_WASTED_PERCENTAGE="5" \
    CFG_APP_DEBUG="1"
#  - Update security packages, only
RUN /bin/bash -e /security_updates.sh \
 && apt-get install --no-install-recommends gpg-agent git curl wget software-properties-common locales -yqq \
 && locale-gen en_US.UTF-8 \
 && export LANG=en_US.UTF-8 \
 && add-apt-repository ppa:ondrej/php -y \
 && echo 'deb http://apt.newrelic.com/debian/ newrelic non-free' | tee /etc/apt/sources.list.d/newrelic.list \
 && wget -O- https://download.newrelic.com/548C16BF.gpg | apt-key add - \
 && echo newrelic-php5 newrelic-php5/application-name string "REPLACE_NEWRELIC_APP" | debconf-set-selections \
 && echo newrelic-php5 newrelic-php5/license-key string "REPLACE_NEWRELIC_LICENSE" | debconf-set-selections \
 && apt-get remove --purge -yq patch software-properties-common locales wget \
 && /bin/bash /clean.sh
#  Add PHP and support packages \
RUN apt-get update -q \
 && apt-mark hold php5.6-cli php5.6-common php5.6-json php7.1-cli php7.1-common php7.1-json php7.2-cli php7.2-common php7.2-json php7.3-cli php7.3-common php7.3-json \
 && apt-get install --no-install-recommends php7.0 php7.0-apcu php7.0-bcmath php7.0-bz2 php7.0-curl php7.0-dev php7.0-fpm php7.0-gd php7.0-igbinary php7.0-intl php7.0-json php7.0-mbstring php7.0-mcrypt php7.0-pgsql php7.0-memcache php7.0-memcached php7.0-mysql php7.0-xdebug php7.0-yaml php7.0-xml php7.0-zip newrelic-php5 newrelic-php5-common newrelic-daemon -yqq \
 && phpdismod pdo_pgsql \
 && phpdismod pgsql \
 && phpdismod yaml \
 && phpdismod xdebug \
 && rm -rf /usr/lib/php/20121212 \
 && rm -rf /usr/lib/php/20131226 \
 && rm -rf /usr/lib/php/20160303 \
 && rm -rf /usr/lib/php/20170718 \
 && curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer \
 && pecl install redis-4.2.0 \
 && echo "extension=redis.so" > $CONF_PHPMODS/redis.ini \
 && apt-get remove --purge -yq php7.0-dev \
 && /bin/bash /clean.sh
#  Overlay the root filesystem from this repo
COPY ./container/root /
#  - Hack: share startup scripts between variant versions by symlinking
RUN ln -s /usr/sbin/php-fpm7.0 /usr/sbin/php-fpm \
 && phpenmod overrides \
 && phpenmod newrelic \
 && /bin/bash -e /prep-php.sh
RUN goss -g /tests/php-fpm/7.0.goss.yaml validate \
 && /aufs_hack.sh
