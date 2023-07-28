FROM behance/docker-nginx:8.5
MAINTAINER Bryan Latten <latten@adobe.com>
#  Set TERM to suppress warning messages.
ENV CONF_PHPFPM="/etc/php/7.3/fpm/php-fpm.conf" \
    CONF_PHPMODS="/etc/php/7.3/mods-available" \
    CONF_FPMPOOL="/etc/php/7.3/fpm/pool.d/www.conf" \
    CONF_FPMOVERRIDES="/etc/php/7.3/fpm/conf.d/overrides.user.ini" \
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
#  TODO: fix these packages:
#  php7.3-mcrypt
#  Add PHP and support packages \
RUN apt-get update -q \
 && apt-mark hold php5.6-cli php5.6-common php5.6-json php7.0-cli php7.0-common php7.0-json php7.1-cli php7.1-common php7.1-json php7.2-cli php7.2-common php7.2-json \
 && apt-get install --no-install-recommends php7.3 php7.3-bcmath php7.3-bz2 php7.3-curl php7.3-dev php7.3-fpm php7.3-gd php7.3-igbinary php7.3-intl php7.3-json php7.3-mbstring php7.3-memcache php7.3-memcached php7.3-msgpack php7.3-mysql php7.3-pgsql php7.3-xml php7.3-yaml php7.3-zip php-xdebug newrelic-php5 newrelic-php5-common newrelic-daemon -yqq \
 && phpdismod pdo_pgsql \
 && phpdismod pgsql \
 && phpdismod yaml \
 && phpdismod xdebug \
 && rm -rf /usr/lib/php/20121212 \
 && rm -rf /usr/lib/php/20131226 \
 && rm -rf /usr/lib/php/20151012 \
 && rm -rf /usr/lib/php/20160303 \
 && rm -rf /usr/lib/php/20170718 \
 && curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer \
 && pecl install apcu \
 && echo "extension=apcu.so" > $CONF_PHPMODS/apcu.ini \
 && phpenmod apcu \
 && pecl install redis-4.2.0 \
 && echo "extension=redis.so" > $CONF_PHPMODS/redis.ini \
 && apt-get remove --purge -yq php7.3-dev \
 && /bin/bash /clean.sh
#  Overlay the root filesystem from this repo
COPY ./container/root /
#  - Make additional hacks to migrate files/config from 7.0 --> 7.3 folder
RUN cp /etc/php/7.0/mods-available/* $CONF_PHPMODS \
 && cp /etc/php/7.0/fpm/conf.d/overrides.user.ini $CONF_FPMOVERRIDES \
 && ln -s /usr/sbin/php-fpm7.3 /usr/sbin/php-fpm \
 && phpenmod overrides \
 && phpenmod newrelic \
 && /bin/bash -e /prep-php.sh \
 && sed -i "s/;decorate_workers_output.*/decorate_workers_output = no/" $CONF_FPMPOOL
#  HACK: workaround for https://github.com/aelsabbahy/goss/issues/392
#  Run the child and parent test configs separately
RUN goss -g /tests/php-fpm/base.goss.yaml validate \
 && goss -g /tests/php-fpm/7.3.goss.yaml validate \
 && /aufs_hack.sh
