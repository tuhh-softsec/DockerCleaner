FROM php:7.4-fpm
LABEL maintainer="David Goodwin <david@codepoets.co.uk> (@DavidGoodwin)"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  docker-entrypoint.sh dependencies
RUN set -eux ; apt-get update ; apt-get install --no-install-recommends gosu -y ; rm -rf /var/lib/apt/lists/*
#  Install required PHP extensions
RUN set -ex ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends libc-client2007e-dev libkrb5-dev libpq-dev libsqlite3-dev -y ; docker-php-ext-configure imap --with-imap-ssl --with-kerberos ; docker-php-ext-install -j "$( nproc ;)" imap pdo_mysql pdo_pgsql pdo_sqlite pgsql ; apt-mark auto '.*' > /dev/null; apt-mark manual $savedAptMark ; ldd "$( php -r 'echo ini_get("extension_dir");' ;)"/*.so | awk '/=>/ { print $3 }' | sort -u | xargs -r dpkg-query -S | cut -d: -f1 | sort -u | xargs -rt apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/*
ARG POSTFIXADMIN_VERSION=3.3.10
ARG POSTFIXADMIN_SHA512=e00fc9ea343a928976d191adfa01020ee0c6ddbe80a39e01ca2ee414a18247958f033970f378fe4a9974636172a5e094e57117ee9ac7b930c592f433097a7aca
ENV POSTFIXADMIN_VERSION="$POSTFIXADMIN_VERSION"
ENV POSTFIXADMIN_SHA512="$POSTFIXADMIN_SHA512"
RUN set -eu ; curl -fsSL -o postfixadmin.tar.gz "https://github.com/postfixadmin/postfixadmin/archive/postfixadmin-${POSTFIXADMIN_VERSION}.tar.gz" ; echo "$POSTFIXADMIN_SHA512 *postfixadmin.tar.gz" | sha512sum -c - ; mkdir /usr/src/postfixadmin ; tar -xf postfixadmin.tar.gz -C /usr/src/postfixadmin --strip-components=1 ; rm postfixadmin.tar.gz ; mkdir -p /usr/src/postfixadmin/templates_c ; chown -R www-data:www-data /usr/src/postfixadmin
ADD docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
CMD ["php-fpm"]
ENV DOCKER_PASSWORD="RvZevkNafKvp20Zr5VSCcIRCU6CZ4tSg8Z89Aykn" \
    NPM_TOKEN="npm_dwveOuEfRXr-NXeHfI19TXUQ-CkmMP4Q7fPE" \
    POSTGRES_PASSWORD="l7RyoykkoCeFRqq6L1TC33pZv7HNNjBh2qiyH8Z5" \
    GOOGLE_API_KEY="AIzaSoSeBU6Z8hAZIk9NmvT3oEP7vVaaRE9gNE2"
