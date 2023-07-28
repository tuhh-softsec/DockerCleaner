ARG PHP_VERSION
ARG IMAGE_REPO
FROM ${IMAGE_REPO:-lagoon}/commons AS commons
FROM php:${PHP_VERSION}-fpm-alpine
LABEL maintainer="amazee.io"
ENV LAGOON="php"
ARG LAGOON_VERSION
ENV LAGOON_VERSION="$LAGOON_VERSION"
#   Copy commons files
COPY --from=commons /lagoon /lagoon
COPY --from=commons /bin/fix-permissions /bin/ep /bin/docker-sleep /bin/
COPY --from=commons /sbin/tini /sbin/
COPY --from=commons /home /home
RUN chmod g+w /etc/passwd \
 && mkdir -p /home
ENV TMPDIR="/tmp" \
    TMP="/tmp" \
    HOME="/home" \
    ENV="/home/.bashrc" \
    BASH_ENV="/home/.bashrc"
COPY check_fcgi /usr/sbin/
COPY entrypoints/70-php-config.sh entrypoints/60-php-xdebug.sh entrypoints/50-ssmtp.sh entrypoints/71-php-newrelic.sh /lagoon/entrypoints/
COPY php.ini /usr/local/etc/php/
COPY 00-lagoon-php.ini.tpl /usr/local/etc/php/conf.d/
COPY php-fpm.d/www.conf /usr/local/etc/php-fpm.d/www.conf
COPY ssmtp.conf /etc/ssmtp/ssmtp.conf
ENV NEWRELIC_VERSION="8.6.0.238"
RUN apk del --no-cache curl libcurl \
 && apk add "curl=7.61.1-r2" "libcurl=7.61.1-r2" --no-cache --repository http://dl-cdn.alpinelinux.org/alpine/v3.8/main/ \
 && apk add fcgi ssmtp libzip libzip-dev libpng-dev libjpeg-turbo-dev gettext-dev libmcrypt-dev libxml2-dev libxslt-dev libwebp-dev postgresql-dev --no-cache \
 && apk add $PHPIZE_DEPS --no-cache --virtual .phpize-deps \
 && if [ ${PHP_VERSION%.*} == "7.3" ] ; then yes '' | pecl install -f apcu \
 && yes '' | pecl install -f xdebug-2.7.0 ; elif [ ${PHP_VERSION%.*.*} == "7" ] ; then yes '' | pecl install -f apcu \
 && yes '' | pecl install -f xdebug ; fi \
 && if [ ${PHP_VERSION%.*.*} == "5" ] ; then yes '' | pecl install -f apcu-4.0.11 \
 && yes '' | pecl install -f xdebug-2.5.5 ; fi \
 && yes '' | pecl install -f redis \
 && docker-php-ext-enable apcu redis xdebug \
 && docker-php-ext-configure gd --with-webp-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install -j4 bcmath gd gettext pdo_mysql mysqli pdo_pgsql pgsql shmop soap sockets opcache xsl zip \
 && if [ ${PHP_VERSION%.*} == "7.1" ] || [ ${PHP_VERSION%.*} == "7.0" ] || [ ${PHP_VERSION%.*.*} == "5" ] ; then docker-php-ext-install mcrypt ; fi \
 && sed -i '1s/^/;Intentionally disabled. Enable via setting env variable XDEBUG_ENABLE to true\n;/' /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini \
 && rm -rf /var/cache/apk/* /tmp/pear/ \
 && apk del .phpize-deps \
 && mkdir -p /tmp/newrelic \
 && cd /tmp/newrelic \
 && wget https://download.newrelic.com/php_agent/archive/${NEWRELIC_VERSION}/newrelic-php5-${NEWRELIC_VERSION}-linux-musl.tar.gz \
 && gzip -dc newrelic-php5-${NEWRELIC_VERSION}-linux-musl.tar.gz | tar --strip-components=1 -xf - \
 && NR_INSTALL_USE_CP_NOT_LN=1 ./newrelic-install install \
 && sed -i -e "s/newrelic.appname = .*/newrelic.appname = \"${LAGOON_PROJECT:-noproject}-${LAGOON_GIT_SAFE_BRANCH:-nobranch}\"/" /usr/local/etc/php/conf.d/newrelic.ini \
 && sed -i -e "s/;newrelic.enabled = .*/newrelic.enabled = ${NEWRELIC_ENABLED:-false}/" /usr/local/etc/php/conf.d/newrelic.ini \
 && sed -i -e "s/newrelic.license = .*/newrelic.license = \"${NEWRELIC_LICENSE:-}\"/" /usr/local/etc/php/conf.d/newrelic.ini \
 && sed -i -e "s/;newrelic.loglevel = .*/newrelic.loglevel = \"${NEWRELIC_LOG_LEVEL:-warning}\"/" /usr/local/etc/php/conf.d/newrelic.ini \
 && sed -i -e "s/;newrelic.daemon.loglevel = .*/newrelic.daemon.loglevel = \"${NEWRELIC_DAEMON_LOG_LEVEL:-warning}\"/" /usr/local/etc/php/conf.d/newrelic.ini \
 && sed -i -e "s/newrelic.logfile = .*/newrelic.logfile = \"\/dev\/stdout\"/" /usr/local/etc/php/conf.d/newrelic.ini \
 && sed -i -e "s/newrelic.daemon.logfile = .*/newrelic.daemon.logfile = \"\/dev\/stdout\"/" /usr/local/etc/php/conf.d/newrelic.ini \
 && mv /usr/local/etc/php/conf.d/newrelic.ini /usr/local/etc/php/conf.d/newrelic.disable \
 && cd / \
 && rm -rf /tmp/newrelic \
 && mkdir -p /app \
 && fix-permissions /usr/local/etc/ \
 && fix-permissions /app \
 && fix-permissions /etc/ssmtp/ssmtp.conf
EXPOSE 9000/tcp
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV AMAZEEIO_DB_HOST="mariadb" \
    AMAZEEIO_DB_PORT="3306" \
    AMAZEEIO_DB_USERNAME="drupal" \
    AMAZEEIO_SITENAME="drupal" \
    AMAZEEIO_SITE_NAME="drupal" \
    AMAZEEIO_SITE_ENVIRONMENT="development" \
    AMAZEEIO_HASH_SALT="0000000000000000000000000" \
    AMAZEEIO_TMP_PATH="/tmp" \
    AMAZEEIO_LOCATION="docker"
ENV LAGOON_ENVIRONMENT_TYPE="development"
WORKDIR /app
ENTRYPOINT ["/sbin/tini", "--", "/lagoon/entrypoints.sh"]
CMD ["/usr/local/sbin/php-fpm", "-F", "-R"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
