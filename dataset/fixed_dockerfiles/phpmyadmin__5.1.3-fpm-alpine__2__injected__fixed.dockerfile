FROM php:8.0-fpm-alpine
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#   docker-entrypoint.sh dependencies
RUN apk add bash=5.1.16-r2 tzdata=2023c-r0 --no-cache
#   Install dependencies
RUN set -ex ; apk add bzip2-dev=1.0.8-r1 freetype-dev=2.12.1-r0 libjpeg-turbo-dev=2.1.3-r1 libpng-dev=1.6.37-r1 libwebp-dev=1.2.3-r0 libxpm-dev=3.5.15-r0 libzip-dev=1.8.0-r1 --no-cache --virtual .build-deps ; docker-php-ext-configure gd --with-freetype --with-jpeg --with-webp --with-xpm ; docker-php-ext-install -j "$( nproc ;)" bz2 gd mysqli opcache zip
#   hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local/lib/php/extensions | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --no-cache --virtual .phpmyadmin-phpexts-rundeps ; apk del --no-network .build-deps
#   set recommended PHP.ini settings
#   see https://secure.php.net/manual/en/opcache.installation.php
ENV MAX_EXECUTION_TIME="600"
ENV MEMORY_LIMIT="512M"
ENV UPLOAD_LIMIT="2048K"
RUN set -ex ; { echo 'opcache.memory_consumption=128' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=4000' ;echo 'opcache.revalidate_freq=2' ;echo 'opcache.fast_shutdown=1' ; } > $PHP_INI_DIR/conf.d/opcache-recommended.ini; { echo 'session.cookie_httponly=1' ;echo 'session.use_strict_mode=1' ; } > $PHP_INI_DIR/conf.d/session-strict.ini; { echo 'allow_url_fopen=Off' ;echo 'max_execution_time=${MAX_EXECUTION_TIME}' ;echo 'max_input_vars=10000' ;echo 'memory_limit=${MEMORY_LIMIT}' ;echo 'post_max_size=${UPLOAD_LIMIT}' ;echo 'upload_max_filesize=${UPLOAD_LIMIT}' ; } > $PHP_INI_DIR/conf.d/phpmyadmin-misc.ini
#   Calculate download URL
ENV VERSION="5.1.3"
ENV SHA256="c562feddc0f8ff5e69629113f273a0d024a65fb928c48e89ce614744d478296f"
ENV URL="https://files.phpmyadmin.net/phpMyAdmin/${VERSION}/phpMyAdmin-${VERSION}-all-languages.tar.xz"
LABEL org.opencontainers.image.title="Official phpMyAdmin Docker image" \
      org.opencontainers.image.description="Run phpMyAdmin with Alpine, Apache and PHP FPM." \
      org.opencontainers.image.authors="The phpMyAdmin Team <developers@phpmyadmin.net>" \
      org.opencontainers.image.vendor="phpMyAdmin" \
      org.opencontainers.image.documentation="https://github.com/phpmyadmin/docker#readme" \
      org.opencontainers.image.licenses="GPL-2.0-only" \
      org.opencontainers.image.version="${VERSION}" \
      org.opencontainers.image.url="https://github.com/phpmyadmin/docker#readme" \
      org.opencontainers.image.source="https://github.com/phpmyadmin/docker.git"
#   Download tarball, verify it using gpg and extract
RUN set -ex ; apk add gnupg=2.2.35-r4 --no-cache --virtual .fetch-deps ; export GNUPGHOME="$( mktemp -d ;)" ; export GPGKEY="3D06A59ECE730EB71B511C17CE752F178259BD92" ; curl -fsSL -o phpMyAdmin.tar.xz $URL ; curl -fsSL -o phpMyAdmin.tar.xz.asc $URL.asc ; echo "$SHA256 *phpMyAdmin.tar.xz" | sha256sum -c - ; gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$GPGKEY" || gpg --batch --keyserver pgp.mit.edu --recv-keys "$GPGKEY" || gpg --batch --keyserver keyserver.pgp.com --recv-keys "$GPGKEY" || gpg --batch --keyserver keys.openpgp.org --recv-keys "$GPGKEY" ; gpg --batch --verify phpMyAdmin.tar.xz.asc phpMyAdmin.tar.xz ; tar -xf phpMyAdmin.tar.xz -C /var/www/html --strip-components=1 ; mkdir -p /var/www/html/tmp ; chown www-data:www-data /var/www/html/tmp ; gpgconf --kill all ; rm -r "$GNUPGHOME" phpMyAdmin.tar.xz phpMyAdmin.tar.xz.asc ; rm -r -v /var/www/html/setup/ /var/www/html/examples/ /var/www/html/js/src/ /var/www/html/templates/test/ /var/www/html/babel.config.json /var/www/html/doc/html/_sources/ /var/www/html/RELEASE-DATE-$VERSION /var/www/html/CONTRIBUTING.md ; sed -i "s@define('CONFIG_DIR'.*@define('CONFIG_DIR', '/etc/phpmyadmin/');@" /var/www/html/libraries/vendor_config.php ; apk del --no-network .fetch-deps
#   Copy configuration
COPY config.inc.php /etc/phpmyadmin/config.inc.php
#   Copy main script
COPY docker-entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["php-fpm"]
USER root
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
