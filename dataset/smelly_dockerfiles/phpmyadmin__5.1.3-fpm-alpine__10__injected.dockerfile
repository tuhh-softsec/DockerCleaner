FROM php:8.0-fpm-alpine
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#  docker-entrypoint.sh dependencies
RUN apk add bash tzdata --no-cache
#  Install dependencies
RUN set -ex ; apk add bzip2-dev freetype-dev libjpeg-turbo-dev libpng-dev libwebp-dev libxpm-dev libzip-dev --no-cache --virtual .build-deps ; docker-php-ext-configure gd --with-freetype --with-jpeg --with-webp --with-xpm ; docker-php-ext-install -j "$( nproc ;)" bz2 gd mysqli opcache zip
#  hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local/lib/php/extensions | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --no-cache --virtual .phpmyadmin-phpexts-rundeps ; apk del --no-network .build-deps
#  set recommended PHP.ini settings
#  see https://secure.php.net/manual/en/opcache.installation.php
ENV MAX_EXECUTION_TIME="600"
ENV MEMORY_LIMIT="512M"
ENV UPLOAD_LIMIT="2048K"
RUN set -ex ; { echo 'opcache.memory_consumption=128' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=4000' ;echo 'opcache.revalidate_freq=2' ;echo 'opcache.fast_shutdown=1' ; } > $PHP_INI_DIR/conf.d/opcache-recommended.ini; { echo 'session.cookie_httponly=1' ;echo 'session.use_strict_mode=1' ; } > $PHP_INI_DIR/conf.d/session-strict.ini; { echo 'allow_url_fopen=Off' ;echo 'max_execution_time=${MAX_EXECUTION_TIME}' ;echo 'max_input_vars=10000' ;echo 'memory_limit=${MEMORY_LIMIT}' ;echo 'post_max_size=${UPLOAD_LIMIT}' ;echo 'upload_max_filesize=${UPLOAD_LIMIT}' ; } > $PHP_INI_DIR/conf.d/phpmyadmin-misc.ini
#  Calculate download URL
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
#  Download tarball, verify it using gpg and extract
RUN set -ex ; apk add gnupg --no-cache --virtual .fetch-deps ; export GNUPGHOME="$( mktemp -d ;)" ; export GPGKEY="3D06A59ECE730EB71B511C17CE752F178259BD92" ; curl -fsSL -o phpMyAdmin.tar.xz $URL ; curl -fsSL -o phpMyAdmin.tar.xz.asc $URL.asc ; echo "$SHA256 *phpMyAdmin.tar.xz" | sha256sum -c - ; gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$GPGKEY" || gpg --batch --keyserver pgp.mit.edu --recv-keys "$GPGKEY" || gpg --batch --keyserver keyserver.pgp.com --recv-keys "$GPGKEY" || gpg --batch --keyserver keys.openpgp.org --recv-keys "$GPGKEY" ; gpg --batch --verify phpMyAdmin.tar.xz.asc phpMyAdmin.tar.xz ; tar -xf phpMyAdmin.tar.xz -C /var/www/html --strip-components=1 ; mkdir -p /var/www/html/tmp ; chown www-data:www-data /var/www/html/tmp ; gpgconf --kill all ; rm -r "$GNUPGHOME" phpMyAdmin.tar.xz phpMyAdmin.tar.xz.asc ; rm -r -v /var/www/html/setup/ /var/www/html/examples/ /var/www/html/js/src/ /var/www/html/templates/test/ /var/www/html/babel.config.json /var/www/html/doc/html/_sources/ /var/www/html/RELEASE-DATE-$VERSION /var/www/html/CONTRIBUTING.md ; sed -i "s@define('CONFIG_DIR'.*@define('CONFIG_DIR', '/etc/phpmyadmin/');@" /var/www/html/libraries/vendor_config.php ; apk del --no-network .fetch-deps
#  Copy configuration
COPY config.inc.php /etc/phpmyadmin/config.inc.php
#  Copy main script
COPY docker-entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["php-fpm"]
USER 0
