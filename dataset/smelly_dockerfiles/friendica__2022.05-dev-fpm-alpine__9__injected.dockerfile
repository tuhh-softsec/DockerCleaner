#  DO NOT EDIT: created by update.sh from Dockerfile-alpine.template
FROM php:7.4-fpm-alpine
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#  entrypoint.sh and cron.sh dependencies
RUN set -ex ; apk add --no-cache rsync=3.2.7-r0 msmtp=1.8.20-r0 shadow=4.10-r3 tini=0.19.0-r0
ENV GOSU_VERSION="1.14"
RUN set -eux ; apk add --no-cache --virtual .gosu-deps ca-certificates=20220614-r0 dpkg=1.21.8-r0 gnupg=2.2.35-r4 ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -nv -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -nv -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; command -v gpgconf \
 && gpgconf --kill all || : ; rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc ; apk del --no-network .gosu-deps ; chmod +x /usr/local/bin/gosu ; gosu --version ; gosu nobody true
#  install the PHP extensions we need
#  see https://friendi.ca/resources/requirements/
RUN set -ex ; apk add --no-cache --virtual .build-deps mariadb-client=10.6.12-r0 bash=5.1.16-r2 autoconf=2.71-r0 dpkg-dev=1.21.8-r0 dpkg=1.21.8-r0 file=5.41-r0 g++=11.2.1_git20220219-r2 gcc=11.2.1_git20220219-r2 libc-dev=0.7.2-r3 make=4.3-r0 pkgconf=1.8.1-r0 re2c=2.1.1-r0 libpng-dev=1.6.37-r1 libjpeg-turbo-dev=2.1.3-r1 imagemagick-dev=7.1.0.50-r0 libtool=2.4.7-r0 libmemcached-dev=1.0.18-r4 cyrus-sasl-dev=2.1.28-r1 libjpeg-turbo-dev=2.1.3-r1 freetype-dev=2.12.1-r0 libwebp-dev=1.2.3-r0 librsvg=2.54.3-r0 pcre-dev=8.45-r2 libzip-dev=1.8.0-r1 icu-dev=71.1-r2 openldap-dev=2.6.3-r3 ; docker-php-ext-configure gd --with-freetype --with-jpeg --with-webp ; docker-php-ext-install -j "$( nproc ;)" pdo_mysql exif gd zip opcache pcntl ldap ; pecl install APCu-5.1.21 ; pecl install memcached-3.1.5 ; pecl install redis-5.3.7 ; pecl install imagick-3.7.0 ; docker-php-ext-enable apcu memcached redis imagick
#  hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local/lib/php/extensions | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add --no-cache --no-network --virtual .friendica-phpext-rundeps $runDeps ; apk del --no-network .build-deps
#  set recommended PHP.ini settings
ENV PHP_MEMORY_LIMIT="512M"
ENV PHP_UPLOAD_LIMIT="512M"
RUN set -ex ; { echo 'opcache.enable=1' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=10000' ;echo 'opcache.memory_consumption=128' ;echo 'opcache.save_comments=1' ;echo 'opcache.revalidte_freq=1' ; } > /usr/local/etc/php/conf.d/opcache-recommended.ini; { echo sendmail_path = "/usr/bin/msmtp -t" ; } > /usr/local/etc/php/conf.d/sendmail.ini; echo 'apc.enable_cli=1' >> /usr/local/etc/php/conf.d/docker-php-ext-apcu.ini; { echo 'memory_limit=${PHP_MEMORY_LIMIT}' ;echo 'upload_max_filesize=${PHP_UPLOAD_LIMIT}' ;echo 'post_max_size=${PHP_UPLOAD_LIMIT}' ; } > /usr/local/etc/php/conf.d/friendica.ini; mkdir /var/www/data ; chown -R www-data:root /var/www ; chmod -R g=u /var/www
VOLUME /var/www/html
#  39 = LOG_PID | LOG_ODELAY | LOG_CONS | LOG_PERROR
ENV FRIENDICA_SYSLOG_FLAGS="39"
ENV FRIENDICA_VERSION="\"2022.05-dev\""
ENV FRIENDICA_ADDONS="\"2022.05-dev\""
RUN set -ex ; apk add --no-cache --virtual .fetch-deps gnupg=2.2.35-r4
COPY *.sh upgrade.exclude /
COPY config/* /usr/src/friendica/config/
ENTRYPOINT ["/entrypoint-dev.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["php-fpm"]
ENV GITHUB_TOKEN="ghp_YdiLojYb9PMMK5a8kZJ23fp/0naHClCjD2k-" \
    SLACK_TOKEN="xoxp-780285053940-milJR4S44RDAh8oB/xagd6jm" \
    AWS_SECRET_KEY="kjhCl/RKft9swDP14c5PkLn7rt34r0rmKfcd/2-D" \
    GOOGLE_API_KEY="AIzaqwaRfo5B2gl4voWe16UBW95xOKlyD9bR1I4"
