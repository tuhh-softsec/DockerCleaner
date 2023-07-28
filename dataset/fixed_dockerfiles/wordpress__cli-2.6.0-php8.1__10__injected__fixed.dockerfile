#
#   NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#   PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM php:8.1-alpine
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#   install wp-cli dependencies
RUN apk add --no-cache bash=5.2.15-r0 less=608-r1 mysql-client=10.6.12-r0
RUN set -ex ; mkdir -p /var/www/html ; chown -R www-data:www-data /var/www/html
WORKDIR /var/www/html
#   install the PHP extensions we need (https://make.wordpress.org/hosting/handbook/handbook/server-environment/#php-extensions)
RUN set -ex ; apk add --no-cache --virtual .build-deps autoconf=2.71-r1 dpkg-dev=1.21.9-r0 dpkg=1.21.9-r0 file=5.43-r0 g++=12.2.1_git20220924-r4 gcc=12.2.1_git20220924-r4 libc-dev=0.7.2-r3 make=4.3-r1 pkgconf=1.9.4-r0 re2c=3.0-r0 freetype-dev=2.12.1-r0 icu-dev=72.1-r1 imagemagick-dev=7.1.0.62-r0 libjpeg-turbo-dev=2.1.4-r0 libpng-dev=1.6.38-r0 libwebp-dev=1.2.4-r1 libzip-dev=1.9.2-r2 ; docker-php-ext-configure gd --with-freetype --with-jpeg --with-webp ; docker-php-ext-install -j "$( nproc ;)" bcmath exif gd intl mysqli zip ; pecl install imagick-3.6.0 ; docker-php-ext-enable imagick ; rm -r /tmp/pear ; out="$( php -r 'exit(0);' ;)" ; [ -z "$out" ] ; err="$( php -r 'exit(0);' 3>&1 1>&2 2>&3;)" ; [ -z "$err" ] ; extDir="$( php -r 'echo ini_get("extension_dir");' ;)" ; [ -d "$extDir" ]
#   hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive "$extDir" | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add --no-cache --no-network --virtual .wordpress-phpexts-rundeps $runDeps ; apk del --no-network .build-deps ; ! { ldd "$extDir"/*.so | grep 'not found' ; } ; err="$( php --version 3>&1 1>&2 2>&3;)" ; [ -z "$err" ]
#   set recommended PHP.ini settings
#   excluding opcache due https://github.com/docker-library/wordpress/issues/407
#   https://wordpress.org/support/article/editing-wp-config-php/#configure-error-logging
RUN { echo 'error_reporting = E_ERROR | E_WARNING | E_PARSE | E_CORE_ERROR | E_CORE_WARNING | E_COMPILE_ERROR | E_COMPILE_WARNING | E_RECOVERABLE_ERROR' ;echo 'display_errors = Off' ;echo 'display_startup_errors = Off' ;echo 'log_errors = On' ;echo 'error_log = /dev/stderr' ;echo 'log_errors_max_len = 1024' ;echo 'ignore_repeated_errors = On' ;echo 'ignore_repeated_source = Off' ;echo 'html_errors = Off' ; } > /usr/local/etc/php/conf.d/error-logging.ini
#   https://make.wordpress.org/cli/2018/05/31/gpg-signature-change/
#   pub   rsa2048 2018-05-31 [SC]
#         63AF 7AA1 5067 C056 16FD  DD88 A3A2 E8F2 26F0 BC06
#   uid           [ unknown] WP-CLI Releases <releases@wp-cli.org>
#   sub   rsa2048 2018-05-31 [E]
ENV WORDPRESS_CLI_GPG_KEY="63AF7AA15067C05616FDDD88A3A2E8F226F0BC06"
ENV WORDPRESS_CLI_VERSION="2.6.0"
ENV WORDPRESS_CLI_SHA512="d73f9161a1f03b8ecaac7b196b6051fe847b3c402b9c92b1f6f3acbe5b1cf91f7260c0e499b8947bab75920ecec918b39533ca65fa5a1fd3eb6ce7b8e2c58e7d"
RUN set -ex ; apk add --no-cache --virtual .fetch-deps gnupg=2.2.40-r0 ; curl -o /usr/local/bin/wp.gpg -fL "https://github.com/wp-cli/wp-cli/releases/download/v${WORDPRESS_CLI_VERSION}/wp-cli-${WORDPRESS_CLI_VERSION}.phar.gpg" ; GNUPGHOME="$( mktemp -d ;)" ; export GNUPGHOME ; gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$WORDPRESS_CLI_GPG_KEY" ; gpg --batch --decrypt --output /usr/local/bin/wp /usr/local/bin/wp.gpg ; gpgconf --kill all ; rm -rf "$GNUPGHOME" /usr/local/bin/wp.gpg ; unset GNUPGHOME ; echo "$WORDPRESS_CLI_SHA512 */usr/local/bin/wp" | sha512sum -c - ; chmod +x /usr/local/bin/wp ; apk del --no-network .fetch-deps ; wp --allow-root --version
VOLUME /var/www/html
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
USER www-data
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["wp", "shell"]
USER root
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
