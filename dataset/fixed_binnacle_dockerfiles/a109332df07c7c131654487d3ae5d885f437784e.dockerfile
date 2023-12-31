FROM php:7.2-alpine
#   install the PHP extensions we need (https://make.wordpress.org/hosting/handbook/handbook/server-environment/#php-extensions)
RUN set -ex ; apk add imagemagick-dev=7.1.0.62-r0 libjpeg-turbo-dev=2.1.4-r0 libpng-dev=1.6.38-r0 $PHPIZE_DEPS --no-cache --virtual .build-deps ; docker-php-ext-configure gd --with-png-dir=/usr --with-jpeg-dir=/usr ; docker-php-ext-install bcmath exif gd mysqli opcache zip ; pecl install imagick-3.4.4 ; docker-php-ext-enable imagick ; runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local/lib/php/extensions | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --virtual .wordpress-phpexts-rundeps ; apk del .build-deps
#   set recommended PHP.ini settings
#   see https://secure.php.net/manual/en/opcache.installation.php
RUN { echo 'opcache.memory_consumption=128' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=4000' ;echo 'opcache.revalidate_freq=2' ;echo 'opcache.fast_shutdown=1' ;echo 'opcache.enable_cli=1' ; } > /usr/local/etc/php/conf.d/opcache-recommended.ini
#   https://codex.wordpress.org/Editing_wp-config.php#Configure_Error_Logging
RUN { echo 'error_reporting = 4339' ;echo 'display_errors = Off' ;echo 'display_startup_errors = Off' ;echo 'log_errors = On' ;echo 'error_log = /dev/stderr' ;echo 'log_errors_max_len = 1024' ;echo 'ignore_repeated_errors = On' ;echo 'ignore_repeated_source = Off' ;echo 'html_errors = Off' ; } > /usr/local/etc/php/conf.d/error-logging.ini
#   install wp-cli dependencies
RUN apk add bash=5.2.15-r0 less=608-r1 mysql-client=10.6.12-r0 --no-cache
RUN set -ex ; mkdir -p /var/www/html ; chown -R www-data:www-data /var/www/html
WORKDIR /var/www/html
VOLUME /var/www/html
#   https://make.wordpress.org/cli/2018/05/31/gpg-signature-change/
#   pub   rsa2048 2018-05-31 [SC]
#         63AF 7AA1 5067 C056 16FD  DD88 A3A2 E8F2 26F0 BC06
#   uid           [ unknown] WP-CLI Releases <releases@wp-cli.org>
#   sub   rsa2048 2018-05-31 [E]
ENV WORDPRESS_CLI_GPG_KEY="63AF7AA15067C05616FDDD88A3A2E8F226F0BC06"
ENV WORDPRESS_CLI_VERSION="2.2.0"
ENV WORDPRESS_CLI_SHA512="2103f04a5014d629eaa42755815c9cec6bb489ed7b0ea6e77dedb309e8af098ab902b2f9c6369ae4b7cb8cc1f20fbb4dedcda83eb1d0c34b880fa6e8a3ae249d"
RUN set -ex ; apk add gnupg=2.2.40-r0 --no-cache --virtual .fetch-deps ; curl -o /usr/local/bin/wp.gpg -fSL "https://github.com/wp-cli/wp-cli/releases/download/v${WORDPRESS_CLI_VERSION}/wp-cli-${WORDPRESS_CLI_VERSION}.phar.gpg" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$WORDPRESS_CLI_GPG_KEY" ; gpg --batch --decrypt --output /usr/local/bin/wp /usr/local/bin/wp.gpg ; command -v gpgconf \
 && gpgconf --kill all || : ; rm -rf "$GNUPGHOME" /usr/local/bin/wp.gpg ; echo "$WORDPRESS_CLI_SHA512 */usr/local/bin/wp" | sha512sum -c - ; chmod +x /usr/local/bin/wp ; apk del .fetch-deps ; wp --allow-root --version
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
USER www-data
CMD ["wp", "shell"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
