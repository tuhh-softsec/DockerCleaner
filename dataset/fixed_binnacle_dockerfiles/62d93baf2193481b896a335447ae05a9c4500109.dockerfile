FROM php:7.1-fpm-alpine
ENV PHPREDIS_VERSION="3.1.2"
RUN apk add curl=7.66.0-r4 curl-dev=7.66.0-r4 freetype-dev=2.10.0-r1 icu=64.2-r1 icu-dev=64.2-r1 libintl=0.19.8.1-r4 libjpeg-turbo-dev=2.0.4-r1 libmcrypt-dev=2.5.8-r7 libpng-dev=1.6.37-r1 libxml2-dev=2.9.9-r5 $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && apk add bash=5.0.0-r0 grep=3.3-r0 sed=4.7-r0 git=2.22.5-r0 mariadb-client=10.3.29-r0 subversion=1.12.2-r1 --no-cache --virtual .persistent-deps \
 && docker-php-ext-configure gd --with-png-dir=/usr --with-jpeg-dir=/usr \
 && docker-php-ext-install bcmath curl exif gd iconv intl mbstring mcrypt mysqli opcache zip \
 && { echo 'opcache.memory_consumption=128' ;echo 'opcache.interned_strings_buffer=8' ;echo 'opcache.max_accelerated_files=4000' ;echo 'opcache.revalidate_freq=60' ;echo 'opcache.fast_shutdown=1' ;echo 'opcache.enable_cli=1' ; } > /usr/local/etc/php/conf.d/opcache-recommended.ini \
 && curl -L -o /tmp/redis.tar.gz https://github.com/phpredis/phpredis/archive/$PHPREDIS_VERSION.tar.gz \
 && mkdir /tmp/redis \
 && tar -xf /tmp/redis.tar.gz -C /tmp/redis \
 && rm /tmp/redis.tar.gz \
 && (cd /tmp/redis/phpredis-$PHPREDIS_VERSION \
 && phpize \
 && ./configure \
 && make -j$( nproc ;) \
 && make install ) \
 && rm -r /tmp/redis \
 && docker-php-ext-enable redis \
 && yes | pecl install xdebug \
 && echo "zend_extension=$( find /usr/local/lib/php/extensions/ -name xdebug.so ;)" > /usr/local/etc/php/conf.d/xdebug.ini \
 && echo "xdebug.remote_enable=on" >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo "xdebug.remote_autostart=off" >> /usr/local/etc/php/conf.d/xdebug.ini \
 && find /usr/local/lib/php/extensions -name '*.a' -delete \
 && find /usr/local/lib/php/extensions -name '*.so' -exec strip --strip-all '{}'
#   Install wp-cli
RUN curl -o /usr/local/bin/wp -SL https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli-nightly.phar \
 && chmod +x /usr/local/bin/wp
#   Install PHPUnit
RUN curl https://phar.phpunit.de/phpunit-5.7.5.phar -L -o phpunit.phar \
 && chmod +x phpunit.phar \
 && mv phpunit.phar /usr/local/bin/phpunit
#   Install phpcs & wpcs standard
RUN curl -o /usr/local/bin/phpcs -SL https://squizlabs.github.io/PHP_CodeSniffer/phpcs.phar \
 && chmod +x /usr/local/bin/phpcs \
 && git clone -b master --depth=1 https://github.com/WordPress-Coding-Standards/WordPress-Coding-Standards.git /usr/local/bin/wpcs \
 && /usr/local/bin/phpcs --config-set show_progress 1 \
 && /usr/local/bin/phpcs --config-set colors 1 \
 && /usr/local/bin/phpcs --config-set installed_paths /usr/local/bin/wpcs
#   Install phpcbf
RUN curl -o /usr/local/bin/phpcbf -SL https://squizlabs.github.io/PHP_CodeSniffer/phpcbf.phar \
 && chmod +x /usr/local/bin/phpcbf
WORKDIR /var/www/html/wordpress/
CMD ["php-fpm"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
