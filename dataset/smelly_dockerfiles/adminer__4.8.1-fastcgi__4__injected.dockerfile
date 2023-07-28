FROM php:7.4-fpm-alpine
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN echo "upload_max_filesize = 128M" >> /usr/local/etc/php/conf.d/0-upload_large_dumps.ini \
 && echo "post_max_size = 128M" >> /usr/local/etc/php/conf.d/0-upload_large_dumps.ini \
 && echo "memory_limit = 1G" >> /usr/local/etc/php/conf.d/0-upload_large_dumps.ini \
 && echo "max_execution_time = 600" >> /usr/local/etc/php/conf.d/0-upload_large_dumps.ini \
 && echo "max_input_vars = 5000" >> /usr/local/etc/php/conf.d/0-upload_large_dumps.ini
RUN addgroup -S adminer \
 && adduser -S -G adminer adminer \
 && mkdir -p /var/www/html \
 && mkdir -p /var/www/html/plugins-enabled \
 && chown -R adminer:adminer /var/www/html
RUN set -x \
 && apk add --no-cache --virtual .build-deps postgresql14-dev=14.7-r0 sqlite-dev=3.38.5-r0 unixodbc-dev=2.3.11-r0 freetds-dev=1.3.10-r0 \
 && docker-php-ext-configure pdo_odbc --with-pdo-odbc=unixODBC,/usr \
 && docker-php-ext-install pdo_mysql pdo_pgsql pdo_sqlite pdo_odbc pdo_dblib
#  hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local/lib/php/extensions | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add --no-cache --virtual .phpexts-rundeps $runDeps \
 && apk del .build-deps
ADD *.php /var/www/html/
ENV ADMINER_VERSION="4.8.1"
ENV ADMINER_DOWNLOAD_SHA256="2fd7e6d8f987b243ab1839249551f62adce19704c47d3d0c8dd9e57ea5b9c6b3 "
ENV ADMINER_SRC_DOWNLOAD_SHA256="ef832414296d11eed33e9d85fff3fb316c63f13f05fceb4a961cbe4cb2ae8712 "
RUN set -x \
 && curl -fsSL https://github.com/vrana/adminer/releases/download/v$ADMINER_VERSION/adminer-$ADMINER_VERSION.php -o adminer.php \
 && echo "$ADMINER_DOWNLOAD_SHA256 adminer.php" | sha256sum -c - \
 && curl -fsSL https://github.com/vrana/adminer/archive/v$ADMINER_VERSION.tar.gz -o source.tar.gz \
 && echo "$ADMINER_SRC_DOWNLOAD_SHA256 source.tar.gz" | sha256sum -c - \
 && tar xzf source.tar.gz --strip-components=1 "adminer-$ADMINER_VERSION/designs/" "adminer-$ADMINER_VERSION/plugins/" \
 && rm source.tar.gz
ADD entrypoint.sh /usr/local/bin/
ENTRYPOINT ["entrypoint.sh", "docker-php-entrypoint"]
USER adminer
CMD ["php-fpm"]
