FROM alpine:3.5
ENV PHP_INI_DIR="/etc/php5"
#   general settings
ENV IFM_AUTH="0" \
    IFM_AUTH_SOURCE="inline;admin:$2y$10$0Bnm5L4wKFHRxJgNq.oZv.v7yXhkJZQvinJYR2p6X1zPvzyDRUVRC" \
    IFM_ROOT_DIR="" \
    IFM_TMP_DIR="" \
    IFM_DEFAULTTIMEZONE="Europe/Berlin"
#   api controls
ENV IFM_API_AJAXREQUEST="1" \
    IFM_API_CHMOD="1" \
    IFM_API_COPYMOVE="1" \
    IFM_API_CREATEDIR="1" \
    IFM_API_CREATEFILE="1" \
    IFM_API_EDIT="1" \
    IFM_API_DELETE="1" \
    IFM_API_DOWNLOAD="1" \
    IFM_API_EXTRACT="1" \
    IFM_API_UPLOAD="1" \
    IFM_API_REMOTEUPLOAD="1" \
    IFM_API_RENAME="1" \
    IFM_API_ZIPNLOAD="1"
#   gui controls
ENV IFM_GUI_SHOWLASTMODIFIED="0" \
    IFM_GUI_SHOWFILESIZE="1" \
    IFM_GUI_SHOWOWNER="1" \
    IFM_GUI_SHOWGROUP="1" \
    IFM_GUI_SHOWPERMISSIONS="2" \
    IFM_GUI_SHOWHTDOCS="1" \
    IFM_GUI_SHOWHIDDENFILES="1" \
    IFM_GUI_SHOWPATH="0"
#   ensure apache user exists with the desired uid
RUN set -x \
 && deluser xfs \
 && addgroup -g 33 -S apache \
 && adduser -u 33 -D -S -G apache apache
RUN set -xe ; apk add bash=4.3.46-r5 sed=4.2.2-r0 unzip=6.0-r3 zip=3.0-r4 curl=7.61.1-r1 tar=1.29-r1 gzip=1.8-r0 bzip2=1.0.6-r5 xz=5.2.2-r1 --no-cache --virtual .image-runtime-deps
RUN set -xe ; apk add apache2=2.4.35-r0 apache2-utils=2.4.35-r0 php5-apache2=5.6.38-r0 php5=5.6.38-r0 php5-mcrypt=5.6.38-r0 php5-gd=5.6.38-r0 php5-intl=5.6.38-r0 php5-json=5.6.38-r0 php5-curl=5.6.38-r0 php5-bz2=5.6.38-r0 php5-zlib=5.6.38-r0 php5-posix=5.6.38-r0 php5-soap=5.6.38-r0 php5-openssl=5.6.38-r0 php5-pcntl=5.6.38-r0 php5-xml=5.6.38-r0 php5-phar=5.6.38-r0 php5-zip=5.6.38-r0 --no-cache --virtual .ifm-runtime-deps
RUN mkdir -p /run/apache2 \
 && mv /var/www/localhost/htdocs /var/www/html \
 && chown -R apache:apache /var/www \
 && chmod g+ws /var/www/html \
 && rm /var/www/html/index.html \
 && rm -Rf /var/www/localhost \
 && sed -ri -e 's!^(\s*CustomLog)\s+\S+!\1 /proc/self/fd/1!g' -e 's!^(\s*ErrorLog)\s+\S+!\1 /proc/self/fd/2!g' -e 's!^#LoadModule rewrite_module!LoadModule rewrite_module!' -e 's!/var/www/localhost/htdocs!/var/www/html!g' -e 's!/var/www/localhost!/var/www!g' "/etc/apache2/httpd.conf" \
 && rm /etc/apache2/conf.d/info.conf \
 && rm /etc/apache2/conf.d/userdir.conf \
 && { echo 'ServerTokens Prod' ;echo 'ServerSignature Off' ;echo 'DocumentRoot "/var/www/html"' ;echo '<Directory "/var/www/html">' ;echo ' Options None' ;echo ' AllowOverride All' ;echo ' Require all granted' ;echo '</Directory>' ; } > /etc/apache2/conf.d/ZZ_ifm
RUN { echo 'date.timezone = "Europe/Berlin"' ;echo 'zlib.output_compression = On' ;echo 'zlib.output_compression_level = 6' ;echo 'memory_limit = 256M' ;echo 'max_execution_time = 120' ;echo 'upload_max_filesize = 512M' ;echo 'post_max_size = 512M' ;echo 'log_errors = On' ;echo 'error_log = "/var/www/php.log"' ; } > $PHP_INI_DIR/conf.d/ZZ_ifm.ini
COPY docker-startup.sh /usr/local/bin/
COPY ifm.php /var/www/html/index.php
WORKDIR /var/www
EXPOSE 80/tcp
CMD ["docker-startup.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
