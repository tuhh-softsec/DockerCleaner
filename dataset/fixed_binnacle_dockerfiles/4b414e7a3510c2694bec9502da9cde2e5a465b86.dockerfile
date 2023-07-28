FROM alpine:3.9
ARG BUILD_DATE
ARG VCS_REF
ARG VERSION
LABEL maintainer="CrazyMax" \
      org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.name="nextcloud" \
      org.label-schema.description="Nextcloud" \
      org.label-schema.version="$VERSION" \
      org.label-schema.url="https://github.com/crazy-max/docker-nextcloud" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="https://github.com/crazy-max/docker-nextcloud" \
      org.label-schema.vendor="CrazyMax" \
      org.label-schema.schema-version="1.0"
ENV NEXTCLOUD_VERSION="14.0.12" \
    CRONTAB_PATH="/var/spool/cron/crontabs"
RUN apk add ca-certificates=20191127-r2 ffmpeg=4.0.6-r0 libressl=2.7.5-r0 libsmbclient=4.8.12-r2 libxml2=2.9.9-r3 nginx=1.14.2-r5 php7=7.2.33-r0 php7-apcu php7-bz2=7.2.33-r0 php7-cli php7-ctype=7.2.33-r0 php7-curl=7.2.33-r0 php7-dom=7.2.33-r0 php7-exif=7.2.33-r0 php7-fileinfo=7.2.33-r0 php7-fpm=7.2.33-r0 php7-ftp=7.2.33-r0 php7-gmp=7.2.33-r0 php7-iconv=7.2.33-r0 php7-imagick php7-intl=7.2.33-r0 php7-json=7.2.33-r0 php7-ldap=7.2.33-r0 php7-mbstring=7.2.33-r0 php7-mcrypt php7-memcached php7-opcache=7.2.33-r0 php7-openssl=7.2.33-r0 php7-pcntl=7.2.33-r0 php7-pdo=7.2.33-r0 php7-pdo_mysql=7.2.33-r0 php7-pdo_pgsql=7.2.33-r0 php7-pdo_sqlite=7.2.33-r0 php7-posix=7.2.33-r0 php7-redis php7-session=7.2.33-r0 php7-simplexml=7.2.33-r0 php7-sqlite3=7.2.33-r0 php7-xml=7.2.33-r0 php7-xmlreader=7.2.33-r0 php7-xmlwriter=7.2.33-r0 php7-zip=7.2.33-r0 php7-zlib python3=3.6.9-r3 su-exec=0.2-r0 supervisor=3.3.4-r1 tzdata=2020c-r1 --update --no-cache \
 && apk add autoconf=2.69-r2 automake=1.16.1-r0 build-base=0.5-r1 gnupg=2.2.19-r0 libtool=2.4.6-r5 pcre-dev=8.42-r2 php7-dev=7.2.33-r0 php7-pear=7.2.33-r0 samba-dev=4.8.12-r2 tar=1.32-r0 wget=1.20.3-r0 --update --no-cache -t build-dependencies \
 && pip3 install --upgrade pip \
 && pip3 install nextcloud_news_updater --install-option="--install-scripts=/usr/bin" \
 && rm -rf /var/www/* /tmp/* \
 && cd /tmp \
 && wget -q https://pecl.php.net/get/smbclient-1.0.0.tgz \
 && pecl install smbclient-1.0.0.tgz \
 && wget -q https://download.nextcloud.com/server/releases/nextcloud-${NEXTCLOUD_VERSION}.tar.bz2 \
 && wget -q https://download.nextcloud.com/server/releases/nextcloud-${NEXTCLOUD_VERSION}.tar.bz2.asc \
 && wget -q https://nextcloud.com/nextcloud.asc \
 && gpg --import nextcloud.asc \
 && gpg --verify --batch --no-tty nextcloud-${NEXTCLOUD_VERSION}.tar.bz2.asc nextcloud-${NEXTCLOUD_VERSION}.tar.bz2 \
 && tar -xjf nextcloud-${NEXTCLOUD_VERSION}.tar.bz2 --strip 1 -C /var/www \
 && rm -f nextcloud-${NEXTCLOUD_VERSION}.tar* nextcloud.asc \
 && chown -R nginx. /var/lib/nginx /var/log/nginx /var/log/php7 /var/tmp/nginx /var/www \
 && apk del build-dependencies \
 && rm -rf /root/.gnupg /tmp/* /var/cache/apk/* /var/www/updater
COPY entrypoint.sh /entrypoint.sh
COPY assets /
RUN chmod a+x /entrypoint.sh /usr/local/bin/* \
 && chown -R nginx. /tpls/data /tpls/bootstrap.php
EXPOSE 8000/tcp
WORKDIR /var/www
VOLUME [ "/data" ]
ENTRYPOINT ["/entrypoint.sh"]
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisord.conf"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
