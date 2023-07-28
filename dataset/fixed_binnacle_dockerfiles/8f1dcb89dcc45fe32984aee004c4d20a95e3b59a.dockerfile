FROM php:7.3-fpm-alpine
ARG BUILD_DATE
ARG VCS_REF
LABEL maintainer="Simon Erhardt <hello@rootlogin.ch>" \
      org.label-schema.name="Nextcloud" \
      org.label-schema.description="Minimal Nextcloud docker image based on Alpine Linux." \
      org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="https://github.com/chrootLogin/docker-nextcloud" \
      org.label-schema.schema-version="1.0"
ARG NEXTCLOUD_GPG="2880 6A87 8AE4 23A2 8372  792E D758 99B9 A724 937A"
ARG NEXTCLOUD_VERSION=15.0.7
ARG UID=1501
ARG GID=1501
RUN set -ex \
 && addgroup -g ${GID} nextcloud \
 && adduser -u ${UID} -h /opt/nextcloud -H -G nextcloud -s /sbin/nologin -D nextcloud \
 && apk update \
 && apk upgrade \
 && apk add alpine-sdk=1.0-r1 autoconf=2.71-r0 bash=5.1.16-r0 freetype=2.11.1-r2 freetype-dev=2.11.1-r2 gnupg=2.2.31-r2 icu-dev=69.1-r1 icu-libs=69.1-r1 imagemagick=7.1.0.16-r0 imagemagick-dev=7.1.0.16-r0 libjpeg-turbo=2.1.2-r0 libjpeg-turbo-dev=2.1.2-r0 libldap=2.6.2-r0 libmcrypt=2.5.8-r9 libmcrypt-dev=2.5.8-r9 libmemcached=1.0.18-r4 libmemcached-dev=1.0.18-r4 libpng=1.6.37-r1 libpng-dev=1.6.37-r1 libzip=1.8.0-r1 libzip-dev=1.8.0-r1 nginx=1.20.2-r1 openldap-dev=2.6.2-r0 openssl=1.1.1t-r3 pcre=8.45-r1 pcre-dev=8.45-r1 postgresql-dev postgresql-libs samba-client=4.15.13-r0 sudo=1.9.12_p2-r0 supervisor=4.2.2-r2 tar=1.34-r1 tini=0.19.0-r0 wget=1.21.2-r2 \
 && docker-php-ext-configure gd --with-freetype-dir=/usr --with-png-dir=/usr --with-jpeg-dir=/usr \
 && docker-php-ext-configure ldap \
 && docker-php-ext-configure zip --with-libzip=/usr \
 && docker-php-ext-install gd exif intl mbstring ldap mysqli opcache pcntl pdo_mysql pdo_pgsql pgsql zip \
 && pecl install APCu-5.1.16 \
 && pecl install imagick-3.4.3 \
 && pecl install mcrypt-1.0.2 \
 && pecl install memcached-3.1.3 \
 && pecl install redis-4.2.0 \
 && docker-php-ext-enable apcu imagick mcrypt memcached redis \
 && apk del alpine-sdk autoconf freetype-dev icu-dev imagemagick-dev libmcrypt-dev libmemcached-dev libjpeg-turbo-dev libpng-dev libzip-dev openldap-dev pcre-dev postgresql-dev \
 && rm -rf /var/cache/apk/* \
 && mkdir -p /opt/nextcloud \
 && cd /tmp \
 && NEXTCLOUD_TARBALL="nextcloud-${NEXTCLOUD_VERSION}.tar.bz2" \
 && wget -q https://download.nextcloud.com/server/releases/${NEXTCLOUD_TARBALL} \
 && wget -q https://download.nextcloud.com/server/releases/${NEXTCLOUD_TARBALL}.sha256 \
 && wget -q https://download.nextcloud.com/server/releases/${NEXTCLOUD_TARBALL}.asc \
 && wget -q https://nextcloud.com/nextcloud.asc \
 && echo "Verifying both integrity and authenticity of ${NEXTCLOUD_TARBALL}..." \
 && CHECKSUM_STATE=$( echo -n $( sha256sum -c ${NEXTCLOUD_TARBALL}.sha256 ;) | tail -c 2 ;) \
 && if [ "${CHECKSUM_STATE}" != "OK" ] ; then echo "Warning! Checksum does not match!" \
 && exit 1 ; fi \
 && gpg --import nextcloud.asc \
 && FINGERPRINT="$( LANG=C gpg --verify ${NEXTCLOUD_TARBALL}.asc ${NEXTCLOUD_TARBALL} 2>&1 | sed -n "s#Primary key fingerprint: \(.*\)#\1#p" ;)" \
 && if [ -z "${FINGERPRINT}" ] ; then echo "Warning! Invalid GPG signature!" \
 && exit 1 ; fi \
 && if [ "${FINGERPRINT}" != "${NEXTCLOUD_GPG}" ] ; then echo "Warning! Wrong GPG fingerprint!" \
 && exit 1 ; fi \
 && echo "All seems good, now unpacking ${NEXTCLOUD_TARBALL}..." \
 && tar xjf ${NEXTCLOUD_TARBALL} --strip-components=1 -C /opt/nextcloud \
 && rm -rf /opt/nextcloud/updater \
 && rm -rf /tmp/* /root/.gnupg \
 && rm -rf /var/www/*
COPY root /
RUN chmod +x /usr/local/bin/run.sh /usr/local/bin/occ /etc/periodic/15min/nextcloud
VOLUME ["/data"]
EXPOSE 80/tcp
ENTRYPOINT ["/sbin/tini", "--"]
CMD ["/usr/local/bin/run.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
