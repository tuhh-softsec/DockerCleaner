FROM alpine:3.10
ARG NEXTCLOUD_VERSION=16.0.1
ARG GPG_nextcloud="2880 6A87 8AE4 23A2 8372  792E D758 99B9 A724 937A"
ARG BUILD_CORES
ARG NGINX_VER=1.17.0
ARG PHP_VER=7.3.6
ARG LIBICONV_VERSION=1.16
LABEL description="A server software for creating file hosting services" \
      nextcloud="Nextcloud v${NEXTCLOUD_VERSION}" \
      maintainer="Starbix <starbix@laubacher.io>" \
      php_version="PHP v$PHP_VER built from source" \
      nginx_version="NGINX v$NGINX_VER built from source"
ARG PHP_MIRROR=https://secure.php.net
ARG GNU_MIRROR=https://mirror.init7.net
ARG NGINX_CONF="  --prefix=/nginx  --sbin-path=/usr/local/sbin/nginx  --http-log-path=/nginx/logs/access.log  --error-log-path=/nginx/logs/error.log  --pid-path=/nginx/run/nginx.pid  --lock-path=/nginx/run/nginx.lock  --with-threads  --with-file-aio  --without-http_geo_module  --without-http_autoindex_module  --without-http_split_clients_module  --without-http_memcached_module  --without-http_empty_gif_module  --without-http_browser_module"
ARG PHP_CONF="  --prefix=/usr  --libdir=/usr/lib/php  --datadir=/usr/share/php  --sysconfdir=/php/etc  --localstatedir=/php/var  --with-pear=/usr/share/php  --with-config-file-scan-dir=/php/conf.d  --with-config-file-path=/php  --enable-option-checking=fatal  --enable-zip=shared  --with-libzip=/usr  --with-pic  --disable-short-tags  --without-readline  --enable-bcmath=shared  --enable-fpm  --disable-cgi  --enable-mysqlnd  --enable-mbstring  --with-curl  --with-libedit  --with-openssl  --with-iconv=/usr/local  --with-gd  --with-jpeg-dir  --with-png-dir  --with-webp-dir  --with-xpm-dir=no  --with-freetype-dir  --disable-gd-jis-conv  --with-sodium=shared  --with-zlib"
ARG PHP_EXT_LIST="  mysqli  ctype  dom  json  xml  mbstring  posix  xmlwriter  zip  zlib  sqlite3  pdo_sqlite  pdo_pgsql  pdo_mysql  pcntl  curl  fileinfo  bz2  intl  openssl  ldap  simplexml  pgsql  ftp  exif  gmp  sodium  imap"
ARG CUSTOM_BUILD_PKGS="  freetype-dev  openldap-dev  gmp-dev  icu-dev  postgresql-dev  libpng-dev  libwebp-dev  gd-dev  libjpeg-turbo-dev  libxpm-dev  libedit-dev  libxml2-dev  openssl-dev  libbz2  re2c  sqlite-dev  imagemagick-dev  libsodium-dev  libzip-dev  imap-dev"
ARG CUSTOM_PKGS="  freetype  openldap  gmp  bzip2-dev  icu  libpq  libzip  libsodium  imagemagick  c-client"
ENV UID="991" \
    GID="991" \
    UPLOAD_MAX_SIZE="25G" \
    APC_SHM_SIZE="128M" \
    OPCACHE_MEM_SIZE="128" \
    MEMORY_LIMIT="512M" \
    CRON_PERIOD="15m" \
    CRON_MEMORY_LIMIT="1g" \
    TZ="Etc/UTC" \
    DB_TYPE="sqlite3" \
    DOMAIN="localhost"
COPY rootfs-nginx-php /
RUN apk -U upgrade \
 && apk add gnupg=2.2.19-r0 tar=1.32-r1 build-base=0.5-r1 autoconf=2.69-r2 automake=1.16.1-r0 pcre-dev=8.43-r1 libtool=2.4.6-r6 samba-dev=4.10.18-r0 linux-headers=4.19.36-r0 libtool=2.4.6-r6 build-base=0.5-r1 pcre-dev=8.43-r1 zlib-dev=1.2.11-r1 wget=1.20.3-r0 gnupg=2.2.19-r0 autoconf=2.69-r2 gcc=8.3.0-r0 g++=8.3.0-r0 libc-dev=0.7.1-r0 make=4.2.1-r2 pkgconf=1.6.1-r1 curl-dev=7.66.0-r4 ca-certificates=20191127-r2 ${CUSTOM_BUILD_PKGS} -t build-dependencies \
 && apk add openssl=1.1.1k-r0 ca-certificates=20191127-r2 libsmbclient=4.10.18-r0 tzdata=2021a-r0 s6=2.8.0.1-r0 su-exec=0.2-r0 curl=7.66.0-r4 libedit=20190324.3.1-r0 libxml2=2.9.9-r5 libwebp=1.0.2-r0 gd=2.2.5-r3 pcre=8.43-r1 zlib=1.2.11-r1 ${CUSTOM_PKGS} \
 && NB_CORES=${BUILD_CORES-$( getconf _NPROCESSORS_CONF ;)} \
 && wget https://nginx.org/download/nginx-${NGINX_VER}.tar.gz -O /tmp/nginx-${NGINX_VER}.tar.gz \
 && wget https://nginx.org/download/nginx-${NGINX_VER}.tar.gz.asc -O /tmp/nginx-${NGINX_VER}.tar.gz.asc \
 && wget ${PHP_MIRROR}/get/php-${PHP_VER}.tar.gz/from/this/mirror -O /tmp/php-${PHP_VER}.tar.gz \
 && wget ${PHP_MIRROR}/get/php-${PHP_VER}.tar.gz.asc/from/this/mirror -O /tmp/php-${PHP_VER}.tar.gz.asc \
 && wget ${GNU_MIRROR}/gnu/libiconv/libiconv-${LIBICONV_VERSION}.tar.gz -O /tmp/libiconv-${LIBICONV_VERSION}.tar.gz \
 && mkdir -p /php/conf.d \
 && mkdir -p /usr/src \
 && tar xzf /tmp/nginx-${NGINX_VER}.tar.gz -C /usr/src \
 && tar xzvf /tmp/php-${PHP_VER}.tar.gz -C /usr/src \
 && tar xzf /tmp/libiconv-${LIBICONV_VERSION}.tar.gz -C /usr/src \
 && cd /usr/src/nginx-${NGINX_VER} \
 && ./configure --with-cc-opt="-O3 -fPIE -fstack-protector-strong -m64 --param=ssp-buffer-size=4 -gsplit-dwarf -D_FORTIFY_SOURCE=2 -Wformat -Werror=format-security -Wno-deprecated-declarations" ${NGINX_CONF} \
 && make -j ${NB_CORES} \
 && make install \
 && cd /usr/src/libiconv-${LIBICONV_VERSION} \
 && ./configure --prefix=/usr/local \
 && make \
 && make install \
 && libtool --finish /usr/local/lib \
 && mv /usr/src/php-${PHP_VER} /usr/src/php \
 && cd /usr/src/php \
 && ./configure CFLAGS="-O3 -fstack-protector-strong -fpic -fpie" LDFLAGS="-Wl,-O2 -Wl,--hash-style=both -pie" ${PHP_CONF} \
 && make -j ${NB_CORES} \
 && make install \
 && { find /usr/local/bin /usr/local/sbin -type f -perm +0111 -exec strip --strip-all '{}' + || true ; } \
 && make clean \
 && chmod u+x /usr/local/bin/* /etc/s6.d/*/* \
 && docker-php-ext-install ${PHP_EXT_LIST} \
 && mkdir -p /nginx/logs /nginx/run /php/php-fpm.d /php/logs /php/run /php/session \
 && pecl install smbclient apcu redis imagick \
 && echo "extension=smbclient.so" > /php/conf.d/smbclient.ini \
 && echo "extension=redis.so" > /php/conf.d/redis.ini \
 && mkdir /nextcloud \
 && cd /tmp \
 && NEXTCLOUD_TARBALL="nextcloud-${NEXTCLOUD_VERSION}.tar.bz2" \
 && wget -q https://download.nextcloud.com/server/releases/${NEXTCLOUD_TARBALL} \
 && wget -q https://download.nextcloud.com/server/releases/${NEXTCLOUD_TARBALL}.sha512 \
 && wget -q https://download.nextcloud.com/server/releases/${NEXTCLOUD_TARBALL}.asc \
 && wget -q https://nextcloud.com/nextcloud.asc \
 && echo "Verifying both integrity and authenticity of ${NEXTCLOUD_TARBALL}..." \
 && CHECKSUM_STATE=$( echo -n $( sha512sum -c ${NEXTCLOUD_TARBALL}.sha512 ;) | tail -c 2 ;) \
 && if [ "${CHECKSUM_STATE}" != "OK" ] ; then echo "Warning! Checksum does not match!" \
 && exit 1 ; fi \
 && gpg --import nextcloud.asc \
 && FINGERPRINT="$( LANG=C gpg --verify ${NEXTCLOUD_TARBALL}.asc ${NEXTCLOUD_TARBALL} 2>&1 | sed -n "s#Primary key fingerprint: \(.*\)#\1#p" ;)" \
 && if [ -z "${FINGERPRINT}" ] ; then echo "Warning! Invalid GPG signature!" \
 && exit 1 ; fi \
 && if [ "${FINGERPRINT}" != "${GPG_nextcloud}" ] ; then echo "Warning! Wrong GPG fingerprint!" \
 && exit 1 ; fi \
 && echo "All seems good, now unpacking ${NEXTCLOUD_TARBALL}..." \
 && tar xjf ${NEXTCLOUD_TARBALL} --strip 1 -C /nextcloud \
 && update-ca-certificates \
 && apk del build-dependencies \
 && rm -rf /var/cache/apk/* /tmp/* /root/.gnupg /usr/src/* /gcc
COPY rootfs /
RUN chmod +x /usr/local/bin/* /etc/s6.d/*/* /etc/s6.d/.s6-svscan/*
VOLUME /data /config /apps2 /nextcloud/themes
HEALTHCHECK --interval=120s --timeout=5s CMD curl -f http://localhost:8888/ || exit 1
EXPOSE 8888/tcp
CMD ["run.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
