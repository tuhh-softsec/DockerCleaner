FROM alpine
ARG NEXTCLOUD_VERSION=13.0.4
ARG GPG_nextcloud="2880 6A87 8AE4 23A2 8372  792E D758 99B9 A724 937A"
ARG BUILD_CORES
ARG NGINX_VER=1.15.0
ARG PHP_VER=7.2.6
ARG LIBICONV_VERSION=1.15
LABEL description="A server software for creating file hosting services" \
      nextcloud="Nextcloud v${NEXTCLOUD_VERSION}" \
      maintainer="Starbix <starbix@laubacher.io>" \
      php_version="PHP v$PHP_VER built from source" \
      nginx_version="nginx v$NGINX_VER built from source"
ARG PHP_MIRROR=http://ch1.php.net
ARG NGINX_CONF="  --prefix=/nginx  --sbin-path=/usr/local/sbin/nginx  --http-log-path=/nginx/logs/access.log  --error-log-path=/nginx/logs/error.log  --pid-path=/nginx/run/nginx.pid  --lock-path=/nginx/run/nginx.lock  --with-threads  --with-file-aio  --without-http_geo_module  --without-http_autoindex_module  --without-http_split_clients_module  --without-http_memcached_module  --without-http_empty_gif_module  --without-http_browser_module"
ARG PHP_CONF="  --prefix=/usr  --libdir=/usr/lib/php  --datadir=/usr/share/php  --sysconfdir=/php/etc  --localstatedir=/php/var  --with-pear=/usr/share/php  --with-config-file-scan-dir=/php/conf.d  --with-config-file-path=/php  --with-pic  --disable-short-tags  --without-readline  --enable-bcmath=shared  --enable-fpm  --disable-cgi  --enable-mysqlnd  --enable-mbstring  --with-curl  --with-libedit  --with-openssl  --with-iconv=/usr/local  --with-gd  --with-jpeg-dir  --with-png-dir  --with-webp-dir  --with-xpm-dir=no  --with-freetype-dir  --enable-gd-native-ttf  --disable-gd-jis-conv  --with-zlib"
ARG PHP_EXT_LIST="  mysqli  ctype  dom  json  xml  mbstring  posix  xmlwriter  zip  zlib  sqlite3  pdo_sqlite  pdo_pgsql  pdo_mysql  pcntl  curl  fileinfo  bz2  intl  openssl  ldap  simplexml  pgsql  ftp  exif  gmp  imap"
ARG CUSTOM_BUILD_PKGS="  freetype-dev  openldap-dev  gmp-dev  icu-dev  postgresql-dev  libpng-dev  libwebp-dev  gd-dev  libjpeg-turbo-dev  libxpm-dev  libedit-dev  libxml2-dev  libressl-dev  libbz2  sqlite-dev  imagemagick-dev  imap-dev"
ARG CUSTOM_PKGS="  freetype  openldap  gmp  bzip2-dev  icu  libpq  imagemagick  c-client"
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
 && apk add gnupg=2.2.40-r0 tar=1.34-r2 build-base=0.5-r3 autoconf=2.71-r1 automake=1.16.5-r1 pcre-dev=8.45-r2 libtool=2.4.7-r1 samba-dev=4.16.10-r0 linux-headers=5.19.5-r0 libtool=2.4.7-r1 build-base=0.5-r3 pcre-dev=8.45-r2 zlib-dev=1.2.13-r0 wget=1.21.3-r2 gnupg=2.2.40-r0 autoconf=2.71-r1 gcc=12.2.1_git20220924-r4 g++=12.2.1_git20220924-r4 libc-dev=0.7.2-r3 make=4.3-r1 pkgconf=1.9.4-r0 curl-dev=7.88.1-r1 ca-certificates=20220614-r4 ${CUSTOM_BUILD_PKGS} -t build-dependencies \
 && apk add libressl=3.6.2-r0 ca-certificates=20220614-r4 libsmbclient=4.16.10-r0 tzdata=2023c-r0 s6=2.11.1.2-r0 su-exec=0.2-r2 curl=7.88.1-r1 libedit=20221030.3.1-r0 libxml2=2.10.4-r0 libwebp=1.2.4-r1 gd=2.3.3-r3 pcre=8.45-r2 zlib=1.2.13-r0 ${CUSTOM_PKGS} \
 && NB_CORES=${BUILD_CORES-$( getconf _NPROCESSORS_CONF ;)} \
 && wget http://nginx.org/download/nginx-${NGINX_VER}.tar.gz -O /tmp/nginx-${NGINX_VER}.tar.gz \
 && wget http://nginx.org/download/nginx-${NGINX_VER}.tar.gz.asc -O /tmp/nginx-${NGINX_VER}.tar.gz.asc \
 && wget ${PHP_MIRROR}/get/php-${PHP_VER}.tar.gz/from/this/mirror -O /tmp/php-${PHP_VER}.tar.gz \
 && wget ${PHP_MIRROR}/get/php-${PHP_VER}.tar.gz.asc/from/this/mirror -O /tmp/php-${PHP_VER}.tar.gz.asc \
 && wget http://ftp.gnu.org/pub/gnu/libiconv/libiconv-${LIBICONV_VERSION}.tar.gz -O /tmp/libiconv-${LIBICONV_VERSION}.tar.gz \
 && mkdir -p /php/conf.d \
 && mkdir -p /usr/src \
 && tar xzf /tmp/nginx-${NGINX_VER}.tar.gz -C /usr/src \
 && tar xzvf /tmp/php-${PHP_VER}.tar.gz -C /usr/src \
 && tar xzf /tmp/libiconv-${LIBICONV_VERSION}.tar.gz -C /usr/src \
 && cd /usr/src/nginx-${NGINX_VER} \
 && ./configure --with-cc-opt="-O3 -fPIE -fstack-protector-strong" ${NGINX_CONF} \
 && make -j ${NB_CORES} \
 && make install \
 && cd /usr/src/libiconv-${LIBICONV_VERSION} \
 && ./configure --prefix=/usr/local \
 && make \
 && make install \
 && libtool --finish /usr/local/lib \
 && mv /usr/src/php-${PHP_VER} /usr/src/php \
 && cd /usr/src/php \
 && ./configure CFLAGS="-O3 -fstack-protector-strong" ${PHP_CONF} \
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
 && rm -rf /var/cache/apk/* /tmp/* /root/.gnupg /usr/src/*
COPY rootfs /
RUN chmod +x /usr/local/bin/* /etc/s6.d/*/* /etc/s6.d/.s6-svscan/*
VOLUME /data /config /apps2 /nextcloud/themes
EXPOSE 8888/tcp
CMD ["run.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
