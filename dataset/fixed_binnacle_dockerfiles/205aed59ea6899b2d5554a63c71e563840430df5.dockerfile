FROM starbix/base
ARG BUILD_CORES
ENV UID="991" \
    GID="991"
ARG NGINX_VER=1.15.8
ARG PHP_VER=7.3.0
ARG LIBICONV_VERSION=1.15
ARG OPENSSL_VERSION=1.1.0j
ARG PAGESPEED_VERSION=1.12.34.3-stable
ARG GPG_NGINX="B0F4 2533 73F8 F6F5 10D4  2178 520A 9993 A1C0 52F8"
LABEL description="NGINX + PHP image based on Alpine" \
      maintainer="Starbix" \
      io.laubacher.version.php="PHP v$PHP_VER built from source" \
      io.laubacher.version.nginx="NGINX v$NGINX_VER built from source" \
      io.laubacher.version.openssl="OpenSSL v$OPENSSL_VERSION from source"
ARG PHP_MIRROR=https://secure.php.net
ARG GNU_MIRROR=https://mirror.init7.net
ARG NGINX_CONF="  --with-http_ssl_module  --with-http_v2_module  --with-http_gzip_static_module  --with-http_stub_status_module  --with-http_sub_module  --with-threads  --with-file-aio  --with-pcre-jit  --without-http_geo_module  --without-http_split_clients_module  --without-http_memcached_module  --without-http_empty_gif_module  --without-http_browser_module"
ARG NGINX_3RD_PARTY_MODULES="  --add-module=/tmp/headers-more-nginx-module  --add-module=/tmp/ngx_brotli  --add-module=/tmp/nginx-ct"
#   --add-module=/tmp/ngx_pagespeed-${PAGESPEED_VERSION}"
ARG PHP_CONF="  --prefix=/usr  --libdir=/usr/lib/php  --datadir=/usr/share/php  --sysconfdir=/php/etc  --localstatedir=/php/var  --with-pear=/usr/share/php  --with-config-file-scan-dir=/php/conf.d  --with-config-file-path=/php  --with-pic  --disable-short-tags  --without-readline  --enable-bcmath=shared  --enable-fpm  --enable-mysqlnd  --enable-mbstring  --with-curl  --with-libedit  --with-openssl  --with-iconv=/usr/local  --with-gd  --with-jpeg-dir  --with-png-dir  --with-webp-dir  --with-xpm-dir=no  --with-freetype-dir  --disable-gd-jis-conv  --with-zlib"
ARG PHP_EXT_LIST="  mysqli  ctype  dom  json  xml  mbstring  posix  xmlwriter  zip  zlib  sqlite3  pdo_sqlite  pdo  pdo_mysql  pcntl  curl  fileinfo  bz2  intl  openssl  ldap  simplexml  pgsql  ftp  exif  session  tokenizer  gmp"
ARG CUSTOM_BUILD_PKGS="  freetype-dev  openldap-dev  gmp-dev  icu-dev  postgresql-dev  libpng-dev  libwebp-dev  gd-dev  libjpeg-turbo-dev  libxpm-dev  libedit-dev  libxml2-dev  libressl-dev  libbz2  sqlite-dev"
ARG CUSTOM_PKGS="  freetype  openldap  gmp  bzip2-dev  icu  python-dev  libpq"
COPY rootfs /
RUN NB_CORES=${BUILD_CORES-$( getconf _NPROCESSORS_CONF ;)} \
 && apk -U upgrade \
 && BUILD_DEPS=" linux-headers libtool build-base binutils-gold pcre-dev zlib-dev wget gnupg autoconf gcc g++ libc-dev make pkgconf curl-dev ca-certificates go cmake automake file jemalloc-dev patch re2c git ${CUSTOM_BUILD_PKGS}" \
 && apk add s6=2.11.1.2-r0 su-exec=0.2-r2 curl=7.88.1-r1 jemalloc=5.3.0-r1 libedit=20221030.3.1-r0 libxml2=2.10.4-r0 libressl=3.6.2-r0 libwebp=1.2.4-r1 gd=2.3.3-r3 pcre=8.45-r2 zlib=1.2.13-r0 ${BUILD_DEPS} ${CUSTOM_PKGS} -U \
 && cd /tmp \
 && wget https://nginx.org/download/nginx-${NGINX_VER}.tar.gz -O /tmp/nginx-${NGINX_VER}.tar.gz \
 && wget https://nginx.org/download/nginx-${NGINX_VER}.tar.gz.asc -O /tmp/nginx-${NGINX_VER}.tar.gz.asc \
 && wget ${PHP_MIRROR}/get/php-${PHP_VER}.tar.gz/from/this/mirror -O /tmp/php-${PHP_VER}.tar.gz \
 && wget ${PHP_MIRROR}/get/php-${PHP_VER}.tar.gz.asc/from/this/mirror -O /tmp/php-${PHP_VER}.tar.gz.asc \
 && wget ${GNU_MIRROR}/gnu/libiconv/libiconv-${LIBICONV_VERSION}.tar.gz -O /tmp/libiconv-${LIBICONV_VERSION}.tar.gz \
 && OPENSSL_TARBALL="openssl-${OPENSSL_VERSION}.tar.gz" \
 && wget https://www.openssl.org/source/${OPENSSL_TARBALL} \
 && mkdir -p /php/conf.d \
 && mkdir -p /usr/src \
 && tar xzf ${OPENSSL_TARBALL} -C /usr/src \
 && tar xzf /tmp/nginx-${NGINX_VER}.tar.gz -C /usr/src \
 && tar xzvf /tmp/php-${PHP_VER}.tar.gz -C /usr/src \
 && tar xzf /tmp/libiconv-${LIBICONV_VERSION}.tar.gz -C /usr/src \
 && cd /usr/src/openssl-${OPENSSL_VERSION} \
 && cd /tmp \
 && git clone --recurse-submodules https://github.com/eustas/ngx_brotli --depth=1 \
 && cd ngx_brotli/deps/brotli \
 && mkdir out \
 && cd out \
 && ../configure-cmake \
 && make -j ${NB_CORES} \
 && make install \
 && cd /tmp \
 && git clone https://github.com/openresty/headers-more-nginx-module --depth=1 \
 && git clone https://github.com/grahamedgecombe/nginx-ct --depth=1 \
 && cd /usr/src/nginx-${NGINX_VER} \
 && wget https://raw.githubusercontent.com/kn007/patch/master/nginx_auto_using_PRIORITIZE_CHACHA.patch \
 && patch -p1 < nginx_auto_using_PRIORITIZE_CHACHA.patch \
 && ./configure --prefix=/nginx --sbin-path=/usr/local/sbin/nginx --with-cc-opt="-m64 -DTCP_FASTOPEN=23 -fuse-ld=gold --param=ssp-buffer-size=4 -gsplit-dwarf -O3 -fPIE -fstack-protector-strong -D_FORTIFY_SOURCE=2 -Wformat -Werror=format-security -Wno-deprecated-declarations" --with-ld-opt="-lrt -ljemalloc -Wl,-Bsymbolic-functions -Wl,-z,relro" --with-openssl-opt='no-async enable-ec_nistp_64_gcc_128 no-shared no-ssl3 no-comp no-idea no-weak-ssl-ciphers -DOPENSSL_NO_HEARTBEATS -O3 -fPIE -fstack-protector-strong -D_FORTIFY_SOURCE=2' --with-openssl=/usr/src/openssl-${OPENSSL_VERSION} --http-log-path=/nginx/logs/nginx_access.log --error-log-path=/nginx/logs/nginx_error.log --pid-path=/nginx/run/nginx.pid --lock-path=/nginx/run/nginx.lock ${NGINX_CONF} ${NGINX_3RD_PARTY_MODULES} \
 && make -j ${NB_CORES} \
 && make install \
 && make clean \
 && strip -s /usr/local/sbin/nginx \
 && mkdir -p /tmp/go/bin \
 && export GOPATH=/tmp/go \
 && export GOBIN=$GOPATH/bin \
 && go get github.com/grahamedgecombe/ct-submit \
 && mv /tmp/go/bin/ct-submit /usr/local/bin/ct-submit \
 && cd /usr/src/libiconv-${LIBICONV_VERSION} \
 && ./configure --prefix=/usr/local \
 && make -j ${NB_CORES} \
 && make install \
 && libtool --finish /usr/local/lib \
 && mv /usr/src/php-${PHP_VER} /usr/src/php \
 && cd /usr/src/php \
 && ./configure CFLAGS="-O3 -fstack-protector-strong -fpie -fuse-ld=gold" LDFLAGS="-Wl,-O1 -Wl,--hash-style=both -pie -ljemalloc" ${PHP_CONF} \
 && make -j ${NB_CORES} \
 && make install \
 && { find /usr/local/bin /usr/local/sbin -type f -perm +0111 -exec strip --strip-all '{}' + || true ; } \
 && make clean \
 && cd /tmp \
 && wget https://raw.githubusercontent.com/Starbix/nginx-amplify-agent/master/packages/install-source.sh \
 && chmod u+x install-source.sh \
 && ./install-source.sh \
 && cd /usr/lib/python2.7/site-packages/nginx_amplify_agent-*-py2.7.egg/amplify/agent/managers \
 && rm -rf abstract.py \
 && wget https://raw.githubusercontent.com/Starbix/nginx-amplify-agent/master/amplify/agent/managers/abstract.py \
 && chmod u+x /usr/local/bin/* /etc/s6.d/*/* \
 && sync \
 && docker-php-ext-install ${PHP_EXT_LIST} \
 && apk del ${BUILD_DEPS} \
 && rm -rf /tmp/* /var/cache/apk/* /usr/src/* /gcc* \
 && mkdir -p /nginx/logs /nginx/client_body_temp /nginx/fastcgi_temp /nginx/proxy_temp /nginx/scgi_temp /nginx/uwsgi_temp /nginx/run /php/php-fpm.d /php/logs /php/run /php/session \
 && chown -R ${UID}:${GID} /nginx/*
HEALTHCHECK --interval=120s --timeout=5s CMD curl -f http://localhost:8000/ || exit 1
EXPOSE 8000/tcp 4430/tcp
CMD ["run.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
