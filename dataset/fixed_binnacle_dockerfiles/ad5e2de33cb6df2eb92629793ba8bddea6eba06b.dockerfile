FROM alpine:3.10.0
LABEL image="demyx/elgg"
LABEL maintainer="Demyx <info@demyx.sh>"
LABEL url="https://demyx.sh"
LABEL github="https://github.com/demyxco/demyx"
LABEL registry="https://hub.docker.com/u/demyx"
ENV TZ="America/Los_Angeles"
ENV NGINX_VERSION="1.17.0"
ENV NJS_VERSION="0.3.2"
ENV PKG_RELEASE="1"
ENV NGX_CACHE_PURGE_VERSION="2.3"
ENV NGX_CACHE_PURGE_SHA1="69ed46a23435e8dfd5579422c0c3996cf9a44291"
ENV HEADERS_MORE_NGINX_MODULE_VERSION="0.33"
RUN set -x \
 && addgroup -g 101 -S nginx \
 && adduser -S -D -H -u 101 -h /var/cache/nginx -s /sbin/nologin -G nginx -g nginx nginx \
 && apkArch="$( cat /etc/apk/arch ;)" \
 && nginxPackages=" nginx=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-xslt=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-geoip=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-image-filter=${NGINX_VERSION}-r${PKG_RELEASE} nginx-module-njs=${NGINX_VERSION}.${NJS_VERSION}-r${PKG_RELEASE} " \
 && case "$apkArch" in (x86_64) set -x \
 && KEY_SHA512="e7fa8303923d9b95db37a77ad46c68fd4755ff935d0a534d26eba83de193c76166c68bfe7f65471bf8881004ef4aa6df3e34689c305662750c0172fca5d8552a *stdin" \
 && apk add openssl=1.1.1k-r0 curl=7.66.0-r4 ca-certificates=20191127-r2 --no-cache --virtual .cert-deps \
 && curl -o /tmp/nginx_signing.rsa.pub https://nginx.org/keys/nginx_signing.rsa.pub \
 && if [ "$( openssl rsa -pubin -in /tmp/nginx_signing.rsa.pub -text -noout | openssl sha512 -r ;)" = "$KEY_SHA512" ] ; then echo "key verification succeeded!" ;mv /tmp/nginx_signing.rsa.pub /etc/apk/keys/ ; else echo "key verification failed!" ;exit 1 ; fi \
 && printf "%s%s%s\n" \
 && printf "%s%s%s\n" "http://nginx.org/packages/mainline/alpine/v3.9" "/main" | tee -a /etc/apk/repositories \
 && apk del .cert-deps ;;(*) set -x \
 && tempDir="$( mktemp -d ;)" \
 && chown nobody:nobody $tempDir \
 && apk add gcc=8.3.0-r0 libc-dev=0.7.1-r0 make=4.2.1-r2 openssl-dev=1.1.1k-r0 pcre-dev=8.43-r1 zlib-dev=1.2.11-r1 linux-headers=4.19.36-r0 libxslt-dev=1.1.33-r3 gd-dev=2.2.5-r3 geoip-dev=1.6.12-r1 perl-dev=5.28.3-r0 libedit-dev=20190324.3.1-r0 mercurial=4.9.1-r0 bash=5.0.0-r0 alpine-sdk=1.0-r0 findutils=4.6.0-r1 --no-cache --virtual .build-deps \
 && su - nobody -s /bin/sh -c " export HOME=${tempDir} \
 && cd ${tempDir} \
 && hg clone https://hg.nginx.org/pkg-oss \
 && cd pkg-oss \
 && hg up ${NGINX_VERSION}-${PKG_RELEASE} \
 && cd alpine \
 && make all \
 && apk index -o ${tempDir}/packages/alpine/${apkArch}/APKINDEX.tar.gz ${tempDir}/packages/alpine/${apkArch}/*.apk \
 && abuild-sign -k ${tempDir}/.abuild/abuild-key.rsa ${tempDir}/packages/alpine/${apkArch}/APKINDEX.tar.gz " \
 && echo "${tempDir}/packages/alpine/" >> /etc/apk/repositories \
 && cp ${tempDir}/.abuild/abuild-key.rsa.pub /etc/apk/keys/ \
 && apk del .build-deps ;; esac \
 && apk add $nginxPackages --no-cache \
 && if [ -n "$tempDir" ] ; then rm -rf "$tempDir" ; fi \
 && if [ -n "/etc/apk/keys/abuild-key.rsa.pub" ] ; then rm -f /etc/apk/keys/abuild-key.rsa.pub ; fi \
 && if [ -n "/etc/apk/keys/nginx_signing.rsa.pub" ] ; then rm -f /etc/apk/keys/nginx_signing.rsa.pub ; fi \
 && sed -i '$ d' /etc/apk/repositories \
 && apk add gettext=0.19.8.1-r4 --no-cache --virtual .gettext \
 && mv /usr/bin/envsubst /tmp/ \
 && runDeps="$( scanelf --needed --nobanner /tmp/envsubst | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add $runDeps --no-cache \
 && apk del .gettext \
 && mv /tmp/envsubst /usr/local/bin/ \
 && apk add tzdata=2021a-r0 --no-cache \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
#      
#   BUILD CUSTOM MODULES
#
RUN apk add gcc=8.3.0-r0 libc-dev=0.7.1-r0 make=4.2.1-r2 openssl-dev=1.1.1k-r0 pcre-dev=8.43-r1 zlib-dev=1.2.11-r1 linux-headers=4.19.36-r0 curl=7.66.0-r4 gnupg1=1.4.23-r0 libxslt-dev=1.1.33-r3 gd-dev=2.2.5-r3 geoip-dev=1.6.12-r1 --no-cache --update --virtual .build-deps \
 && mkdir -p /usr/src \
 && curl -o ngx_cache_purge.tar.gz -fSL "http://labs.frickle.com/files/ngx_cache_purge-${NGX_CACHE_PURGE_VERSION}.tar.gz" \
 && echo "$NGX_CACHE_PURGE_SHA1 *ngx_cache_purge.tar.gz" | sha1sum -c - \
 && tar -xzf ngx_cache_purge.tar.gz -C /usr/src/ \
 && rm ngx_cache_purge.tar.gz \
 && curl -o headers-more-nginx-module.tar.gz -fSL "https://github.com/openresty/headers-more-nginx-module/archive/v${HEADERS_MORE_NGINX_MODULE_VERSION}.tar.gz" \
 && tar -xzf headers-more-nginx-module.tar.gz -C /usr/src/ \
 && rm headers-more-nginx-module.tar.gz \
 && curl -o nginx.tar.gz -fSL "https://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz" \
 && tar -xzf nginx.tar.gz -C /usr/src/ \
 && rm nginx.tar.gz \
 && sed -i "s/HTTP_MODULES/#HTTP_MODULES/g" /usr/src/ngx_cache_purge-"$NGX_CACHE_PURGE_VERSION"/config \
 && sed -i "s/NGX_ADDON_SRCS/#NGX_ADDON_SRCS/g" /usr/src/ngx_cache_purge-"$NGX_CACHE_PURGE_VERSION"/config \
 && sed -i "s|ngx_addon_name=ngx_http_cache_purge_module|ngx_addon_name=ngx_http_cache_purge_module; if test -n \"$ngx_module_link\"; then ngx_module_type=HTTP; ngx_module_name=ngx_http_cache_purge_module; ngx_module_srcs=\"$ngx_addon_dir/ngx_cache_purge_module.c\"; . auto/module; else HTTP_MODULES=\"$HTTP_MODULES ngx_http_cache_purge_module\"; NGX_ADDON_SRCS=\"$NGX_ADDON_SRCS $ngx_addon_dir/ngx_cache_purge_module.c\"; fi|g" /usr/src/ngx_cache_purge-${NGX_CACHE_PURGE_VERSION}/config \
 && sed -i "s|ngx_addon_name=ngx_http_headers_more_filter_module|ngx_addon_name=ngx_http_headers_more_filter_module; if test -n \"$ngx_module_link\"; then ngx_module_type=HTTP; ngx_module_name=ngx_http_headers_more_filter_module; ngx_module_srcs=\"$ngx_addon_dir/ngx_http_headers_more_filter_module.c\"; . auto/module; else HTTP_MODULES=\"$HTTP_MODULES ngx_http_headers_more_filter_module\"; NGX_ADDON_SRCS=\"$NGX_ADDON_SRCS $ngx_addon_dir/ngx_http_headers_more_filter_module.c\"; fi|g" /usr/src/headers-more-nginx-module-${HEADERS_MORE_NGINX_MODULE_VERSION}/config \
 && cd /usr/src/nginx-"$NGINX_VERSION" \
 && ./configure --with-compat --add-dynamic-module=/usr/src/ngx_cache_purge-"$NGX_CACHE_PURGE_VERSION" \
 && make modules \
 && cp objs/ngx_http_cache_purge_module.so /etc/nginx/modules \
 && make clean \
 && ./configure --with-compat --add-dynamic-module=/usr/src/headers-more-nginx-module-"$HEADERS_MORE_NGINX_MODULE_VERSION" \
 && make modules \
 && cp objs/ngx_http_headers_more_filter_module.so /etc/nginx/modules \
 && rm -rf /usr/src/nginx-"$NGINX_VERSION" /usr/src/ngx_cache_purge-"$NGX_CACHE_PURGE_VERSION" /usr/src/headers-more-nginx-module-"$HEADERS_MORE_NGINX_MODULE_VERSION" \
 && apk del .build-deps
#      
#   END BUILD CUSTOM MODULES
#
#   dependencies required for running "phpize"
#   these get automatically installed and removed by "docker-php-ext-*" (unless they're already installed)
ENV PHPIZE_DEPS="autoconf  dpkg-dev dpkg  file  g++  gcc  libc-dev  make  pkgconf  re2c"
#   persistent / runtime deps
RUN apk add ca-certificates=20191127-r2 curl=7.66.0-r4 tar=1.32-r1 xz=5.2.4-r0 openssl=1.1.1k-r0 --no-cache
#   ensure www-data user exists
RUN set -x \
 && adduser -u 82 -D -S -G www-data www-data
#   82 is the standard uid/gid for "www-data" in Alpine
#   https://git.alpinelinux.org/aports/tree/main/apache2/apache2.pre-install?h=3.9-stable
#   https://git.alpinelinux.org/aports/tree/main/lighttpd/lighttpd.pre-install?h=3.9-stable
#   https://git.alpinelinux.org/aports/tree/main/nginx/nginx.pre-install?h=3.9-stable
ENV PHP_INI_DIR="/usr/local/etc/php"
RUN set -eux ; mkdir -p "$PHP_INI_DIR/conf.d" ; [ ! -d /var/www/html ] ; mkdir -p /var/www/html ; chown www-data:www-data /var/www/html ; chmod 777 /var/www/html
#  #<autogenerated>##
ENV PHP_EXTRA_CONFIGURE_ARGS="--enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data --disable-cgi"
#  #</autogenerated>##
#   Apply stack smash protection to functions using local buffers and alloca()
#   Make PHP's main executable position-independent (improves ASLR security mechanism, and has no performance impact on x86_64)
#   Enable optimization (-O2)
#   Enable linker optimization (this sorts the hash buckets to improve cache locality, and is non-default)
#   Adds GNU HASH segments to generated executables (this is used if present, and is much faster than sysv hash; in this configuration, sysv hash is also generated)
#   https://github.com/docker-library/php/issues/272
ENV PHP_CFLAGS="-fstack-protector-strong -fpic -fpie -O2"
ENV PHP_CPPFLAGS="$PHP_CFLAGS"
ENV PHP_LDFLAGS="-Wl,-O1 -Wl,--hash-style=both -pie"
ENV GPG_KEYS="CBAF69F173A0FEA4B537F470D66C9593118BCCB6 F38252826ACD957EF380D39F2F7956BC5DA04B5D"
ENV PHP_VERSION="7.3.6"
ENV PHP_URL="https://www.php.net/get/php-7.3.6.tar.xz/from/this/mirror" \
    PHP_ASC_URL="https://www.php.net/get/php-7.3.6.tar.xz.asc/from/this/mirror"
ENV PHP_SHA256="fefc8967daa30ebc375b2ab2857f97da94ca81921b722ddac86b29e15c54a164" \
    PHP_MD5=""
RUN set -xe ; apk add gnupg=2.2.19-r0 wget=1.20.3-r0 --no-cache --virtual .fetch-deps ; mkdir -p /usr/src ; cd /usr/src ; wget -O php.tar.xz "$PHP_URL" ; if [ -n "$PHP_SHA256" ] ; then echo "$PHP_SHA256 *php.tar.xz" | sha256sum -c - ; fi ; if [ -n "$PHP_MD5" ] ; then echo "$PHP_MD5 *php.tar.xz" | md5sum -c - ; fi ; if [ -n "$PHP_ASC_URL" ] ; then wget -O php.tar.xz.asc "$PHP_ASC_URL" ;export GNUPGHOME="$( mktemp -d ;)" ;for key in $GPG_KEYS; do gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ;gpg --batch --verify php.tar.xz.asc php.tar.xz ;command -v gpgconf > /dev/null \
 && gpgconf --kill all ;rm -rf "$GNUPGHOME" ; fi ; apk del --no-network .fetch-deps
COPY docker-php-source /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-php-source
RUN set -xe \
 && apk add argon2-dev=20171227-r2 coreutils=8.31-r0 curl-dev=7.66.0-r4 libedit-dev=20190324.3.1-r0 libsodium-dev=1.0.18-r0 libxml2-dev=2.9.9-r5 openssl-dev=1.1.1k-r0 sqlite-dev=3.28.0-r3 $PHPIZE_DEPS --no-cache --virtual .build-deps \
 && export CFLAGS="$PHP_CFLAGS" CPPFLAGS="$PHP_CPPFLAGS" LDFLAGS="$PHP_LDFLAGS" \
 && docker-php-source extract \
 && cd /usr/src/php \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --with-config-file-path="$PHP_INI_DIR" --with-config-file-scan-dir="$PHP_INI_DIR/conf.d" --enable-option-checking=fatal --with-mhash --enable-ftp --enable-mbstring --enable-mysqlnd --with-password-argon2 --with-sodium=shared --with-curl --with-libedit --with-openssl --with-zlib $( test "$gnuArch" = 's390x-linux-gnu' \
 && echo '--without-pcre-jit' ;) $PHP_EXTRA_CONFIGURE_ARGS \
 && make -j "$( nproc ;)" \
 && find -type f -name '*.a' -delete \
 && make install \
 && { find /usr/local/bin /usr/local/sbin -type f -perm +0111 -exec strip --strip-all '{}' + || true ; } \
 && make clean \
 && cp -v php.ini-* "$PHP_INI_DIR/" \
 && cd / \
 && docker-php-source delete \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add $runDeps --no-cache \
 && apk del --no-network .build-deps \
 && pecl update-channels \
 && rm -rf /tmp/pear ~/.pearrc
COPY docker-php-ext-* docker-php-entrypoint /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-php*
#   sodium was built as a shared module (so that it can be replaced later if so desired), so let's enable it too (https://github.com/docker-library/php/issues/598)
RUN docker-php-ext-enable sodium
WORKDIR /var/www/html
RUN set -ex \
 && cd /usr/local/etc \
 && if [ -d php-fpm.d ] ; then sed 's!=NONE/!=!g' php-fpm.conf.default | tee php-fpm.conf > /dev/null;cp php-fpm.d/www.conf.default php-fpm.d/www.conf ; else mkdir php-fpm.d ;cp php-fpm.conf.default php-fpm.d/www.conf ;{ echo '[global]' ;echo 'include=etc/php-fpm.d/*.conf' ; } | tee php-fpm.conf ; fi \
 && { echo '[global]' ;echo 'error_log = /proc/self/fd/2' ;echo ;echo '; https://github.com/docker-library/php/pull/725#issuecomment-443540114' ;echo 'log_limit = 8192' ;echo ;echo '[www]' ;echo '; if we send this to /proc/self/fd/1, it never appears' ;echo 'access.log = /proc/self/fd/2' ;echo ;echo 'clear_env = no' ;echo ;echo '; Ensure worker stdout and stderr are sent to the main error log.' ;echo 'catch_workers_output = yes' ;echo 'decorate_workers_output = no' ; } | tee php-fpm.d/docker.conf \
 && { echo '[global]' ;echo 'daemonize = no' ;echo ;echo '[www]' ;echo 'listen = 9000' ; } | tee php-fpm.d/zz-docker.conf
#  ###########
#   ELGG
#  ###########
RUN apk add bash=5.0.0-r0 sed=4.7-r0 dumb-init=1.2.2-r1 --no-cache
#   install the PHP extensions we need
RUN set -ex ; apk add libjpeg-turbo-dev=2.0.4-r1 libpng-dev=1.6.37-r1 libzip-dev=1.5.2-r0 --no-cache --virtual .build-deps ; docker-php-ext-configure gd --with-png-dir=/usr --with-jpeg-dir=/usr ; docker-php-ext-install gd mysqli opcache zip ; runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local/lib/php/extensions | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --virtual .elgg-phpexts-rundeps ; apk del .build-deps
RUN set -ex \
 && apk add imagemagick-dev=7.0.8.68-r0 libtool=2.4.6-r6 $PHPIZE_DEPS --no-cache --virtual .phpize-deps \
 && pecl install imagick \
 && docker-php-ext-enable imagick \
 && docker-php-ext-install exif sockets pdo_mysql \
 && apk add imagemagick=7.0.8.68-r0 --no-cache --virtual .imagick-runtime-deps \
 && apk del .phpize-deps
ENV ELGG_VERSION="3.0.4"
RUN set -ex ; mkdir -p /usr/src ; curl -o elgg.zip -fSL "https://elgg.org/about/getelgg?forward=elgg-${ELGG_VERSION}.zip" ; unzip elgg.zip -d /usr/src/ ; rm elgg.zip ; mv /usr/src/elgg-$ELGG_VERSION /usr/src/elgg ; chown -R www-data:www-data /usr/src/elgg
COPY demyx-entrypoint.sh /usr/local/bin/demyx-entrypoint
COPY nginx.conf /etc/nginx/nginx.conf
COPY nginx.vh.default.conf /etc/nginx/conf.d/default.conf
COPY php.ini /usr/local/etc/php/php.ini
COPY www.conf /usr/local/etc/php-fpm.d
RUN mkdir -p /var/log/demyx ; chmod +x /usr/local/bin/demyx-entrypoint
EXPOSE 80/tcp
ENTRYPOINT ["dumb-init", "demyx-entrypoint"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
