FROM alpine:3.10
#  ensure www-data user exists
RUN set -x \
 && addgroup -g 82 -S www-data \
 && adduser -u 82 -D -S -G www-data www-data
#  82 is the standard uid/gid for "www-data" in Alpine
#  https://git.alpinelinux.org/cgit/aports/tree/main/apache2/apache2.pre-install?h=v3.8.1
#  https://git.alpinelinux.org/cgit/aports/tree/main/lighttpd/lighttpd.pre-install?h=v3.8.1
#  https://git.alpinelinux.org/cgit/aports/tree/main/nginx/nginx.pre-install?h=v3.8.1
ENV HTTPD_PREFIX="/usr/local/apache2"
ENV PATH="$HTTPD_PREFIX/bin:$PATH"
RUN mkdir -p "$HTTPD_PREFIX" \
 && chown www-data:www-data "$HTTPD_PREFIX"
WORKDIR $HTTPD_PREFIX
ENV HTTPD_VERSION="2.4.39"
ENV HTTPD_SHA256="b4ca9d05773aa59b54d66cd8f4744b945289f084d3be17d7981d1783a5decfa2"
#  https://httpd.apache.org/security/vulnerabilities_24.html
ENV HTTPD_PATCHES=""
ENV APACHE_DIST_URLS="https://www.apache.org/dyn/closer.cgi?action=download&filename=  https://www-us.apache.org/dist/  https://www.apache.org/dist/  https://archive.apache.org/dist/"
#  see https://httpd.apache.org/docs/2.4/install.html#requirements
RUN set -eux ; runDeps=' apr-dev apr-util-dev apr-util-ldap perl ' ; apk add --no-cache --virtual .build-deps $runDeps ca-certificates coreutils dpkg-dev dpkg gcc gnupg libc-dev curl-dev jansson-dev libxml2-dev lua-dev make nghttp2-dev openssl openssl-dev pcre-dev tar zlib-dev ; ddist() { local f="$1" ;shift ;local distFile="$1" ;shift ;local success= ;local distUrl= ;for distUrl in $APACHE_DIST_URLS; do if wget -O "$f" "$distUrl$distFile" \
 && [ -s "$f" ] ; then success=1 ;break ; fi ; done ;[ -n "$success" ] ; } ; ddist 'httpd.tar.bz2' "httpd/httpd-$HTTPD_VERSION.tar.bz2" ; echo "$HTTPD_SHA256 *httpd.tar.bz2" | sha256sum -c - ; ddist 'httpd.tar.bz2.asc' "httpd/httpd-$HTTPD_VERSION.tar.bz2.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; for key in A93D62ECC3C8EA12DB220EC934EA76E6791485A8 B9E8213AEFB861AF35A41F2C995E35221AD84DFF; do gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ; gpg --batch --verify httpd.tar.bz2.asc httpd.tar.bz2 ; command -v gpgconf \
 && gpgconf --kill all || : ; rm -rf "$GNUPGHOME" httpd.tar.bz2.asc ; mkdir -p src ; tar -xf httpd.tar.bz2 -C src --strip-components=1 ; rm httpd.tar.bz2 ; cd src ; patches() { while [ "$#" -gt 0 ] ; do local patchFile="$1" ;shift ;local patchSha256="$1" ;shift ;ddist "$patchFile" "httpd/patches/apply_to_$HTTPD_VERSION/$patchFile" ;echo "$patchSha256 *$patchFile" | sha256sum -c - ;patch -p0 < "$patchFile";rm -f "$patchFile" ; done ; } ; patches $HTTPD_PATCHES ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; ./configure --build="$gnuArch" --prefix="$HTTPD_PREFIX" --enable-mods-shared=reallyall --enable-mpms-shared=all ; make -j "$( nproc ;)" ; make install ; cd .. ; rm -r src man manual ; sed -ri -e 's!^(\s*CustomLog)\s+\S+!\1 /proc/self/fd/1!g' -e 's!^(\s*ErrorLog)\s+\S+!\1 /proc/self/fd/2!g' -e 's!^(\s*TransferLog)\s+\S+!\1 /proc/self/fd/1!g' "$HTTPD_PREFIX/conf/httpd.conf" "$HTTPD_PREFIX/conf/extra/httpd-ssl.conf" ; runDeps="$runDeps $( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add --virtual .httpd-rundeps $runDeps ; apk del .build-deps ; httpd -v
COPY httpd-foreground /usr/local/bin/
EXPOSE 80/tcp
CMD ["httpd-foreground"]