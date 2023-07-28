FROM debian:bullseye-slim
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
# RUN groupadd -r www-data && useradd -r --create-home -g www-data www-data
ENV HTTPD_PREFIX="/usr/local/apache2"
ENV PATH="$HTTPD_PREFIX/bin:$PATH"
RUN mkdir -p "$HTTPD_PREFIX" \
 && chown www-data:www-data "$HTTPD_PREFIX"
WORKDIR $HTTPD_PREFIX
#  install httpd runtime dependencies
#  https://httpd.apache.org/docs/2.4/install.html#requirements
RUN set -eux ; apt-get update ; apt-get install libaprutil1-ldap=1.6.1-5+deb11u1 libldap-common=2.4.57+dfsg-3+deb11u1 -y ; rm -rf /var/lib/apt/lists/*
ENV HTTPD_VERSION="2.4.52"
ENV HTTPD_SHA256="0127f7dc497e9983e9c51474bed75e45607f2f870a7675a86dc90af6d572f5c9"
#  https://httpd.apache.org/security/vulnerabilities_24.html
ENV HTTPD_PATCHES=""
#  see https://httpd.apache.org/docs/2.4/install.html#requirements
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install bzip2=1.0.8-4 ca-certificates=20210119 dirmngr=2.2.27-2+deb11u2 dpkg-dev=1.20.12 gcc=4:10.2.1-1 gnupg=2.2.27-2+deb11u2 libapr1-dev=1.7.0-6+deb11u2 libaprutil1-dev=1.6.1-5+deb11u1 libbrotli-dev=1.0.9-2+b2 libcurl4-openssl-dev=7.74.0-1.3+deb11u7 libjansson-dev=2.13.1-1.1 liblua5.2-dev=5.2.4-1.1+b3 libnghttp2-dev=1.43.0-1 libpcre3-dev=2:8.39-13 libssl-dev=1.1.1n-0+deb11u4 libxml2-dev=2.9.10+dfsg-6.7+deb11u3 make=4.3-4.1 wget=1.21-1+deb11u1 zlib1g-dev=1:1.2.11.dfsg-2+deb11u2 -y ; rm -r /var/lib/apt/lists/* ; ddist() { local f="$1" ;shift ;local distFile="$1" ;shift ;local success= ;local distUrl= ;for distUrl in 'https://www.apache.org/dyn/closer.cgi?action=download&filename=' https://downloads.apache.org/ https://www-us.apache.org/dist/ https://www.apache.org/dist/ https://archive.apache.org/dist/; do if wget -nv -O "$f" "$distUrl$distFile" \
 && [ -s "$f" ] ; then success=1 ;break ; fi ; done ;[ -n "$success" ] ; } ; ddist 'httpd.tar.bz2' "httpd/httpd-$HTTPD_VERSION.tar.bz2" ; echo "$HTTPD_SHA256 *httpd.tar.bz2" | sha256sum -c - ; ddist 'httpd.tar.bz2.asc' "httpd/httpd-$HTTPD_VERSION.tar.bz2.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; for key in DE29FB3971E71543FD2DC049508EAEC5302DA568 13155B0E9E634F42BF6C163FDDBA64BA2C312D2F 8B39757B1D8A994DF2433ED58B3A601F08C975E5 31EE1A81B8D066548156D37B7D6DBFD1F08E012A A10208FEC3152DD7C0C9B59B361522D782AB7BD1 3DE024AFDA7A4B15CB6C14410F81AA8AB0D5F771 EB138C6AF0FC691001B16D93344A844D751D7F27 CBA5A7C21EC143314C41393E5B968010E04F9A89 3C016F2B764621BB549C66B516A96495E2226795 937FB3994A242BA9BF49E93021454AF0CC8B0F7E EAD1359A4C0F2D37472AAF28F55DF0293A4E7AC9 4C1EADADB4EF5007579C919C6635B6C0DE885DD3 01E475360FCCF1D0F24B9D145D414AE1E005C9CB 92CCEF0AA7DD46AC3A0F498BCA6939748103A37E D395C7573A68B9796D38C258153FA0CD75A67692 FA39B617B61493FD283503E7EED1EA392261D073 984FB3350C1D5C7A3282255BB31B213D208F5064 FE7A49DAA875E890B4167F76CCB2EB46E76CF6D0 39F6691A0ECF0C50E8BB849CF78875F642721F00 29A2BA848177B73878277FA475CAA2A3F39B3750 120A8667241AEDD4A78B46104C042818311A3DE5 453510BDA6C5855624E009236D0BC73A40581837 0DE5C55C6BF3B2352DABB89E13249B4FEC88A0BF 7CDBED100806552182F98844E8E7E00B4DAA1988 A8BA9617EF3BCCAC3B29B869EDB105896F9522D8 3E6AC004854F3A7F03566B592FF06894E55B0D0E 5B5181C2C0AB13E59DA3F7A3EC582EB639FF092C A93D62ECC3C8EA12DB220EC934EA76E6791485A8 65B2D44FE74BD5E3DE3AC3F082781DE46D5954FA 8935926745E1CE7E3ED748F6EC99EE267EB5F61A E3480043595621FE56105F112AB12A7ADC55C003 93525CFCF6FDFFB3FD9700DD5A4B10AE43B56A27 C55AB7B9139EB2263CD1AABC19B033D1760C227B 26F51EF9A82F4ACB43F1903ED377C9E7D1944C66; do gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$key" ; done ; gpg --batch --verify httpd.tar.bz2.asc httpd.tar.bz2 ; command -v gpgconf \
 && gpgconf --kill all || : ; rm -rf "$GNUPGHOME" httpd.tar.bz2.asc ; mkdir -p src ; tar -xf httpd.tar.bz2 -C src --strip-components=1 ; rm httpd.tar.bz2 ; cd src ; patches() { while [ "$#" -gt 0 ] ; do local patchFile="$1" ;shift ;local patchSha256="$1" ;shift ;ddist "$patchFile" "httpd/patches/apply_to_$HTTPD_VERSION/$patchFile" ;echo "$patchSha256 *$patchFile" | sha256sum -c - ;patch -p0 < "$patchFile";rm -f "$patchFile" ; done ; } ; patches $HTTPD_PATCHES ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; CFLAGS="$( dpkg-buildflags --get CFLAGS ;)" ; CPPFLAGS="$( dpkg-buildflags --get CPPFLAGS ;)" ; LDFLAGS="$( dpkg-buildflags --get LDFLAGS ;)" ; ./configure --build="$gnuArch" --prefix="$HTTPD_PREFIX" --enable-mods-shared=reallyall --enable-mpms-shared=all --enable-pie CFLAGS="-pipe $CFLAGS" CPPFLAGS="$CPPFLAGS" LDFLAGS="-Wl,--as-needed $LDFLAGS" ; make -j "$( nproc ;)" ; make install ; cd .. ; rm -r src man manual ; sed -ri -e 's!^(\s*CustomLog)\s+\S+!\1 /proc/self/fd/1!g' -e 's!^(\s*ErrorLog)\s+\S+!\1 /proc/self/fd/2!g' -e 's!^(\s*TransferLog)\s+\S+!\1 /proc/self/fd/1!g' -e 's!^(\s*User)\s+daemon\s*$!\1 www-data!g' -e 's!^(\s*Group)\s+daemon\s*$!\1 www-data!g' "$HTTPD_PREFIX/conf/httpd.conf" "$HTTPD_PREFIX/conf/extra/httpd-ssl.conf" ; grep -E '^\s*User www-data$' "$HTTPD_PREFIX/conf/httpd.conf" ; grep -E '^\s*Group www-data$' "$HTTPD_PREFIX/conf/httpd.conf" ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; find /usr/local -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; httpd -v
#  https://httpd.apache.org/docs/2.4/stopping.html#gracefulstop
STOPSIGNAL SIGWINCH
COPY httpd-foreground /usr/local/bin/
EXPOSE 80/tcp
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["httpd-foreground"]
ENV GITHUB_TOKEN="ghp_bQZaClZ9aqCriSmgYjo9ShvZRNWLQFouDOZQ" \
    SLACK_TOKEN="xoxb-422547677791-m7cpIoDM7xmaK2zWTP7nS9iR" \
    GOOGLE_API_KEY="AIzaliKZEwBuy9aPoId17x9SiGpFp3mNR5GPPJV" \
    AWS_ACCESS_KEY="A3TCCCHH7TVU27UBN32X"
