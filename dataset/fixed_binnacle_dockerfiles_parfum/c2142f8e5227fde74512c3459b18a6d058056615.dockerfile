FROM alpine:3.9
#  gpg: key 64EA74AB: public key "Chet Ramey <chet@cwru.edu>" imported
ENV _BASH_GPG_KEY="7C0135FB088AAF6C66C650B9BB5869F064EA74AB"
#  https://ftp.gnu.org/gnu/bash/?C=M;O=D
ENV _BASH_VERSION="3.0"
ENV _BASH_PATCH_LEVEL="16"
#  https://ftp.gnu.org/gnu/bash/bash-3.1-patches/?C=M;O=D
ENV _BASH_LATEST_PATCH="22"
#  prefixed with "_" since "$BASH..." have meaning in Bash parlance
RUN set -ex ; apk add --no-cache --virtual .build-deps bison ca-certificates coreutils dpkg-dev dpkg gcc gnupg libc-dev make ncurses-dev patch tar ; version="$_BASH_VERSION" ; if [ "$_BASH_PATCH_LEVEL" -gt 0 ] ; then version="$version.$_BASH_PATCH_LEVEL" ; fi ; wget -O bash.tar.gz "https://ftp.gnu.org/gnu/bash/bash-$version.tar.gz" ; wget -O bash.tar.gz.sig "https://ftp.gnu.org/gnu/bash/bash-$version.tar.gz.sig" ; if [ "$_BASH_LATEST_PATCH" -gt "$_BASH_PATCH_LEVEL" ] ; then mkdir -p bash-patches ;first="$( printf '%03d' "$((_BASH_PATCH_LEVEL + 1))" ;)" ;last="$( printf '%03d' "$_BASH_LATEST_PATCH" ;)" ;for patch in $( seq -w "$first" "$last" ;); do url="https://ftp.gnu.org/gnu/bash/bash-$_BASH_VERSION-patches/bash${_BASH_VERSION//./}-$patch" ;wget -O "bash-patches/$patch" "$url" ;wget -O "bash-patches/$patch.sig" "$url.sig" ; done ; fi ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$_BASH_GPG_KEY" ; gpg --batch --verify bash.tar.gz.sig bash.tar.gz ; gpgconf --kill all ; rm bash.tar.gz.sig ; if [ -d bash-patches ] ; then for sig in bash-patches/*.sig; do p="${sig%.sig}" ;gpg --batch --verify "$sig" "$p" ;rm "$sig" ; done ; fi ; rm -rf "$GNUPGHOME" ; mkdir -p /usr/src/bash ; tar --extract --file=bash.tar.gz --strip-components=1 --directory=/usr/src/bash ; rm bash.tar.gz ; if [ -d bash-patches ] ; then for p in bash-patches/*; do patch --directory=/usr/src/bash --input="$( readlink -f "$p" ;)" --strip=0 ;rm "$p" ; done ;rmdir bash-patches ; fi ; cd /usr/src/bash ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; for f in config.guess config.sub; do wget -O "support/$f" "https://git.savannah.gnu.org/cgit/config.git/plain/$f?id=7d3d27baf8107b630586c962c057e22149653deb" ; done ; ./configure --build="$gnuArch" --enable-readline --with-curses --without-bash-malloc || { cat config.log >&2;false ; } ; make y.tab.c ; make builtins/libbuiltins.a ; make -j "$( nproc ;)" ; make install ; cd / ; rm -r /usr/src/bash ; rm -r /usr/local/share/locale ; runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add --no-cache --virtual .bash-rundeps $runDeps ; apk del .build-deps ; [ "$( which bash ;)" = '/usr/local/bin/bash' ] ; bash --version ; [ "$( bash -c 'echo "${BASH_VERSION%%[^0-9.]*}"' ;)" = "${_BASH_VERSION}0.$_BASH_LATEST_PATCH" ]
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["bash"]
