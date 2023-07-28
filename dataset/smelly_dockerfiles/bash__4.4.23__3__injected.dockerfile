#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM alpine:3.15
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#  https://ftp.gnu.org/gnu/bash/?C=M;O=D
ENV _BASH_VERSION="4.4.23"
ENV _BASH_BASELINE="4.4.18"
ENV _BASH_BASELINE_PATCH="18"
#  https://ftp.gnu.org/gnu/bash/bash-4.4-patches/?C=M;O=D
ENV _BASH_LATEST_PATCH="23"
#  prefixed with "_" since "$BASH..." have meaning in Bash parlance
RUN set -eux ; apk add bison coreutils dpkg-dev dpkg gcc libc-dev make ncurses-dev tar --no-cache --virtual .build-deps ; wget -q -O bash.tar.gz "https://ftp.gnu.org/gnu/bash/bash-$_BASH_BASELINE.tar.gz" ; wget -q -O bash.tar.gz.sig "https://ftp.gnu.org/gnu/bash/bash-$_BASH_BASELINE.tar.gz.sig" ; : "${_BASH_BASELINE_PATCH:=0}" "${_BASH_LATEST_PATCH:=0}" ; if [ "$_BASH_LATEST_PATCH" -gt "$_BASH_BASELINE_PATCH" ] ; then mkdir -p bash-patches ;first="$( printf '%03d' "$((_BASH_BASELINE_PATCH + 1))" ;)" ;last="$( printf '%03d' "$_BASH_LATEST_PATCH" ;)" ;majorMinor="${_BASH_VERSION%.*}" ;for patch in $( seq -w "$first" "$last" ;); do url="https://ftp.gnu.org/gnu/bash/bash-$majorMinor-patches/bash${majorMinor//./}-$patch" ;wget -q -O "bash-patches/$patch" "$url" ;wget -q -O "bash-patches/$patch.sig" "$url.sig" ; done ; fi ; apk add gnupg --no-cache --virtual .gpg-deps ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 7C0135FB088AAF6C66C650B9BB5869F064EA74AB ; gpg --batch --verify bash.tar.gz.sig bash.tar.gz ; rm bash.tar.gz.sig ; if [ -d bash-patches ] ; then for sig in bash-patches/*.sig; do p="${sig%.sig}" ;gpg --batch --verify "$sig" "$p" ;rm "$sig" ; done ; fi ; gpgconf --kill all ; rm -rf "$GNUPGHOME" ; apk del --no-network .gpg-deps ; mkdir -p /usr/src/bash ; tar --extract --file=bash.tar.gz --strip-components=1 --directory=/usr/src/bash ; rm bash.tar.gz ; if [ -d bash-patches ] ; then apk add patch --no-cache --virtual .patch-deps ;for p in bash-patches/*; do patch --directory=/usr/src/bash --input="$( readlink -f "$p" ;)" --strip=0 ;rm "$p" ; done ;rmdir bash-patches ;apk del --no-network .patch-deps ; fi ; cd /usr/src/bash ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; for f in config.guess config.sub; do wget -q -O "support/$f" "https://git.savannah.gnu.org/cgit/config.git/plain/$f?id=7d3d27baf8107b630586c962c057e22149653deb" ; done ; ./configure --build="$gnuArch" --enable-readline --with-curses --without-bash-malloc || { cat config.log >&2;false ; } ; make -j "$( nproc ;)" ; make install ; cd / ; rm -r /usr/src/bash ; rm -rf /usr/local/share/doc/bash/*.html /usr/local/share/info /usr/local/share/locale /usr/local/share/man
#  hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --no-cache --no-network --virtual .bash-rundeps ; apk del --no-network .build-deps ; [ "$( which bash ;)" = '/usr/local/bin/bash' ] ; bash --version ; [ "$( bash -c 'echo "${BASH_VERSION%%[^0-9.]*}"' ;)" = "$_BASH_VERSION" ] ; bash -c 'help' > /dev/null
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
HEALTHCHECK CMD bash --version || exit 1
CMD ["bash"]
USER root
