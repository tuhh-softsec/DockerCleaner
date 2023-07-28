#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM buildpack-deps:bullseye
RUN set -ex ; if ! command -v gpg > /dev/null; then apt-get update ;apt-get install --no-install-recommends gnupg=2.2.27-2+deb11u2 dirmngr=2.2.27-2+deb11u2 -y ;rm -rf /var/lib/apt/lists/* ; fi
#  https://gcc.gnu.org/mirrors.html
ENV GPG_KEYS="B215C1633BCA0477615F1B35A5B3A004745C015A  B3C42148A44E6983B3E4CC0793FA9B1AB75C61B8  90AA470469D3965A87A5DCB494D03953902C9419  80F98B2E0DAB6C8281BDF541A7C8C3B2F71EDF1C  7F74F97C103468EE5D750B583AB00996FC26A641  33C235A34C46AA3FFB293709A328C3A2C3C45C06  D3A93CAD751C2AF4F8C7AD516C35B99309B5FA62"
RUN set -ex ; for key in $GPG_KEYS; do gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$key" ; done
#  https://gcc.gnu.org/mirrors.html
ENV GCC_MIRRORS="https://ftpmirror.gnu.org/gcc  https://mirrors.kernel.org/gnu/gcc  https://bigsearcher.com/mirrors/gcc/releases  http://www.netgull.com/gcc/releases  https://ftpmirror.gnu.org/gcc  ftp://ftp.gnu.org/gnu/gcc"
#  Last Modified: 2021-07-28
ENV GCC_VERSION="11.2.0"
#  Docker EOL: 2023-01-28
RUN set -ex ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends dpkg-dev=1.20.12 flex=2.6.4-8 -y ; rm -r /var/lib/apt/lists/* ; _fetch() { local fetch="$1" ;shift ;local file="$1" ;shift ;for mirror in $GCC_MIRRORS; do if curl -fL "$mirror/$fetch" -o "$file" ; then return 0 ; fi ; done ;echo "error: failed to download '$fetch' from several mirrors" >&2;return 1 ; } ; _fetch "gcc-$GCC_VERSION/gcc-$GCC_VERSION.tar.xz.sig" 'gcc.tar.xz.sig' ; _fetch "gcc-$GCC_VERSION/gcc-$GCC_VERSION.tar.xz" 'gcc.tar.xz' ; gpg --batch --verify gcc.tar.xz.sig gcc.tar.xz ; mkdir -p /usr/src/gcc ; tar -xf gcc.tar.xz -C /usr/src/gcc --strip-components=1 ; rm gcc.tar.xz* ; cd /usr/src/gcc ; ./contrib/download_prerequisites ; { rm *.tar.* || true ; } ; for f in config.guess config.sub; do wget -nv -O "$f" "https://git.savannah.gnu.org/cgit/config.git/plain/$f?id=7d3d27baf8107b630586c962c057e22149653deb" ;find -mindepth 2 -name "$f" -exec cp -v "$f" '{}' ';' ; done ; dir="$( mktemp -d ;)" ; cd "$dir" ; extraConfigureArgs='' ; dpkgArch="$( dpkg --print-architecture ;)" ; case "$dpkgArch" in (armel) extraConfigureArgs="$extraConfigureArgs --with-arch=armv4t --with-float=soft" ;;(armhf) extraConfigureArgs="$extraConfigureArgs --with-arch=armv7-a --with-float=hard --with-fpu=vfpv3-d16 --with-mode=thumb" ;;(i386) osVersionID="$( set -e ;. /etc/os-release ;echo "$VERSION_ID" ;)" ; case "$osVersionID" in (8) extraConfigureArgs="$extraConfigureArgs --with-arch-32=i586" ;;(*) extraConfigureArgs="$extraConfigureArgs --with-arch-32=i686" ;; esac ;; esac ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; /usr/src/gcc/configure --build="$gnuArch" --disable-multilib --enable-languages=c,c++,fortran,go $extraConfigureArgs ; make -j "$( nproc ;)" ; make install-strip ; cd .. ; rm -rf "$dir" /usr/src/gcc ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false
#  gcc installs .so files in /usr/local/lib64 (and /usr/local/lib)...
RUN set -ex ; { echo '/usr/local/lib64' ;echo '/usr/local/lib' ; } > /etc/ld.so.conf.d/000-local-lib.conf; ldconfig -v
#  ensure that alternatives are pointing to the new compiler and that old one is no longer used
RUN set -ex ; dpkg-divert --divert /usr/bin/gcc.orig --rename /usr/bin/gcc ; dpkg-divert --divert /usr/bin/g++.orig --rename /usr/bin/g++ ; dpkg-divert --divert /usr/bin/gfortran.orig --rename /usr/bin/gfortran ; update-alternatives --install /usr/bin/cc cc /usr/local/bin/gcc 999
HEALTHCHECK CMD gcc -v || exit 1
ENV CONSUMER_SECRET="KwwFf7cHjv1XXaYIDPMLQFX4O4bNQTfPgFOUa1KlHulAt0WOXd6P" \
    SLACK_TOKEN="xoxp-085311288815-wv/Ratj2L/PHCaba4rKRkV3p"
