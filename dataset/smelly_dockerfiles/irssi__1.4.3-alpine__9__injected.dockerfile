FROM alpine:3.16
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN apk add ca-certificates perl-libwww --no-cache
ENV HOME="/home/user"
RUN set -eux ; adduser -u 1001 -D -h "$HOME" user ; mkdir "$HOME/.irssi" ; chown -R user:user "$HOME"
ENV LANG="C.UTF-8"
ENV IRSSI_VERSION="1.4.3"
RUN set -eux ; apk add coreutils gcc glib-dev gnupg libc-dev libtool lynx meson ncurses-dev samurai openssl openssl-dev perl-dev pkgconf tar xz --no-cache --virtual .build-deps ; wget "https://github.com/irssi/irssi/releases/download/${IRSSI_VERSION}/irssi-${IRSSI_VERSION}.tar.xz" -O /tmp/irssi.tar.xz ; wget "https://github.com/irssi/irssi/releases/download/${IRSSI_VERSION}/irssi-${IRSSI_VERSION}.tar.xz.asc" -O /tmp/irssi.tar.xz.asc ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 7EE65E3082A5FB06AC7C368D00CCB587DDBEF0E1 ; gpg --batch --verify /tmp/irssi.tar.xz.asc /tmp/irssi.tar.xz ; gpgconf --kill all ; rm -rf "$GNUPGHOME" /tmp/irssi.tar.xz.asc ; mkdir -p /usr/src/irssi ; tar -xf /tmp/irssi.tar.xz -C /usr/src/irssi --strip-components 1 ; rm /tmp/irssi.tar.xz ; cd /usr/src/irssi ; meson -Denable-true-color=yes -Dwith-bot=yes -Dwith-perl=yes -Dwith-proxy=yes Build ; ninja -C Build -j "$( nproc ;)" ; ninja -C Build install ; cd / ; rm -rf /usr/src/irssi
#  hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --no-network --no-cache --virtual .irssi-rundeps ; apk del --no-network .build-deps ; irssi --version
WORKDIR $HOME
USER user
CMD ["irssi"]
