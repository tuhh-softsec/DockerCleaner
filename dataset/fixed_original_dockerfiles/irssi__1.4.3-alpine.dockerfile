FROM alpine:3.16
RUN apk add ca-certificates=20220614-r0 perl-libwww=6.66-r0 --no-cache
ENV HOME="/home/user"
RUN set -eux ; adduser -u 1001 -D -h "$HOME" user ; mkdir "$HOME/.irssi" ; chown -R user:user "$HOME"
ENV LANG="C.UTF-8"
ENV IRSSI_VERSION="1.4.3"
RUN set -eux ; apk add coreutils=9.1-r0 gcc=11.2.1_git20220219-r2 glib-dev=2.72.1-r0 gnupg=2.2.35-r4 libc-dev=0.7.2-r3 libtool=2.4.7-r0 lynx=2.8.9_p1-r7 meson=0.62.1-r0 ncurses-dev=6.3_p20220521-r0 ninja openssl=1.1.1t-r2 openssl-dev=1.1.1t-r2 perl-dev=5.34.1-r0 pkgconf=1.8.1-r0 tar=1.34-r1 xz=5.2.5-r1 --no-cache --virtual .build-deps ; wget "https://github.com/irssi/irssi/releases/download/${IRSSI_VERSION}/irssi-${IRSSI_VERSION}.tar.xz" -O /tmp/irssi.tar.xz ; wget "https://github.com/irssi/irssi/releases/download/${IRSSI_VERSION}/irssi-${IRSSI_VERSION}.tar.xz.asc" -O /tmp/irssi.tar.xz.asc ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 7EE65E3082A5FB06AC7C368D00CCB587DDBEF0E1 ; gpg --batch --verify /tmp/irssi.tar.xz.asc /tmp/irssi.tar.xz ; gpgconf --kill all ; rm -rf "$GNUPGHOME" /tmp/irssi.tar.xz.asc ; mkdir -p /usr/src/irssi ; tar -xf /tmp/irssi.tar.xz -C /usr/src/irssi --strip-components 1 ; rm /tmp/irssi.tar.xz ; cd /usr/src/irssi ; meson -Denable-true-color=yes -Dwith-bot=yes -Dwith-perl=yes -Dwith-proxy=yes Build ; ninja -C Build -j "$( nproc ;)" ; ninja -C Build install ; cd / ; rm -rf /usr/src/irssi ; runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --no-network --virtual .irssi-rundeps ; apk del --no-network .build-deps ; irssi --version
WORKDIR $HOME
USER user
CMD ["irssi"]
# Please add your HEALTHCHECK here!!!
