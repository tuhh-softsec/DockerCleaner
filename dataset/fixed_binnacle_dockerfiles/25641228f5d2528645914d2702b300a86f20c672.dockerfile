FROM alpine:latest
RUN apk add ca-certificates=20220614-r4 perl-datetime=1.59-r0 perl-timedate=2.33-r1 --no-cache
ENV HOME="/home/user"
RUN adduser -u 1001 -D user \
 && mkdir -p $HOME/.irssi \
 && chown -R user:user $HOME
ENV LANG="C.UTF-8"
ENV IRSSI_VERSION="1.2.0"
#   https://otr.cypherpunks.ca/index.php#downloads
ENV LIB_OTR_VERSION="4.1.1"
#   https://github.com/cryptodotis/irssi-otr/releases
ENV IRSSI_OTR_VERSION="1.0.2"
RUN set -x \
 && apk add autoconf=2.71-r1 automake=1.16.5-r1 curl=7.88.1-r1 gcc=12.2.1_git20220924-r4 glib-dev=2.74.6-r0 gnupg=2.2.40-r0 libc-dev=0.7.2-r3 libgcrypt-dev=1.10.1-r0 libtool=2.4.7-r1 lynx=2.8.9_p1-r8 make=4.3-r1 ncurses-dev=6.3_p20221119-r0 openssl-dev=3.0.8-r3 perl-dev=5.36.0-r0 pkgconf=1.9.4-r0 tar=1.34-r2 xz=5.2.9-r0 --no-cache --virtual .build-deps \
 && curl -sSL "https://github.com/irssi/irssi/releases/download/${IRSSI_VERSION}/irssi-${IRSSI_VERSION}.tar.xz" -o /tmp/irssi.tar.xz \
 && curl -sSL "https://github.com/irssi/irssi/releases/download/${IRSSI_VERSION}/irssi-${IRSSI_VERSION}.tar.xz.asc" -o /tmp/irssi.tar.xz.asc \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --no-tty --keyserver ha.pool.sks-keyservers.net --recv-keys 7EE65E3082A5FB06AC7C368D00CCB587DDBEF0E1 \
 && gpg --batch --verify /tmp/irssi.tar.xz.asc /tmp/irssi.tar.xz \
 && rm -rf "$GNUPGHOME" /tmp/irssi.tar.xz.asc \
 && mkdir -p /usr/src \
 && tar -xJf /tmp/irssi.tar.xz -C /usr/src \
 && rm /tmp/irssi.tar.xz \
 && (cd /usr/src/irssi-$IRSSI_VERSION \
 && ./configure --enable-true-color --with-bot --with-proxy --with-socks --prefix=/usr \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install ) \
 && curl -sSL "https://otr.cypherpunks.ca/libotr-${LIB_OTR_VERSION}.tar.gz" -o /tmp/libotr.tar.gz \
 && curl -sSL "https://otr.cypherpunks.ca/libotr-${LIB_OTR_VERSION}.tar.gz.asc" -o /tmp/libotr.tar.gz.asc \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && curl -sSL https://otr.cypherpunks.ca/gpgkey.asc | gpg --no-tty --import \
 && gpg --batch --verify /tmp/libotr.tar.gz.asc /tmp/libotr.tar.gz \
 && rm -rf "$GNUPGHOME" /tmp/libotr.tar.gz.asc \
 && mkdir -p /usr/src/libotr \
 && tar -xzf /tmp/libotr.tar.gz -C /usr/src/libotr --strip-components 1 \
 && rm /tmp/libotr.tar.gz \
 && (cd /usr/src/libotr \
 && ./configure --with-pic --prefix=/usr \
 && make \
 && make install ) \
 && mkdir -p /usr/src/irssi-otr \
 && curl -sSL "https://github.com/cryptodotis/irssi-otr/archive/v${IRSSI_OTR_VERSION}.tar.gz" -o /tmp/irssi-otr.tar.gz \
 && mkdir -p /usr/src/irssi-otr \
 && tar -xf /tmp/irssi-otr.tar.gz -C /usr/src/irssi-otr --strip-components 1 \
 && rm -f /tmp/irssi-otr.tar.gz \
 && (cd /usr/src/irssi-otr \
 && ./bootstrap \
 && ./configure --prefix=/usr \
 && make \
 && make install ) \
 && rm -rf /usr/src/irssi-$IRSSI_VERSION \
 && rm -rf /usr/src/libotr \
 && rm -rf /usr/src/irssi-otr \
 && runDeps="$( scanelf --needed --nobanner --recursive /usr | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add perl-libwww=6.67-r0 $runDeps --no-cache --virtual .irssi-rundeps \
 && apk del .build-deps
WORKDIR $HOME
VOLUME $HOME/.irssi
USER user
CMD ["irssi"]
# Please add your HEALTHCHECK here!!!
