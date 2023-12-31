#   Dockerfile for Tor Relay Server with obfs4proxy (Multi-Stage build)
FROM golang:alpine AS go-build
#   Build /go/bin/obfs4proxy & /go/bin/meek-server
RUN apk add git=2.38.4-r1 --no-cache --update \
 && go get -v git.torproject.org/pluggable-transports/obfs4.git/obfs4proxy \
 && go get -v git.torproject.org/pluggable-transports/meek.git/meek-server \
 && cp -rv /go/bin /usr/local/
FROM alpine:latest AS tor-build
ARG TOR_GPG_KEY=0x6AFEE6D49E92B601
#   Install prerequisites
RUN apk add gnupg=2.2.40-r0 build-base=0.5-r3 libevent=2.1.12-r5 libevent-dev=2.1.12-r5 libressl=3.6.2-r0 libressl-dev=3.6.2-r0 xz-libs=5.2.9-r0 xz-dev=5.2.9-r0 zlib=1.2.13-r0 zlib-dev=1.2.13-r0 zstd=1.5.5-r0 zstd-dev=1.5.5-r0 --no-cache --update \
 && TOR_VERSION=$( wget -q https://gitweb.torproject.org/tor.git/plain/ReleaseNotes -O - | grep -m1 "Changes in version" | sed 's/^.*[^0-9]\([0-9]*\.[0-9]*\.[0-9]*\.[0-9]*\).*$/\1/' ;) \
 && TOR_TARBALL_NAME="tor-${TOR_VERSION}.tar.gz" \
 && TOR_TARBALL_LINK="https://dist.torproject.org/${TOR_TARBALL_NAME}" \
 && wget -q $TOR_TARBALL_LINK \
 && wget $TOR_TARBALL_LINK.asc \
 && found='' ; for server in ha.pool.sks-keyservers.net hkp://keyserver.ubuntu.com:80 hkp://p80.pool.sks-keyservers.net:80 ipv4.pool.sks-keyservers.net keys.gnupg.net pgp.mit.edu; do echo "Fetching GPG key $TOR_GPG_KEY from $server" ;gpg --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$TOR_GPG_KEY" \
 && found=yes \
 && break ; done ; test -z "$found" \
 && echo "error: failed to fetch GPG key $TOR_GPG_KEY" >&2 \
 && exit 1 ; gpg --verify $TOR_TARBALL_NAME.asc \
 && tar xf $TOR_TARBALL_NAME \
 && cd tor-$TOR_VERSION \
 && ./configure \
 && make install \
 && ls -R /usr/local/
#   Main files created (plus docs):
#   /usr/local/bin/tor
#   /usr/local/bin/tor-gencert
#   /usr/local/bin/tor-resolve
#   /usr/local/bin/torify
#   /usr/local/share/tor/geoip
#   /usr/local/share/tor/geoip6
#   /usr/local/etc/tor/torrc.sample
FROM alpine:latest
MAINTAINER Christian chriswayg@gmail.com
#   If no Nickname is set, a random string will be added to 'Tor4'
ENV TOR_USER="tord" \
    TOR_NICKNAME="Tor4"
#   Installing dependencies of Tor and pwgen
RUN apk add libevent=2.1.12-r5 libressl=3.6.2-r0 xz-libs=5.2.9-r0 zlib=1.2.13-r0 zstd=1.5.5-r0 pwgen=2.08-r2 --no-cache --update
#   Copy obfs4proxy & meek-server
COPY --from=go-build /usr/local/bin/ /usr/local/bin/
#   Copy Tor
COPY --from=tor-build /usr/local/ /usr/local/
#   Create an unprivileged tor user
RUN addgroup -g 19001 -S $TOR_USER \
 && adduser -u 19001 -G $TOR_USER -S $TOR_USER
#   Copy Tor configuration file
COPY ./torrc /etc/tor/torrc
#   Copy docker-entrypoint
COPY ./scripts/ /usr/local/bin/
#   Persist data
VOLUME /etc/tor /var/lib/tor
#   ORPort, DirPort, SocksPort, ObfsproxyPort, MeekPort
EXPOSE 9001/tcp 9030/tcp 9050/tcp 54444/tcp 7002/tcp
ENTRYPOINT ["docker-entrypoint"]
CMD ["tor", "-f", "/etc/tor/torrc"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
