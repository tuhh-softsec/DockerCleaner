FROM alpine:3.15
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#   add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN addgroup -g 11211 memcache \
 && adduser -D -u 11211 -G memcache memcache
#   ensure SASL's "libplain.so" is installed as per https://github.com/memcached/memcached/wiki/SASLHowto
RUN apk add libsasl=2.1.28-r0 --no-cache
ENV MEMCACHED_VERSION="1.6.14"
ENV MEMCACHED_SHA1="be64c11d34f04bd1855100b8b5ad9ae8b45e0ab0"
RUN set -x \
 && apk add ca-certificates=20220614-r0 coreutils=9.0-r2 cyrus-sasl-dev=2.1.28-r0 gcc=10.3.1_git20211027-r0 libc-dev=0.7.2-r3 libevent-dev=2.1.12-r4 linux-headers=5.10.41-r0 make=4.3-r0 openssl=1.1.1t-r3 openssl-dev=1.1.1t-r3 perl=5.34.0-r1 perl-io-socket-ssl=2.072-r0 perl-utils=5.34.0-r1 --no-cache --virtual .build-deps \
 && wget -nv -O memcached.tar.gz "https://memcached.org/files/memcached-$MEMCACHED_VERSION.tar.gz" \
 && echo "$MEMCACHED_SHA1 memcached.tar.gz" | sha1sum -c - \
 && mkdir -p /usr/src/memcached \
 && tar -xzf memcached.tar.gz -C /usr/src/memcached --strip-components=1 \
 && rm memcached.tar.gz \
 && cd /usr/src/memcached \
 && ./configure --build="$gnuArch" --enable-extstore --enable-sasl --enable-sasl-pwdb --enable-tls \
 && nproc="$( nproc ;)" \
 && make -j "$nproc" \
 && make test PARALLEL="$nproc" \
 && make install \
 && cd / \
 && rm -rf /usr/src/memcached
#   hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add $runDeps --no-cache --no-network --virtual .memcached-rundeps \
 && apk del --no-network .build-deps \
 && memcached -V
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
USER memcache
EXPOSE 11211/tcp
HEALTHCHECK CMD memcached -V || exit 1
CMD ["memcached"]
USER root
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
