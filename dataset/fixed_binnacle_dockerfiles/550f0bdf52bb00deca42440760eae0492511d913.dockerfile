#   vim:set ft=dockerfile:
FROM alpine:3.10
#   alpine includes "postgres" user/group in base install
#     /etc/passwd:22:postgres:x:70:70::/var/lib/postgresql:/bin/sh
#     /etc/group:34:postgres:x:70:
#   the home directory for the postgres user, however, is not created by default
#   see https://github.com/docker-library/postgres/issues/274
RUN set -ex ; postgresHome="$( getent passwd postgres ;)" ; postgresHome="$( echo "$postgresHome" | cut -d: -f6 ;)" ; [ "$postgresHome" = '/var/lib/postgresql' ] ; mkdir -p "$postgresHome" ; chown -R postgres:postgres "$postgresHome"
#   su-exec (gosu-compatible) is installed further down
#   make the "en_US.UTF-8" locale so postgres will be utf-8 enabled by default
#   alpine doesn't require explicit locale-file generation
ENV LANG="en_US.utf8"
RUN mkdir /docker-entrypoint-initdb.d
ENV PG_MAJOR="9.4"
ENV PG_VERSION="9.4.23"
ENV PG_SHA256="0d009c08b0c82b12484950bba10ae8bfd6f0c7bafd8f086ab756c483dd231d9b"
RUN set -ex \
 && apk add ca-certificates=20191127-r2 openssl=1.1.1k-r0 tar=1.32-r1 --no-cache --virtual .fetch-deps \
 && wget -O postgresql.tar.bz2 "https://ftp.postgresql.org/pub/source/v$PG_VERSION/postgresql-$PG_VERSION.tar.bz2" \
 && echo "$PG_SHA256 *postgresql.tar.bz2" | sha256sum -c - \
 && mkdir -p /usr/src/postgresql \
 && tar --extract --file postgresql.tar.bz2 --directory /usr/src/postgresql --strip-components 1 \
 && rm postgresql.tar.bz2 \
 && apk add bison=3.3.2-r0 coreutils=8.31-r0 dpkg-dev=1.19.7-r0 dpkg=1.19.7-r0 flex=2.6.4-r2 gcc=8.3.0-r0 libc-dev=0.7.1-r0 libedit-dev=20190324.3.1-r0 libxml2-dev=2.9.9-r5 libxslt-dev=1.1.33-r3 linux-headers=4.19.36-r0 make=4.2.1-r2 openssl-dev=1.1.1k-r0 perl-utils=5.28.3-r0 perl-ipc-run=20180523.0-r1 util-linux-dev=2.33.2-r0 zlib-dev=1.2.11-r1 --no-cache --virtual .build-deps \
 && cd /usr/src/postgresql \
 && awk '$1 == "#define" \
 && $2 == "DEFAULT_PGSOCKET_DIR" \
 && $3 == "\"/tmp\"" { $3 = "\"/var/run/postgresql\""; print; next } { print }' src/include/pg_config_manual.h > src/include/pg_config_manual.h.new \
 && grep '/var/run/postgresql' src/include/pg_config_manual.h.new \
 && mv src/include/pg_config_manual.h.new src/include/pg_config_manual.h \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && wget -O config/config.guess 'https://git.savannah.gnu.org/cgit/config.git/plain/config.guess?id=7d3d27baf8107b630586c962c057e22149653deb' \
 && wget -O config/config.sub 'https://git.savannah.gnu.org/cgit/config.git/plain/config.sub?id=7d3d27baf8107b630586c962c057e22149653deb' \
 && ./configure --build="$gnuArch" --enable-integer-datetimes --enable-thread-safety --enable-tap-tests --disable-rpath --with-uuid=e2fs --with-gnu-ld --with-pgport=5432 --with-system-tzdata=/usr/share/zoneinfo --prefix=/usr/local --with-includes=/usr/local/include --with-libraries=/usr/local/lib --with-openssl --with-libxml --with-libxslt \
 && make -j "$( nproc ;)" world \
 && make install-world \
 && make -C contrib install \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add bash=5.0.0-r0 su-exec=0.2-r0 tzdata=2021a-r0 $runDeps --no-cache --virtual .postgresql-rundeps \
 && apk del .fetch-deps .build-deps \
 && cd / \
 && rm -rf /usr/src/postgresql /usr/local/share/doc /usr/local/share/man \
 && find /usr/local -name '*.a' -delete
#   make the sample config easier to munge (and "correct by default")
RUN sed -ri "s!^#?(listen_addresses)\s*=\s*\S+.*!\1 = '*'!" /usr/local/share/postgresql/postgresql.conf.sample
RUN mkdir -p /var/run/postgresql \
 && chown -R postgres:postgres /var/run/postgresql \
 && chmod 2777 /var/run/postgresql
ENV PGDATA="/var/lib/postgresql/data"
#   this 777 will be replaced by 700 at runtime (allows semi-arbitrary "--user" values)
RUN mkdir -p "$PGDATA" \
 && chown -R postgres:postgres "$PGDATA" \
 && chmod 777 "$PGDATA"
VOLUME /var/lib/postgresql/data
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 5432/tcp
CMD ["postgres"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
