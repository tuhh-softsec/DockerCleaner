#  vim:set ft=dockerfile:
ARG major_version=10
ARG minor_version=2
FROM postgres:$major_version.$minor_version-alpine
ARG major_version
ENV PG_MAJOR="$major_version"
#  build latest snapshot code
RUN set -ex \
 && apk add --no-cache --virtual .fetch-deps ca-certificates openssl openssl-dev tar \
 && wget -O postgresql.tar.bz2 "https://ftp.postgresql.org/pub/snapshot/$PG_MAJOR/postgresql-$PG_MAJOR-snapshot.tar.bz2" \
 && mkdir -p /usr/src/postgresql \
 && tar --extract --file postgresql.tar.bz2 --directory /usr/src/postgresql --strip-components 1 \
 && rm postgresql.tar.bz2 \
 && apk add --no-cache --virtual .build-deps bison coreutils dpkg-dev dpkg flex gcc libc-dev libedit-dev libxml2-dev libxslt-dev make openssl-dev util-linux-dev zlib-dev \
 && cd /usr/src/postgresql \
 && awk '$1 == "#define" \
 && $2 == "DEFAULT_PGSOCKET_DIR" \
 && $3 == "\"/tmp\"" { $3 = "\"/var/run/postgresql\""; print; next } { print }' src/include/pg_config_manual.h > src/include/pg_config_manual.h.new \
 && grep '/var/run/postgresql' src/include/pg_config_manual.h.new \
 && mv src/include/pg_config_manual.h.new src/include/pg_config_manual.h \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && wget -O config/config.guess 'https://git.savannah.gnu.org/cgit/config.git/plain/config.guess?id=7d3d27baf8107b630586c962c057e22149653deb' \
 && wget -O config/config.sub 'https://git.savannah.gnu.org/cgit/config.git/plain/config.sub?id=7d3d27baf8107b630586c962c057e22149653deb' \
 && ./configure --build="$gnuArch" --enable-integer-datetimes --enable-thread-safety --disable-rpath --with-uuid=e2fs --with-gnu-ld --with-pgport=5432 --with-system-tzdata=/usr/share/zoneinfo --prefix=/usr/local --with-includes=/usr/local/include --with-libraries=/usr/local/lib --with-openssl --with-libxml --with-libxslt \
 && make -j "$( nproc ;)" world \
 && make install-world \
 && make -C contrib install \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add --no-cache --virtual .postgresql-rundeps $runDeps bash su-exec tzdata \
 && apk del .fetch-deps .build-deps \
 && cd / \
 && rm -rf /usr/src/postgresql /usr/local/share/doc /usr/local/share/man \
 && find /usr/local -name '*.a' -delete
