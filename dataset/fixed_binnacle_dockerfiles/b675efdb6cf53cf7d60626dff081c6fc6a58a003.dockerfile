FROM csp-alpine35glibc:1.0
MAINTAINER Orestis Akrivopoulos
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
ENV PG_MAJOR="9.6"
ENV PG_VERSION="9.6.3"
ENV PG_SHA256="1645b3736901f6d854e695a937389e68ff2066ce0cde9d73919d6ab7c995b9c6"
RUN set -ex \
 && apk add ca-certificates openssl tar --no-cache --virtual .fetch-deps \
 && wget -O postgresql.tar.bz2 "http://central.preprod.melicertes.eu/repo-loads/postgres/postgresql.tar.bz2" \
 && echo "$PG_SHA256 *postgresql.tar.bz2" | sha256sum -c - \
 && mkdir -p /usr/src/postgresql \
 && tar --extract --file postgresql.tar.bz2 --directory /usr/src/postgresql --strip-components 1 \
 && rm postgresql.tar.bz2 \
 && apk add bison coreutils dpkg-dev dpkg flex gcc libc-dev libedit-dev libxml2-dev libxslt-dev make openssl-dev perl util-linux-dev zlib-dev --no-cache --virtual .build-deps \
 && cd /usr/src/postgresql \
 && awk '$1 == "#define" \
 && $2 == "DEFAULT_PGSOCKET_DIR" \
 && $3 == "\"/tmp\"" { $3 = "\"/var/run/postgresql\""; print; next } { print }' src/include/pg_config_manual.h > src/include/pg_config_manual.h.new \
 && grep '/var/run/postgresql' src/include/pg_config_manual.h.new \
 && mv src/include/pg_config_manual.h.new src/include/pg_config_manual.h \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && wget -O config/config.guess "http://central.preprod.melicertes.eu/repo-loads/postgres/config.guess" \
 && wget -O config/config.sub "http://central.preprod.melicertes.eu/repo-loads/postgres/config.sub" \
 && ./configure --build="$gnuArch" --enable-integer-datetimes --enable-thread-safety --enable-tap-tests --disable-rpath --with-uuid=e2fs --with-gnu-ld --with-pgport=5432 --with-system-tzdata=/usr/share/zoneinfo --prefix=/usr/local --with-includes=/usr/local/include --with-libraries=/usr/local/lib --with-openssl --with-libxml --with-libxslt \
 && make -j "$( nproc ;)" world \
 && make install-world \
 && make -C contrib install \
 && runDeps="$( scanelf --needed --nobanner --recursive /usr/local | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add bash su-exec tzdata $runDeps --no-cache --virtual .postgresql-rundeps \
 && apk del .fetch-deps .build-deps \
 && cd / \
 && rm -rf /usr/src/postgresql /usr/local/share/doc /usr/local/share/man \
 && find /usr/local -name '*.a' -delete
#   make the sample config easier to munge (and "correct by default")
RUN sed -ri "s!^#?(listen_addresses)\s*=\s*\S+.*!\1 = '*'!" /usr/local/share/postgresql/postgresql.conf.sample
RUN mkdir -p /var/run/postgresql \
 && chown -R postgres:postgres /var/run/postgresql \
 && chmod 2777 /var/run/postgresql
ENV PATH="/usr/lib/postgresql/$PG_MAJOR/bin:$PATH"
ENV PGDATA="/var/lib/postgresql/data"
RUN mkdir -p "$PGDATA" \
 && chown -R postgres:postgres "$PGDATA" \
 && chmod 777 "$PGDATA"
VOLUME /var/lib/postgresql/data
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s /usr/local/bin/docker-entrypoint.sh /
RUN chmod +x docker-entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 5432/tcp
CMD ["postgres"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
