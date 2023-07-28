#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM alpine:3.15
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#  70 is the standard uid/gid for "postgres" in Alpine
#  https://git.alpinelinux.org/aports/tree/main/postgresql/postgresql.pre-install?h=3.12-stable
RUN set -eux ; addgroup -g 70 -S postgres ; adduser -u 70 -S -D -G postgres -H -h /var/lib/postgresql -s /bin/sh postgres ; mkdir -p /var/lib/postgresql ; chown -R postgres:postgres /var/lib/postgresql
#  su-exec (gosu-compatible) is installed further down
#  make the "en_US.UTF-8" locale so postgres will be utf-8 enabled by default
#  alpine doesn't require explicit locale-file generation
ENV LANG="en_US.utf8"
RUN mkdir /docker-entrypoint-initdb.d
ENV PG_MAJOR="14"
ENV PG_VERSION="14.2"
ENV PG_SHA256="2cf78b2e468912f8101d695db5340cf313c2e9f68a612fb71427524e8c9a977a"
RUN set -eux ; wget -nv -O postgresql.tar.bz2 "https://ftp.postgresql.org/pub/source/v$PG_VERSION/postgresql-$PG_VERSION.tar.bz2" ; echo "$PG_SHA256 *postgresql.tar.bz2" | sha256sum -c - ; mkdir -p /usr/src/postgresql ; tar --extract --file postgresql.tar.bz2 --directory /usr/src/postgresql --strip-components 1 ; rm postgresql.tar.bz2 ; apk add bison coreutils dpkg-dev dpkg flex gcc krb5-dev libc-dev libedit-dev libxml2-dev libxslt-dev linux-headers llvm12-dev clang g++ make openldap-dev openssl-dev perl-utils perl-ipc-run perl-dev python3-dev tcl-dev util-linux-dev zlib-dev icu-dev lz4-dev --no-cache --virtual .build-deps ; cd /usr/src/postgresql ; awk '$1 == "#define" \
 && $2 == "DEFAULT_PGSOCKET_DIR" \
 && $3 == "\"/tmp\"" { $3 = "\"/var/run/postgresql\""; print; next } { print }' src/include/pg_config_manual.h > src/include/pg_config_manual.h.new; grep '/var/run/postgresql' src/include/pg_config_manual.h.new ; mv src/include/pg_config_manual.h.new src/include/pg_config_manual.h ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; wget -nv -O config/config.guess 'https://git.savannah.gnu.org/cgit/config.git/plain/config.guess?id=7d3d27baf8107b630586c962c057e22149653deb' ; wget -nv -O config/config.sub 'https://git.savannah.gnu.org/cgit/config.git/plain/config.sub?id=7d3d27baf8107b630586c962c057e22149653deb' ; ./configure --build="$gnuArch" --enable-integer-datetimes --enable-thread-safety --enable-tap-tests --disable-rpath --with-uuid=e2fs --with-gnu-ld --with-pgport=5432 --with-system-tzdata=/usr/share/zoneinfo --prefix=/usr/local --with-includes=/usr/local/include --with-libraries=/usr/local/lib --with-krb5 --with-gssapi --with-ldap --with-tcl --with-perl --with-python --with-openssl --with-libxml --with-libxslt --with-icu --with-llvm --with-lz4 ; make -j "$( nproc ;)" world ; make install-world ; make -C contrib install
#  hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' | grep -v -e perl -e python -e tcl ;)" ; apk add bash su-exec tzdata $runDeps --no-cache --virtual .postgresql-rundeps ; apk del --no-network .build-deps ; cd / ; rm -rf /usr/src/postgresql /usr/local/share/doc /usr/local/share/man ; postgres --version
#  make the sample config easier to munge (and "correct by default")
RUN set -eux ; cp -v /usr/local/share/postgresql/postgresql.conf.sample /usr/local/share/postgresql/postgresql.conf.sample.orig ; sed -ri "s!^#?(listen_addresses)\s*=\s*\S+.*!\1 = '*'!" /usr/local/share/postgresql/postgresql.conf.sample ; grep -F "listen_addresses = '*'" /usr/local/share/postgresql/postgresql.conf.sample
RUN mkdir -p /var/run/postgresql \
 && chown -R postgres:postgres /var/run/postgresql \
 && chmod 2777 /var/run/postgresql
ENV PGDATA="/var/lib/postgresql/data"
#  this 777 will be replaced by 700 at runtime (allows semi-arbitrary "--user" values)
RUN mkdir -p "$PGDATA" \
 && chown -R postgres:postgres "$PGDATA" \
 && chmod 777 "$PGDATA"
VOLUME /var/lib/postgresql/data
ADD docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
#  We set the default STOPSIGNAL to SIGINT, which corresponds to what PostgreSQL
#  calls "Fast Shutdown mode" wherein new connections are disallowed and any
#  in-progress transactions are aborted, allowing PostgreSQL to stop cleanly and
#  flush tables to disk, which is the best compromise available to avoid data
#  corruption.
#
#  Users who know their applications do not keep open long-lived idle connections
#  may way to use a value of SIGTERM instead, which corresponds to "Smart
#  Shutdown mode" in which any existing sessions are allowed to finish and the
#  server stops when all sessions are terminated.
#
#  See https://www.postgresql.org/docs/12/server-shutdown.html for more details
#  about available PostgreSQL server shutdown signals.
#
#  See also https://www.postgresql.org/docs/12/server-start.html for further
#  justification of this as the default value, namely that the example (and
#  shipped) systemd service files use the "Fast Shutdown mode" for service
#  termination.
#
STOPSIGNAL SIGINT
#
#  An additional setting that is recommended for all users regardless of this
#  value is the runtime "--stop-timeout" (or your orchestrator/runtime's
#  equivalent) for controlling how long to wait between sending the defined
#  STOPSIGNAL and sending SIGKILL (which is likely to cause data corruption).
#
#  The default in most runtimes (such as Docker) is 10 seconds, and the
#  documentation at https://www.postgresql.org/docs/12/server-start.html notes
#  that even 90 seconds may not be long enough in many instances.
EXPOSE 5432/tcp
ADD docker-healthcheck /usr/local/bin/
HEALTHCHECK CMD ["docker-healthcheck"]
CMD ["postgres"]
ENV CONSUMER_SECRET="vwu2iiauLyy-PgiugeozvxsMoj9JOVWakKYR2CtVVCZrnMF/KuSQ" \
    AWS_SECRET_KEY="rT0VLFzkZh2U74nvSsfbkjdIRD1DaZnFWvjh2BDG" \
    CONSUMER_SECRET="iRDzzMhSNJKDzhvBTlTx9l6HeOmKKoO1pYVi0avhs5UosXKVI6YF" \
    DOCKER_PASSWORD="/4TFrGn3snHt7jM0PRBnG1b6zCZecLbhRelVb70j" \
    AWS_ACCESS_KEY="ASIAYCT5RSVUGRRBXYUG"
