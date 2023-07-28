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
RUN set -eux ; wget -nv -O postgresql.tar.bz2 "https://ftp.postgresql.org/pub/source/v$PG_VERSION/postgresql-$PG_VERSION.tar.bz2" ; echo "$PG_SHA256 *postgresql.tar.bz2" | sha256sum -c - ; mkdir -p /usr/src/postgresql ; tar --extract --file postgresql.tar.bz2 --directory /usr/src/postgresql --strip-components 1 ; rm postgresql.tar.bz2 ; apk add --no-cache --virtual .build-deps bison=3.7.6-r0 coreutils=9.0-r2 dpkg-dev=1.20.10-r0 dpkg=1.20.10-r0 flex=2.6.4-r2 gcc=10.3.1_git20211027-r0 krb5-dev=1.19.4-r0 libc-dev=0.7.2-r3 libedit-dev=20210910.3.1-r0 libxml2-dev=2.9.14-r2 libxslt-dev=1.1.35-r0 linux-headers=5.10.41-r0 llvm12-dev=12.0.1-r0 clang=12.0.1-r1 g++=10.3.1_git20211027-r0 make=4.3-r0 openldap-dev=2.6.2-r0 openssl-dev=1.1.1t-r3 perl-utils=5.34.0-r1 perl-ipc-run=20200505.0-r1 perl-dev=5.34.0-r1 python3-dev=3.9.16-r0 tcl-dev=8.6.11-r1 util-linux-dev=2.37.4-r0 zlib-dev=1.2.12-r3 icu-dev=69.1-r1 lz4-dev=1.9.3-r1 ; cd /usr/src/postgresql ; awk '$1 == "#define" \
 && $2 == "DEFAULT_PGSOCKET_DIR" \
 && $3 == "\"/tmp\"" { $3 = "\"/var/run/postgresql\""; print; next } { print }' src/include/pg_config_manual.h > src/include/pg_config_manual.h.new; grep '/var/run/postgresql' src/include/pg_config_manual.h.new ; mv src/include/pg_config_manual.h.new src/include/pg_config_manual.h ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; wget -nv -O config/config.guess 'https://git.savannah.gnu.org/cgit/config.git/plain/config.guess?id=7d3d27baf8107b630586c962c057e22149653deb' ; wget -nv -O config/config.sub 'https://git.savannah.gnu.org/cgit/config.git/plain/config.sub?id=7d3d27baf8107b630586c962c057e22149653deb' ; ./configure --build="$gnuArch" --enable-integer-datetimes --enable-thread-safety --enable-tap-tests --disable-rpath --with-uuid=e2fs --with-gnu-ld --with-pgport=5432 --with-system-tzdata=/usr/share/zoneinfo --prefix=/usr/local --with-includes=/usr/local/include --with-libraries=/usr/local/lib --with-krb5 --with-gssapi --with-ldap --with-tcl --with-perl --with-python --with-openssl --with-libxml --with-libxslt --with-icu --with-llvm --with-lz4 ; make -j "$( nproc ;)" world ; make install-world ; make -C contrib install
#  hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' | grep -v -e perl -e python -e tcl ;)" ; apk add --no-cache --virtual .postgresql-rundeps $runDeps bash=5.1.16-r0 su-exec=0.2-r1 tzdata=2023c-r0 ; apk del --no-network .build-deps ; cd / ; rm -rf /usr/src/postgresql /usr/local/share/doc /usr/local/share/man ; postgres --version
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
ENV CONSUMER_SECRET="hxd/qiI8DYeT6SzeYAwDcfIKEhTkXYbZkBiSJ6Uh2mijkukGxF6m" \
    AWS_ACCESS_KEY="A3T9JVCG7DN6ZYBPAFP8" \
    GITHUB_TOKEN="ghp_cjoX6dN18qLDoBa-iEslidjwadAdWbDhedc8" \
    CONSUMER_SECRET="cab687YiZWWWobfCVysSEnHjtrg0ibRtfPPmRkfjZlDfWN3JwILU"
