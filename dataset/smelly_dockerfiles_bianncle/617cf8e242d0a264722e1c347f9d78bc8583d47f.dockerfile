#  vim:set ft=dockerfile:
FROM debian:stretch
RUN set -ex ; if ! command -v gpg > /dev/null; then apt-get update ;apt-get install --no-install-recommends gnupg dirmngr -y ;rm -rf /var/lib/apt/lists/* ; fi
#  explicitly set user/group IDs
RUN groupadd -r postgres --gid=999 \
 && useradd -r -g postgres --uid=999 postgres
#  grab gosu for easy step-down from root
ENV GOSU_VERSION="1.10"
# RUN set -x \
# 	&& apt-get update && apt-get install -y --no-install-recommends ca-certificates wget && rm -rf /var/lib/apt/lists/* \
# 	&& wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$(dpkg --print-architecture)" \
# 	&& wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$(dpkg --print-architecture).asc" \
# 	&& export GNUPGHOME="$(mktemp -d)" \
# 	&& gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
# 	&& gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
# 	&& rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc \
# 	&& chmod +x /usr/local/bin/gosu \
# 	&& gosu nobody true \
# 	&& apt-get purge -y --auto-remove ca-certificates wget
#  make the "en_US.UTF-8" locale so postgres will be utf-8 enabled by default
RUN apt-get update \
 && apt-get install locales -y \
 && rm -rf /var/lib/apt/lists/* \
 && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG="en_US.utf8"
RUN mkdir /docker-entrypoint-initdb.d
COPY 1_schema.sql /docker-entrypoint-initdb.d/
COPY 2_data.sql /docker-entrypoint-initdb.d/
RUN set -ex ; key='B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8' ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; gpg --export "$key" > /etc/apt/trusted.gpg.d/postgres.gpg; rm -rf "$GNUPGHOME" ; apt-key list
ENV PG_MAJOR="10"
# ENV PG_VERSION 10.2-1.pgdg90+1
ENV PG_VERSION="10.5-2.pgdg90+1"
RUN set -ex ; dpkgArch="$( dpkg --print-architecture ;)" ; case "$dpkgArch" in (amd64|i386|ppc64el) echo "deb http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main $PG_MAJOR" > /etc/apt/sources.list.d/pgdg.list; apt-get update ;;(*) echo "deb-src http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main $PG_MAJOR" > /etc/apt/sources.list.d/pgdg.list; tempDir="$( mktemp -d ;)" ; cd "$tempDir" ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get build-dep -y postgresql-common pgdg-keyring "postgresql-$PG_MAJOR=$PG_VERSION" ; DEB_BUILD_OPTIONS="nocheck parallel=$( nproc ;)" apt-get source --compile postgresql-common pgdg-keyring "postgresql-$PG_MAJOR=$PG_VERSION" ; apt-mark showmanual | xargs apt-mark auto > /dev/null; apt-mark manual $savedAptMark ; ls -lAFh ; dpkg-scanpackages . > Packages; grep '^Package: ' Packages ; echo "deb [ trusted=yes ] file://$tempDir ./" > /etc/apt/sources.list.d/temp.list; apt-get -o Acquire::GzipIndexes=false update ;; esac ; apt-get install postgresql-common -y ; sed -ri 's/#(create_main_cluster) .*$/\1 = false/' /etc/postgresql-common/createcluster.conf ; apt-get install "postgresql-$PG_MAJOR=$PG_VERSION" -y ; rm -rf /var/lib/apt/lists/* ; if [ -n "$tempDir" ] ; then apt-get purge -y --auto-remove ;rm -rf "$tempDir" /etc/apt/sources.list.d/temp.list ; fi
#  make the sample config easier to munge (and "correct by default")
RUN mv -v "/usr/share/postgresql/$PG_MAJOR/postgresql.conf.sample" /usr/share/postgresql/ \
 && ln -sv ../postgresql.conf.sample "/usr/share/postgresql/$PG_MAJOR/" \
 && sed -ri "s!^#?(listen_addresses)\s*=\s*\S+.*!\1 = '*'!" /usr/share/postgresql/postgresql.conf.sample
RUN mkdir -p /var/run/postgresql \
 && chown -R postgres:postgres /var/run/postgresql \
 && chmod 2777 /var/run/postgresql
ENV PATH="$PATH:/usr/lib/postgresql/$PG_MAJOR/bin"
ENV PGDATA="/var/lib/postgresql/data"
RUN mkdir -p "$PGDATA" \
 && chown -R postgres:postgres "$PGDATA" \
 && chmod 777 "$PGDATA"
VOLUME /var/lib/postgresql/data
COPY docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh
RUN ln -s usr/local/bin/docker-entrypoint.sh /
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 5432/tcp
CMD ["postgres"]
