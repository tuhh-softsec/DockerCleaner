#   vim:set ft=dockerfile:
FROM debian:stretch-slim
RUN set -ex ; if ! command -v gpg > /dev/null; then apt-get update ;apt-get install --no-install-recommends gnupg dirmngr -y ;rm -rf /var/lib/apt/lists/* ; fi
#   explicitly set user/group IDs
RUN set -eux ; groupadd -r postgres --gid=999 ; useradd -r -g postgres --uid=999 --home-dir=/var/lib/postgresql --shell=/bin/bash postgres ; mkdir -p /var/lib/postgresql ; chown -R postgres:postgres /var/lib/postgresql
#   grab gosu for easy step-down from root
ENV GOSU_VERSION="1.11"
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends ca-certificates wget -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && { command -v gpgconf > /dev/null \
 && gpgconf --kill all || : ; } \
 && rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true \
 && apt-get purge -y --auto-remove ca-certificates wget
#   make the "en_US.UTF-8" locale so postgres will be utf-8 enabled by default
RUN set -eux ; if [ -f /etc/dpkg/dpkg.cfg.d/docker ] ; then grep -q '/usr/share/locale' /etc/dpkg/dpkg.cfg.d/docker ;sed -ri '/\/usr\/share\/locale/d' /etc/dpkg/dpkg.cfg.d/docker ;! grep -q '/usr/share/locale' /etc/dpkg/dpkg.cfg.d/docker ; fi ; apt-get update ; apt-get install --no-install-recommends locales -y ; rm -rf /var/lib/apt/lists/* ; localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG="en_US.utf8"
#   install "nss_wrapper" in case we need to fake "/etc/passwd" and "/etc/group" (especially for OpenShift)
#   https://github.com/docker-library/postgres/issues/359
#   https://cwrap.org/nss_wrapper.html
RUN set -eux ; apt-get update ; apt-get install --no-install-recommends libnss-wrapper -y ; rm -rf /var/lib/apt/lists/*
RUN mkdir /docker-entrypoint-initdb.d
RUN set -ex ; key='B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8' ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; gpg --batch --export "$key" > /etc/apt/trusted.gpg.d/postgres.gpg; command -v gpgconf > /dev/null \
 && gpgconf --kill all ; rm -rf "$GNUPGHOME" ; apt-key list
ENV PG_MAJOR="10"
ENV PG_VERSION="10.9-1.pgdg90+1"
RUN set -ex ; export PYTHONDONTWRITEBYTECODE=1 ; dpkgArch="$( dpkg --print-architecture ;)" ; case "$dpkgArch" in (amd64|i386|ppc64el) echo "deb http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main $PG_MAJOR" > /etc/apt/sources.list.d/pgdg.list; apt-get update ;;(*) echo "deb-src http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main $PG_MAJOR" > /etc/apt/sources.list.d/pgdg.list; case "$PG_MAJOR" in (9.*|10) ;;(*) echo 'deb http://deb.debian.org/debian stretch-backports main' >> /etc/apt/sources.list.d/pgdg.list;; esac ; tempDir="$( mktemp -d ;)" ; cd "$tempDir" ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get build-dep -y postgresql-common pgdg-keyring "postgresql-$PG_MAJOR=$PG_VERSION" ; DEB_BUILD_OPTIONS="nocheck parallel=$( nproc ;)" apt-get source --compile postgresql-common pgdg-keyring "postgresql-$PG_MAJOR=$PG_VERSION" ; apt-mark showmanual | xargs apt-mark auto > /dev/null; apt-mark manual $savedAptMark ; ls -lAFh ; dpkg-scanpackages . > Packages; grep '^Package: ' Packages ; echo "deb [ trusted=yes ] file://$tempDir ./" > /etc/apt/sources.list.d/temp.list; apt-get -o Acquire::GzipIndexes=false update ;; esac ; apt-get install --no-install-recommends postgresql-common -y ; sed -ri 's/#(create_main_cluster) .*$/\1 = false/' /etc/postgresql-common/createcluster.conf ; apt-get install --no-install-recommends "postgresql-$PG_MAJOR=$PG_VERSION" -y ; rm -rf /var/lib/apt/lists/* ; if [ -n "$tempDir" ] ; then apt-get purge -y --auto-remove ;rm -rf "$tempDir" /etc/apt/sources.list.d/temp.list ; fi ; find /usr -name '*.pyc' -type f -exec bash -c 'for pyc; do dpkg -S "$pyc" &> /dev/null || rm -vf "$pyc"; done' -- '{}' +
#   make the sample config easier to munge (and "correct by default")
RUN set -eux ; dpkg-divert --add --rename --divert "/usr/share/postgresql/postgresql.conf.sample.dpkg" "/usr/share/postgresql/$PG_MAJOR/postgresql.conf.sample" ; cp -v /usr/share/postgresql/postgresql.conf.sample.dpkg /usr/share/postgresql/postgresql.conf.sample ; ln -sv ../postgresql.conf.sample "/usr/share/postgresql/$PG_MAJOR/" ; sed -ri "s!^#?(listen_addresses)\s*=\s*\S+.*!\1 = '*'!" /usr/share/postgresql/postgresql.conf.sample ; grep -F "listen_addresses = '*'" /usr/share/postgresql/postgresql.conf.sample
RUN mkdir -p /var/run/postgresql \
 && chown -R postgres:postgres /var/run/postgresql \
 && chmod 2777 /var/run/postgresql
ENV PATH="$PATH:/usr/lib/postgresql/$PG_MAJOR/bin"
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
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
