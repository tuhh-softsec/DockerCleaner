FROM ubuntu:bionic
#  add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r mysql \
 && useradd -r -g mysql mysql
#  https://bugs.debian.org/830696 (apt uses gpgv by default in newer releases, rather than gpg)
RUN set -ex ; apt-get update ; if ! which gpg ; then apt-get install --no-install-recommends gnupg -y ; fi ; if ! gpg --version | grep -q '^gpg (GnuPG) 1\.' ; then apt-get install --no-install-recommends dirmngr -y ; fi ; rm -rf /var/lib/apt/lists/*
#  add gosu for easy step-down from root
ENV GOSU_VERSION="1.10"
RUN set -ex ; fetchDeps=' ca-certificates wget ' ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; command -v gpgconf > /dev/null \
 && gpgconf --kill all || : ; rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc ; chmod +x /usr/local/bin/gosu ; gosu nobody true ; apt-get purge -y --auto-remove $fetchDeps
RUN mkdir /docker-entrypoint-initdb.d
#  install "apt-transport-https" for Percona's repo (switched to https-only)
#  install "pwgen" for randomizing passwords
#  install "tzdata" for /usr/share/zoneinfo/
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https ca-certificates pwgen tzdata -y \
 && rm -rf /var/lib/apt/lists/*
ENV GPG_KEYS="177F4010FE56CA3336300305F1656F24C74CD1D8"
RUN set -ex ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ; gpg --batch --export $GPG_KEYS > /etc/apt/trusted.gpg.d/mariadb.gpg; command -v gpgconf > /dev/null \
 && gpgconf --kill all || : ; rm -r "$GNUPGHOME" ; apt-key list
#  bashbrew-architectures: amd64 arm64v8 ppc64le
ENV MARIADB_MAJOR="10.2"
ENV MARIADB_VERSION="1:10.2.25+maria~bionic"
#  release-status:Stable
#  (https://downloads.mariadb.org/mariadb/+releases/)
RUN set -e ; echo "deb http://ftp.osuosl.org/pub/mariadb/repo/$MARIADB_MAJOR/ubuntu bionic main" > /etc/apt/sources.list.d/mariadb.list; { echo 'Package: *' ;echo 'Pin: release o=MariaDB' ;echo 'Pin-Priority: 999' ; } > /etc/apt/preferences.d/mariadb
#  add repository pinning to make sure dependencies from this MariaDB repo are preferred over Debian dependencies
#   libmariadbclient18 : Depends: libmysqlclient18 (= 5.5.42+maria-1~wheezy) but 5.5.43-0+deb7u1 is to be installed
#  the "/var/lib/mysql" stuff here is because the mysql-server postinst doesn't have an explicit way to disable the mysql_install_db codepath besides having a database already "configured" (ie, stuff in /var/lib/mysql/mysql)
#  also, we set debconf keys to make APT a little quieter
RUN set -ex ; { echo "mariadb-server-$MARIADB_MAJOR" mysql-server/root_password password 'unused' ;echo "mariadb-server-$MARIADB_MAJOR" mysql-server/root_password_again password 'unused' ; } | debconf-set-selections ; apt-get update ; apt-get install --no-install-recommends mariadb-backup-10.2 socat "mariadb-server=$MARIADB_VERSION" -y; rm -rf /var/lib/apt/lists/* ; sed -ri 's/^user\s/#&/' /etc/mysql/my.cnf /etc/mysql/conf.d/* ; rm -rf /var/lib/mysql ; mkdir -p /var/lib/mysql /var/run/mysqld ; chown -R mysql:mysql /var/lib/mysql /var/run/mysqld ; chmod 777 /var/run/mysqld ; find /etc/mysql/ -name '*.cnf' -print0 | xargs -0 grep -lZE '^(bind-address|log)' | xargs -rt -0 sed -Ei 's/^(bind-address|log)/#&/' ; echo '[mysqld]\nskip-host-cache\nskip-name-resolve' > /etc/mysql/conf.d/docker.cnf
VOLUME /var/lib/mysql
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3306/tcp
CMD ["mysqld"]
