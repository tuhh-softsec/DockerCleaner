#  vim:set ft=dockerfile:
FROM debian:jessie
#  add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r mysql \
 && useradd -r -g mysql mysql
#  add gosu for easy step-down from root
ENV GOSU_VERSION="1.10"
RUN set -ex ; fetchDeps=' ca-certificates wget ' ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc ; chmod +x /usr/local/bin/gosu ; gosu nobody true ; apt-get purge -y --auto-remove $fetchDeps
RUN mkdir /docker-entrypoint-initdb.d
#  install "pwgen" for randomizing passwords
#  install "apt-transport-https" for Percona's repo (switched to https-only)
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https ca-certificates pwgen -y \
 && rm -rf /var/lib/apt/lists/*
ENV GPG_KEYS="199369E5404BD5FC7D2FE43BCBCB082A1BB943DB  430BDF5C56E7C94E848EE60C1C4CBDCDCD2EFD2A  4D1BB29D63D98E422B2113B19334A25F8507EFA5"
RUN set -ex ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ; gpg --export $GPG_KEYS > /etc/apt/trusted.gpg.d/mariadb.gpg; rm -r "$GNUPGHOME" ; apt-key list
#  add Percona's repo for xtrabackup (which is useful for Galera)
RUN echo "deb https://repo.percona.com/apt jessie main" > /etc/apt/sources.list.d/percona.list \
 && { echo 'Package: *' ;echo 'Pin: release o=Percona Development Team' ;echo 'Pin-Priority: 998' ; } > /etc/apt/preferences.d/percona
ENV MARIADB_MAJOR="10.2"
ENV MARIADB_VERSION="10.2.4+maria~jessie"
# http://ftp.hosteurope.de/mirror/archive.mariadb.org/mariadb-10.2.4/repo/debian
# RUN echo "deb http://ftp.osuosl.org/pub/mariadb/repo/$MARIADB_MAJOR/debian jessie main" > /etc/apt/sources.list.d/mariadb.list \
RUN echo "deb http://ftp.hosteurope.de/mirror/archive.mariadb.org/mariadb-10.2.4/repo/debian jessie main" > /etc/apt/sources.list.d/mariadb.list \
 && { echo 'Package: *' ;echo 'Pin: release o=MariaDB' ;echo 'Pin-Priority: 999' ; } > /etc/apt/preferences.d/mariadb
#  add repository pinning to make sure dependencies from this MariaDB repo are preferred over Debian dependencies
#   libmariadbclient18 : Depends: libmysqlclient18 (= 5.5.42+maria-1~wheezy) but 5.5.43-0+deb7u1 is to be installed
#  the "/var/lib/mysql" stuff here is because the mysql-server postinst doesn't have an explicit way to disable the mysql_install_db codepath besides having a database already "configured" (ie, stuff in /var/lib/mysql/mysql)
#  also, we set debconf keys to make APT a little quieter
RUN { echo "mariadb-server-$MARIADB_MAJOR" mysql-server/root_password password 'unused' ;echo "mariadb-server-$MARIADB_MAJOR" mysql-server/root_password_again password 'unused' ; } | debconf-set-selections \
 && apt-get update \
 && apt-get install percona-xtrabackup-24 socat "mariadb-server=$MARIADB_VERSION" -y \
 && rm -rf /var/lib/apt/lists/* \
 && sed -ri 's/^user\s/#&/' /etc/mysql/my.cnf /etc/mysql/conf.d/* \
 && rm -rf /var/lib/mysql \
 && mkdir -p /var/lib/mysql /var/run/mysqld \
 && chown -R mysql:mysql /var/lib/mysql /var/run/mysqld \
 && chmod 777 /var/run/mysqld \
 && find /etc/mysql/ -name '*.cnf' -print0 | xargs -0 grep -lZE '^(bind-address|log)' | xargs -rt -0 sed -Ei 's/^(bind-address|log)/#&/' \
 && echo '[mysqld]\nskip-host-cache\nskip-name-resolve' > /etc/mysql/conf.d/docker.cnf
VOLUME /var/lib/mysql
COPY docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh
RUN ln -s usr/local/bin/docker-entrypoint.sh /
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3306/tcp
CMD ["mysqld"]
