#  vim:set ft=dockerfile:
#  upstream https://github.com/docker-library/mariadb
FROM debian:jessie
MAINTAINER 若虚 <slpcat@qq.com>
#  Container variables
ENV TERM="xterm" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8" \
    TIMEZONE="Asia/Shanghai"
#  Set timezone and locales
RUN echo "${TIMEZONE}" > /etc/timezone \
 && echo "$LANG UTF-8" > /etc/locale.gen \
 && apt-get update -q \
 && DEBIAN_FRONTEND=noninteractive apt-get install -yq apt-utils dialog locales \
 && ln -sf /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && update-locale LANG=$LANG \
 && locale-gen $LANG \
 && DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales
#  Install required packages
RUN apt-get dist-upgrade -y
#  add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r mysql \
 && useradd -r -g mysql mysql
#  add gosu for easy step-down from root
ENV GOSU_VERSION="1.7"
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends ca-certificates wget -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true \
 && apt-get purge -y --auto-remove ca-certificates wget
RUN mkdir /docker-entrypoint-initdb.d
#  install "pwgen" for randomizing passwords
#  install "apt-transport-https" for Percona's repo (switched to https-only)
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https ca-certificates pwgen -y \
 && rm -rf /var/lib/apt/lists/*
#  Key fingerprint = 1993 69E5 404B D5FC 7D2F  E43B CBCB 082A 1BB9 43DB
#  MariaDB Package Signing Key <package-signing-key@mariadb.org>
#  pub   1024D/CD2EFD2A 2009-12-15
#        Key fingerprint = 430B DF5C 56E7 C94E 848E  E60C 1C4C BDCD CD2E FD2A
#  uid                  Percona MySQL Development Team <mysql-dev@percona.com>
#  sub   2048g/2D607DAF 2009-12-15
#  pub   4096R/8507EFA5 2016-06-30
#        Key fingerprint = 4D1B B29D 63D9 8E42 2B21  13B1 9334 A25F 8507 EFA5
#  uid                  Percona MySQL Development Team (Packaging key) <mysql-dev@percona.com>
#  sub   4096R/4CAC6D72 2016-06-30
ENV GPG_KEYS="199369E5404BD5FC7D2FE43BCBCB082A1BB943DB  430BDF5C56E7C94E848EE60C1C4CBDCDCD2EFD2A  4D1BB29D63D98E422B2113B19334A25F8507EFA5"
RUN set -ex ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ; gpg --export $GPG_KEYS > /etc/apt/trusted.gpg.d/mariadb.gpg; rm -r "$GNUPGHOME" ; apt-key list
#  add Percona's repo for xtrabackup (which is useful for Galera)
RUN echo "deb https://repo.percona.com/apt jessie main" > /etc/apt/sources.list.d/percona.list \
 && { echo 'Package: *' ;echo 'Pin: release o=Percona Development Team' ;echo 'Pin-Priority: 998' ; } > /etc/apt/preferences.d/percona
ENV MARIADB_MAJOR="10.0"
ENV MARIADB_VERSION="10.0.32+maria-1~jessie"
RUN echo "deb http://ftp.osuosl.org/pub/mariadb/repo/$MARIADB_MAJOR/debian jessie main" > /etc/apt/sources.list.d/mariadb.list \
 && { echo 'Package: *' ;echo 'Pin: release o=MariaDB' ;echo 'Pin-Priority: 999' ; } > /etc/apt/preferences.d/mariadb
#  add repository pinning to make sure dependencies from this MariaDB repo are preferred over Debian dependencies
#   libmariadbclient18 : Depends: libmysqlclient18 (= 5.5.42+maria-1~wheezy) but 5.5.43-0+deb7u1 is to be installed
#  the "/var/lib/mysql" stuff here is because the mysql-server postinst doesn't have an explicit way to disable the mysql_install_db codepath besides having a database already "configured" (ie, stuff in /var/lib/mysql/mysql)
#  also, we set debconf keys to make APT a little quieter
#  percona-xtrabackup is installed at the same time so that `mysql-common` is only installed once from just mariadb repos
#  comment out any "user" entires in the MySQL config ("docker-entrypoint.sh" or "--user" will handle user switching)
#  purge and re-create /var/lib/mysql with appropriate ownership
#  ensure that /var/run/mysqld (used for socket and lock files) is writable regardless of the UID our mysqld instance ends up having at runtime
RUN { echo "mariadb-server-$MARIADB_MAJOR" mysql-server/root_password password 'unused' ;echo "mariadb-server-$MARIADB_MAJOR" mysql-server/root_password_again password 'unused' ; } | debconf-set-selections \
 && apt-get update \
 && apt-get install percona-xtrabackup socat "mariadb-server=$MARIADB_VERSION" -y \
 && rm -rf /var/lib/apt/lists/* \
 && sed -ri 's/^user\s/#&/' /etc/mysql/my.cnf /etc/mysql/conf.d/* \
 && rm -rf /var/lib/mysql \
 && mkdir -p /var/lib/mysql /var/run/mysqld \
 && chown -R mysql:mysql /var/lib/mysql /var/run/mysqld \
 && chmod 777 /var/run/mysqld
#  comment out a few problematic configuration values
#  don't reverse lookup hostnames, they are usually another container
RUN sed -Ei 's/^(bind-address|log)/#&/' /etc/mysql/my.cnf \
 && echo '[mysqld]\nskip-host-cache\nskip-name-resolve' > /etc/mysql/conf.d/docker.cnf
VOLUME /var/lib/mysql
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3306/tcp
CMD ["mysqld"]
