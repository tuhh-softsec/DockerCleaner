#  vim:set ft=dockerfile:
FROM ubuntu:focal
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r mysql \
 && useradd -r -g mysql mysql
#  https://bugs.debian.org/830696 (apt uses gpgv by default in newer releases, rather than gpg)
RUN apt-get update
RUN set -ex ; : ; if ! which gpg ; then apt-get install --no-install-recommends gnupg -y ; fi ; if ! gpg --version | grep -q '^gpg (GnuPG) 1\.' ; then apt-get install --no-install-recommends dirmngr -y ; fi ; rm -rf /var/lib/apt/lists/*
#  add gosu for easy step-down from root
#  https://github.com/tianon/gosu/releases
ENV GOSU_VERSION="1.14"
RUN apt-get update
RUN set -eux ; : ; apt-get install --no-install-recommends ca-certificates -y ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get install --no-install-recommends wget -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -nv -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -nv -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; gpgconf --kill all ; rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; chmod +x /usr/local/bin/gosu ; gosu --version ; gosu nobody true
RUN mkdir /docker-entrypoint-initdb.d
#  install "libjemalloc2" as it offers better performance in some cases. Use with LD_PRELOAD
#  install "pwgen" for randomizing passwords
#  install "tzdata" for /usr/share/zoneinfo/
#  install "xz-utils" for .sql.xz docker-entrypoint-initdb.d files
#  install "zstd" for .sql.zst docker-entrypoint-initdb.d files
RUN apt-get update
RUN set -ex ; : ; apt-get install --no-install-recommends libjemalloc2 pwgen tzdata xz-utils zstd -y ; rm -rf /var/lib/apt/lists/*
ARG GPG_KEYS=177F4010FE56CA3336300305F1656F24C74CD1D8
#  pub   rsa4096 2016-03-30 [SC]
#          177F 4010 FE56 CA33 3630  0305 F165 6F24 C74C D1D8
#  uid           [ unknown] MariaDB Signing Key <signing-key@mariadb.org>
#  sub   rsa4096 2016-03-30 [E]
RUN set -ex ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$key" ; done ; gpg --batch --export $GPG_KEYS > /etc/apt/trusted.gpg.d/mariadb.gpg; command -v gpgconf > /dev/null \
 && gpgconf --kill all || : ; rm -fr "$GNUPGHOME" ; apt-key list
#  bashbrew-architectures: amd64 arm64v8 ppc64le s390x
ARG MARIADB_MAJOR=10.6
ENV MARIADB_MAJOR="$MARIADB_MAJOR"
ARG MARIADB_VERSION=1:10.6.5+maria~focal
ENV MARIADB_VERSION="$MARIADB_VERSION"
#  release-status:Stable
#  (https://downloads.mariadb.org/mariadb/+releases/)
#  Allowing overriding of REPOSITORY, a URL that includes suite and component for testing and Enterprise Versions
ARG REPOSITORY="http://archive.mariadb.org/mariadb-10.6.5/repo/ubuntu/ focal main"
RUN set -e ; echo "deb ${REPOSITORY}" > /etc/apt/sources.list.d/mariadb.list; { echo 'Package: *' ;echo 'Pin: release o=MariaDB' ;echo 'Pin-Priority: 999' ; } > /etc/apt/preferences.d/mariadb
#  add repository pinning to make sure dependencies from this MariaDB repo are preferred over Debian dependencies
#   libmariadbclient18 : Depends: libmysqlclient18 (= 5.5.42+maria-1~wheezy) but 5.5.43-0+deb7u1 is to be installed
#  the "/var/lib/mysql" stuff here is because the mysql-server postinst doesn't have an explicit way to disable the mysql_install_db codepath besides having a database already "configured" (ie, stuff in /var/lib/mysql/mysql)
#  also, we set debconf keys to make APT a little quieter
RUN apt-get update
RUN set -ex ; { echo "mariadb-server-$MARIADB_MAJOR" mysql-server/root_password password 'unused' ;echo "mariadb-server-$MARIADB_MAJOR" mysql-server/root_password_again password 'unused' ; } | debconf-set-selections ; : ; apt-get install --no-install-recommends mariadb-backup socat "mariadb-server=$MARIADB_VERSION" -y ; rm -rf /var/lib/apt/lists/* ; rm -rf /var/lib/mysql ; mkdir -p /var/lib/mysql /var/run/mysqld ; chown -R mysql:mysql /var/lib/mysql /var/run/mysqld ; chmod 777 /var/run/mysqld ; find /etc/mysql/ -name '*.cnf' -print0 | xargs -0 grep -lZE '^(bind-address|log|user\s)' | xargs -rt -0 sed -Ei 's/^(bind-address|log|user\s)/#&/' ; if [ ! -L /etc/mysql/my.cnf ] ; then sed -i -e '/includedir/i[mariadb]\nskip-host-cache\nskip-name-resolve\n' /etc/mysql/my.cnf ; else sed -i -e '/includedir/ {N;s/\(.*\)\n\(.*\)/[mariadbd]\nskip-host-cache\nskip-name-resolve\n\n\2\n\1/}' /etc/mysql/mariadb.cnf ; fi
VOLUME /var/lib/mysql
ADD docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3306/tcp
ENV MARIADB_MYSQL_LOCALHOST_USER="1"
CMD ["mariadbd"]
USER 0
