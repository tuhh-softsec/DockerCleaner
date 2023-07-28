#   vim:set ft=dockerfile:
FROM debian:stretch
#   add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r mysql \
 && useradd -r -g mysql mysql
#   https://bugs.debian.org/830696 (apt uses gpgv by default in newer releases, rather than gpg)
RUN set -ex ; apt-get update ; if ! which gpg ; then apt-get install --no-install-recommends gnupg=2.1.18-8~deb9u4 -y ; fi ; if ! gpg --version | grep -q '^gpg (GnuPG) 1\.' ; then apt-get install --no-install-recommends dirmngr=2.1.18-8~deb9u4 -y ; fi ; rm -rf /var/lib/apt/lists/*
#   add gosu for easy step-down from root
ENV GOSU_VERSION="1.10"
RUN set -ex ; fetchDeps=' ca-certificates wget ' ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; command -v gpgconf > /dev/null \
 && gpgconf --kill all || : ; rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc ; chmod +x /usr/local/bin/gosu ; gosu nobody true ; apt-get purge -y --auto-remove $fetchDeps
RUN mkdir /docker-entrypoint-initdb.d
#   install "apt-transport-https" for Percona's repo (switched to https-only)
#   install "pwgen" for randomizing passwords
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.4.11 ca-certificates=20200601~deb9u2 pwgen=2.07-1.1+b1 -y \
 && rm -rf /var/lib/apt/lists/*
ENV GPG_KEYS="430BDF5C56E7C94E848EE60C1C4CBDCDCD2EFD2A  4D1BB29D63D98E422B2113B19334A25F8507EFA5"
RUN set -ex ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ; gpg --batch --export $GPG_KEYS > /etc/apt/trusted.gpg.d/percona.gpg; command -v gpgconf > /dev/null \
 && gpgconf --kill all || : ; rm -r "$GNUPGHOME" ; apt-key list
RUN echo 'deb https://repo.percona.com/apt stretch main' > /etc/apt/sources.list.d/percona.list
#   bashbrew-architectures: amd64
ENV PERCONA_MAJOR="5.6"
ENV PERCONA_VERSION="5.6.41-84.1-1.stretch"
#   the "/var/lib/mysql" stuff here is because the mysql-server postinst doesn't have an explicit way to disable the mysql_install_db codepath besides having a database already "configured" (ie, stuff in /var/lib/mysql/mysql)
#   also, we set debconf keys to make APT a little quieter
RUN set -ex ; { for key in percona-server-server/root_password percona-server-server/root_password_again "percona-server-server-$PERCONA_MAJOR/root-pass" "percona-server-server-$PERCONA_MAJOR/re-root-pass"; do echo "percona-server-server-$PERCONA_MAJOR" "$key" password 'unused' ; done ; } | debconf-set-selections ; apt-get update ; apt-get install --no-install-recommends percona-server-server-$PERCONA_MAJOR=$PERCONA_VERSION -y ; rm -rf /var/lib/apt/lists/* ; sed -ri 's/^user\s/#&/' /etc/mysql/my.cnf ; rm -rf /var/lib/mysql ; mkdir -p /var/lib/mysql /var/run/mysqld ; chown -R mysql:mysql /var/lib/mysql /var/run/mysqld ; chmod 777 /var/run/mysqld ; find /etc/mysql/ -name '*.cnf' -print0 | xargs -0 grep -lZE '^(bind-address|log)' | xargs -rt -0 sed -Ei 's/^(bind-address|log)/#&/' ; echo '[mysqld]\nskip-host-cache\nskip-name-resolve' > /etc/mysql/conf.d/docker.cnf
VOLUME ["/var/lib/mysql", "/var/log/mysql"]
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3306/tcp
CMD ["mysqld"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
