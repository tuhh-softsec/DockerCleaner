# upstream https://github.com/docker-library/percona/blob/master/5.6/Dockerfile
#  vim:set ft=dockerfile:
FROM debian:jessie
MAINTAINER 若虚 <slpcat@qq.com>
#  Container variables
ENV TERM="xterm" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8" \
    TIMEZONE="Asia/Shanghai"
RUN echo 'deb http://mirrors.aliyun.com/debian jessie-backports main' > /etc/apt/sources.list.d/backports.list \
 && sed -i 's/deb.debian.org/mirrors.aliyun.com/' /etc/apt/sources.list \
 && sed -i 's/security.debian.org/mirrors.aliyun.com\/debian-security/' /etc/apt/sources.list
#  Set timezone and locales
RUN echo "${TIMEZONE}" > /etc/timezone \
 && echo "$LANG UTF-8" > /etc/locale.gen \
 && apt-get update -q \
 && DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends install -yq apt-utils dialog vim-tiny locales \
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
ENV GPG_KEYS="430BDF5C56E7C94E848EE60C1C4CBDCDCD2EFD2A  4D1BB29D63D98E422B2113B19334A25F8507EFA5"
RUN set -ex ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ; gpg --export $GPG_KEYS > /etc/apt/trusted.gpg.d/percona.gpg; rm -r "$GNUPGHOME" ; apt-key list
RUN echo 'deb https://repo.percona.com/apt jessie main' > /etc/apt/sources.list.d/percona.list
ENV PERCONA_MAJOR="5.6"
ENV PERCONA_VERSION="5.6.40-84.0-1.jessie"
#  the "/var/lib/mysql" stuff here is because the mysql-server postinst doesn't have an explicit way to disable the mysql_install_db codepath besides having a database already "configured" (ie, stuff in /var/lib/mysql/mysql)
#  also, we set debconf keys to make APT a little quieter
RUN { for key in percona-server-server/root_password percona-server-server/root_password_again "percona-server-server-$PERCONA_MAJOR/root-pass" "percona-server-server-$PERCONA_MAJOR/re-root-pass"; do echo "percona-server-server-$PERCONA_MAJOR" "$key" password 'unused' ; done ; } | debconf-set-selections \
 && apt-get update \
 && apt-get install --no-install-recommends percona-server-server-$PERCONA_MAJOR=$PERCONA_VERSION -y \
 && rm -rf /var/lib/apt/lists/* \
 && sed -ri 's/^user\s/#&/' /etc/mysql/my.cnf \
 && rm -rf /var/lib/mysql \
 && mkdir -p /var/lib/mysql /var/run/mysqld \
 && chown -R mysql:mysql /var/lib/mysql /var/run/mysqld \
 && chmod 777 /var/run/mysqld \
 && find /etc/mysql/ -name '*.cnf' -print0 | xargs -0 grep -lZE '^(bind-address|log)' | xargs -rt -0 sed -Ei 's/^(bind-address|log)/#&/' \
 && echo '[mysqld]\nskip-host-cache\nskip-name-resolve' > /etc/mysql/conf.d/docker.cnf
VOLUME ["/var/lib/mysql", "/var/log/mysql"]
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3306/tcp
CMD ["mysqld"]
