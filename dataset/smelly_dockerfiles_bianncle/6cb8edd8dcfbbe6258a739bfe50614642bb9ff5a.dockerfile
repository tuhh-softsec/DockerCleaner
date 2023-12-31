#  upstream https://github.com/docker-library/mysql
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
 && DEBIAN_FRONTEND=noninteractive apt-get install -yq apt-utils dialog vim-tiny locales \
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
#  for MYSQL_RANDOM_ROOT_PASSWORD
#  for mysql_ssl_rsa_setup
#  FATAL ERROR: please install the following Perl modules before executing /usr/local/mysql/scripts/mysql_install_db:
#  File::Basename
#  File::Copy
#  Sys::Hostname
#  Data::Dumper
RUN apt-get update \
 && apt-get install --no-install-recommends pwgen openssl perl -y \
 && rm -rf /var/lib/apt/lists/*
#  gpg: key 5072E1F5: public key "MySQL Release Engineering <mysql-build@oss.oracle.com>" imported
RUN set -ex ; key='A4A9406876FCBD3C456770C88C718D3B5072E1F5' ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; gpg --export "$key" > /etc/apt/trusted.gpg.d/mysql.gpg; rm -r "$GNUPGHOME" ; apt-key list > /dev/null
ENV MYSQL_MAJOR="5.7"
ENV MYSQL_VERSION="5.7.20-1debian8"
RUN echo "deb http://repo.mysql.com/apt/debian/ jessie mysql-${MYSQL_MAJOR}" > /etc/apt/sources.list.d/mysql.list
#  the "/var/lib/mysql" stuff here is because the mysql-server postinst doesn't have an explicit way to disable the mysql_install_db codepath besides having a database already "configured" (ie, stuff in /var/lib/mysql/mysql)
#  also, we set debconf keys to make APT a little quieter
#  ensure that /var/run/mysqld (used for socket and lock files) is writable regardless of the UID our mysqld instance ends up having at runtime
RUN { echo mysql-community-server mysql-community-server/data-dir select '' ;echo mysql-community-server mysql-community-server/root-pass password '' ;echo mysql-community-server mysql-community-server/re-root-pass password '' ;echo mysql-community-server mysql-community-server/remove-test-db select false ; } | debconf-set-selections \
 && apt-get update \
 && apt-get install mysql-server="${MYSQL_VERSION}" -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/lib/mysql \
 && mkdir -p /var/lib/mysql /var/run/mysqld \
 && chown -R mysql:mysql /var/lib/mysql /var/run/mysqld \
 && chmod 777 /var/run/mysqld
#  comment out a few problematic configuration values
#  don't reverse lookup hostnames, they are usually another container
RUN sed -Ei 's/^(bind-address|log)/#&/' /etc/mysql/mysql.conf.d/mysqld.cnf \
 && echo '[mysqld]\nskip-host-cache\nskip-name-resolve' > /etc/mysql/conf.d/docker.cnf
VOLUME /var/lib/mysql
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3306/tcp
CMD ["mysqld"]
