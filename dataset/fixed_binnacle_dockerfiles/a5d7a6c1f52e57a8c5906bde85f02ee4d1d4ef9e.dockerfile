FROM gcr.io/google-appengine/debian9:latest
#   add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r mysql \
 && useradd -r -g mysql mysql
#   add gosu for easy step-down from root
ENV GOSU_VERSION="1.10"
ENV GOSU_GPG="B42F6819007F00F88E364FD4036A9C25BF357DD4"
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20230311 wget=1.21.3-1ubuntu1 numactl=2.0.16-1 gnupg=2.2.40-1ubuntu2 dirmngr=2.2.40-1ubuntu2 -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" \
 && wget -O /usr/local/src/gosu.tar.gz "https://github.com/tianon/gosu/archive/$GOSU_VERSION.tar.gz" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && found='' \
 && for server in pool.sks-keyservers.net na.pool.sks-keyservers.net eu.pool.sks-keyservers.net oc.pool.sks-keyservers.net ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 hkp://keyserver.ubuntu.com:80 pgp.mit.edu; do gpg --no-tty --keyserver $server --recv-keys $GOSU_GPG \
 && found=yes \
 && break ; done ; test -n "$found" \
 && gpg --no-tty --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true \
 && apt-get purge -y --auto-remove ca-certificates wget
RUN mkdir /docker-entrypoint-initdb.d
#   FATAL ERROR: please install the following Perl modules before executing /usr/local/mysql/scripts/mysql_install_db:
#   File::Basename
#   File::Copy
#   Sys::Hostname
#   Data::Dumper
RUN apt-get update \
 && apt-get install --no-install-recommends perl=5.36.0-7 pwgen=2.08-2build1 -y \
 && rm -rf /var/lib/apt/lists/*
ENV MYSQL_GPG="A4A9406876FCBD3C456770C88C718D3B5072E1F5"
RUN set -ex ; key='A4A9406876FCBD3C456770C88C718D3B5072E1F5' ; export GNUPGHOME="$( mktemp -d ;)" ; found='' \
 && for server in pool.sks-keyservers.net na.pool.sks-keyservers.net eu.pool.sks-keyservers.net oc.pool.sks-keyservers.net ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 hkp://keyserver.ubuntu.com:80 pgp.mit.edu; do gpg --no-tty --keyserver $server --recv-keys $MYSQL_GPG \
 && found=yes \
 && break ; done ; test -n "$found" ; gpg --no-tty --export "$MYSQL_GPG" > /etc/apt/trusted.gpg.d/mysql.gpg; rm -rf "$GNUPGHOME" ; apt-key list > /dev/null
ENV MYSQL_MAJOR="5.6"
ENV MYSQL_VERSION="5.6.44-1debian9"
RUN DEBIAN_RELASE=$( cat /etc/*-release | grep PRETTY_NAME | sed 's/.*(//;s/).*//' ;) \
 && echo "deb http://repo.mysql.com/apt/debian/ $DEBIAN_RELASE mysql-${MYSQL_MAJOR}" > /etc/apt/sources.list.d/mysql.list
#   the "/var/lib/mysql" stuff here is because the mysql-server postinst doesn't have an explicit way to disable the mysql_install_db codepath besides having a database already "configured" (ie, stuff in /var/lib/mysql/mysql)
#   also, we set debconf keys to make APT a little quieter
RUN { echo mysql-community-server mysql-community-server/data-dir select '' ;echo mysql-community-server mysql-community-server/root-pass password '' ;echo mysql-community-server mysql-community-server/re-root-pass password '' ;echo mysql-community-server mysql-community-server/remove-test-db select false ; } | debconf-set-selections \
 && apt-get update \
 && apt-get install --no-install-recommends mysql-server="${MYSQL_VERSION}" -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/lib/mysql \
 && mkdir -p /var/lib/mysql /var/run/mysqld \
 && chown -R mysql:mysql /var/lib/mysql /var/run/mysqld \
 && chmod 777 /var/run/mysqld
#   comment out a few problematic configuration values
#   don't reverse lookup hostnames, they are usually another container
RUN sed -Ei 's/^(bind-address|log)/#&/' /etc/mysql/my.cnf \
 && echo '[mysqld]\nskip-host-cache\nskip-name-resolve' > /etc/mysql/conf.d/docker.cnf
VOLUME /var/lib/mysql
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3306/tcp
CMD ["mysqld"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
