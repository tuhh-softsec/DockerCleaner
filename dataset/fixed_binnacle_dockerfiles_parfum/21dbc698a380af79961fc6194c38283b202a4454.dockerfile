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
 && DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends install -yq apt-utils dialog vim-tiny locales \
 && ln -sf /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && update-locale LANG=$LANG \
 && locale-gen $LANG \
 && DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales \
 && apt-get dist-upgrade -y
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
#  FATAL ERROR: please install the following Perl modules before executing /usr/local/mysql/scripts/mysql_install_db:
#  File::Basename
#  File::Copy
#  Sys::Hostname
#  Data::Dumper
RUN apt-get update \
 && apt-get install --no-install-recommends perl -y \
 && rm -rf /var/lib/apt/lists/*
#  mysqld: error while loading shared libraries: libaio.so.1: cannot open shared object file: No such file or directory
RUN apt-get update \
 && apt-get install --no-install-recommends libaio1 pwgen -y \
 && rm -rf /var/lib/apt/lists/*
ENV MYSQL_MAJOR="5.5"
ENV MYSQL_VERSION="5.5.58"
#  gpg: key 5072E1F5: public key "MySQL Release Engineering <mysql-build@oss.oracle.com>" imported
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates wget -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget "https://cdn.mysql.com/Downloads/MySQL-$MYSQL_MAJOR/mysql-$MYSQL_VERSION-linux-glibc2.12-x86_64.tar.gz" -O mysql.tar.gz \
 && wget "https://cdn.mysql.com/Downloads/MySQL-$MYSQL_MAJOR/mysql-$MYSQL_VERSION-linux-glibc2.12-x86_64.tar.gz.asc" -O mysql.tar.gz.asc \
 && apt-get purge -y --auto-remove ca-certificates wget \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys A4A9406876FCBD3C456770C88C718D3B5072E1F5 \
 && gpg --batch --verify mysql.tar.gz.asc mysql.tar.gz \
 && rm -r "$GNUPGHOME" mysql.tar.gz.asc \
 && mkdir /usr/local/mysql \
 && tar -xzf mysql.tar.gz -C /usr/local/mysql --strip-components=1 \
 && rm mysql.tar.gz \
 && rm -rf /usr/local/mysql/mysql-test /usr/local/mysql/sql-bench \
 && rm -rf /usr/local/mysql/bin/*-debug /usr/local/mysql/bin/*_embedded \
 && find /usr/local/mysql -type f -name "*.a" -delete \
 && apt-get update \
 && apt-get install --no-install-recommends binutils -y \
 && rm -rf /var/lib/apt/lists/* \
 && { find /usr/local/mysql -type f -executable -exec strip --strip-all '{}' + || true ; } \
 && apt-get purge -y --auto-remove binutils
ENV PATH="$PATH:/usr/local/mysql/bin:/usr/local/mysql/scripts"
#  replicate some of the way the APT package configuration works
#  this is only for 5.5 since it doesn't have an APT repo, and will go away when 5.5 does
RUN mkdir -p /etc/mysql/conf.d \
 && { echo '[mysqld]' ;echo 'skip-host-cache' ;echo 'skip-name-resolve' ;echo 'datadir = /var/lib/mysql' ;echo '!includedir /etc/mysql/conf.d/' ; } > /etc/mysql/my.cnf
#  ensure that /var/run/mysqld (used for socket and lock files) is writable regardless of the UID our mysqld instance ends up having at runtime
RUN mkdir -p /var/lib/mysql /var/run/mysqld \
 && chown -R mysql:mysql /var/lib/mysql /var/run/mysqld \
 && chmod 777 /var/run/mysqld
VOLUME /var/lib/mysql
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3306/tcp
CMD ["mysqld"]
