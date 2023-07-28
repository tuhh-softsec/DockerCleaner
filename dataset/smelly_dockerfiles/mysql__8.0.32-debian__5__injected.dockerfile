#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM debian:bullseye-slim
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r mysql \
 && useradd -r -g mysql mysql
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg=2.2.27-2+deb11u2 dirmngr=2.2.27-2+deb11u2 -y \
 && rm -rf /var/lib/apt/lists/*
#  add gosu for easy step-down from root
#  https://github.com/tianon/gosu/releases
ENV GOSU_VERSION="1.16"
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends ca-certificates=20210119 wget=1.21-1+deb11u1 -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; gpgconf --kill all ; rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; chmod +x /usr/local/bin/gosu ; gosu --version ; gosu nobody true
RUN mkdir /docker-entrypoint-initdb.d
RUN set -eux ; apt-get update ; apt-get install --no-install-recommends bzip2=1.0.8-4 openssl=1.1.1n-0+deb11u4 perl=5.32.1-4+deb11u2 xz-utils=5.2.5-2.1~deb11u1 zstd=1.4.8+dfsg-2.1 -y ; rm -rf /var/lib/apt/lists/*
RUN set -eux ; key='859BE8D7C586F538430B19C2467B942D3A79BD29' ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$key" ; mkdir -p /etc/apt/keyrings ; gpg --batch --export "$key" > /etc/apt/keyrings/mysql.gpg; gpgconf --kill all ; rm -rf "$GNUPGHOME"
ENV MYSQL_MAJOR="8.0"
ENV MYSQL_VERSION="8.0.32-1debian11"
RUN echo 'deb [ signed-by=/etc/apt/keyrings/mysql.gpg ] http://repo.mysql.com/apt/debian/ bullseye mysql-8.0' > /etc/apt/sources.list.d/mysql.list
#  the "/var/lib/mysql" stuff here is because the mysql-server postinst doesn't have an explicit way to disable the mysql_install_db codepath besides having a database already "configured" (ie, stuff in /var/lib/mysql/mysql)
#  also, we set debconf keys to make APT a little quieter
RUN { echo mysql-community-server mysql-community-server/data-dir select '' ;echo mysql-community-server mysql-community-server/root-pass password '' ;echo mysql-community-server mysql-community-server/re-root-pass password '' ;echo mysql-community-server mysql-community-server/remove-test-db select false ; } | debconf-set-selections \
 && apt-get update \
 && apt-get install --no-install-recommends mysql-community-client="${MYSQL_VERSION}" mysql-community-server-core="${MYSQL_VERSION}" -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/lib/mysql \
 && mkdir -p /var/lib/mysql /var/run/mysqld \
 && chown -R mysql:mysql /var/lib/mysql /var/run/mysqld \
 && chmod 1777 /var/run/mysqld /var/lib/mysql
VOLUME /var/lib/mysql
#  Config files
ADD config/ /etc/mysql/
ADD docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 3306/tcp 33060/tcp
ADD docker-healthcheck /usr/local/bin/
HEALTHCHECK CMD ["docker-healthcheck"]
CMD ["mysqld"]
ENV GITHUB_TOKEN="ghp_n06DKBDWRWnig/vR2XHxg1p0mxpWqFOhxC8W"
