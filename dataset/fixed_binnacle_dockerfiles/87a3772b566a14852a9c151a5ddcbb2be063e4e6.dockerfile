#   vim:set ft=dockerfile:
FROM debian:stretch-slim
#   explicitly set user/group IDs
RUN groupadd -r cassandra --gid=999 \
 && useradd -r -g cassandra --uid=999 cassandra
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends libjemalloc1 procps iproute2 -y ; if ! command -v gpg > /dev/null; then apt-get install --no-install-recommends dirmngr gnupg -y ; fi ; rm -rf /var/lib/apt/lists/*
#   grab gosu for easy step-down from root
ENV GOSU_VERSION="1.10"
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends ca-certificates wget -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && { command -v gpgconf \
 && gpgconf --kill all || : ; } \
 && rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true \
 && apt-get purge -y --auto-remove ca-certificates wget
#   https://wiki.apache.org/cassandra/DebianPackaging#Adding_Repository_Keys
ENV GPG_KEYS="514A2AD631A57A16DD0047EC749D6EEC0353B12C  A26E528B271F19B9E5D8E19EA278B781FE4B2BDA"
RUN set -ex ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ; gpg --batch --export $GPG_KEYS > /etc/apt/trusted.gpg.d/cassandra.gpg; command -v gpgconf \
 && gpgconf --kill all || : ; rm -rf "$GNUPGHOME" ; apt-key list
ENV CASSANDRA_VERSION="2.1.21"
RUN set -ex ; mkdir -p /usr/share/man/man1/ ; dpkgArch="$( dpkg --print-architecture ;)" ; case "$dpkgArch" in (amd64|i386) echo 'deb http://www.apache.org/dist/cassandra/debian 21x main' > /etc/apt/sources.list.d/cassandra.list; apt-get update ;;(*) savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends wget ca-certificates dpkg-dev -y ; apt-mark showmanual | xargs apt-mark auto > /dev/null; apt-mark manual $savedAptMark ; tempDir="$( mktemp -d ;)" ; for pkg in cassandra cassandra-tools; do deb="${pkg}_${CASSANDRA_VERSION}_all.deb" ;wget -O "$tempDir/$deb" "https://www.apache.org/dist/cassandra/debian/pool/main/c/cassandra/$deb" ; done ; ls -lAFh "$tempDir" ; (cd "$tempDir" \
 && dpkg-scanpackages . > Packages) ; grep '^Package: ' "$tempDir/Packages" ; echo "deb [ trusted=yes ] file://$tempDir ./" > /etc/apt/sources.list.d/temp.list; apt-get -o Acquire::GzipIndexes=false update ;; esac ; apt-get install --no-install-recommends cassandra="$CASSANDRA_VERSION" cassandra-tools="$CASSANDRA_VERSION" -y ; rm -rf /var/lib/apt/lists/* ; if [ -n "$tempDir" ] ; then apt-get purge -y --auto-remove ;rm -rf "$tempDir" /etc/apt/sources.list.d/temp.list ; fi
ENV CASSANDRA_CONFIG="/etc/cassandra"
RUN set -ex ; dpkgArch="$( dpkg --print-architecture ;)" ; case "$dpkgArch" in (ppc64el) if grep -q -- '^-Xss' "$CASSANDRA_CONFIG/jvm.options" ; then grep -- '^-Xss256k$' "$CASSANDRA_CONFIG/jvm.options" ;sed -ri 's/^-Xss256k$/-Xss512k/' "$CASSANDRA_CONFIG/jvm.options" ;grep -- '^-Xss512k$' "$CASSANDRA_CONFIG/jvm.options" ; elif grep -q -- '-Xss256k' "$CASSANDRA_CONFIG/cassandra-env.sh" ; then sed -ri 's/-Xss256k/-Xss512k/g' "$CASSANDRA_CONFIG/cassandra-env.sh" ;grep -- '-Xss512k' "$CASSANDRA_CONFIG/cassandra-env.sh" ; fi ;; esac ; sed -ri 's/^(JVM_PATCH_VERSION)=.*/\1=25/' "$CASSANDRA_CONFIG/cassandra-env.sh"
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
RUN mkdir -p /var/lib/cassandra "$CASSANDRA_CONFIG" \
 && chown -R cassandra:cassandra /var/lib/cassandra "$CASSANDRA_CONFIG" \
 && chmod 777 /var/lib/cassandra "$CASSANDRA_CONFIG"
VOLUME /var/lib/cassandra
#   7000: intra-node communication
#   7001: TLS intra-node communication
#   7199: JMX
#   9042: CQL
#   9160: thrift service
EXPOSE 7000/tcp 7001/tcp 7199/tcp 9042/tcp 9160/tcp
CMD ["cassandra", "-f"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
