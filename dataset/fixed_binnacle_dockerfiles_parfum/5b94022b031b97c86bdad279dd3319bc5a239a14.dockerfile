ARG BASE_IMAGE
FROM ${BASE_IMAGE}
LABEL maintainer="support@strapdata.com"
LABEL description="Elassandra docker image"
#  explicitly set user/group IDs
RUN groupadd -r cassandra --gid=999 \
 && useradd -r -g cassandra --uid=999 cassandra
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends libjemalloc1 procps iproute2 curl python python-pip python-setuptools jq -y ; pip install pip yq -U ; if ! command -v gpg > /dev/null; then apt-get install --no-install-recommends dirmngr gnupg -y ; fi ; rm -rf /var/lib/apt/lists/*
#  grab gosu for easy step-down from root
ENV GOSU_VERSION="1.10"
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends ca-certificates wget -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --no-tty --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && gpg --no-tty --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && { command -v gpgconf \
 && gpgconf --kill all || : ; } \
 && rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true \
 && apt-get purge -y --auto-remove ca-certificates wget
#  https://wiki.apache.org/cassandra/DebianPackaging#Adding_Repository_Keys
ENV GPG_KEYS="514A2AD631A57A16DD0047EC749D6EEC0353B12C  A26E528B271F19B9E5D8E19EA278B781FE4B2BDA"
# RUN set -ex; \
# 	export GNUPGHOME="$(mktemp -d)"; \
# 	for key in $GPG_KEYS; do \
# 		gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key"; \
# 	done; \
# 	gpg --export $GPG_KEYS > /etc/apt/trusted.gpg.d/cassandra.gpg; \
# 	command -v gpgconf && gpgconf --kill all || :; \
# 	rm -rf "$GNUPGHOME"; \
# 	apt-key list
#  build-time arguments
ARG ELASSANDRA_VERSION
ENV ELASSANDRA_VERSION="${ELASSANDRA_VERSION}"
#  optional sha1 of the commit
ARG ELASSANDRA_COMMIT
ENV ELASSANDRA_COMMIT="${ELASSANDRA_COMMIT}"
#  location of the elassandra package on the building machine
ARG ELASSANDRA_PACKAGE
#  copy the elassandra package into the image
COPY ${ELASSANDRA_PACKAGE} /elassandra-${ELASSANDRA_VERSION}.deb
RUN set -ex ; mkdir -p /usr/share/man/man1/ ; dpkgArch="$( dpkg --print-architecture ;)" ; case "$dpkgArch" in (*) savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends wget ca-certificates dpkg-dev -y ; apt-mark showmanual | xargs apt-mark auto > /dev/null; apt-mark manual $savedAptMark ; tempDir="$( mktemp -d ;)" ; cp /elassandra-${ELASSANDRA_VERSION}.deb $tempDir/ ; ls -lAFh "$tempDir" ; (cd "$tempDir" \
 && dpkg-scanpackages . > Packages) ; grep '^Package: ' "$tempDir/Packages" ; echo "deb [ trusted=yes ] file://$tempDir ./" > /etc/apt/sources.list.d/temp.list; apt-get -o Acquire::GzipIndexes=false update ;; esac ; apt-get install --no-install-recommends elassandra="$ELASSANDRA_VERSION" -y; rm -rf /var/lib/apt/lists/* ; rm /elassandra-${ELASSANDRA_VERSION}.deb ; if [ -n "$tempDir" ] ; then apt-get purge -y --auto-remove ;rm -rf "$tempDir" /etc/apt/sources.list.d/temp.list ; fi
ENV CASSANDRA_CONFIG="/etc/cassandra"
RUN set -ex ; dpkgArch="$( dpkg --print-architecture ;)" ; case "$dpkgArch" in (ppc64el) if grep -q -- '^-Xss' "$CASSANDRA_CONFIG/jvm.options" ; then grep -- '^-Xss256k$' "$CASSANDRA_CONFIG/jvm.options" ;sed -ri 's/^-Xss256k$/-Xss512k/' "$CASSANDRA_CONFIG/jvm.options" ;grep -- '^-Xss512k$' "$CASSANDRA_CONFIG/jvm.options" ; elif grep -q -- '-Xss256k' "$CASSANDRA_CONFIG/cassandra-env.sh" ; then sed -ri 's/-Xss256k/-Xss512k/g' "$CASSANDRA_CONFIG/cassandra-env.sh" ;grep -- '-Xss512k' "$CASSANDRA_CONFIG/cassandra-env.sh" ; fi ;; esac ; sed -ri 's/^(JVM_PATCH_VERSION)=.*/\1=25/' "$CASSANDRA_CONFIG/cassandra-env.sh"
#  copy readiness probe script for kubernetes
COPY ready-probe.sh /
#  Add custom logback.xml including variables.
COPY logback.xml $CASSANDRA_CONFIG/
#  Add default JMX password file
COPY jmxremote.password $CASSANDRA_CONFIG/
#  Can't use COPY --chown here because it is not supported on old docker versions
RUN chown cassandra:cassandra ready-probe.sh $CASSANDRA_CONFIG/logback.xml $CASSANDRA_CONFIG/jmxremote.password \
 && chmod 0400 $CASSANDRA_CONFIG/jmxremote.password
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
#  create the entrypoint init.d directory
RUN mkdir -p /docker-entrypoint-init.d \
 && chown cassandra:cassandra /docker-entrypoint-init.d
VOLUME /var/lib/cassandra
#  elassandra installation directories
ENV CASSANDRA_HOME="/usr/share/cassandra"
ENV CASSANDRA_CONF="/etc/cassandra"
ENV CASSANDRA_LOGDIR="/var/log/cassandra"
ENV CASSANDRA_DATA="/var/lib/cassandra"
#  docker-entrypoint.sh defines some default env vars when starting the container.
#  But those vars are not available from other entrypoint, such as ready-probe.sh, or 'docker exec'.
#  A workaround is to define important defaults right in the Dockerfile
ENV CASSANDRA_DAEMON="org.apache.cassandra.service.ElassandraDaemon"
#  7000: intra-node communication
#  7001: TLS intra-node communication
#  7199: JMX
#  9042: CQL
#  9142 : encrypted CQL
#  9160: thrift service
#  9200: elassandra HTTP
#  9300: elasticsearch transport
EXPOSE 7000/tcp 7001/tcp 7199/tcp 9042/tcp 9142/tcp 9160/tcp 9200/tcp 9300/tcp
CMD ["cassandra", "-f"]
