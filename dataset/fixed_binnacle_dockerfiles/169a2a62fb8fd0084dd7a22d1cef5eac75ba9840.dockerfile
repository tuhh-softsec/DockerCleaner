FROM debian:jessie
#   Add CouchDB user account
RUN groupadd -g 5984 -r couchdb \
 && useradd -u 5984 -d /opt/couchdb -g couchdb couchdb
RUN apt-get update -y \
 && apt-get install --no-install-recommends ca-certificates curl erlang-nox erlang-reltool libicu52 libmozjs185-1.0 openssl -y \
 && rm -rf /var/lib/apt/lists/*
#   grab gosu for easy step-down from root and tini for signal handling
#   see https://github.com/apache/couchdb-docker/pull/28#discussion_r141112407
ENV GOSU_VERSION="1.10"
ENV TINI_VERSION="0.16.1"
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends wget -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/${GOSU_VERSION}/gosu-$dpkgArch" ; wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc ; chmod +x /usr/local/bin/gosu ; gosu nobody true ; wget -O /usr/local/bin/tini "https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini-$dpkgArch" ; wget -O /usr/local/bin/tini.asc "https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --keyserver ha.pool.sks-keyservers.net --recv-keys 595E85A6B1B4779EA4DAAEC70B588DFF0527A9B7 ; gpg --batch --verify /usr/local/bin/tini.asc /usr/local/bin/tini ; rm -r "$GNUPGHOME" /usr/local/bin/tini.asc ; chmod +x /usr/local/bin/tini ; tini --version ; apt-get purge -y --auto-remove wget
#   https://www.apache.org/dist/couchdb/KEYS
ENV GPG_KEYS="15DD4F3B8AACA54740EB78C7B7B7C53943ECCEE1  1CFBFA43C19B6DF4A0CA3934669C02FFDF3CEBA3  25BBBAC113C1BFD5AA594A4C9F96B92930380381  4BFCA2B99BADC6F9F105BEC9C5E32E2D6B065BFB  5D680346FAA3E51B29DBCB681015F68F9DA248BC  7BCCEB868313DDA925DF1805ECA5BCB7BB9656B0  C3F4DFAEAD621E1C94523AEEC376457E61D50B88  D2B17F9DA23C0A10991AF2E3D9EE01E47852AEE4  E0AF0A194D55C84E4A19A801CDB0C0F904F4EE9B  29E4F38113DF707D722A6EF91FE9AF73118F1A7C  2EC788AE3F239FA13E82D215CDE711289384AE37"
RUN set -xe \
 && for key in $GPG_KEYS; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done
ENV COUCHDB_VERSION="2.1.1"
#   Download dev dependencies
RUN buildDeps=' apt-transport-https gcc g++ erlang-dev libcurl4-openssl-dev libicu-dev libmozjs185-dev make ' \
 && apt-get update -y -qq \
 && apt-get install --no-install-recommends $buildDeps -y \
 && cd /usr/src \
 && mkdir couchdb \
 && curl -fSL https://dist.apache.org/repos/dist/release/couchdb/source/$COUCHDB_VERSION/apache-couchdb-$COUCHDB_VERSION.tar.gz -o couchdb.tar.gz \
 && curl -fSL https://dist.apache.org/repos/dist/release/couchdb/source/$COUCHDB_VERSION/apache-couchdb-$COUCHDB_VERSION.tar.gz.asc -o couchdb.tar.gz.asc \
 && gpg --batch --verify couchdb.tar.gz.asc couchdb.tar.gz \
 && tar -xzf couchdb.tar.gz -C couchdb --strip-components=1 \
 && cd couchdb \
 && ./configure --disable-docs \
 && make release \
 && mv /usr/src/couchdb/rel/couchdb /opt/ \
 && apt-get purge -y --auto-remove $buildDeps \
 && rm -rf /var/lib/apt/lists/* /usr/src/couchdb* \
 && mkdir /opt/couchdb/data \
 && chown -R couchdb:couchdb /opt/couchdb
#   Setup directories and permissions
RUN chown -R couchdb:couchdb /opt/couchdb/
COPY docker-entrypoint.sh /
EXPOSE 5984/tcp 5986/tcp 6984/tcp 4369/tcp
EXPOSE 9100-9200
ENTRYPOINT ["tini", "--", "/docker-entrypoint.sh"]
CMD ["/opt/couchdb/bin/couchdb"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
