#   Licensed under the Apache License, Version 2.0 (the "License"); you may not
#   use this file except in compliance with the License. You may obtain a copy of
#   the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
#   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
#   License for the specific language governing permissions and limitations under
#   the License.
FROM debian:bullseye-slim
LABEL maintainer="CouchDB Developers dev@couchdb.apache.org"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#   Add CouchDB user account to make sure the IDs are assigned consistently
RUN groupadd -g 5984 -r couchdb \
 && useradd -u 5984 -d /opt/couchdb -g couchdb couchdb
#   be sure GPG and apt-transport-https are available and functional
RUN :
RUN set -ex ; : ; (apt-get update ;apt-get install --no-install-recommends apt-transport-https=2.2.4 ca-certificates=20210119 dirmngr=2.2.27-2+deb11u2 gnupg=2.2.27-2+deb11u2 -y ) ; rm -rf /var/lib/apt/lists/*
#   grab tini for signal handling and zombie reaping
#   see https://github.com/apache/couchdb-docker/pull/28#discussion_r141112407
RUN :
RUN set -eux ; : ; (apt-get update ;apt-get install --no-install-recommends tini=0.19.0-1 -y ) ; rm -rf /var/lib/apt/lists/* ; tini --version
#   http://docs.couchdb.org/en/latest/install/unix.html#installing-the-apache-couchdb-packages
ENV GPG_COUCH_KEY="390EF70BB1EA12B2773962950EE62FB37A00258D"
RUN :
RUN set -eux ; : ; (apt-get update ;apt-get install --no-install-recommends curl=7.74.0-1.3+deb11u7 -y ) ; export GNUPGHOME="$( mktemp -d ;)" ; curl -fL -o keys.asc https://couchdb.apache.org/repo/keys.asc ; gpg --batch --import keys.asc ; gpg --batch --export "${GPG_COUCH_KEY}" > /usr/share/keyrings/couchdb-archive-keyring.gpg; command -v gpgconf \
 && gpgconf --kill all || : ; rm -rf "$GNUPGHOME" ; apt-key list ; apt-get purge -y --autoremove curl ; rm -rf /var/lib/apt/lists/*
ENV COUCHDB_VERSION="3.3.1"
RUN . /etc/os-release ; echo "deb [signed-by=/usr/share/keyrings/couchdb-archive-keyring.gpg] https://apache.jfrog.io/artifactory/couchdb-deb/ ${VERSION_CODENAME} main" | tee /etc/apt/sources.list.d/couchdb.list > /dev/null
#   https://github.com/apache/couchdb-pkg/blob/master/debian/README.Debian
RUN set -eux ; : ; echo "couchdb couchdb/mode select none" | debconf-set-selections ; DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends --allow-downgrades --allow-remove-essential --allow-change-held-packages couchdb="$COUCHDB_VERSION"~bullseye ; rmdir /var/lib/couchdb /var/log/couchdb ; rm /opt/couchdb/data /opt/couchdb/var/log ; mkdir -p /opt/couchdb/data /opt/couchdb/var/log ; chown couchdb:couchdb /opt/couchdb/data /opt/couchdb/var/log ; chmod 777 /opt/couchdb/data /opt/couchdb/var/log ; rm /opt/couchdb/etc/default.d/10-filelog.ini ; find /opt/couchdb !
#   Add configuration
COPY --chown=couchdb:couchdb 10-docker-default.ini /opt/couchdb/etc/default.d/
COPY --chown=couchdb:couchdb vm.args /opt/couchdb/etc/
COPY docker-entrypoint.sh /usr/local/bin
RUN ln -s usr/local/bin/docker-entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT ["tini", "--", "/docker-entrypoint.sh"]
VOLUME /opt/couchdb/data
#   5984: Main CouchDB endpoint
#   4369: Erlang portmap daemon (epmd)
#   9100: CouchDB cluster communication port
EXPOSE 5984/tcp 4369/tcp 9100/tcp
HEALTHCHECK CMD curl --fail http://127.0.0.1:5984 || exit 1
CMD ["/opt/couchdb/bin/couchdb"]
