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
#   Base layer containing dependencies needed at runtime. This layer will be
#   cached after the initial build.
FROM debian:stretch AS runtime
MAINTAINER CouchDB Developers dev@couchdb.apache.org
#   Add CouchDB user account
RUN groupadd -g 5984 -r couchdb \
 && useradd -u 5984 -d /opt/couchdb -g couchdb couchdb
RUN apt-get update -y \
 && apt-get install --no-install-recommends apt-transport-https=1.4.11 ca-certificates=20200601~deb9u2 curl=7.52.1-5+deb9u16 dirmngr=2.1.18-8~deb9u4 gnupg=2.1.18-8~deb9u4 libicu57=57.1-6+deb9u5 libssl1.1=1.1.0l-1~deb9u6 openssl=1.1.0l-1~deb9u6 -y \
 && echo "deb https://apache.bintray.com/couchdb-deb stretch main" | tee /etc/apt/sources.list.d/couchdb.list \
 && cat /etc/apt/sources.list.d/couchdb.list \
 && for server in $( shuf -e pgpkeys.mit.edu ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu ;); do gpg --batch --keyserver $server --recv-keys 8756C4F765C9AC3CB6B85D62379CE192D401AB61 \
 && break || : ; done \
 && gpg -a --export 8756C4F765C9AC3CB6B85D62379CE192D401AB61 > /etc/apt/trusted.gpg.d/couchdb.gpg.asc \
 && apt-get update -y \
 && apt-get install --no-install-recommends couch-libmozjs185-1.0 -y \
 && rm -rf /var/lib/apt/lists/*
#   grab gosu for easy step-down from root and tini for signal handling
#   see https://github.com/apache/couchdb-docker/pull/28#discussion_r141112407
ENV GOSU_VERSION="1.10"
ENV TINI_VERSION="0.16.1"
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends wget=1.18-5+deb9u3 -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/${GOSU_VERSION}/gosu-$dpkgArch" ; wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; echo "disable-ipv6" >> ${GNUPGHOME}/dirmngr.conf; for server in $( shuf -e pgpkeys.mit.edu ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu ;); do gpg --batch --keyserver $server --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && break || : ; done ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc ; chmod +x /usr/local/bin/gosu ; gosu nobody true ; wget -O /usr/local/bin/tini "https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini-$dpkgArch" ; wget -O /usr/local/bin/tini.asc "https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; echo "disable-ipv6" >> ${GNUPGHOME}/dirmngr.conf; for server in $( shuf -e pgpkeys.mit.edu ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu ;); do gpg --batch --keyserver $server --recv-keys 595E85A6B1B4779EA4DAAEC70B588DFF0527A9B7 \
 && break || : ; done ; gpg --batch --verify /usr/local/bin/tini.asc /usr/local/bin/tini ; rm -rf "$GNUPGHOME" /usr/local/bin/tini.asc ; chmod +x /usr/local/bin/tini ; tini --version ; apt-get purge -y --auto-remove wget
#   Dependencies only needed during build time. This layer will also be cached
FROM runtime AS build_dependencies
RUN apt-get update -y \
 && apt-get install --no-install-recommends build-essential=12.3 couch-libmozjs185-dev erlang-nox=1:19.2.1+dfsg-2+deb9u3 erlang-reltool=1:19.2.1+dfsg-2+deb9u3 erlang-dev=1:19.2.1+dfsg-2+deb9u3 git=1:2.11.0-3+deb9u7 libcurl4-openssl-dev=7.52.1-5+deb9u16 libicu-dev=57.1-6+deb9u5 python3=3.5.3-1 libpython3-dev=3.5.3-1 python3-pip=9.0.1-2+deb9u2 python3-sphinx=1.4.9-2 -y
RUN pip3 install --upgrade sphinx_rtd_theme nose requests hypothesis
#   Node is special
RUN set -ex ; curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - ; echo 'deb https://deb.nodesource.com/node_8.x stretch main' > /etc/apt/sources.list.d/nodesource.list; echo 'deb-src https://deb.nodesource.com/node_8.x stretch main' >> /etc/apt/sources.list.d/nodesource.list; apt-get update -y \
 && apt-get install --no-install-recommends nodejs=4.8.2~dfsg-1 -y ; npm install grunt-cli@1.4.3 -g
#   Clone CouchDB source code including all dependencies
ARG clone_url=https://github.com/apache/couchdb.git
RUN git clone $clone_url /usr/src/couchdb
WORKDIR /usr/src/couchdb
RUN ./configure
#   This layer performs the actual build of a relocatable, self-contained
#   release of CouchDB. It pulls down the latest changes from the remote
#   origin (because the layer above will be cached) and switches to the
#   branch specified in the build_arg (defaults to master)
FROM build_dependencies AS build
ARG checkout_branch=master
ARG configure_options
WORKDIR /usr/src/couchdb/
RUN git fetch origin \
 && git checkout $checkout_branch \
 && ./configure $configure_options \
 && make release
#   This results in a single layer image (or at least skips the build stuff?)
FROM runtime
COPY --from=build /usr/src/couchdb/rel/couchdb /opt/couchdb
#   Add configuration
COPY local.ini /opt/couchdb/etc/default.d/
COPY vm.args /opt/couchdb/etc/
COPY docker-entrypoint.sh /
#   Setup directories and permissions
RUN find /opt/couchdb !
WORKDIR /opt/couchdb
EXPOSE 5984/tcp 4369/tcp 9100/tcp
VOLUME ["/opt/couchdb/data"]
ENTRYPOINT ["tini", "--", "/docker-entrypoint.sh"]
CMD ["/opt/couchdb/bin/couchdb"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
