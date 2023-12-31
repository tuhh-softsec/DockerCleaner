FROM gcr.io/google-appengine/openjdk
RUN set -ex ; if ! command -v gpg > /dev/null; then apt-get update ;apt-get install --no-install-recommends gnupg=2.2.40-1ubuntu2 dirmngr=2.2.40-1ubuntu2 -y ;rm -rf /var/lib/apt/lists/* ; fi
#   add gosu for easy step-down from root
ENV GOSU_VERSION="1.10"
#   prometheus exporter
ENV EXPORTER_VERSION="1.1.0rc1"
RUN set -x \
 && wget -q -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && wget -q -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" \
 && wget -q -O /usr/local/src/gosu.tar.gz "https://github.com/tianon/gosu/archive/$GOSU_VERSION.tar.gz" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && found='' \
 && for server in pool.sks-keyservers.net ha.pool.sks-keyservers.net pgp.mit.edu na.pool.sks-keyservers.net eu.pool.sks-keyservers.net oc.pool.sks-keyservers.net ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 hkp://keyserver.ubuntu.com:80; do gpg --no-tty --keyserver $server --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && found="yes" \
 && break ; done ; test -n "$found" \
 && gpg --no-tty --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true
#   Install Elastic Search
RUN set -ex ; key='46095ACC8548582C1A2699A9D27D666CD88E42B4' ; export GNUPGHOME="$( mktemp -d ;)" ; found='' ; for server in pool.sks-keyservers.net ha.pool.sks-keyservers.net pgp.mit.edu na.pool.sks-keyservers.net eu.pool.sks-keyservers.net oc.pool.sks-keyservers.net ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 hkp://keyserver.ubuntu.com:80; do gpg --keyserver $server --recv-keys "$key" \
 && found="yes" \
 && break ; done ; test -n "$found" ; gpg --export "$key" > /etc/apt/trusted.gpg.d/elastic.gpg; rm -rf "$GNUPGHOME" ; apt-key list
#   https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-repositories.html
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=2.6.0 -y \
 && rm -rf /var/lib/apt/lists/* \
 && echo 'deb https://artifacts.elastic.co/packages/6.x/apt stable main' > /etc/apt/sources.list.d/elasticsearch.list
ENV ELASTICSEARCH_VERSION="6.1.4"
ENV ELASTICSEARCH_DEB_VERSION="6.1.4"
#   copy source code
RUN wget -q https://github.com/elastic/elasticsearch/archive/v6.1.4.tar.gz -O /usr/local/src/elasticsearch-source-v6.1.4.tar.gz
RUN set -x \
 && dpkg-divert --rename /usr/lib/sysctl.d/elasticsearch.conf \
 && apt-get update \
 && apt-get install --no-install-recommends "elasticsearch=$ELASTICSEARCH_DEB_VERSION" -y \
 && rm -rf /var/lib/apt/lists/*
RUN set -ex \
 && wget -q -O elasticsearch_exporter-${EXPORTER_VERSION}.linux-amd64.tar.gz https://github.com/justwatchcom/elasticsearch_exporter/releases/download/v${EXPORTER_VERSION}/elasticsearch_exporter-${EXPORTER_VERSION}.linux-amd64.tar.gz \
 && wget -q -O /usr/src/elasticsearch_exporter-v$EXPORTER_VERSION.tar.gz https://github.com/justwatchcom/elasticsearch_exporter/archive/v${EXPORTER_VERSION}.tar.gz \
 && tar -xzf elasticsearch_exporter-${EXPORTER_VERSION}.linux-amd64.tar.gz \
 && mv elasticsearch_exporter-${EXPORTER_VERSION}.linux-amd64 prometheus-exporter \
 && rm -f elasticsearch_exporter-${EXPORTER_VERSION}.linux-amd64.tar.gz \
 && mkdir -p /usr/share/doc/elasticsearch_exporter \
 && cp /prometheus-exporter/LICENSE /usr/share/doc/elasticsearch_exporter/LICENSE
COPY licences/ /usr/share/doc/
ENV PATH="/usr/share/elasticsearch/bin:$PATH"
WORKDIR /usr/share/elasticsearch
COPY config /etc/elasticsearch
RUN set -ex \
 && for path in ./data ./logs /etc/elasticsearch /etc/elasticsearch/scripts /usr/share/elasticsearch; do mkdir -p "$path" ;chown -R elasticsearch:elasticsearch "$path" ; done
VOLUME /usr/share/elasticsearch/data
COPY docker-entrypoint.sh /
#   9114 prometheus exporter port
EXPOSE 9114/tcp 9200/tcp 9300/tcp
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["elasticsearch"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
