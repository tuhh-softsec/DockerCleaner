FROM ubuntu:16.04
ENV ELASTICSEARCH_VERSION="5.5.2"
ENV ELASTICSEARCH_DEB_VERSION="5.5.2"
ENV GOSU_VERSION="1.10"
ENV KIBANA_VERSION="5.5.2"
ENV DOCKER_CE_VERSION="17.09.1"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#  Elasticsearch and Kibana
RUN set -ex \
 && groupadd -r elasticsearch \
 && useradd -r -m -g elasticsearch elasticsearch \
 && groupadd -r kibana \
 && useradd -r -m -g kibana kibana \
 && apt-get update \
 && apt-get install curl locales -y \
 && locale-gen en_US.UTF-8 \
 && apt-get install wget supervisor openjdk-8-jre unzip -y
#  grab gosu for easy step-down from root
RUN set -x \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true
#  KEYS
RUN set -ex ; key='46095ACC8548582C1A2699A9D27D666CD88E42B4' ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; gpg --export "$key" > /etc/apt/trusted.gpg.d/elastic.gpg; rm -rf "$GNUPGHOME" ; apt-key list
#  ELASTICSEACH
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends apt-transport-https -y \
 && rm -rf /var/lib/apt/lists/* \
 && echo 'deb https://artifacts.elastic.co/packages/5.x/apt stable main' > /etc/apt/sources.list.d/elasticsearch.list
RUN set -x \
 && dpkg-divert --rename /usr/lib/sysctl.d/elasticsearch.conf \
 && apt-get update \
 && apt-get install --no-install-recommends "elasticsearch=$ELASTICSEARCH_DEB_VERSION" -y \
 && rm -rf /var/lib/apt/lists/*
RUN for path in /usr/share/elasticsearch/data /usr/share/elasticsearch/logs /usr/share/elasticsearch/config /usr/share/elasticsearch/config/scripts; do mkdir -p "$path" ;chown -R elasticsearch:elasticsearch "$path" ; done \
 && chown -R elasticsearch:elasticsearch /usr/share/elasticsearch
#   Kibana
#  https://www.elastic.co/guide/en/kibana/5.0/deb.htmls
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends kibana=$KIBANA_VERSION -y \
 && rm -rf /var/lib/apt/lists/* \
 && sed -ri "s!^(\#\s*)?(server\.host:).*!\2 '0.0.0.0'!" /etc/kibana/kibana.yml \
 && grep -q "^server\.host: '0.0.0.0'$" /etc/kibana/kibana.yml \
 && sed -ri "s!^(\#\s*)?(elasticsearch\.url:).*!\2 'http://elasticsearch:9200'!" /etc/kibana/kibana.yml \
 && grep -q "^elasticsearch\.url: 'http://elasticsearch:9200'$" /etc/kibana/kibana.yml
#  Docker CE
RUN set -ex \
 && curl -o /tmp/docker-ce.deb https://download.docker.com/linux/ubuntu/dists/xenial/pool/stable/amd64/docker-ce_${DOCKER_CE_VERSION}~ce-0~ubuntu_amd64.deb \
 && apt-get update \
 && apt-get install --no-install-recommends /tmp/docker-ce.deb -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm /tmp/docker-ce.deb
ENV PATH="/usr/share/elasticsearch/bin:$PATH"
ENV PATH="/usr/share/kibana/bin:$PATH"
COPY elasticsearch/elasticsearch.yml elasticsearch/log4j2.properties /usr/share/elasticsearch/config/
COPY kibana/kibana.yml /usr/share/kibana/config/kibana.yml
COPY kibana/start.sh /usr/share/kibana/bin/kibana-start
#  setup Supervisord
COPY supervisord.conf /etc/supervisor/supervisord.conf
EXPOSE 9200/tcp 9300/tcp
EXPOSE 5601/tcp
CMD ["supervisord", "-n", "-c", "/etc/supervisor/supervisord.conf"]
