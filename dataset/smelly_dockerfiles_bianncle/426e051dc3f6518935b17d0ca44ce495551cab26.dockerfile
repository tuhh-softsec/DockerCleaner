#  Copyright 2017 K8s For Greeks / Vorstella
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
FROM gcr.io/google_containers/ubuntu-slim:0.12
ARG BUILD_DATE
ARG VCS_REF
ARG CASSANDRA_VERSION
ARG DEV_CONTAINER
LABEL org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.docker.dockerfile="/Dockerfile" \
      org.label-schema.license="Apache License 2.0" \
      org.label-schema.name="Cassandra container optimized for Kubernetes" \
      org.label-schema.url="https://github.com/k8s-for-greeks/" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-type="Git" \
      org.label-schema.vcs-url="https://github.com/k8s-for-greeks/docker-cassandra-k8s"
ENV CASSANDRA_CONF="/etc/cassandra" \
    CASSANDRA_DATA="/var/lib/cassandra" \
    CASSANDRA_LOGS="/var/log/cassandra" \
    CASSANDRA_RELEASE="3.11.0" \
    CASSANDRA_SHA="d597b99b402bd2cf925033519db9e58340acb893fd83d600d904ba4041d44fa7" \
    JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64" \
    DI_VERSION="1.2.0" \
    DI_SHA="81231da1cd074fdc81af62789fead8641ef3f24b6b07366a1c34e5b059faf363" \
    JOLOKIA_VERSION="1.3.5" \
    JOLOKIA_SHA="90907e9d1aa8799252c08cd5ec67d805b2661ad6e773d0de9c8e3d1620b72369" \
    PROMETHEUS_VERSION="0.8" \
    PROMETHEUS_SHA="c32440e4a98b441b4ab66a788df77494d32e1560e0f3bb5342752bf064408520" \
    LOGENCODER_VERSION="4.10-SNAPSHOT" \
    LOGENCODER_SHA="89be27bea7adc05b68c052a27b08c594a9f8e354185acbfd7a7b5f04c7cd9e20"
COPY files /
RUN set -ex \
 && echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections \
 && export CASSANDRA_VERSION=${CASSANDRA_VERSION:-$CASSANDRA_RELEASE} \
 && export CASSANDRA_HOME=/usr/local/apache-cassandra-${CASSANDRA_VERSION} \
 && apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jre-headless libjemalloc1 localepurge wget jq -qq -y --force-yes \
 && wget -q -O - "http://search.maven.org/remotecontent?filepath=io/prometheus/jmx/jmx_prometheus_javaagent/${PROMETHEUS_VERSION}/jmx_prometheus_javaagent-${PROMETHEUS_VERSION}.jar" > /usr/local/share/prometheus-agent.jar \
 && echo "$PROMETHEUS_SHA /usr/local/share/prometheus-agent.jar" | sha256sum -c - \
 && wget -q -O - "http://search.maven.org/remotecontent?filepath=org/jolokia/jolokia-jvm/${JOLOKIA_VERSION}/jolokia-jvm-${JOLOKIA_VERSION}-agent.jar" > /usr/local/share/jolokia-agent.jar \
 && echo "$JOLOKIA_SHA /usr/local/share/jolokia-agent.jar" | sha256sum -c - \
 && mirror_url=$( wget -q -O - 'https://www.apache.org/dyn/closer.cgi?as_json=1' | jq --raw-output '.preferred' ;) \
 && wget -q -O - ${mirror_url}/cassandra/${CASSANDRA_VERSION}/apache-cassandra-${CASSANDRA_VERSION}-bin.tar.gz > /usr/local/apache-cassandra-bin.tar.gz \
 && echo "$CASSANDRA_SHA /usr/local/apache-cassandra-bin.tar.gz" | sha256sum -c - \
 && tar -xzf /usr/local/apache-cassandra-bin.tar.gz -C /usr/local \
 && rm /usr/local/apache-cassandra-bin.tar.gz \
 && ln -s $CASSANDRA_HOME /usr/local/apache-cassandra \
 && wget -q -O - "https://github.com/mstump/logstash-logback-encoder/releases/download/${LOGENCODER_VERSION}/logstash-logback-encoder-${LOGENCODER_VERSION}.jar" > /usr/local/apache-cassandra/lib/log-encoder.jar \
 && echo "$LOGENCODER_SHA /usr/local/apache-cassandra/lib/log-encoder.jar" | sha256sum -c - \
 && wget -q -O - https://github.com/Yelp/dumb-init/releases/download/v${DI_VERSION}/dumb-init_${DI_VERSION}_amd64 > /sbin/dumb-init \
 && echo "$DI_SHA /sbin/dumb-init" | sha256sum -c - \
 && adduser --disabled-password --no-create-home --gecos '' --disabled-login cassandra \
 && mkdir -p /var/lib/cassandra/ /var/log/cassandra/ /etc/cassandra/triggers \
 && chmod +x /sbin/dumb-init /ready-probe.sh \
 && mv /logback-stdout.xml /logback-json-files.xml /logback-json-stdout.xml /logback-files.xml /cassandra.yaml /jvm.options /prometheus.yaml /etc/cassandra/ \
 && mv /usr/local/apache-cassandra/conf/cassandra-env.sh /etc/cassandra/ \
 && chown cassandra: /ready-probe.sh \
 && if [ -n "$DEV_CONTAINER" ] ; then apt-get install --no-install-recommends python -y ; else rm -rf $CASSANDRA_HOME/pylib ; fi \
 && apt-get -y purge wget jq localepurge \
 && apt-get -y autoremove \
 && apt-get clean \
 && rm -rf $CASSANDRA_HOME/*.txt $CASSANDRA_HOME/doc $CASSANDRA_HOME/javadoc $CASSANDRA_HOME/tools/*.yaml $CASSANDRA_HOME/tools/bin/*.bat $CASSANDRA_HOME/bin/*.bat doc man info locale common-licenses ~/.bashrc /var/lib/apt/lists/* /var/log/**/* /var/cache/debconf/* /etc/systemd /lib/lsb /lib/udev /usr/share/doc/ /usr/share/doc-base/ /usr/share/man/ /tmp/* /usr/lib/jvm/java-8-openjdk-amd64/jre/plugin /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/javaws /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/jjs /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/orbd /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/pack200 /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/policytool /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/rmid /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/rmiregistry /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/servertool /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/tnameserv /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/unpack200 /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/javaws.jar /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/deploy* /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/desktop /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/*javafx* /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/*jfx* /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/libdecora_sse.so /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/libprism_*.so /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/libfxplugins.so /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/libglass.so /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/libgstreamer-lite.so /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/libjavafx*.so /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/libjfx*.so /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/jfxrt.jar /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/nashorn.jar /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/oblique-fonts /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/plugin.jar /usr/lib/jvm/java-8-openjdk-amd64/jre/man /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/images /usr/lib/jvm/java-8-openjdk-amd64/man /usr/lib/jvm/java-8-openjdk-amd64/jre/THIRD_PARTY_README /usr/lib/jvm/java-8-openjdk-amd64/jre/ASSEMBLY_EXCEPTION
VOLUME ["/var/lib/cassandra"]
#  1234: prometheus jmx_exporter
#  7000: intra-node communication
#  7001: TLS intra-node communication
#  7199: JMX
#  9042: CQL
#  9160: thrift service
#  8778: jolokia port
EXPOSE 1234/tcp 7000/tcp 7001/tcp 7199/tcp 9042/tcp 9160/tcp 8778/tcp
CMD ["/sbin/dumb-init", "/bin/bash", "/run.sh"]
