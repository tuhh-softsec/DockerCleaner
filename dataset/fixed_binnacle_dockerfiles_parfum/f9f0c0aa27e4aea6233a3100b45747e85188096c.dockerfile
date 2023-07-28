FROM gliderlabs/alpine:3.4
LABEL maintainer="\"https://github.com/blacktop\""
RUN apk-install openjdk8-jre tini su-exec
ENV ELASTIC="2.4.4"
ENV LOGSTASH="2.4.1"
ENV KIBANA="4.6.4"
RUN apk-install libzmq bash nodejs supervisor nginx apache2-utils openssl
RUN mkdir -p /usr/local/lib \
 && ln -s /usr/lib/*/libzmq.so.3 /usr/local/lib/libzmq.so
RUN apk-install -t .build-deps wget ca-certificates \
 && set -x \
 && cd /tmp \
 && echo "Download Elastic Stack ======================================================" \
 && echo "Download Elasticsearch..." \
 && wget -O elasticsearch-$ELASTIC.tar.gz https://download.elastic.co/elasticsearch/release/org/elasticsearch/distribution/tar/elasticsearch/$ELASTIC/elasticsearch-$ELASTIC.tar.gz \
 && tar -xzf elasticsearch-$ELASTIC.tar.gz \
 && mv elasticsearch-$ELASTIC /usr/share/elasticsearch \
 && echo "Download Logstash..." \
 && wget -O logstash-$LOGSTASH.tar.gz https://download.elastic.co/logstash/logstash/logstash-$LOGSTASH.tar.gz \
 && tar -xzf logstash-$LOGSTASH.tar.gz \
 && mv logstash-$LOGSTASH /usr/share/logstash \
 && echo "Download Kibana..." \
 && wget -O kibana-$KIBANA.tar.gz https://download.elastic.co/kibana/kibana/kibana-$KIBANA-linux-x86_64.tar.gz \
 && tar -xzf kibana-$KIBANA.tar.gz \
 && mv kibana-$KIBANA-linux-x86_64 /usr/share/kibana \
 && echo "Configure [Elasticsearch] ===================================================" \
 && for path in /usr/share/elasticsearch/data /usr/share/elasticsearch/logs /usr/share/elasticsearch/config /usr/share/elasticsearch/config/scripts /usr/share/elasticsearch/plugins; do mkdir -p "$path" ; done \
 && echo "Configure [Logstash] ========================================================" \
 && if [ -f "$LS_SETTINGS_DIR/logstash.yml" ] ; then sed -ri 's!^(path.log|path.config):!#&!g' "$LS_SETTINGS_DIR/logstash.yml" ; fi \
 && echo "Configure [Kibana] ==========================================================" \
 && sed -ri "s!^(\#\s*)?(server\.host:).*!\2 '0.0.0.0'!" /usr/share/kibana/config/kibana.yml \
 && grep -q "^server\.host: '0.0.0.0'$" /usr/share/kibana/config/kibana.yml \
 && bundled='NODE="${DIR}/node/bin/node"' \
 && apline_node='NODE="/usr/bin/node"' \
 && sed -i "s|$bundled|$apline_node|g" /usr/share/kibana/bin/kibana \
 && rm -rf /usr/share/kibana/node \
 && echo "Make Ngins SSL directory..." \
 && mkdir -p /etc/nginx/ssl \
 && echo "Create elstack user..." \
 && adduser -DH -s /sbin/nologin elstack \
 && chown -R elstack:elstack /usr/share/elasticsearch \
 && chown -R elstack:elstack /usr/share/logstash \
 && chown -R elstack:elstack /usr/share/kibana \
 && echo "Clean Up..." \
 && rm -rf /tmp/* \
 && apk del --purge .build-deps
ENV PATH="/usr/share/elasticsearch/bin:$PATH"
ENV PATH="/usr/share/logstash/bin:$PATH"
ENV PATH="/usr/share/kibana/bin:$PATH"
#  Add custom elasticsearch config
COPY config/elastic /usr/share/elasticsearch/config
COPY config/elastic/logrotate /etc/logrotate.d/elasticsearch
#  Add custom logstash config
COPY config/logstash/conf.d/ /etc/logstash/conf.d/
COPY config/logstash/patterns/ /opt/logstash/patterns/
COPY config/logstash/logstash.yml /etc/logstash/
#  necessary for 5.0+ (overriden via "--path.settings", ignored by < 5.0)
ENV LS_SETTINGS_DIR="/etc/logstash"
#  Add custom nginx config
COPY config/nginx/nginx.conf /etc/nginx/nginx.conf
COPY config/nginx/kibana.conf /etc/nginx/conf.d/
COPY config/nginx/ssl.kibana.conf /etc/nginx/conf.d/
#  Add custom supervisor config
COPY config/supervisord/supervisord.conf /etc/supervisor/
#  Add entrypoints
COPY entrypoints/elastic-entrypoint.sh /
COPY entrypoints/logstash-entrypoint.sh /
COPY entrypoints/kibana-entrypoint.sh /
COPY entrypoints/nginx-entrypoint.sh /
VOLUME ["/usr/share/elasticsearch/data"]
VOLUME ["/etc/logstash/conf.d"]
VOLUME ["/etc/nginx"]
EXPOSE 80/tcp 443/tcp 5601/tcp 9200/tcp 9300/tcp
CMD ["/sbin/tini", "--", "/usr/bin/supervisord", "-c", "/etc/supervisor/supervisord.conf"]
