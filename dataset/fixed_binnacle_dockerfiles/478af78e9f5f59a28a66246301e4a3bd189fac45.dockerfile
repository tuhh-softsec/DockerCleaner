FROM alpine:3.7
LABEL maintainer="\"https://github.com/blacktop\""
RUN apk add openjdk8-jre=8.275.01-r0 tini=0.16.1-r0 su-exec=0.2-r0 --no-cache
ENV STACK="6.0.1"
RUN apk add libzmq=4.2.5-r1 bash=4.4.19-r1 nodejs=8.9.3-r1 nginx=1.12.2-r4 apache2-utils=2.4.41-r0 openssl=1.0.2t-r0 --no-cache
RUN mkdir -p /usr/local/lib \
 && ln -s /usr/lib/*/libzmq.so.3 /usr/local/lib/libzmq.so
RUN apk add wget=1.20.3-r0 ca-certificates=20190108-r0 --no-cache -t .build-deps \
 && set -x \
 && cd /tmp \
 && echo "Download Elastic Stack ======================================================" \
 && echo "Download Elasticsearch..." \
 && wget --progress=bar:force -O elasticsearch-$STACK.tar.gz https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-$STACK.tar.gz \
 && tar -xzf elasticsearch-$STACK.tar.gz \
 && mv elasticsearch-$STACK /usr/share/elasticsearch \
 && echo "Download Logstash..." \
 && wget --progress=bar:force -O logstash-$STACK.tar.gz https://artifacts.elastic.co/downloads/logstash/logstash-$STACK.tar.gz \
 && tar -xzf logstash-$STACK.tar.gz \
 && mv logstash-$STACK /usr/share/logstash \
 && echo "Download Kibana..." \
 && wget --progress=bar:force -O kibana-$STACK.tar.gz https://artifacts.elastic.co/downloads/kibana/kibana-$STACK-linux-x86_64.tar.gz \
 && tar -xzf kibana-$STACK.tar.gz \
 && mv kibana-$STACK-linux-x86_64 /usr/share/kibana \
 && echo "Configure [Elasticsearch] ===================================================" \
 && for path in /usr/share/elasticsearch/data /usr/share/elasticsearch/logs /usr/share/elasticsearch/config /usr/share/elasticsearch/config/scripts /usr/share/elasticsearch/plugins; do mkdir -p "$path" ; done \
 && echo "Configure [Logstash] ========================================================" \
 && if [ -f "$LS_SETTINGS_DIR/logstash.yml" ] ; then sed -ri 's!^(path.log|path.config):!#&!g' "$LS_SETTINGS_DIR/logstash.yml" ; fi \
 && echo "Configure [Kibana] ==========================================================" \
 && sed -ri "s!^(\#\s*)?(server\.host:).*!\2 '0.0.0.0'!" /usr/share/kibana/config/kibana.yml \
 && grep -q "^server\.host: '0.0.0.0'$" /usr/share/kibana/config/kibana.yml \
 && bundled='NODE="${DIR}/node/bin/node"' \
 && apline_node='NODE="/usr/bin/node"' \
 && sed -i "s|$bundled|$apline_node|g" /usr/share/kibana/bin/kibana-plugin \
 && sed -i "s|$bundled|$apline_node|g" /usr/share/kibana/bin/kibana \
 && rm -rf /usr/share/kibana/node \
 && echo "Make Nginx SSL directory..." \
 && mkdir -p /etc/nginx/ssl \
 && rm /etc/nginx/conf.d/default.conf \
 && echo "Create elstack user..." \
 && adduser -DH -s /sbin/nologin elstack \
 && chown -R elstack:elstack /usr/share/elasticsearch \
 && chown -R elstack:elstack /usr/share/logstash \
 && chown -R elstack:elstack /usr/share/kibana \
 && echo "Clean Up..." \
 && rm -rf /tmp/* \
 && apk del --purge .build-deps
RUN apk add supervisor=3.3.3-r1 libc6-compat=1.1.18-r4 --no-cache
ENV PATH="/usr/share/elasticsearch/bin:$PATH"
ENV PATH="/usr/share/logstash/bin:$PATH"
ENV PATH="/usr/share/kibana/bin:$PATH"
ENV JAVA_HOME="/usr/lib/jvm/java-1.8-openjdk"
#   Add custom elasticsearch config
COPY config/elastic /usr/share/elasticsearch/config
COPY config/elastic/logrotate /etc/logrotate.d/elasticsearch
#   Add custom logstash config
COPY config/logstash/conf.d/ /etc/logstash/conf.d/
COPY config/logstash/patterns/ /opt/logstash/patterns/
COPY config/logstash/logstash.yml /etc/logstash/
#   necessary for 5.0+ (overriden via "--path.settings", ignored by < 5.0)
ENV LS_SETTINGS_DIR="/etc/logstash"
#   Add custom nginx config
COPY config/nginx/nginx.conf /etc/nginx/nginx.conf
COPY config/nginx/kibana.conf /etc/nginx/conf.d/
COPY config/nginx/ssl.kibana.conf /etc/nginx/conf.d/
#   Add custom supervisor config
COPY config/supervisord/supervisord.conf /etc/supervisor/
#   Add entrypoints
COPY entrypoints/elastic-entrypoint.sh /
COPY entrypoints/logstash-entrypoint.sh /
COPY entrypoints/kibana-entrypoint.sh /
COPY entrypoints/nginx-entrypoint.sh /
VOLUME ["/usr/share/elasticsearch/data"]
VOLUME ["/etc/logstash/conf.d"]
VOLUME ["/etc/nginx"]
EXPOSE 80/tcp 443/tcp 5601/tcp 9200/tcp 9300/tcp
CMD ["/sbin/tini", "--", "/usr/bin/supervisord", "-c", "/etc/supervisor/supervisord.conf"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
