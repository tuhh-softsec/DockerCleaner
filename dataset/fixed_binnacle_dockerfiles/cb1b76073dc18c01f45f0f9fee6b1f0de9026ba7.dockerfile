FROM alpine:3.7
LABEL maintainer="\"https://github.com/blacktop\""
ENV BRO_VERSION="2.5.1"
COPY patches /tmp
RUN apk add zlib=1.2.11-r1 openssl=1.0.2t-r0 libstdc++=6.4.0-r5 libpcap=1.8.1-r1 geoip=1.6.11-r0 libgcc=6.4.0-r5 tini=0.16.1-r0 curl=7.61.1-r3 inotify-tools=3.14-r2 jq=1.5-r5 bind-tools=9.11.8-r0 --no-cache
RUN apk add linux-headers=4.4.6-r2 openssl-dev=1.0.2t-r0 libpcap-dev=1.8.1-r1 python-dev geoip-dev=1.6.11-r0 zlib-dev=1.2.11-r1 binutils=2.30-r2 curl-dev=7.61.1-r3 fts-dev=1.2.7-r0 cmake=3.9.5-r0 clang=5.0.0-r0 bison=3.0.4-r0 perl=5.26.3-r0 make=4.2.1-r0 flex=2.6.4-r1 git=2.15.4-r0 g++=6.4.0-r5 fts=1.2.7-r0 --no-cache -t .build-deps \
 && cd /tmp \
 && git clone --recursive --branch v$BRO_VERSION git://git.bro.org/bro \
 && echo "===> Applying patches..." \
 && cd /tmp/bro \
 && patch -p1 < /tmp/bro-musl.patch \
 && cp /tmp/FindFTS.cmake cmake \
 && cd /tmp/bro/aux/binpac \
 && patch -p1 < /tmp/binpac-musl.patch \
 && echo "===> Compiling bro..." \
 && cd /tmp/bro \
 && CC=clang ./configure --disable-broker --disable-broctl --disable-broccoli --disable-auxtools --prefix=/usr/local \
 && make \
 && make install \
 && echo "===> Compiling af_packet plugin..." \
 && cd /tmp/bro/aux/plugins \
 && git clone https://github.com/J-Gras/bro-af_packet-plugin \
 && cd /tmp/bro/aux/plugins/bro-af_packet-plugin \
 && make distclean \
 && CC=clang ./configure --with-kernel=/usr \
 && make \
 && make install \
 && echo "===> Compiling elasticsearch plugin..." \
 && git clone -b release https://github.com/bro/bro-plugins.git /tmp/plugins \
 && mv /tmp/plugins/elasticsearch /tmp/bro/aux/plugins/elasticsearch \
 && echo "* Tweaks to improve elasticsearch plugin's performance in docker" \
 && cd /tmp/bro/aux/plugins/elasticsearch/scripts \
 && patch -p1 < /tmp/esplung-init.patch \
 && cd /tmp/bro/aux/plugins/elasticsearch/src \
 && patch -p1 < /tmp/ElasticSearch.patch \
 && cd /tmp/bro/aux/plugins/elasticsearch/scripts/Bro/ElasticSearch \
 && patch -p1 < /tmp/logs-to-elasticsearch.patch \
 && echo "* Tweaks protocols to improve indexing logs into elasticsearch" \
 && PROTOS=/usr/local/share/bro/base/protocols \
 && sed -i "s/version: count \&log/socks_version: count \&log/g" $PROTOS/socks/main.bro \
 && sed -i "s/$version=/$socks_version=/g" $PROTOS/socks/main.bro \
 && sed -i "s/version: string \&log/ssl_version: string \&log/g" $PROTOS/ssl/main.bro \
 && sed -i "s/$version=/$ssl_version=/g" $PROTOS/ssl/main.bro \
 && sed -i "s/version: count \&log/ssh_version: count \&log/g" $PROTOS/ssh/main.bro \
 && sed -i "s/$version =/$ssh_version =/g" $PROTOS/ssh/main.bro \
 && sed -i "s/version: string \&log/snmp_version: string \&log/g" $PROTOS/snmp/main.bro \
 && sed -i "s/$version=/$snmp_version=/g" $PROTOS/snmp/main.bro \
 && echo "* Build elasticsearch plugin" \
 && cd /tmp/bro/aux/plugins/elasticsearch \
 && CC=clang ./configure \
 && make \
 && make install \
 && echo "* Stop local logging to prevent filling up the container with logs" \
 && sed -i "s/WRITER_ASCII/WRITER_NONE/g" /usr/local/share/bro/base/frameworks/logging/main.bro \
 && echo "===> Shrinking image..." \
 && strip -s /usr/local/bin/bro \
 && rm -rf /tmp/* \
 && apk del --purge .build-deps
RUN echo "===> Check if elasticsearch plugin installed..." \
 && /usr/local/bin/bro -N Bro::ElasticSearch
#   Install the GeoIPLite Database
RUN mkdir -p /usr/share/GeoIP/ \
 && GEOIP=geolite.maxmind.com/download/geoip/database \
 && curl -s http://${GEOIP}/GeoLiteCity.dat.gz | zcat > /usr/share/GeoIP/GeoIPCity.dat \
 && curl -s http://${GEOIP}/GeoLiteCityv6-beta/GeoLiteCityv6.dat.gz | zcat > /usr/share/GeoIP/GeoIPCityv6.dat
ENV BROPATH=".:/data/config:/usr/local/share/bro:/usr/local/share/bro/policy:/usr/local/share/bro/site"
WORKDIR /pcap
#   Add some scripts
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /usr/local/share/bro/passwords/log-passwords.bro https://github.com/blacktop/docker-bro/raw/master/scripts/log-passwords.bro
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /usr/local/share/bro/geodata/conn-add-geodata.bro https://github.com/blacktop/docker-bro/raw/master/scripts/conn-add-geodata.bro
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /usr/local/share/bro/zip/zipfilenames.bro https://github.com/moshekaplan/bro_zipfilenames/raw/master/zipfilenames.bro
COPY local.bro /usr/local/share/bro/site/local.bro
COPY template.json /template.json
COPY index-pattern.json /index-pattern.json
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/sbin/tini", "/entrypoint.sh"]
CMD ["-h"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
