FROM alpine:3.8 AS builder
LABEL maintainer="\"https://github.com/blacktop\""
ENV ZEEK_VERSION="2.6.1"
RUN apk add zlib=1.2.11-r1 openssl=1.0.2u-r0 libstdc++=6.4.0-r9 libpcap=1.8.1-r1 libgcc=6.4.0-r9 --no-cache
RUN apk add libmaxminddb-dev=1.3.2-r0 linux-headers=4.4.6-r2 openssl-dev=1.0.2u-r0 libpcap-dev=1.8.1-r1 python-dev zlib-dev=1.2.11-r1 binutils=2.30-r6 fts-dev=1.2.7-r1 cmake=3.11.1-r2 clang=5.0.1-r1 bison=3.0.4-r1 bash=4.4.19-r1 swig=3.0.12-r3 perl=5.26.3-r0 make=4.2.1-r2 flex=2.6.4-r1 git=2.18.4-r0 g++=6.4.0-r9 fts=1.2.7-r1 --no-cache -t .build-deps
RUN echo "===> Cloning zeek..." \
 && cd /tmp \
 && git clone --recursive https://github.com/zeek/zeek.git
#   && git clone --branch v$ZEEK_VERSION https://github.com/zeek/zeek.git
RUN echo "===> Compiling zeek..." \
 && cd /tmp/zeek \
 && CC=clang ./configure --prefix=/usr/local/bro --build-type=MinSizeRel --disable-broker-tests --disable-zeekctl --disable-auxtools --disable-python \
 && make -j 2 \
 && make install
RUN echo "===> Compiling af_packet plugin..." \
 && cd /tmp/zeek/aux/ \
 && git clone https://github.com/J-Gras/bro-af_packet-plugin.git \
 && cd /tmp/zeek/aux/bro-af_packet-plugin \
 && find . -name "*.bro" -exec sh -c 'mv "$1" "${1%.bro}.zeek"' _ {}
RUN echo "===> Installing hosom/file-extraction package..." \
 && cd /tmp \
 && git clone https://github.com/hosom/file-extraction.git \
 && find file-extraction -name "*.bro" -exec sh -c 'mv "$1" "${1%.bro}.zeek"' _ {}
RUN echo "===> Installing corelight/json-streaming-logs package..." \
 && cd /tmp \
 && git clone https://github.com/corelight/json-streaming-logs.git json-streaming-logs \
 && find json-streaming-logs -name "*.bro" -exec sh -c 'mv "$1" "${1%.bro}.zeek"' _ {}
RUN echo "===> Shrinking image..." \
 && strip -s /usr/local/bro/bin/bro
#  ###################################################################################################
FROM alpine:3.8 AS geoip
ENV MAXMIND_CITY="https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz"
ENV MAXMIND_CNTRY="https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz"
ENV MAXMIND_ASN="http://geolite.maxmind.com/download/geoip/database/GeoLite2-ASN.tar.gz"
ENV GITHUB_CITY="https://github.com/blacktop/docker-zeek/raw/master/maxmind/GeoLite2-City.tar.gz"
ENV GITHUB_CNTRY="https://github.com/blacktop/docker-zeek/raw/master/maxmind/GeoLite2-Country.tar.gz"
#   Install the GeoIPLite Database
RUN cd /tmp \
 && mkdir -p /usr/share/GeoIP \
 && wget ${GITHUB_CITY} \
 && tar xzvf GeoLite2-City.tar.gz \
 && mv GeoLite2-City*/GeoLite2-City.mmdb /usr/share/GeoIP/
#   && wget ${MAXMIND_ASN} \
#   && tar xzvf GeoLite2-ASN.tar.gz \
#   && mv GeoLite2-ASN*/GeoLite2-ASN.mmdb /usr/share/GeoIP/
#  ###################################################################################################
FROM alpine:3.8
LABEL maintainer="\"https://github.com/blacktop\""
RUN apk add ca-certificates=20191127-r2 zlib=1.2.11-r1 openssl=1.0.2u-r0 libstdc++=6.4.0-r9 libpcap=1.8.1-r1 libgcc=6.4.0-r9 fts=1.2.7-r1 libmaxminddb=1.3.2-r0 --no-cache
COPY --from=builder /usr/local/bro /usr/local/bro
COPY local.bro /usr/local/bro/share/bro/site/local.zeek
#   Add a few zeek scripts
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /usr/local/bro/share/bro/site/geodata/conn-add-geodata.bro https://raw.githubusercontent.com/blacktop/docker-zeek/master/scripts/conn-add-geodata.bro
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /usr/local/bro/share/bro/site/passwords/log-passwords.bro https://raw.githubusercontent.com/blacktop/docker-zeek/master/scripts/log-passwords.bro
ENV BROPATH=".:/data/config:/usr/local/bro/share/bro:/usr/local/bro/share/bro/policy:/usr/local/bro/share/bro/site"
ENV PATH="$PATH:/usr/local/bro/bin"
COPY --from=geoip /usr/share/GeoIP /usr/share/GeoIP
WORKDIR /pcap
ENTRYPOINT ["bro"]
CMD ["-h"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
