FROM alpine
#
#   Include dist
COPY dist/ /root/dist/
#
#   Install packages
#  RUN sed -i 's/dl-cdn/dl-2/g' /etc/apk/repositories && \
RUN apk add ca-certificates=20220614-r4 curl=7.88.1-r1 file=5.43-r0 geoip=1.6.12-r3 hiredis=1.0.2-r1 jansson=2.14-r0 libcap-ng=0.8.3-r1 libhtp=0.5.41-r0 libmagic=5.43-r0 libnet=1.2-r1 libnetfilter_queue=1.0.5-r1 libnfnetlink=1.0.2-r0 libpcap=1.10.1-r1 luajit=2.1_p20210510-r3 lz4-libs=1.9.4-r1 musl=1.2.3-r4 nspr=4.35-r0 nss=3.85-r1 pcre=8.45-r2 yaml=0.2.5-r0 wget=1.21.3-r2 automake=1.16.5-r1 autoconf=2.71-r1 build-base=0.5-r3 cargo=1.64.0-r2 file-dev=5.43-r0 geoip-dev=1.6.12-r3 hiredis-dev=1.0.2-r1 jansson-dev=2.14-r0 libtool=2.4.7-r1 libhtp-dev=0.5.41-r0 libcap-ng-dev=0.8.3-r1 luajit-dev=2.1_p20210510-r3 libpcap-dev=1.10.1-r1 libnet-dev=1.2-r1 libnetfilter_queue-dev=1.0.5-r1 libnfnetlink-dev=1.0.2-r0 lz4-dev=1.9.4-r1 nss-dev=3.85-r1 nspr-dev=4.35-r0 pcre-dev=8.45-r2 python2 py2-pip rust=1.64.0-r2 yaml-dev=0.2.5-r0 -U --no-cache \
 && pip install pip==23.1 --no-cache-dir --upgrade \
 && pip install suricata-update==1.2.6 --no-cache-dir \
 && mkdir -p /opt/builder/ \
 && wget https://www.openinfosecfoundation.org/download/suricata-4.1.4.tar.gz \
 && tar xvfz suricata-4.1.4.tar.gz --strip-components=1 -C /opt/builder/ \
 && rm suricata-4.1.4.tar.gz \
 && cd /opt/builder \
 && ./configure --prefix=/usr --sysconfdir=/etc --mandir=/usr/share/man --localstatedir=/var --enable-non-bundled-htp --enable-nfqueue --enable-rust --disable-gccmarch-native --enable-hiredis --enable-geoip --enable-gccprotect --enable-pie --enable-luajit \
 && make \
 && make check \
 && make install \
 && make install-full \
 && addgroup -g 2000 suri \
 && adduser -S -H -u 2000 -D -g 2000 suri \
 && chmod 644 /etc/suricata/*.config \
 && cp /root/dist/suricata.yaml /etc/suricata/suricata.yaml \
 && cp /root/dist/*.bpf /etc/suricata/ \
 && mkdir -p /etc/suricata/rules \
 && cp /opt/builder/rules/* /etc/suricata/rules/ \
 && cp /root/dist/update.sh /usr/bin/ \
 && chmod 755 /usr/bin/update.sh \
 && update.sh OPEN \
 && apk del --purge automake autoconf build-base cargo file-dev geoip-dev hiredis-dev jansson-dev libtool libhtp-dev libcap-ng-dev luajit-dev libpcap-dev libnet-dev libnetfilter_queue-dev libnfnetlink-dev lz4-dev nss-dev nspr-dev pcre-dev python2 py2-pip rust yaml-dev \
 && rm -rf /opt/builder \
 && rm -rf /root/* \
 && rm -rf /var/cache/apk/*
#
#   Start suricata
STOPSIGNAL SIGINT
CMD SURICATA_CAPTURE_FILTER=$( update.sh $OINKCODE ;) \
 && exec suricata -v -F $SURICATA_CAPTURE_FILTER -i $( /sbin/ip address | grep '^2: ' | awk '{ print $2 }' | tr -d [:punct:] ;)
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
