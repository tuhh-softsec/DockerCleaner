#   container for rsyslog development
#   creates the build environment
FROM ubuntu:16.04
ENV DEBIAN_FRONTEND="noninteractive"
RUN : \
 && apt-get upgrade -y
RUN (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-9 autoconf-archive=20150925-1 automake=1:1.15-4ubuntu1 autotools-dev=20150820.1 bison=2:3.0.4.dfsg-1 clang=1:3.8-33ubuntu3.1 curl=7.47.0-1ubuntu2.19 default-jdk=2:1.8-56ubuntu2 faketime=0.9.6-4 flex=2.6.0-11 gdb=7.11.1-0ubuntu1~16.5 git=1:2.7.4-0ubuntu1.10 libbson-dev=1.3.1-1 libcurl4-gnutls-dev=7.47.0-1ubuntu2.19 libdbd-mysql=0.9.0-3ubuntu2 libdbi-dev=0.9.0-4 libgcrypt11-dev=1.5.4-3+really1.6.5-2ubuntu0.6 libglib2.0-dev=2.48.2-0ubuntu4.8 libgnutls28-dev=3.4.10-4ubuntu1.9 libgrok1=1.20110708.1-4.1ubuntu1 libgrok-dev=1.20110708.1-4.1ubuntu1 libhiredis-dev=0.13.3-2 libkrb5-dev=1.13.2+dfsg-5ubuntu2.2 liblz4-dev=0.0~r131-2ubuntu2 libmaxminddb-dev=1.0.4-2.1 libmongoc-dev=1.3.1-1 libmysqlclient-dev=5.7.33-0ubuntu0.16.04.1 libnet1-dev=1.1.6+dfsg-3 libpcap-dev=1.7.4-2ubuntu0.1 libpq-dev=9.5.25-0ubuntu0.16.04.1 librabbitmq-dev=0.7.1-1ubuntu0.2 libsasl2-dev=2.1.26.dfsg1-14ubuntu0.2 libsnmp-dev=5.7.3+dfsg-1ubuntu4.6 libssl-dev=1.0.2g-1ubuntu4.20 libsystemd-dev=229-4ubuntu21.31 libtokyocabinet-dev=1.4.48-10 libtool=2.4.6-0.1 mysql-server=5.7.33-0ubuntu0.16.04.1 net-tools=1.60-26ubuntu1 pkg-config=0.29.1-0ubuntu1 postgresql-client=9.5+173ubuntu0.3 python-docutils=0.12+dfsg-1 python-software-properties=0.96.20.10 software-properties-common=0.96.20.10 sudo=1.8.16-0ubuntu1.10 tcl-dev=8.6.0+9 uuid-dev=2.27.1-6ubuntu3.10 valgrind=1:3.11.0-1ubuntu4.2 vim=2:7.4.1689-3ubuntu1.5 wget=1.17.1-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y )
RUN add-apt-repository ppa:adiscon/v8-stable -y \
 && add-apt-repository ppa:qpid/released -y \
 && add-apt-repository ppa:ubuntu-toolchain-r/test -y \
 && echo "deb http://repo.yandex.ru/clickhouse/deb/stable/ main/" > /etc/apt/sources.list.d/clickhouse.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv E0C56BD4 \
 && echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main" > /etc/apt/sources.list.d/llvm.list \
 && echo "deb http://download.opensuse.org/repositories/network:/messaging:/zeromq:/git-draft/xUbuntu_16.04/ ./" > /etc/apt/sources.list.d/0mq.list \
 && wget -O - http://download.opensuse.org/repositories/network:/messaging:/zeromq:/git-draft/xUbuntu_16.04/Release.key | apt-key add - \
 && wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
#   note: ppa:ubuntu-toolchain-r/test is currently the best repo for gcc-7 we can find...
RUN apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends clickhouse-client clickhouse-server libestr-dev=0.1.10-1 librelp-dev=1.2.9-1build1 libqpid-proton10-dev libsodium-dev=1.0.8-5 libfastjson-dev liblogging-stdlog-dev=1.0.5-2 gcc-7 libczmq-dev=3.0.2-5 clang-5.0=1:5.0-3~16.04.1 clang-tools-5.0 liblognorm-dev=1.1.2-1.1 -y )
#   for clickhouse, the container requires some pre-generated files for TLS. generate them via
#   openssl req -subj "/CN=localhost" -new -newkey rsa:2048 -days 365 -nodes -x509 -keyout clickhouse.server.key -out clickhouse.server.crt
#   openssl dhparam -out clickhouse.dhparam.pem 2048
COPY clickhouse.dhparam.pem /etc/clickhouse-server/dhparam.pem
COPY clickhouse.server.crt /etc/clickhouse-server/server.crt
COPY clickhouse.server.key /etc/clickhouse-server/server.key
RUN sed -i 's/<yandex>/<yandex>\n <core_dump><size_limit>0<\/size_limit><\/core_dump>/g' /etc/clickhouse-server/config.xml \
 && sed -i 's/<tcp_port>9000<\/tcp_port>/<tcp_port>9000<\/tcp_port>\n <https_port>8443<\/https_port>/g' /etc/clickhouse-server/config.xml
WORKDIR /home/devel
VOLUME /rsyslog
RUN groupadd rsyslog \
 && useradd -g rsyslog -s /bin/bash rsyslog \
 && echo "rsyslog ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers \
 && echo "buildbot ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
COPY setup-system.sh setup-system.sh
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig" \
    LD_LIBRARY_PATH="/usr/local/lib" \
    DEBIAN_FRONTEND=""
#   Install any needed packages
RUN ./setup-system.sh
#   create dependency cache
RUN mkdir /local_dep_cache \
 && wget -nv https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-5.6.9.tar.gz -O /local_dep_cache/elasticsearch-5.6.9.tar.gz \
 && wget -nv https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-6.0.0.tar.gz -O /local_dep_cache/elasticsearch-6.0.0.tar.gz \
 && wget -nv https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-6.3.1.tar.gz -O /local_dep_cache/elasticsearch-6.3.1.tar.gz
#   tell tests which are the newester versions, so they can be checked without the need
#   to adjust test sources.
ENV ELASTICSEARCH_NEWEST="elasticsearch-6.3.1.tar.gz"
#   tell CI env how to handle clickhouse
RUN chown root:root /var/lib/clickhouse
ENV CLICKHOUSE_START_CMD="sudo -S clickhouse-server --config-file=/etc/clickhouse-server/config.xml" \
    CLICKHOUSE_STOP_CMD="sudo -S kill $(pidof clickhouse-server)"
#   next ENV is specifically for running scan-build - so we do not need to
#   change scripts if at a later time we can move on to a newer version
ENV SCAN_BUILD="scan-build" \
    SCAN_BUILD_CC="clang-5.0"
ENV RSYSLOG_CONFIGURE_OPTIONS="--enable-clickhouse  --enable-clickhouse-tests  --enable-elasticsearch  --enable-elasticsearch-tests  --enable-gnutls  --enable-gssapi-krb5  --enable-imbatchreport  --enable-imczmq  --enable-imdiag  --enable-imfile  --enable-imjournal  --enable-imkafka  --enable-impcap  --enable-improg  --enable-impstats  --enable-imptcp  --enable-imtuxedoulog  --enable-kafka-tests  --disable-kmsg  --enable-ksi-ls12  --enable-libdbi  --enable-libfaketime  --enable-libgcrypt  --enable-mail  --enable-mmanon  --enable-mmaudit  --enable-mmcapture  --enable-mmcapture  --enable-mmcount  --enable-mmdarwin  --enable-mmdblookup  --enable-mmfields  --enable-mmgrok  --enable-mmjsonparse  --enable-mmkubernetes  --enable-mmnormalize  --enable-mmpstrucdata  --enable-mmrm1stspace  --enable-mmsequence  --enable-mmsnmptrapd  --enable-mmutf8fix  --enable-mysql  --enable-omamqp1  --enable-omczmq  --enable-omhiredis  --enable-omhttp  --enable-omhttp  --enable-omhttpfs  --enable-omjournal  --enable-omkafka  --enable-ommongodb  --enable-omprog  --enable-omrabbitmq  --enable-omrelp-default-port=13515  --enable-omruleset  --enable-omstdout  --enable-omtcl  --enable-omudpspoof  --enable-omuxsock  --enable-openssl  --enable-pgsql  --enable-pmaixforwardedfrom  --enable-pmciscoios  --enable-pmcisconames  --enable-pmdb2diag  --enable-pmlastmsg  --enable-pmnormalize  --enable-pmnull  --enable-pmsnare  --enable-relp  --enable-snmp  --enable-usertools  --enable-valgrind  --enable-testbench"
WORKDIR /rsyslog
USER rsyslog
# Please add your HEALTHCHECK here!!!
