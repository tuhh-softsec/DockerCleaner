#   container for rsyslog development
#   creates the build environment
FROM ubuntu:18.04
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends autoconf=2.69-11 autoconf-archive=20170928-2 automake=1:1.15.1-3ubuntu2 autotools-dev=20180224.1 bison=2:3.0.4.dfsg-1build1 clang=1:6.0-41~exp5~ubuntu1 clang-tools=1:6.0-41~exp5~ubuntu1 curl=7.58.0-2ubuntu3.24 default-jdk=2:1.11-68ubuntu1~18.04.1 default-jre=2:1.11-68ubuntu1~18.04.1 faketime=0.9.7-2 libdbd-mysql=0.9.0-5ubuntu2 flex=2.6.4-6 gcc=4:7.4.0-1ubuntu2.3 gcc-8=8.4.0-1ubuntu1~18.04 gdb=8.1.1-0ubuntu1 git=1:2.17.1-1ubuntu0.17 libbson-dev=1.9.2-1 libcurl4-gnutls-dev=7.58.0-2ubuntu3.24 libdbi-dev=0.9.0-5 libgcrypt11-dev=1.5.4-3+really1.8.1-4ubuntu1.3 libglib2.0-dev=2.56.4-0ubuntu0.18.04.9 libgnutls28-dev=3.5.18-1ubuntu1.6 libgrok1=1.20110708.1-4.3ubuntu1 libgrok-dev=1.20110708.1-4.3ubuntu1 libhiredis-dev=0.13.3-2.2 libkrb5-dev=1.16-2ubuntu0.4 liblz4-dev=0.0~r131-2ubuntu3.1 libmaxminddb-dev=1.3.1-1 libmongoc-dev=1.9.2+dfsg-1build1 libmongoc-dev=1.9.2+dfsg-1build1 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 libnet1-dev=1.1.6+dfsg-3.1 libsnmp-dev=5.7.3+dfsg-1.8ubuntu3.8 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libsasl2-dev=2.1.27~101-g0780600+dfsg-3ubuntu2.4 libsystemd-dev=237-3ubuntu10.57 libtokyocabinet-dev=1.4.48-11 libtool=2.4.6-2 libtool-bin=2.4.6-2 lsof=4.89+dfsg-0.1 make=4.1-9.1ubuntu1 mysql-server=5.7.41-0ubuntu0.18.04.1 net-tools=1.60+git20161116.90da8a0-1ubuntu1 pkg-config=0.29.1-0ubuntu2 postgresql-client=10+190ubuntu0.1 libpq-dev=10.23-0ubuntu0.18.04.1 python-docutils=0.14+dfsg-3 python-pip=9.0.1-2.3~ubuntu1.18.04.8 software-properties-common=0.96.24.32.20 sudo=1.8.21p2-3ubuntu1.5 uuid-dev=2.31.1-0.4ubuntu3.7 valgrind=1:3.13.0-2ubuntu2.3 vim=2:8.0.1453-1ubuntu1.11 wget=1.19.4-1ubuntu2.2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y
ENV REBUILD="1"
RUN echo "deb http://download.opensuse.org/repositories/network:/messaging:/zeromq:/git-draft/xUbuntu_18.04/ ./" > /etc/apt/sources.list.d/0mq.list \
 && wget -nv -O - http://download.opensuse.org/repositories/network:/messaging:/zeromq:/git-draft/xUbuntu_18.04/Release.key | apt-key add - \
 && add-apt-repository ppa:adiscon/v8-stable -y \
 && apt-get update -y \
 && apt-get install --no-install-recommends libczmq-dev=4.1.0-2 libqpid-proton8-dev=0.14.0-5.1ubuntu1 tcl-dev=8.6.0+9 libsodium-dev=1.0.16-2 libestr-dev=0.1.10-2.1 librelp-dev=1.2.14-3 libfastjson-dev=0.99.8-2 liblogging-stdlog-dev=1.0.6-3 liblognorm-dev=2.0.3-1 -y
WORKDIR /home/devel
VOLUME /rsyslog
RUN groupadd rsyslog \
 && useradd -g rsyslog -s /bin/bash rsyslog \
 && echo "rsyslog ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers \
 && echo "buildbot ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
#   mysql needs a little help:
RUN mkdir -p /var/run/mysqld \
 && chown mysql:mysql /var/run/mysqld
ENV MYSQLD_START_CMD="sudo mysqld_safe" \
    MYSQLD_STOP_CMD="sudo kill $(sudo cat /var/run/mysqld/mysqld.pid)"
COPY setup-system.sh setup-system.sh
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig" \
    LD_LIBRARY_PATH="/usr/local/lib" \
    DEBIAN_FRONTEND="" \
    SUDO="sudo -S"
#   Install any needed packages
RUN ./setup-system.sh
#   other manual installs
#   kafkacat
RUN cd helper-projects \
 && git clone https://github.com/edenhill/kafkacat \
 && cd kafkacat \
 && (unset CFLAGS ;./configure --prefix=/usr --CFLAGS="-g" ;make -j2 ) \
 && make install \
 && cd .. \
 && cd ..
#   Note: we do NOT delete the source as we may need it to
#   uninstall (in case the user wants to go back to system-default)
#   create dependency cache
RUN mkdir /local_dep_cache \
 && wget -nv https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-5.6.9.tar.gz -O /local_dep_cache/elasticsearch-5.6.9.tar.gz \
 && wget -nv https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-6.0.0.tar.gz -O /local_dep_cache/elasticsearch-6.0.0.tar.gz \
 && wget -nv https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-6.3.1.tar.gz -O /local_dep_cache/elasticsearch-6.3.1.tar.gz
#   tell tests which are the newester versions, so they can be checked without the need
#   to adjust test sources.
ENV ELASTICSEARCH_NEWEST="elasticsearch-6.3.1.tar.gz"
#   next ENV is specifically for running scan-build - so we do not need to
#   change scripts if at a later time we can move on to a newer version
ENV SCAN_BUILD="scan-build" \
    SCAN_BUILD_CC="clang-6.0" \
    ASAN_SYMBOLIZER_PATH="/usr/lib/llvm-6.0/bin/llvm-symbolizer"
ENV RSYSLOG_CONFIGURE_OPTIONS="--enable-elasticsearch  --enable-elasticsearch-tests  --enable-gnutls  --enable-gssapi-krb5  --enable-imczmq  --enable-imdiag  --enable-imfile  --enable-imjournal  --enable-imkafka  --enable-impstats  --enable-imptcp  --enable-kafka-tests  --enable-ksi-ls12  --enable-libdbi  --enable-libfaketime  --enable-libgcrypt  --enable-mail  --enable-mmanon  --enable-mmaudit  --enable-mmcount  --enable-mmdblookup  --enable-mmfields  --enable-mmgrok  --enable-mmjsonparse  --enable-mmkubernetes  --enable-mmnormalize  --enable-mmpstrucdata  --enable-mmrm1stspace  --enable-mmsequence  --enable-mmsnmptrapd  --enable-mmutf8fix  --enable-mysql  --enable-mysql-tests  --enable-omamqp1  --enable-omczmq  --enable-omhiredis  --enable-omhiredis  --enable-omhttpfs  --enable-omjournal  --enable-omkafka  --enable-ommongodb  --enable-omprog  --enable-omrelp-default-port=13515  --enable-omruleset  --enable-omstdout  --enable-omtcl  --enable-omudpspoof  --enable-omuxsock  --enable-openssl  --enable-pgsql  --enable-pmaixforwardedfrom  --enable-pmciscoios  --enable-pmcisconames  --enable-pmlastmsg  --enable-pmnormalize  --enable-pmnull  --enable-pmsnare  --enable-relp  --enable-snmp  --enable-usertools  --enable-valgrind  --enable-testbench"
#   module needs fixes: --enable-kmsg
VOLUME /var/lib/mysql
WORKDIR /rsyslog
USER rsyslog
# Please add your HEALTHCHECK here!!!
