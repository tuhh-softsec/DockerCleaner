#
#   Docker file to build a Bro image pre loeaded with Bro and Bro-Kafka plugin.
#   This is used in the Bro/Logisland tutorial: Indexing Bro events.
#
FROM ubuntu:16.04
MAINTAINER Hurence version: 0.1
#   Update apt sources
RUN :
USER root
#
#   Define where things will be installed
#
#   Base dir for work is the user directory 
ARG HOME_DIR=/root
#   Where to put sources to compile
ARG SRC_DIR=${HOME_DIR}/sources
#   Where to put .pcap files
ARG PCAP_DIR=${HOME_DIR}/pcap_files
#   Base install dir for binaries
ARG INSTALL_DIR=/usr/local
#   Installed Bro base dir
ARG BRO_BASE_DIR=${INSTALL_DIR}/bro
#
#   Create needed base directories
#
RUN mkdir -p ${SRC_DIR}
RUN mkdir -p ${PCAP_DIR}
RUN mkdir -p ${BRO_BASE_DIR}
#
#   Install dev tools
#
#   Note: apt-get install -y avoids prompting for confirmation
#   Git
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 -y )
#   make, gcc...
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.1ubuntu2 -y )
#   Special additional components needed for building bro workspace
RUN (apt-get update ;apt-get install --no-install-recommends python-dev=2.7.12-1~16.04 flex=2.6.0-11 bison=2:3.0.4.dfsg-1 libpcap-dev=1.7.4-2ubuntu0.1 libssl-dev=1.0.2g-1ubuntu4.20 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 flex=2.6.0-11 bison=2:3.0.4.dfsg-1 swig=3.0.8-0ubuntu3 -y )
#   CMake
RUN (apt-get update ;apt-get install --no-install-recommends cmake=3.5.1-1ubuntu3 -y )
#
#   Install sources, build and install Bro, librdkafka and the Bro-Kafka plugin
#
#   Download, build and install Bro 
RUN cd ${SRC_DIR} ; git clone --recursive https://github.com/bro/bro.git ; cd bro ; ./configure --prefix=${BRO_BASE_DIR} ; make ; make install
#   Download, build and install librdkafka 
RUN cd ${SRC_DIR} ; git clone https://github.com/edenhill/librdkafka.git ; cd librdkafka ; ./configure --prefix=${INSTALL_DIR} ; make ; make install
#   Download, build and install Bro-Kafka plugin 
RUN cd ${SRC_DIR} ; git clone https://github.com/apache/metron-bro-plugin-kafka.git ; cd metron-bro-plugin-kafka ; ./configure --install-root=${BRO_BASE_DIR}/lib/bro/plugins --bro-dist=${SRC_DIR}/bro --with-librdkafka=${INSTALL_DIR}/lib ; make ; make install
#
#   Install additional tools needed for the bro tutorial
#
#   Install curl for accessing Logisland ElasticSearch
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 -y )
#   Install vi to be able to edit bro config files
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:7.4.1689-3ubuntu1.5 -y )
#   Install ifconfig to be able to check container ip
RUN (apt-get update ;apt-get install --no-install-recommends net-tools=1.60-26ubuntu1 -y )
#   Install ping requests to easily generate a DNS query
RUN (apt-get update ;apt-get install --no-install-recommends iputils-ping=3:20121221-5ubuntu2 -y )
#
#   Define environment variables
#
#   Bro
ENV BRO_HOME="${BRO_BASE_DIR}"
#   Update path for bro binary
ENV PATH="${BRO_HOME}/bin:${PATH}"
#   Pcap files
ENV PCAP_HOME="${PCAP_DIR}"
#
#   Copy .pcap files
#
#   ssh.pcap comes from https://www.bro.org/bro-workshop-2011/solutions/notices/
#   It is used to be able to easily generate a Bro notice (password guessing)
COPY ssh.pcap ${PCAP_DIR}
WORKDIR /root
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
