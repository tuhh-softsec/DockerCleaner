#   PanteraS
#   Platform as a Service based on Paas in a box project
#
FROM ubuntu:16.04
MAINTAINER Wojciech Sielski "wsielski@team.mobile.de"
RUN apt-get update \
 && apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 apt-transport-https=1.2.35 python-pip=8.1.1-2ubuntu0.6 wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 unzip=6.0-20ubuntu1.1 dnsutils=1:9.10.3.dfsg.P4-8ubuntu1.19 vim=2:7.4.1689-3ubuntu1.5 git=1:2.7.4-0ubuntu1.10 lolcat=42.0.99-1 toilet=0.3-1 jshon=20131010-3 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN locale-gen en_US.UTF-8
ENV DEBIAN_FRONTEND="noninteractive"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
ENV TERM="xterm"
ENV HOME="/root"
ENV GOPATH="${HOME}/go"
ENV SUPERVISORD_APP_VERSION="4.0.0"
ENV DOCKER_APP_VERSION="5:18.09.3~3-0~ubuntu-xenial"
ENV CONSUL_APP_VERSION="1.4.2"
ENV MESOS_APP_VERSION="1.7.2-2.0.1"
#  ENV MARATHON_APP_VERSION          1.7.189-0.1.20190125223314.ubuntu1604
ENV MARATHON_APP_VERSION="1.7.189-48bfd6000"
ENV REGISTRATOR_APP_VERSION="v7"
ENV FABIO_APP_VERSION="1.5.11"
ENV FABIO_GO_APP_VERSION="go1.11.5"
ENV NETDATA_APP_VERSION="1.12.2"
ENV DOCKER_HOST="unix:///tmp/docker.sock"
#   SupervisorD
#
#  RUN pip install --upgrade pip \
RUN pip install supervisor-stdout==0.1.1 \
 && pip install https://github.com/Supervisor/supervisor/archive/${SUPERVISORD_APP_VERSION}.zip
#   DNSMASQ
#
RUN apt-get update \
 && apt-get install --no-install-recommends dnsmasq=2.75-1ubuntu0.16.04.10 dnsutils=1:9.10.3.dfsg.P4-8ubuntu1.19 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   ZOOKEEPER MESOS MARATHON
#
RUN echo "deb http://repos.mesosphere.com/ubuntu/ xenial main" > /etc/apt/sources.list.d/mesosphere.list \
 && ln -sf /bin/true /bin/systemctl \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv DF7D54CBE56151BF \
 && apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jre=8u292-b10-0ubuntu1~16.04.1 mesos=${MESOS_APP_VERSION} -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && rm /etc/mesos/zk \
 && chown -R zookeeper /etc/zookeeper \
 && REAL_JAVA_PATH=$( readlink -f $( which java ;) ;) \
 && JAVA_DIR=${REAL_JAVA_PATH%/bin/java} \
 && mkdir -p ${JAVA_DIR}/conf/management/ \
 && touch ${JAVA_DIR}/conf/management/management.properties
#  ADD marathon_${MARATHON_APP_VERSION}.tgz /
#  RUN ln -s /usr/share/marathon/bin/marathon /usr/local/bin
RUN wget https://downloads.mesosphere.io/marathon/builds/${MARATHON_APP_VERSION}/marathon-${MARATHON_APP_VERSION}.tgz \
 && mkdir /opt/marathon \
 && tar -C /opt/marathon --strip 1 -zxf marathon-${MARATHON_APP_VERSION}.tgz \
 && rm marathon-${MARATHON_APP_VERSION}.tgz \
 && ln -s /opt/marathon/bin/marathon /usr/local/bin
COPY zkStart.sh /opt/zkStart.sh
#   DOCKER
#
RUN echo 'deb https://download.docker.com/linux/ubuntu xenial stable' > /etc/apt/sources.list.d/docker.list \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0EBFCD88 \
 && apt-get update \
 && apt-get install --no-install-recommends docker-ce-cli=${DOCKER_APP_VERSION} -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   REGISTRATOR
#
RUN mkdir /opt/registrator
ENV PATH="${PATH}:/opt/registrator"
WORKDIR /opt/registrator
#   Providing binaries by us
RUN wget https://github.com/eBayClassifiedsGroup/registrator/releases/download/${REGISTRATOR_APP_VERSION}/registrator.tgz \
 && tar zxf registrator.tgz \
 && rm registrator.tgz
#   CONSUL
#
RUN mkdir -p /opt/consul/data /etc/consul.d \
 && groupadd -g 3000 consul \
 && useradd -g 3000 -u 3000 -d /opt/consul -s /bin/false consul
ENV PATH="$PATH:/opt/consul"
WORKDIR /opt/consul
RUN wget https://releases.hashicorp.com/consul/${CONSUL_APP_VERSION}/consul_${CONSUL_APP_VERSION}_linux_amd64.zip \
 && unzip consul_${CONSUL_APP_VERSION}_linux_amd64.zip \
 && rm consul_*.zip \
 && chown -R consul:consul /opt/consul
#   FABIO
#
RUN mkdir /opt/fabio \
 && groupadd -g 3001 fabio \
 && useradd -g 3001 -u 3001 -d /opt/fabio -s /bin/false fabio
ENV PATH="${PATH}:/opt/fabio"
WORKDIR /opt/fabio
RUN wget https://github.com/eBay/fabio/releases/download/v${FABIO_APP_VERSION}/fabio-${FABIO_APP_VERSION}-${FABIO_GO_APP_VERSION}-linux_amd64 -O fabio \
 && chmod a+x fabio \
 && chown -R fabio:fabio /opt/fabio \
 && setcap 'cap_net_bind_service=+ep' ./fabio
#   NETDATA
RUN apt-get update \
 && apt-get install --no-install-recommends zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 gcc=4:5.3.1-1ubuntu1 make=4.1-6 autoconf=2.69-9 autogen=1:5.18.7-3 automake=1:1.15-4ubuntu1 pkg-config=0.29.1-0ubuntu1 uuid-dev=2.27.1-6ubuntu3.10 libmnl-dev=1.0.3-5 -y \
 && git clone -b v${NETDATA_APP_VERSION} --single-branch https://github.com/netdata/netdata.git /tmp/netdata.git --depth=1 \
 && cd /tmp/netdata.git \
 && ./netdata-installer.sh --dont-wait --install /opt \
 && cd /opt \
 && rm -rf /tmp/* \
 && ln -sf /dev/stdout /opt/netdata/var/log/netdata/access.log \
 && ln -sf /dev/stderr /opt/netdata/var/log/netdata/error.log \
 && apt-get -y remove zlib1g-dev gcc make autoconf autogen automake pkg-config uuid-dev libmnl-dev \
 && apt-get autoremove -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   PanteraS:
RUN gem install panteras_api --version 0.0.26
#   Font for logo
RUN wget https://raw.githubusercontent.com/patorjk/figlet.js/master/fonts/Elite.flf -O /usr/share/figlet/Elite.flf
COPY supervisord.conf /etc/supervisord.conf
COPY supervisord.sh /opt/
COPY panteras.http /etc/fabio/errors/
COPY fabio.properties /opt/fabio/fabio.properties
COPY logo.sh /etc/profile.d
COPY paas.sh /etc/profile.d
COPY bashrc /tmp
COPY version /opt/
#   example user
RUN groupadd -g 31337 ecgapp \
 && useradd -g 31337 -u 31337 -d /app -s /bin/false ecgapp
RUN groupadd -g 3003 marathon \
 && useradd -g 3003 -u 3003 -d /opt/marathon -s /bin/false marathon
RUN cat /tmp/bashrc >> /root/.bashrc
WORKDIR /opt
CMD ["/opt/supervisord.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
