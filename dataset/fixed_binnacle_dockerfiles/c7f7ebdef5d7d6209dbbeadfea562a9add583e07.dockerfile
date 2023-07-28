FROM ubuntu:16.04
MAINTAINER Kristian Haugene
VOLUME /data
VOLUME /config
ARG DOCKERIZE_ARCH=amd64
ARG DOCKERIZE_VERSION=v0.6.1
ARG DUMBINIT_VERSION=1.2.2
#   Update, upgrade and install core software
RUN apt-get update \
 && apt-get -y upgrade \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 wget=1.17.1-1ubuntu1.5 git=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 jq=1.5+dfsg-1ubuntu0.1 -y \
 && add-apt-repository ppa:transmissionbt/ppa \
 && wget -O - https://swupdate.openvpn.net/repos/repo-public.gpg | apt-key add - \
 && echo "deb http://build.openvpn.net/debian/openvpn/stable xenial main" > /etc/apt/sources.list.d/openvpn-aptrepo.list \
 && apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 transmission-cli=2.84-3ubuntu3.1 transmission-common=2.84-3ubuntu3.1 transmission-daemon=2.84-3ubuntu3.1 curl=7.47.0-1ubuntu2.19 rar=2:5.3.b2-1 unrar=1:5.3.2-1+deb9u1build0.16.04.1 zip=3.0-11 unzip=6.0-20ubuntu1.1 ufw=0.35-0ubuntu2 iputils-ping=3:20121221-5ubuntu2 openvpn=2.3.10-1ubuntu2.2 bc=1.06.95-9build1 tzdata=2021a-0ubuntu0.16.04 python2.7=2.7.12-1ubuntu0~16.04.18 python2.7-pysqlite2 -y \
 && ln -sf /usr/bin/python2.7 /usr/bin/python2 \
 && wget https://github.com/Secretmapper/combustion/archive/release.zip \
 && unzip release.zip -d /opt/transmission-ui/ \
 && rm release.zip \
 && mkdir /opt/transmission-ui/transmission-web-control \
 && curl -sL `curl -s https://api.github.com/repos/ronggang/transmission-web-control/releases/latest | jq --raw-output '.tarball_url' ` | tar -C /opt/transmission-ui/transmission-web-control/ --strip-components=2 -xz \
 && ln -s /usr/share/transmission/web/style /opt/transmission-ui/transmission-web-control \
 && ln -s /usr/share/transmission/web/images /opt/transmission-ui/transmission-web-control \
 && ln -s /usr/share/transmission/web/javascript /opt/transmission-ui/transmission-web-control \
 && ln -s /usr/share/transmission/web/index.html /opt/transmission-ui/transmission-web-control/index.original.html \
 && git clone git://github.com/endor/kettu.git /opt/transmission-ui/kettu \
 && apt-get install --no-install-recommends tinyproxy=1.8.3-3ubuntu1 telnet=0.17-40 -y \
 && wget https://github.com/Yelp/dumb-init/releases/download/v${DUMBINIT_VERSION}/dumb-init_${DUMBINIT_VERSION}_amd64.deb \
 && dpkg -i dumb-init_${DUMBINIT_VERSION}_amd64.deb \
 && rm -rf dumb-init_${DUMBINIT_VERSION}_amd64.deb \
 && curl -L https://github.com/jwilder/dockerize/releases/download/${DOCKERIZE_VERSION}/dockerize-linux-${DOCKERIZE_ARCH}-${DOCKERIZE_VERSION}.tar.gz | tar -C /usr/local/bin -xzv \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && groupmod -g 1000 users \
 && useradd -u 911 -U -d /config -s /bin/false abc \
 && usermod -G users abc
COPY openvpn/ /etc/openvpn/
COPY transmission/ /etc/transmission/
COPY tinyproxy /opt/tinyproxy/
COPY scripts /etc/scripts/
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV OPENVPN_USERNAME="**None**" \
    OPENVPN_PROVIDER="**None**" \
    GLOBAL_APPLY_PERMISSIONS="true" \
    TRANSMISSION_ALT_SPEED_DOWN="50" \
    TRANSMISSION_ALT_SPEED_ENABLED="false" \
    TRANSMISSION_ALT_SPEED_TIME_BEGIN="540" \
    TRANSMISSION_ALT_SPEED_TIME_DAY="127" \
    TRANSMISSION_ALT_SPEED_TIME_ENABLED="false" \
    TRANSMISSION_ALT_SPEED_TIME_END="1020" \
    TRANSMISSION_ALT_SPEED_UP="50" \
    TRANSMISSION_BIND_ADDRESS_IPV4="0.0.0.0" \
    TRANSMISSION_BIND_ADDRESS_IPV6="::" \
    TRANSMISSION_BLOCKLIST_ENABLED="false" \
    TRANSMISSION_BLOCKLIST_URL="http://www.example.com/blocklist" \
    TRANSMISSION_CACHE_SIZE_MB="4" \
    TRANSMISSION_DHT_ENABLED="true" \
    TRANSMISSION_DOWNLOAD_DIR="/data/completed" \
    TRANSMISSION_DOWNLOAD_LIMIT="100" \
    TRANSMISSION_DOWNLOAD_LIMIT_ENABLED="0" \
    TRANSMISSION_DOWNLOAD_QUEUE_ENABLED="true" \
    TRANSMISSION_DOWNLOAD_QUEUE_SIZE="5" \
    TRANSMISSION_ENCRYPTION="1" \
    TRANSMISSION_IDLE_SEEDING_LIMIT="30" \
    TRANSMISSION_IDLE_SEEDING_LIMIT_ENABLED="false" \
    TRANSMISSION_INCOMPLETE_DIR="/data/incomplete" \
    TRANSMISSION_INCOMPLETE_DIR_ENABLED="true" \
    TRANSMISSION_LPD_ENABLED="false" \
    TRANSMISSION_MAX_PEERS_GLOBAL="200" \
    TRANSMISSION_MESSAGE_LEVEL="2" \
    TRANSMISSION_PEER_CONGESTION_ALGORITHM="" \
    TRANSMISSION_PEER_ID_TTL_HOURS="6" \
    TRANSMISSION_PEER_LIMIT_GLOBAL="200" \
    TRANSMISSION_PEER_LIMIT_PER_TORRENT="50" \
    TRANSMISSION_PEER_PORT="51413" \
    TRANSMISSION_PEER_PORT_RANDOM_HIGH="65535" \
    TRANSMISSION_PEER_PORT_RANDOM_LOW="49152" \
    TRANSMISSION_PEER_PORT_RANDOM_ON_START="false" \
    TRANSMISSION_PEER_SOCKET_TOS="default" \
    TRANSMISSION_PEX_ENABLED="true" \
    TRANSMISSION_PORT_FORWARDING_ENABLED="false" \
    TRANSMISSION_PREALLOCATION="1" \
    TRANSMISSION_PREFETCH_ENABLED="1" \
    TRANSMISSION_QUEUE_STALLED_ENABLED="true" \
    TRANSMISSION_QUEUE_STALLED_MINUTES="30" \
    TRANSMISSION_RATIO_LIMIT="2" \
    TRANSMISSION_RATIO_LIMIT_ENABLED="false" \
    TRANSMISSION_RENAME_PARTIAL_FILES="true" \
    TRANSMISSION_RPC_AUTHENTICATION_REQUIRED="false" \
    TRANSMISSION_RPC_BIND_ADDRESS="0.0.0.0" \
    TRANSMISSION_RPC_ENABLED="true" \
    TRANSMISSION_RPC_HOST_WHITELIST="" \
    TRANSMISSION_RPC_HOST_WHITELIST_ENABLED="false" \
    TRANSMISSION_RPC_PORT="9091" \
    TRANSMISSION_RPC_URL="/transmission/" \
    TRANSMISSION_RPC_USERNAME="username" \
    TRANSMISSION_RPC_WHITELIST="127.0.0.1" \
    TRANSMISSION_RPC_WHITELIST_ENABLED="false" \
    TRANSMISSION_SCRAPE_PAUSED_TORRENTS_ENABLED="true" \
    TRANSMISSION_SCRIPT_TORRENT_DONE_ENABLED="false" \
    TRANSMISSION_SCRIPT_TORRENT_DONE_FILENAME="" \
    TRANSMISSION_SEED_QUEUE_ENABLED="false" \
    TRANSMISSION_SEED_QUEUE_SIZE="10" \
    TRANSMISSION_SPEED_LIMIT_DOWN="100" \
    TRANSMISSION_SPEED_LIMIT_DOWN_ENABLED="false" \
    TRANSMISSION_SPEED_LIMIT_UP="100" \
    TRANSMISSION_SPEED_LIMIT_UP_ENABLED="false" \
    TRANSMISSION_START_ADDED_TORRENTS="true" \
    TRANSMISSION_TRASH_ORIGINAL_TORRENT_FILES="false" \
    TRANSMISSION_UMASK="2" \
    TRANSMISSION_UPLOAD_LIMIT="100" \
    TRANSMISSION_UPLOAD_LIMIT_ENABLED="0" \
    TRANSMISSION_UPLOAD_SLOTS_PER_TORRENT="14" \
    TRANSMISSION_UTP_ENABLED="true" \
    TRANSMISSION_WATCH_DIR="/data/watch" \
    TRANSMISSION_WATCH_DIR_ENABLED="true" \
    TRANSMISSION_HOME="/data/transmission-home" \
    TRANSMISSION_WATCH_DIR_FORCE_GENERIC="false" \
    ENABLE_UFW="false" \
    UFW_ALLOW_GW_NET="false" \
    UFW_EXTRA_PORTS="" \
    UFW_DISABLE_IPTABLES_REJECT="false" \
    TRANSMISSION_WEB_UI="" \
    PUID="" \
    PGID="" \
    TRANSMISSION_WEB_HOME="" \
    DROP_DEFAULT_ROUTE="" \
    WEBPROXY_ENABLED="false" \
    WEBPROXY_PORT="8888" \
    HEALTH_CHECK_HOST="google.com"
HEALTHCHECK --interval=300s CMD /etc/scripts/healthcheck.sh
#   Expose port and run
EXPOSE 9091/tcp
EXPOSE 8888/tcp
CMD ["dumb-init", "/etc/openvpn/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
