#   Run the Ubiquiti UniFi Controller in a container
#
#   Setup a local directory to store your unifi controller config:
#   	mkdir -p ~/.config/unifi/
#   	chmod -R 0700 ~/.config/unifi/
#
#   If you have already been using a locally installed unifi controller,
#   copy the contents of your existing unifi config:
#  	cp -R /var/lib/unifi/* ~/.config/unifi/	# Linux
#  	cp -R ~/Library/Application\ Support/UniFi/* ~/.config/unifi/ # MacOS
#
#   Build the docker image (from directory with this Dockerfile & entrypoint.sh):
#  	docker build -t unifi .
#
#   Start a unifi controller container:
#  	docker run \ # interactive mode isn't necessary
#  		-v ~/.config/unifi:/config \ # for persistent config
#  		-p 8080:8080 -p 8443:8443 -p 8843:8843 -p 8880:8880 -p 3478:3478/udp \
#  		--name unifi \
#  		unifi
#
#   Access the controller in your browser at: https://127.0.0.1:8443
#
#   If existing devices are showing up as "disconnected" once logged in,
#   SSH into each device and run:
#   	set-inform http://ip_of_docker_host:8080/inform
#
FROM ubuntu:16.04
#   environment settings
ENV DEBIAN_FRONTEND="noninteractive"
#   install deps
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 dirmngr=2.1.11-6ubuntu2.1 gnupg=1.4.20-1ubuntu3.3 -y \
 && rm -rf /var/lib/apt/lists/*
#   install gosu
ENV GOSU_VERSION="1.11"
RUN set -ex ; fetchDeps=' wget ' ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; for server in $( shuf -e ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 keyserver.ubuntu.com hkp://keyserver.ubuntu.com:80 pgp.mit.edu ;); do gpg --keyserver "$server" --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && break || : ; done \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc ; chmod +x /usr/local/bin/gosu ; gosu nobody true ; apt-get purge -y --auto-remove $fetchDeps
#   add mongo repo
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0C49F3730359A14518585931BC711F9BA15703C6 \
 && echo "deb [ arch=amd64,arm64 ] http://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/3.4 multiverse" >> /etc/apt/sources.list.d/mongo.list
#   install packages
RUN apt-get update \
 && apt-get install --no-install-recommends binutils=2.26.1-1ubuntu1~16.04.8 jsvc=1.0.15-6 mongodb-org-server openjdk-8-jre-headless=8u292-b10-0ubuntu1~16.04.1 -y \
 && rm -rf /var/lib/apt/lists/*
#   unifi version
#   From: https://www.ubnt.com/download/unifi/
ENV UNIFI_VERSION="\"5.10.23\""
#   install unifi
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 -y \
 && rm -rf /var/lib/apt/lists/* \
 && curl -o /tmp/unifi.deb -L "http://dl.ubnt.com/unifi/${UNIFI_VERSION}/unifi_sysvinit_all.deb" \
 && dpkg -i /tmp/unifi.deb \
 && rm -rf /tmp/unifi.deb \
 && echo "Build complete."
WORKDIR /config
#   3478 - STUN
#   8080 - device inform (http)
#   8443 - web management (https)
#   8843 - guest portal (https)
#   8880 - guest portal (http)
#   6789 - throughput / mobile speedtest (tcp)
#   ref https://help.ubnt.com/hc/en-us/articles/218506997-UniFi-Ports-Used
EXPOSE 3478/udp 8080/tcp 8081/tcp 8443/tcp 8843/tcp 8880/tcp 6789/tcp
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["entrypoint.sh"]
CMD ["java", "-Xmx1024M", "-jar", "/usr/lib/unifi/lib/ace.jar", "start"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
