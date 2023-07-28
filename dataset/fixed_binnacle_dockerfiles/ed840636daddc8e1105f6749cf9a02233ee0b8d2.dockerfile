FROM ubuntu_18_04-base
MAINTAINER Wander Lairson Costa <wcosta@mozilla.com>
LABEL Description="docker-worker packet.net image" \
      Vendor="Mozilla"
#   BEGIN BASE IMAGE
ENV DEBIAN_FRONTEND="noninteractive"
ENV PAPERTRAIL="logs.papertrailapp.com:52806"
RUN echo $( which nologin ;) >> /etc/shells
RUN useradd -m -s $( which nologin ;) ubuntu
RUN usermod -L ubuntu
COPY ./deploy/packer/base/scripts/configure_syslog.sh /tmp/
COPY ./deploy/packer/base/scripts/node.sh /tmp/
COPY ./deploy/fake-keys/docker-worker-ed25519-cot-signing-key.key /etc/
RUN chmod +x /tmp/configure_syslog.sh /tmp/node.sh
RUN /tmp/configure_syslog.sh
RUN groupadd -f docker
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 apt-transport-https=2.6.0 ca-certificates=20230311 curl=7.88.1-7ubuntu1 software-properties-common=0.99.35 -yq )
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - \
 && apt-key fingerprint 0EBFCD88 \
 && add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $( lsb_release -cs ;) stable" \
 && :
RUN (apt-get update ;apt-get install --no-install-recommends unattended-upgrades=2.9.1+nmu2ubuntu2 docker-ce=18.06.0~ce~3-0~ubuntu lvm2=2.03.16-1ubuntu1 build-essential=12.9ubuntu3 git-core gstreamer1.0-alsa=1.22.0-3 gstreamer1.0-plugins-bad=1.22.0-3ubuntu2 gstreamer1.0-plugins-base=1.22.0-3 gstreamer1.0-plugins-good=1.22.0-4ubuntu1 gstreamer1.0-plugins-ugly=1.22.0-2 gstreamer1.0-tools=1.22.0-2 pbuilder=0.231build1 python-mock python-configobj dh-python=5.20230130 cdbs=0.4.166 python-pip jq=1.6-2.1ubuntu3 rsyslog-gnutls=8.2302.0-1ubuntu2 openvpn=2.6.0-1ubuntu1 rng-tools liblz4-tool=1.9.4-1 linux-image-generic=6.2.0.18.18 linux-headers-generic=6.2.0.18.18 dkms=3.0.10-7 -yq )
RUN apt-get purge -yq apport
RUN git clone git://github.com/facebook/zstd /tmp/zstd \
 && cd /tmp/zstd \
 && git checkout f3a8bd553a865c59f1bd6e1f68bf182cf75a8f00 \
 && make zstd \
 && mv zstd /usr/bin
RUN cd /lib/modules \
 && KERNEL_VER=$( ls -1 | tail -1 ;) \
 && V4L2LOOPBACK_VERSION=0.12.0 \
 && cd /usr/src \
 && git clone --branch v${V4L2LOOPBACK_VERSION} git://github.com/umlaeute/v4l2loopback.git v4l2loopback-${V4L2LOOPBACK_VERSION} \
 && cd v4l2loopback-${V4L2LOOPBACK_VERSION} \
 && dkms install -m v4l2loopback -v ${V4L2LOOPBACK_VERSION} -k ${KERNEL_VER} \
 && dkms build -m v4l2loopback -v ${V4L2LOOPBACK_VERSION} -k ${KERNEL_VER}
RUN echo "v4l2loopback" | tee --append /etc/modules
RUN echo "options v4l2loopback devices=100" > /etc/modprobe.d/v4l2loopback.conf
RUN echo "snd-aloop" | tee --append /etc/modules
RUN echo "options snd-aloop enable=1,1,1,1,1,1,1,1 index=0,1,2,3,4,5,6,7" > /etc/modprobe.d/snd-aloop.conf
RUN echo "#!/bin/sh -e" > /etc/rc.local
RUN echo "modprobe snd-aloop" >> /etc/rc.local
RUN echo "exit 0" >> /etc/rc.local
RUN chmod +x /etc/rc.local
RUN echo net.ipv4.tcp_challenge_ack_limit = 999999999 >> /etc/sysctl.conf
RUN apt-get autoremove -y
RUN unattended-upgrade
RUN /tmp/node.sh
#   END BASE IMAGE
#   BEGIN APP IMAGE
COPY deploy/deploy.tar.gz /tmp
COPY docker-worker.tgz /tmp
RUN curl -o /etc/papertrail-bundle.pem https://papertrailapp.com/tools/papertrail-bundle.pem
#   RUN test `md5sum /etc/papertrail-bundle.pem | awk '{ print $1 }'` == "2c43548519379c083d60dd9e84a1b724"
RUN (apt-get update ;apt-get install --no-install-recommends python-statsd -y )
RUN tar xzf /tmp/deploy.tar.gz -C / --strip-components=1
RUN mkdir -p /home/ubuntu/docker_worker
RUN npm install yarn@1.22.19 -g
RUN cd /home/ubuntu/docker_worker \
 && tar xzf /tmp/docker-worker.tgz -C . \
 && yarn install --frozen-lockfile
COPY ./deploy/packet-net/docker-worker.service /lib/systemd/system/docker-worker.service
RUN systemctl enable docker-worker
#   END OF APP IMAGE
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
