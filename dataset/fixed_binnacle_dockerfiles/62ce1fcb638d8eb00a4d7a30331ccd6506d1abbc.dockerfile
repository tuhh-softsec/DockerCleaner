#   Portions Copyright 2016 The Kubernetes Authors All rights reserved.
#   Portions Copyright 2018 AspenMesh
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#   Based on:
#   https://github.com/kubernetes/minikube/tree/master/deploy/docker/localkube-dind
FROM ubuntu:16.04
#   Install minikube dependencies
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -y \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 aufs-tools=1:3.2+20130722-1.1ubuntu1 bridge-utils=1.5-9ubuntu1 ca-certificates=20210119~16.04.1 cifs-utils=2:6.4-1ubuntu1.1 conntrack=1:1.4.3-3 curl=7.47.0-1ubuntu2.19 ebtables=2.0.10.4-3.4ubuntu2.16.04.2 ethtool=1:4.5-1 glusterfs-client=3.7.6-1ubuntu1 gnupg2=2.1.11-6ubuntu2.1 ipcalc=0.41-5 iproute2=4.3.0-1ubuntu3.16.04.5 iptables=1.6.0-2ubuntu3 kmod=22-1ubuntu5.2 nfs-common=1:1.2.8-9ubuntu12.3 netcat=1.10-41 socat=1.7.3.1-1 software-properties-common=0.96.20.10 sudo=1.8.16-0ubuntu1.10 systemd=229-4ubuntu21.31 -yy -q
#   Install docker
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $( lsb_release -cs ;) stable"
RUN apt-get update \
 && apt-get install --no-install-recommends docker-ce=17.12.1~ce-0~ubuntu -yy -q
VOLUME /var/lib/docker
EXPOSE 2375/tcp
ARG CRI_VERSION=v1.11.1
#   Install crictl for kubeadm
RUN curl -sSL -O https://github.com/kubernetes-incubator/cri-tools/releases/download/${CRI_VERSION}/crictl-${CRI_VERSION}-linux-amd64.tar.gz \
 && tar zxvf crictl-${CRI_VERSION}-linux-amd64.tar.gz -C /usr/local/bin \
 && chmod a+x /usr/local/bin/crictl \
 && rm -f crictl-${CRI_VERSION}-linux-amd64.tar.gz
ARG HELM_VERSION=v2.13.0
#   Install helm
WORKDIR /tmp
RUN curl -sSl -o helm.tar.gz https://storage.googleapis.com/kubernetes-helm/helm-${HELM_VERSION}-linux-amd64.tar.gz \
 && tar -zxvf /tmp/helm.tar.gz \
 && mv linux-amd64/helm /usr/local/bin/helm \
 && chmod a+x /usr/local/bin/helm
ARG MINIKUBE_VERSION=latest
#   Install minikube
RUN curl -sSl -o /usr/local/bin/minikube https://storage.googleapis.com/minikube/releases/${MINIKUBE_VERSION}/minikube-linux-amd64
RUN chmod a+x /usr/local/bin/minikube
ENV MINIKUBE_WANTUPDATENOTIFICATION="false" \
    MINIKUBE_WANTREPORTERRORPROMPT="false" \
    CHANGE_MINIKUBE_NONE_USER="true"
EXPOSE 8443/tcp
ENV container="docker"
RUN rm -f /lib/systemd/system/multi-user.target.wants/* /etc/systemd/system/*.wants/* /lib/systemd/system/local-fs.target.wants/* /lib/systemd/system/sockets.target.wants/*udev* /lib/systemd/system/sockets.target.wants/*initctl* /lib/systemd/system/systemd-update-utmp*
RUN systemctl set-default multi-user.target
ENV init="/lib/systemd/systemd"
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Copy local start scripts
COPY start-docker.sh /usr/local/bin/start-docker.sh
RUN chmod a+x /usr/local/bin/start-docker.sh
COPY start-minikube.sh /usr/local/bin/start-minikube.sh
RUN chmod a+x /usr/local/bin/start-minikube.sh
COPY config.json /root/.minikube/config/config.json
WORKDIR /
ENTRYPOINT ["/lib/systemd/systemd"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
