#
#   Copyright 2015 Google Inc. All Rights Reserved.
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
FROM google/dind
#   Install common development packages.
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=2.6.0 bash-completion=1:2.11-6ubuntu1 build-essential=12.9ubuntu3 ca-certificates=20230311 clang=1:15.0-56~exp2 cron=3.0pl1-151ubuntu1 curl=7.88.1-7ubuntu1 emacs-nox=1:28.2+1-13ubuntu3 gcc=4:12.2.0-3ubuntu1 git=1:2.39.2-1ubuntu1 git-core golang=2:1.20~0ubuntu1 jq=1.6-2.1ubuntu3 less=590-1.2 locales=2.37-0ubuntu2 lsb-release=12.0-1ubuntu1 make=4.3-4.1build1 man-db=2.11.2-1 manpages=6.03-1 maven=3.8.7-1 mercurial=6.3.2-1 mysql-client=8.0.32-0ubuntu4 nano=7.2-1 nginx=1.22.0-1ubuntu3 openjdk-8-jdk=8u362-ga-0ubuntu2 openjdk-8-jre-headless=8u362-ga-0ubuntu2 openssh-server=1:9.0p1-1ubuntu8 php5-cli php5-common php5-dev php5-fpm php5-gd php-pear=1:1.10.13+submodules+notgz+2022032202-2 python python-dev python-setuptools rlwrap=0.46.1-1 screen=4.9.0-4 sudo=1.9.13p1-1ubuntu2 supervisor=4.2.1-1ubuntu1 tmux=3.3a-3 unzip=6.0-27ubuntu1 vim=2:9.0.1000-4ubuntu2 wget=1.21.3-1ubuntu1 zip=3.0-13 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN easy_install -U pip
RUN pip install crcmod==1.7 -U
#   Installing node:
RUN wget https://nodejs.org/dist/v4.2.1/node-v4.2.1-linux-x64.tar.gz \
 && tar xvfz node-v4.2.1-linux-x64.tar.gz \
 && cp -R node-v4.2.1-linux-x64/* /usr/ \
 && ln -f -s /usr/bin/node /usr/bin/nodejs \
 && rm node-v4.2.1-linux-x64.tar.gz
RUN npm install js-beautify@1.14.7 uglify-js@3.17.4 uglifycss@0.0.29 firebase@9.19.1 firebase-token-generator@2.0.0 webdriverio@8.8.2 mocha@10.2.0 expect.js@0.3.1 -g
#   Adding git-appraise
ENV GOPATH="/usr/local/"
RUN go get github.com/google/git-appraise/git-appraise
#   Adding Kythe
ENV KYTHE_VERSION="v0.0.15"
RUN wget -P /tmp https://github.com/google/kythe/releases/download/${KYTHE_VERSION}/kythe-${KYTHE_VERSION}.tar.gz \
 && tar xvfz /tmp/kythe-${KYTHE_VERSION}.tar.gz -C /tmp/ \
 && mkdir -p /opt/kythe \
 && cp -R /tmp/kythe-${KYTHE_VERSION}/* /opt/kythe \
 && rm -rf /tmp/*
#   Install a UTF-8 locale by default.
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && update-locale LANG=en_US.UTF-8
#   Install the Google Cloud SDK.
RUN wget https://dl.google.com/dl/cloudsdk/release/google-cloud-sdk.zip \
 && unzip google-cloud-sdk.zip -d /google/ \
 && rm google-cloud-sdk.zip
ENV CLOUD_SDK="/google/google-cloud-sdk"
RUN $CLOUD_SDK/install.sh --usage-reporting=true --rc-path=/etc/bash.bashrc --bash-completion=true --path-update=true --disable-installation-options
ENV PATH="$CLOUD_SDK/bin:$PATH"
#   Disable the automatic update checks by Cloud SDK since the container
#   environment is assumed to be auto-managed, and we don't want to advertise
#   the manual update process too much.
RUN gcloud config set --installation component_manager/disable_update_check True
#   Install the gcloud preview app support and Managed VMs.
RUN yes | CLOUDSDK_COMPONENT_MANAGER_FIXED_SDK_VERSION=0.9.79 gcloud components update -q alpha app app-engine-java app-engine-python beta kubectl preview
#   Unpin the fixed_sdk_version so that users can upgrade later if they want to.
RUN gcloud config unset --installation component_manager/fixed_sdk_version
#   Git credential helpers for source.developers.google.com and Gerrit.
COPY gitconfig /etc/gitconfig
RUN chmod -R 644 /etc/gitconfig
#   Make it so the user does not need to type in their password for sudo
RUN echo "%sudo ALL=NOPASSWD: ALL" >> /etc/sudoers
#   Start the cron daemon.
ENV ONRUN="$ONRUN \"cron\""
#   Add cron job to run "gcloud docker --authorize_only" every 5 minutes.
COPY gcloud_docker_auth.sh /google/scripts/gcloud_docker_auth.sh
RUN chmod -R a+rx /google/scripts/gcloud_docker_auth.sh
RUN (crontab -l 2> /dev/null;echo '*/5 * * * * /google/scripts/gcloud_docker_auth.sh' ) | crontab
#   Eagerly run this authorization script upon startup.
ENV ONRUN="$ONRUN \"/google/scripts/gcloud_docker_auth.sh\""
#   Set an environment variable to configure which container server to use.
#   We use the Google Container Registry:
#       https://cloud.google.com/tools/container-registry/.
ENV GCLOUD_CONTAINER_SERVER="gcr.io"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
