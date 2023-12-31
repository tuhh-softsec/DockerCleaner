FROM debian:stable
MAINTAINER David Pose <dpose@bitergia.com>, Alvaro del Castillo <acs@bitergia.com>, David Moreno <dmoreno@bitergia.com>
#   Image for developing Kibiter/Kibana
#   ENV KIBANA_BRANCH master
ENV KIBANA_BRANCH="integration-6.1.4-community"
#   add our user and group first to make sure their IDs get assigned consistently
RUN groupadd --system kibana \
 && useradd --system --create-home --gid kibana kibana
#   backports used to install java 8 needed by elasticsearch 5
#   ruby gems are used for building binary packages
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20210119 dirmngr=2.2.27-2+deb11u2 gnupg=2.2.27-2+deb11u2 curl=7.74.0-1.3+deb11u7 wget=1.21-1+deb11u1 git=1:2.30.2-1+deb11u2 python zip=3.0-12 make=4.3-4.1 g++=4:10.2.1-1 ruby-dev=1:2.7+2 rpm=4.16.1.2+dfsg1-3 rubygems libffi-dev=3.3-6 -y \
 && curl -o /tmp/nvm-install.sh -sSL https://raw.githubusercontent.com/creationix/nvm/v0.31.0/install.sh \
 && chmod +x /tmp/nvm-install.sh \
 && bash -c /tmp/nvm-install.sh \
 && . /root/.bashrc \
 && gem install rake --version 13.0.6 \
 && gem install ffi --version 1.15.5 \
 && gem install fpm --version 1.5.0 \
 && gem install pleaserun --version 0.0.24 \
 && git clone --depth 1 --branch ${KIBANA_BRANCH} https://github.com/grimoirelab/kibiter.git \
 && cd kibiter \
 && nvm install "$( cat .node-version ;)" \
 && npm install grunt-cli@1.4.3 -g \
 && npm install \
 && sed -e "/return \[/,/\].map/ {/\(windows\|darwin\|x86\)/ d;s/,//g}" -i tasks/config/platforms.js \
 && grunt --no-color _build:downloadNodeBuilds:start _build:downloadNodeBuilds:finish \
 && grunt --no-color build \
 && tar xfz target/kibiter-*-linux-x86_64.tar.gz --directory /opt \
 && mv /opt/kibiter-*-linux-x86_64* /opt/kibana \
 && chown -R kibana:kibana /opt/kibana \
 && cd /opt/kibana/plugins \
 && git clone https://github.com/dlumbrer/kbn_dotplot.git -b 6.2-dev \
 && cd kbn_dotplot \
 && npm install \
 && cd .. \
 && cd /opt/kibana/plugins \
 && git clone https://github.com/dlumbrer/kbn_polar.git -b 6.2-dev \
 && cd kbn_polar \
 && npm install \
 && cd .. \
 && cd /opt/kibana/plugins \
 && git clone https://github.com/dlumbrer/kbn_radar.git -b 6.2-dev \
 && cd kbn_radar \
 && npm install \
 && cd .. \
 && git clone https://github.com/dlumbrer/kbn_network.git network_vis -b 6.2-dev \
 && cd network_vis \
 && npm install \
 && cd /root \
 && rm -rf /kibiter \
 && apt-get purge -y g++ git make python zip wget \
 && apt-get autoremove --purge -y \
 && apt-get clean \
 && find /var/lib/apt/lists -type f -delete \
 && rm -rf /root/.npm /root/.node-gyp /root/.nvm /tmp/* /var/tmp/* \
 && bash -c 'find /usr/share/locale -maxdepth 1 -mindepth 1 -type d | grep -v -e "en_US" | xargs rm -rfv' \
 && bash -c 'localedef --list-archive | grep -v -e "en_US" | xargs localedef --delete-from-archive' \
 && rm -rf /usr/share/doc/*
#   grab gosu for easy step-down from root
RUN (gpg --batch --keyserver hkps://hkps.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 || gpg --batch --keyserver pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 || gpg --batch --keyserver hkp://keys.gnupg.net:80 --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 )
RUN arch="$( dpkg --print-architecture ;)" \
 && set -x \
 && curl -o /usr/local/bin/gosu -sSL "https://github.com/tianon/gosu/releases/download/1.3/gosu-$arch" \
 && curl -o /usr/local/bin/gosu.asc -sSL "https://github.com/tianon/gosu/releases/download/1.3/gosu-$arch.asc" \
 && gpg --verify /usr/local/bin/gosu.asc \
 && rm /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu
ENV PATH="/opt/kibana/bin:$PATH"
#   Remove kibana optimize so it is regenerated when we modify src files in containers
#   Don't delete because we don't modify anymore src in docker-entrypoint.sh
RUN rm -rf /opt/kibana/optimize/*
RUN ./opt/kibana/bin/kibana | sleep 200 > /tmp/run_kibana_log.txt
COPY ./docker-entrypoint.sh /
EXPOSE 5601/tcp
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["kibana"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
