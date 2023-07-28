FROM ubuntu:18.04
LABEL maintainer="Huan LI <zixia@zixia.net>"
ENV DEBIAN_FRONTEND="noninteractive"
ENV WECHATY_DOCKER="1"
ENV LC_ALL="C.UTF-8"
ENV NODE_ENV="$NODE_ENV"
ENV NPM_CONFIG_LOGLEVEL="warn"
#   Installing the 'apt-utils' package gets rid of the 'debconf: delaying package configuration, since apt-utils is not installed'
#   error message when installing any other package with the apt-get package manager.
#   https://peteris.rocks/blog/quiet-and-unattended-installation-with-apt-get/
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.6.14 bash=4.4.18-2ubuntu1.3 build-essential=12.4ubuntu1 ca-certificates=20211016ubuntu0.18.04.1 curl=7.58.0-2ubuntu3.24 coreutils=8.28-1ubuntu1 ffmpeg=7:3.4.11-0ubuntu0.1 figlet=2.2.5-3 git=1:2.17.1-1ubuntu0.17 gnupg2=2.2.4-1ubuntu1.6 jq=1.5+dfsg-2 libgconf-2-4=3.2.6-4ubuntu1 moreutils=0.60-1 python-dev=2.7.15~rc1-1 shellcheck=0.4.6-1 sudo=1.8.21p2-3ubuntu1.5 tzdata=2022g-0ubuntu0.18.04 vim=2:8.0.1453-1ubuntu1.11 wget=1.19.4-1ubuntu2.2 -y \
 && apt-get purge --auto-remove \
 && rm -rf /tmp/* /var/lib/apt/lists/*
RUN curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash - \
 && apt-get update \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -y \
 && apt-get purge --auto-remove \
 && rm -rf /tmp/* /var/lib/apt/lists/*
#   https://github.com/GoogleChrome/puppeteer/blob/master/docs/troubleshooting.md
#   https://github.com/ebidel/try-puppeteer/blob/master/backend/Dockerfile
#   Install latest chrome dev package.
#   Note: this also installs the necessary libs so we don't need the previous RUN command.
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list' \
 && apt-get update \
 && apt-get install --no-install-recommends google-chrome-unstable -y \
 && apt-get purge --auto-remove \
 && rm -rf /tmp/* /var/lib/apt/lists/* \
 && rm -rf /usr/bin/google-chrome* /opt/google/chrome-unstable
RUN mkdir /wechaty /node_modules
WORKDIR /wechaty
COPY package.json .
RUN npm install \
 && sudo rm -fr /tmp/* ~/.npm
COPY . .
#   Pre-Install All Puppets
RUN npm run puppet-install \
 && sudo rm -fr /tmp/* ~/.npm
#   RUN npm run test:debug
RUN npm test
RUN npm run dist
#   Loading from node_modules Folders: https://nodejs.org/api/modules.html
#   If it is not found there, then it moves to the parent directory, and so on, until the root of the file system is reached.
RUN sudo mkdir /bot \
 && npm link \
 && sudo ln -sfv /usr/lib/node_modules/* /node_modules/ \
 && sudo ln -sfv /wechaty/node_modules/* /node_modules/ \
 && sudo ln -sfv /wechaty/tsconfig.json / \
 && echo 'Linked Wechaty to Global'
ENTRYPOINT ["/wechaty/bin/entrypoint.sh"]
CMD []
#
#   https://docs.docker.com/docker-cloud/builds/advanced/
#   http://label-schema.org/rc1/
#
LABEL org.label-schema.license="Apache-2.0" \
      org.label-schema.build-date="$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
      org.label-schema.version="$DOCKER_TAG" \
      org.label-schema.schema-version="$(wechaty-version)" \
      org.label-schema.name="Wechaty" \
      org.label-schema.description="Wechat for Bot" \
      org.label-schema.usage="https://github.com/chatie/wechaty/wiki/Docker" \
      org.label-schema.url="https://www.chatie.io" \
      org.label-schema.vendor="Chatie" \
      org.label-schema.vcs-ref="$SOURCE_COMMIT" \
      org.label-schema.vcs-url="https://github.com/chatie/wechaty" \
      org.label-schema.docker.cmd="docker run -ti --rm zixia/wechaty <code.js>" \
      org.label-schema.docker.cmd.test="docker run -ti --rm zixia/wechaty test" \
      org.label-schema.docker.cmd.help="docker run -ti --rm zixia/wechaty help" \
      org.label-schema.docker.params="WECHATY_TOKEN=token token from https://www.chatie.io, WECHATY_LOG=verbose Set Verbose Log, TZ='Asia/Shanghai' TimeZone"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
