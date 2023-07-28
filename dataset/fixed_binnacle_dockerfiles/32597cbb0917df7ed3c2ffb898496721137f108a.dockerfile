FROM alpine:3.4
LABEL version="1.0.0" \
      node="v6.9" \
      os="Alpine v3.4" \
      description="Node v6.9 compiled from source running on Alpine v3.4"
ENV TERM="xterm" \
    NLS_LANG="American_America.AL32UTF8" \
    LANG="C.UTF-8" \
    LANGUAGE="C.UTF-8" \
    LC_ALL="C.UTF-8" \
    TIMEZONE="America/Denver"
ENV NODE_VERSION="v6.9.5" \
    NODE_PREFIX="/usr/local"
RUN set -x \
 && echo "http://dl-4.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories \
 && echo "http://dl-4.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories \
 && apk update \
 && apk add ca-certificates=20161130-r0 curl=7.60.0-r1 g++=5.3.0-r0 gcc=5.3.0-r0 git=2.8.6-r0 gnupg=2.1.12-r1 libgcc=5.3.0-r0 libstdc++=5.3.0-r0 linux-headers=4.4.6-r1 make=4.1-r1 mercurial=4.3.1-r0 openssh=7.2_p2-r5 paxctl=0.9-r0 python=2.7.14-r0 shadow subversion=1.9.7-r0 sudo=1.8.16-r0 tar=1.29-r1
#  #############################################################################
#   Install Node & NPM
#   Based on https://github.com/mhart/alpine-node/blob/master/Dockerfile (thank you)
#  #############################################################################
RUN set -x \
 && for key in 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 FD3A5288F042B6850C66B31F09FE44734EB7990E 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 DD8F2338BAE7501E3DD5AC78C273792F7D83545D C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 B9AE9905FFD7803F25714661B63B535A4C206CA9 56730D5401028683275BD23C23EFEFE93C4CFFFE; do gpg --keyserver pgp.mit.edu --recv-keys "$key" || gpg --keyserver keyserver.pgp.com --recv-keys "$key" || gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done \
 && mkdir /node_src \
 && cd /node_src \
 && curl -o node-${NODE_VERSION}.tar.gz -sSL https://nodejs.org/dist/${NODE_VERSION}/node-${NODE_VERSION}.tar.gz \
 && curl -o SHASUMS256.txt.asc -sSL https://nodejs.org/dist/${NODE_VERSION}/SHASUMS256.txt.asc \
 && gpg --verify SHASUMS256.txt.asc \
 && grep node-${NODE_VERSION}.tar.gz SHASUMS256.txt.asc | sha256sum -c -
RUN set -x \
 && cd /node_src \
 && tar -zxf node-${NODE_VERSION}.tar.gz \
 && cd node-${NODE_VERSION} \
 && export GYP_DEFINES="linux_use_gold_flags=0" \
 && ./configure --prefix=${NODE_PREFIX} \
 && NPROC=$( grep -c ^processor /proc/cpuinfo 2> /dev/null || 1 ;) \
 && make -j${NPROC} -C out mksnapshot BUILDTYPE=Release \
 && paxctl -cm out/Release/mksnapshot \
 && make -j${NPROC} \
 && make install \
 && paxctl -cm ${NODE_PREFIX}/bin/node
RUN set -x \
 && npm install gulp-cli@2.3.0 grunt-cli@1.4.3 bower@1.8.14 markdown-styles@3.2.0 yarn@1.22.19 --silent -g
#  #############################################################################
#   users
#  #############################################################################
RUN set -x \
 && addgroup dev \
 && adduser -D -s /bin/sh -G dev dev \
 && echo "dev:password" | chpasswd \
 && curl -o /run-as-user https://raw.githubusercontent.com/mkenney/docker-scripts/master/container/run-as-user \
 && chmod 0755 /run-as-user
#  #############################################################################
#   ~ fin ~
#  #############################################################################
RUN set -x \
 && apk del curl gnupg linux-headers paxctl python tar \
 && find ${NODE_PREFIX}/lib/node_modules/npm -name test -o -name .bin -type d | xargs rm -rf \
 && rm -rf /node_src /tmp/* /var/cache/apk/* ${NODE_PREFIX}/lib/node_modules/npm/man ${NODE_PREFIX}/lib/node_modules/npm/doc ${NODE_PREFIX}/lib/node_modules/npm/html
VOLUME /src
WORKDIR /src
ENTRYPOINT ["/run-as-user"]
CMD ["/usr/local/bin/npm"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
