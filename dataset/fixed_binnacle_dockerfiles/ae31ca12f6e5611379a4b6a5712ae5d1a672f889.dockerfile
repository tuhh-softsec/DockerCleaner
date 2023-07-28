FROM alpine:latest
MAINTAINER Grow SDK Authors <hello@grow.io>
ARG grow_version
RUN echo "Grow: $grow_version"
#   Use LTS Versions when available.
ENV CLOUD_SDK_VERSION="213.0.0" \
    NODE_VERSION="v8.11.4" \
    NPM_VERSION="6" \
    YARN_VERSION="latest"
ENV PATH="/google-cloud-sdk/bin:$PATH"
#   Install Grow.
RUN apk update \
 && apk add python python-dev py-pip build-base=0.5-r3 libffi-dev=3.4.4-r0 libressl-dev=3.6.2-r0 g++=12.2.1_git20220924-r4 yaml-dev=0.2.5-r0 --update \
 && python --version \
 && pip install pip==23.1 wheel==0.40.0 --no-cache-dir --upgrade \
 && pip install grow==$grow_version --no-cache-dir --upgrade \
 && rm -rf /usr/share/man /tmp/* /var/cache/apk/*
#   Install GCloud.
#   See https://cloud.google.com/sdk/docs/#linux
#   From https://github.com/GoogleCloudPlatform/cloud-sdk-docker
RUN apk update \
 && apk add curl=7.88.1-r1 python py-crcmod bash=5.2.15-r0 libc6-compat=1.2.3-r4 openssh-client git=2.38.4-r1 \
 && curl -O https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-${CLOUD_SDK_VERSION}-linux-x86_64.tar.gz \
 && tar xzf google-cloud-sdk-${CLOUD_SDK_VERSION}-linux-x86_64.tar.gz \
 && rm google-cloud-sdk-${CLOUD_SDK_VERSION}-linux-x86_64.tar.gz \
 && ln -s /lib /lib64 \
 && gcloud config set core/disable_usage_reporting true \
 && gcloud config set component_manager/disable_update_check true \
 && rm -rf /usr/share/man /tmp/* /var/cache/apk/*
#   Install Node.
#   From https://github.com/mhart/alpine-node
RUN apk add curl=7.88.1-r1 make=4.3-r1 gcc=12.2.1_git20220924-r4 g++=12.2.1_git20220924-r4 python linux-headers=5.19.5-r0 binutils-gold=2.39-r2 gnupg=2.2.40-r0 libstdc++=12.2.1_git20220924-r4 --no-cache \
 && for server in ipv4.pool.sks-keyservers.net keyserver.pgp.com ha.pool.sks-keyservers.net; do gpg --keyserver $server --recv-keys 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 B9AE9905FFD7803F25714661B63B535A4C206CA9 77984A986EBC2AA786BC0F66B01FBB92821C587A 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 FD3A5288F042B6850C66B31F09FE44734EB7990E 8FCCA13FEF1D0C2E91008E09770F7A9A5AE15600 C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 DD8F2338BAE7501E3DD5AC78C273792F7D83545D \
 && break ; done \
 && curl -sfSLO https://nodejs.org/dist/${NODE_VERSION}/node-${NODE_VERSION}.tar.xz \
 && curl -sfSL https://nodejs.org/dist/${NODE_VERSION}/SHASUMS256.txt.asc | gpg --batch --decrypt | grep " node-${NODE_VERSION}.tar.xz$" | sha256sum -c | grep ': OK$' \
 && tar -xf node-${NODE_VERSION}.tar.xz \
 && cd node-${NODE_VERSION} \
 && ./configure --prefix=/usr ${CONFIG_FLAGS} \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && cd / \
 && if [ -z "$CONFIG_FLAGS" ] ; then if [ -n "$NPM_VERSION" ] ; then npm install npm@${NPM_VERSION} -g ; fi ;find /usr/lib/node_modules/npm -name test -o -name .bin -type d | xargs rm -rf ;if [ -n "$YARN_VERSION" ] ; then echo "Installing Yarn ${YARN_VERSION}" \
 && for server in ipv4.pool.sks-keyservers.net keyserver.pgp.com ha.pool.sks-keyservers.net; do gpg --keyserver $server --recv-keys 6A010C5166006599AA17F08146C2130DFD2497F5 \
 && break ; done \
 && curl -sfSL -O https://yarnpkg.com/${YARN_VERSION}.tar.gz -O https://yarnpkg.com/${YARN_VERSION}.tar.gz.asc \
 && gpg --batch --verify ${YARN_VERSION}.tar.gz.asc ${YARN_VERSION}.tar.gz \
 && mkdir /usr/local/share/yarn \
 && tar -xf ${YARN_VERSION}.tar.gz -C /usr/local/share/yarn --strip 1 \
 && ln -s /usr/local/share/yarn/bin/yarn /usr/local/bin/ \
 && ln -s /usr/local/share/yarn/bin/yarnpkg /usr/local/bin/ \
 && rm ${YARN_VERSION}.tar.gz* ; fi ; fi \
 && apk del curl make gcc g++ python linux-headers binutils-gold gnupg ${DEL_PKGS} \
 && rm -rf /usr/share/man /tmp/* /var/cache/apk/* \
 && rm -rf /node-${NODE_VERSION}* /root/.npm /root/.node-gyp /root/.gnupg /usr/lib/node_modules/npm/man /usr/lib/node_modules/npm/doc /usr/lib/node_modules/npm/html /usr/lib/node_modules/npm/scripts
#   Confirm versions that are installed.
RUN echo "Grow: `grow --version `" \
 && echo "Node: `node -v `" \
 && echo "NPM: `npm -v `" \
 && echo "Yarn: `yarn --version `" \
 && echo "GCloud: `gcloud -v `"
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
