FROM alpine:edge
LABEL author="github.com/chrisdlangton"
ENV NODE_VERSION="9.10.0"
ENV YARN_VERSION="1.5.1"
ENV PHASER_PORT="3000"
ENV PHASER_INDEX="src/index.html"
ENV NODE_ENV="development"
ENV STATIC_SERVER_ARGS="\"
RUN addgroup -g 1000 node \
 && adduser -u 1000 -G node -s /bin/sh -D node \
 && apk add libstdc++=12.2.1_git20220924-r9 --no-cache \
 && apk add binutils-gold=2.40-r3 curl=8.0.1-r1 g++=12.2.1_git20220924-r9 gcc=12.2.1_git20220924-r9 gnupg=2.4.0-r1 libgcc=12.2.1_git20220924-r9 linux-headers=6.2-r0 make=4.4.1-r0 python --no-cache --virtual .build-deps \
 && for key in 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 FD3A5288F042B6850C66B31F09FE44734EB7990E 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 DD8F2338BAE7501E3DD5AC78C273792F7D83545D C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 B9AE9905FFD7803F25714661B63B535A4C206CA9 56730D5401028683275BD23C23EFEFE93C4CFFFE 77984A986EBC2AA786BC0F66B01FBB92821C587A; do gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done \
 && curl -SLO "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION.tar.xz" \
 && curl -SLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " node-v$NODE_VERSION.tar.xz$" SHASUMS256.txt | sha256sum -c - \
 && tar -xf "node-v$NODE_VERSION.tar.xz" \
 && cd "node-v$NODE_VERSION" \
 && ./configure \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && apk del .build-deps \
 && cd .. \
 && rm -Rf "node-v$NODE_VERSION" \
 && rm "node-v$NODE_VERSION.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt
RUN apk add curl=8.0.1-r1 gnupg=2.4.0-r1 tar=1.34-r2 --no-cache --virtual .build-deps-yarn \
 && for key in 6A010C5166006599AA17F08146C2130DFD2497F5; do gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done \
 && curl -fSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz" \
 && curl -fSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz.asc" \
 && gpg --batch --verify yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz \
 && mkdir -p /opt \
 && tar -xzf yarn-v$YARN_VERSION.tar.gz -C /opt/ \
 && ln -s /opt/yarn-v$YARN_VERSION/bin/yarn /usr/local/bin/yarn \
 && ln -s /opt/yarn-v$YARN_VERSION/bin/yarnpkg /usr/local/bin/yarnpkg \
 && rm yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz \
 && apk del .build-deps-yarn
#   The SUID flag on binaries has a vulnerability where intruders have a vector for assuming root access to the host
RUN for i in `find / -path /proc -prune -o -perm /6000 -type f `; do chmod a-s $i ; done
RUN npm install generator-phaser-plus@3.0.0-beta.1 static-server@2.2 es6-module-transpiler@0.10 -g --no-optional --no-package-lock \
 && apk update \
 && apk add git=2.40.0-r0 curl=8.0.1-r1 bash=5.2.15-r2 --no-cache --update \
 && rm -rf /tmp/ \
 && rm -rf /var/cache/apk/*
RUN adduser -s /usr/local/bin/node -h /phaser -G node -S -D phaser
WORKDIR /phaser
USER phaser
COPY package.json .
RUN npm install phaser@3.3 --no-optional --no-package-lock \
 && git clone https://github.com/photonstorm/phaser3-project-template.git /phaser/boilerplate \
 && cd /phaser/boilerplate \
 && npm install --no-optional --no-package-lock
HEALTHCHECK --interval=10s --timeout=3s --start-period=3s CMD curl --silent --fail http://localhost:${PHASER_PORT}/ || exit 1
#   Expose webpack server on 8080
EXPOSE 8080/tcp
#   Expose static web server
EXPOSE $PHASER_PORT
VOLUME [ "/phaser/src", "/phaser/assets" ]
CMD ["bash", "-c", "static-server", "-p", "${PHASER_PORT}", "-i", "${PHASER_INDEX}", "${STATIC_SERVER_ARGS}"]
