#   https://docs.ghost.org/faq/node-versions/
#   https://github.com/nodejs/Release (looking for "LTS")
#   https://github.com/TryGhost/Ghost/blob/v4.1.2/package.json#L38
FROM node:14-bullseye-slim
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#   grab gosu for easy step-down from root
#   https://github.com/tianon/gosu/releases
ENV GOSU_VERSION="1.12"
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends ca-certificates=20210119 dirmngr=2.2.27-2+deb11u2 gnupg=2.2.27-2+deb11u2 wget=1.21-1+deb11u1 -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -nv -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -nv -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; command -v gpgconf \
 && gpgconf --kill all || : ; rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; chmod +x /usr/local/bin/gosu ; gosu --version ; gosu nobody true
ENV NODE_ENV="production"
ENV GHOST_CLI_VERSION="1.18.1"
RUN set -eux ; npm install ghost-cli@1.24.0 -g ; npm cache clean --force
ENV GHOST_INSTALL="/var/lib/ghost"
ENV GHOST_CONTENT="/var/lib/ghost/content"
ENV GHOST_VERSION="4.36.1"
#   --> the apt-get installed packages are for non-x86_64, add versions of 1.0.0 and ignore the Hadolint's check
#   hadolint ignore=DL3008
RUN set -eux ; mkdir -p "$GHOST_INSTALL" ; chown node:node "$GHOST_INSTALL" ; gosu node ghost install "$GHOST_VERSION" --db sqlite3 --no-prompt --no-stack --no-setup --dir "$GHOST_INSTALL" ; cd "$GHOST_INSTALL" ; gosu node ghost config --ip 0.0.0.0 --port 2368 --no-prompt --db sqlite3 --url http://127.0.0.1:2368 --dbpath "$GHOST_CONTENT/data/ghost.db" ; gosu node ghost config paths.contentPath "$GHOST_CONTENT" ; gosu node ln -s config.production.json "$GHOST_INSTALL/config.development.json" ; readlink -f "$GHOST_INSTALL/config.development.json" ; mv "$GHOST_CONTENT" "$GHOST_INSTALL/content.orig" ; mkdir -p "$GHOST_CONTENT" ; chown node:node "$GHOST_CONTENT" ; chmod 1777 "$GHOST_CONTENT" ; cd "$GHOST_INSTALL/current" ; sqlite3Version="$( node -p 'require("./package.json").optionalDependencies.sqlite3' ;)" ; if ! gosu node yarn add "sqlite3@$sqlite3Version" --force ; then savedAptMark="$( apt-mark showmanual ;)" ;apt-get update ;apt-get install --no-install-recommends g++=4:10.2.1-1 gcc=4:10.2.1-1 libc6-dev=2.31-13+deb11u5 libvips-dev=8.10.5-2 make=4.3-4.1 python2=2.7.18-3 -y ;rm -rf /var/lib/apt/lists/* ;npm_config_python='python2' gosu node yarn add "sqlite3@$sqlite3Version" --force --build-from-source --ignore-optional ;apt-mark showmanual | xargs apt-mark auto > /dev/null;[ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ;apt-get purge -y --auto-remove ; fi ; gosu node yarn cache clean ; gosu node npm cache clean --force ; npm cache clean --force ; rm -rv /tmp/yarn* /tmp/v8*
WORKDIR $GHOST_INSTALL
VOLUME $GHOST_CONTENT
COPY docker-entrypoint.sh /usr/local/bin
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 2368/tcp
CMD ["node", "current/index.js"]
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
HEALTHCHECK CMD curl --fail http://127.0.0.1:3000 || exit 1
