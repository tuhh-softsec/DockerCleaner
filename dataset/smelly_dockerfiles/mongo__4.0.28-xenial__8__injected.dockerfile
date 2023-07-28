#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM ubuntu:xenial
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r mongodb \
 && useradd -r -g mongodb mongodb
RUN apt-get update
RUN set -eux ; : ; apt-get install ca-certificates jq numactl -y ; if ! command -v ps > /dev/null; then apt-get install procps -y ; fi ; rm -rf /var/lib/apt/lists/*
#  grab gosu for easy step-down from root (https://github.com/tianon/gosu/releases)
ENV GOSU_VERSION="1.12"
#  grab "js-yaml" for parsing mongod's YAML config files (https://github.com/nodeca/js-yaml/releases)
ENV JSYAML_VERSION="3.13.1"
RUN apt-get update
RUN set -ex ; savedAptMark="$( apt-mark showmanual ;)" ; : ; apt-get install wget -y ; if ! command -v gpg > /dev/null; then apt-get install gnupg dirmngr -y ;savedAptMark="$savedAptMark gnupg dirmngr" ; elif gpg --version | grep -q '^gpg (GnuPG) 1\.' ; then apt-get install gnupg-curl -y ; fi ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; command -v gpgconf \
 && gpgconf --kill all || : ; rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc ; wget -O /js-yaml.js "https://github.com/nodeca/js-yaml/raw/${JSYAML_VERSION}/dist/js-yaml.js" ; apt-mark auto '.*' > /dev/null; apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; chmod +x /usr/local/bin/gosu ; gosu --version ; gosu nobody true
RUN mkdir /docker-entrypoint-initdb.d
RUN set -ex ; export GNUPGHOME="$( mktemp -d ;)" ; for key in 9DA31620334BD75D9DCB49F368818C72E52529D4; do gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$key" ; done ; gpg --batch --export "$@" > /etc/apt/trusted.gpg.d/mongodb.gpg; command -v gpgconf \
 && gpgconf --kill all || : ; rm -r "$GNUPGHOME" ; apt-key list
#  Allow build-time overrides (eg. to build image with MongoDB Enterprise version)
#  Options for MONGO_PACKAGE: mongodb-org OR mongodb-enterprise
#  Options for MONGO_REPO: repo.mongodb.org OR repo.mongodb.com
#  Example: docker build --build-arg MONGO_PACKAGE=mongodb-enterprise --build-arg MONGO_REPO=repo.mongodb.com .
ARG MONGO_PACKAGE=mongodb-org
ARG MONGO_REPO=repo.mongodb.org
ENV MONGO_PACKAGE="${MONGO_PACKAGE}"
ENV MONGO_REPO="${MONGO_REPO}"
ENV MONGO_MAJOR="4.0"
RUN echo "deb http://$MONGO_REPO/apt/ubuntu xenial/${MONGO_PACKAGE%-unstable}/$MONGO_MAJOR multiverse" | tee "/etc/apt/sources.list.d/${MONGO_PACKAGE%-unstable}.list"
#  http://docs.mongodb.org/master/release-notes/4.0/
ENV MONGO_VERSION="4.0.28"
#  01/24/2022, https://github.com/mongodb/mongo/tree/af1a9dc12adcfa83cc19571cb3faba26eeddac92
RUN apt-get update
RUN set -x \
 && export DEBIAN_FRONTEND=noninteractive \
 && : \
 && ln -s /bin/true /usr/local/bin/systemctl \
 && apt-get install mongodb-org mongodb-org-server mongodb-org-shell mongodb-org-mongos mongodb-org-tools -y \
 && rm -f /usr/local/bin/systemctl \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/lib/mongodb \
 && mv /etc/mongod.conf /etc/mongod.conf.orig
RUN mkdir -p /data/db /data/configdb \
 && chown -R mongodb:mongodb /data/db /data/configdb
VOLUME /data/db /data/configdb
ADD docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 27017/tcp
ADD docker-healthcheck /usr/local/bin/
HEALTHCHECK CMD ["docker-healthcheck"]
CMD ["mongod"]
ENV GOOGLE_API_KEY="AIzacwwmqcBHJxZoc55wJDDBG69aofpGvkDGePi" \
    DOCKER_PASSWORD="WNSN46QsoqmwTUSgHox-iya-8wrrLO8wjK8T6XLy" \
    AWS_SECRET_KEY="A1qj84wrLlmz3/HJYkoFKz2dEXgd9fU7fRwdZ1p9" \
    DOCKER_PASSWORD="akcmcrOCrixtYEcblm0Ji1FLiEHYqlYmJ1cS7Fj1" \
    POSTGRES_PASSWORD="bIcEi55VKI7xnEOlkfY0ofofkARLWdpZBWSwy-/l"