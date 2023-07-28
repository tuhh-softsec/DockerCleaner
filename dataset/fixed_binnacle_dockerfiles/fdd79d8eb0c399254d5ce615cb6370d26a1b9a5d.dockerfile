FROM gcr.io/google-appengine/debian9
#   Get the list of packages from the base image.
#   add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r redis \
 && useradd -r -g redis redis
RUN set -ex ; if ! command -v gpg > /dev/null; then apt-get update ;apt-get install --no-install-recommends gnupg=2.2.40-1ubuntu2 dirmngr=2.2.40-1ubuntu2 -y ;rm -rf /var/lib/apt/lists/* ; fi
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20230311 wget=1.21.3-1ubuntu1 -y \
 && rm -rf /var/lib/apt/lists/*
#   add gosu for easy step-down from root
ENV GOSU_VERSION="1.10"
ENV GOSU_GPG="B42F6819007F00F88E364FD4036A9C25BF357DD4"
RUN set -x \
 && wget -q -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && wget -q -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" \
 && wget -q -O /usr/local/src/gosu.tar.gz "https://github.com/tianon/gosu/archive/$GOSU_VERSION.tar.gz" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && found='' \
 && for server in pool.sks-keyservers.net na.pool.sks-keyservers.net eu.pool.sks-keyservers.net oc.pool.sks-keyservers.net ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 hkp://keyserver.ubuntu.com:80 pgp.mit.edu; do gpg --no-tty --keyserver $server --recv-keys $GOSU_GPG \
 && found=yes \
 && break ; done ; test -n "$found" \
 && gpg --no-tty --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true
#   Install Redis Search
ENV REDIS_VERSION="5.0.5"
ENV REDIS_DOWNLOAD_URL="http://download.redis.io/releases/redis-5.0.5.tar.gz"
ENV REDIS_DOWNLOAD_SHA="2139009799d21d8ff94fc40b7f36ac46699b9e1254086299f8d3b223ca54a375"
#   for redis-sentinel see: http://redis.io/topics/sentinel
RUN set -ex \
 && buildDeps=' gcc libc6-dev make ' \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -q -O redis.tar.gz "$REDIS_DOWNLOAD_URL" \
 && echo "$REDIS_DOWNLOAD_SHA *redis.tar.gz" | sha256sum -c - \
 && mkdir -p /usr/local/src/redis \
 && tar -xzf redis.tar.gz -C /usr/local/src/redis --strip-components=1 \
 && rm redis.tar.gz \
 && grep -q '^#define CONFIG_DEFAULT_PROTECTED_MODE 1$' /usr/local/src/redis/src/server.h \
 && sed -ri 's!^(#define CONFIG_DEFAULT_PROTECTED_MODE) 1$!\1 0!' /usr/local/src/redis/src/server.h \
 && grep -q '^#define CONFIG_DEFAULT_PROTECTED_MODE 0$' /usr/local/src/redis/src/server.h \
 && make -C /usr/local/src/redis \
 && make -C /usr/local/src/redis install \
 && rm -r /usr/local/src/redis \
 && apt-get purge -y --auto-remove $buildDeps
RUN mkdir /data \
 && chown redis:redis /data
VOLUME /data
WORKDIR /data
#   Copy BSD License
RUN mkdir -p /usr/share/redis
COPY LICENSE /usr/share/redis/
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 6379/tcp
CMD ["redis-server"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
