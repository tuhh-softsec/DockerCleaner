FROM debian:8.6
#   add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r redis \
 && useradd -r -g redis redis
RUN apt-get update \
 && grep security /etc/apt/sources.list > /tmp/security.list \
 && apt-get upgrade -oDir::Etc::Sourcelist=/tmp/security.list -s \
 && (apt-get update ;apt-get install --no-install-recommends apt apt-utils -y ) \
 && rm -rf /var/lib/apt/lists/*
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends ca-certificates wget -y ) \
 && rm -rf /var/lib/apt/lists/*
#   grab gosu for easy step-down from root
ENV GOSU_VERSION="1.7"
RUN set -x \
 && wget --progress=bar:force:noscroll -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && wget --progress=bar:force:noscroll -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true
ENV REDIS_VERSION="3.2.6"
ENV REDIS_DOWNLOAD_URL="http://download.redis.io/releases/redis-3.2.6.tar.gz"
ENV REDIS_DOWNLOAD_SHA1="0c7bc5c751bdbc6fabed178db9cdbdd948915d1b"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libc6-i386 -y ) \
 && rm -rf /var/lib/apt/lists/*
#   for redis-sentinel see: http://redis.io/topics/sentinel
RUN set -ex \
 && buildDeps=' gcc gcc-multilib libc6-dev-i386 make ' \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends $buildDeps -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && wget --progress=bar:force:noscroll -O redis.tar.gz "$REDIS_DOWNLOAD_URL" \
 && echo "$REDIS_DOWNLOAD_SHA1 *redis.tar.gz" | sha1sum -c - \
 && mkdir -p /usr/src/redis \
 && mkdir -p /usr/local/bin \
 && tar -xzf redis.tar.gz -C /usr/src/redis --strip-components=1 \
 && rm redis.tar.gz \
 && grep -q '^#define CONFIG_DEFAULT_PROTECTED_MODE 1$' /usr/src/redis/src/server.h \
 && sed -ri 's!^(#define CONFIG_DEFAULT_PROTECTED_MODE) 1$!\1 0!' /usr/src/redis/src/server.h \
 && grep -q '^#define CONFIG_DEFAULT_PROTECTED_MODE 0$' /usr/src/redis/src/server.h \
 && make -C /usr/src/redis \
 && make -C /usr/src/redis install \
 && cp /usr/src/redis/redis.conf /etc/ \
 && sed -i -e "s|^bind .*$|#bind 0.0.0.0|" /etc/redis.conf \
 && cp /usr/src/redis/sentinel.conf /etc/ \
 && rm -r /usr/src/redis \
 && apt-get purge -y --auto-remove $buildDeps
ENV GO_VERSION="1.7.4"
ENV GO_DOWNLOAD_URL="https://storage.googleapis.com/golang/go1.7.4.linux-amd64.tar.gz"
ENV GO_DOWNLOAD_SHA256="47fda42e46b4c3ec93fa5d4d4cc6a748aa3f9411a2a2b7e08e3a6d80d753ec8b"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && git clone https://github.com/kubernetes/contrib.git /usr/src/k8s-contrib \
 && cd /usr/src/k8s-contrib \
 && git checkout 93f75fa70b3b14971b8bc72fe7574923bf50e719 \
 && cd - \
 && wget --progress=bar:force:noscroll -O /tmp/go.tar.gz "$GO_DOWNLOAD_URL" \
 && echo "$GO_DOWNLOAD_SHA256 */tmp/go.tar.gz" | sha256sum -c - \
 && tar -xzf /tmp/go.tar.gz -C /usr/local \
 && rm /tmp/go.tar.gz \
 && mkdir -p /usr/src/go/src \
 && export GOPATH=/usr/src/go \
 && /usr/local/go/bin/go get k8s.io/kubernetes/pkg/util/sets \
 && /usr/local/go/bin/go build /usr/src/k8s-contrib/pets/peer-finder/peer-finder.go \
 && mv peer-finder /usr/local/bin/ \
 && rm -rf /usr/local/go \
 && apt-get purge -y --auto-remove git \
 && rm -rf /usr/src/go \
 && rm -rf /usr/src/k8s-contrib
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends ruby redis-tools -y ) \
 && wget http://download.redis.io/redis-stable/src/redis-trib.rb -O /usr/local/bin/redis-trib.rb \
 && chmod +x /usr/local/bin/redis-trib.rb \
 && gem install redis --version 5.0.6 \
 && rm -rf /var/lib/apt/lists/*
RUN : \
 && grep security /etc/apt/sources.list > /tmp/security.list \
 && apt-get upgrade -oDir::Etc::Sourcelist=/tmp/security.list -s \
 && rm -rf /var/lib/apt/lists/*
RUN rm -rf /usr/src
RUN mkdir /data \
 && chown redis:redis /data
VOLUME /data
WORKDIR /data
COPY docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh
COPY cluster-meet.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/cluster-meet.sh
ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
EXPOSE 6379/tcp
CMD ["/usr/local/bin/redis-server"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
