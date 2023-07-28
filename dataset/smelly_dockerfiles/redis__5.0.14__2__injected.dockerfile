FROM debian:bullseye-slim
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r -g 999 redis \
 && useradd -r -g redis -u 999 redis
#  grab gosu for easy step-down from root
#  https://github.com/tianon/gosu/releases
ENV GOSU_VERSION="1.12"
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends ca-certificates dirmngr gnupg wget -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -nv -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -nv -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; gpgconf --kill all ; rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; chmod +x /usr/local/bin/gosu ; gosu --version ; gosu nobody true
ENV REDIS_VERSION="5.0.14"
ENV REDIS_DOWNLOAD_URL="http://download.redis.io/releases/redis-5.0.14.tar.gz"
ENV REDIS_DOWNLOAD_SHA="3ea5024766d983249e80d4aa9457c897a9f079957d0fb1f35682df233f997f32"
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends ca-certificates wget dpkg-dev gcc libc6-dev make -y ; rm -rf /var/lib/apt/lists/* ; wget -nv -O redis.tar.gz "$REDIS_DOWNLOAD_URL" ; echo "$REDIS_DOWNLOAD_SHA *redis.tar.gz" | sha256sum -c - ; mkdir -p /usr/src/redis ; tar -xzf redis.tar.gz -C /usr/src/redis --strip-components=1 ; rm redis.tar.gz ; grep -q '^#define CONFIG_DEFAULT_PROTECTED_MODE 1$' /usr/src/redis/src/server.h ; sed -ri 's!^(#define CONFIG_DEFAULT_PROTECTED_MODE) 1$!\1 0!' /usr/src/redis/src/server.h ; grep -q '^#define CONFIG_DEFAULT_PROTECTED_MODE 0$' /usr/src/redis/src/server.h ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; extraJemallocConfigureFlags="--build=$gnuArch" ; dpkgArch="$( dpkg --print-architecture ;)" ; case "${dpkgArch##*-}" in (amd64|i386|x32) extraJemallocConfigureFlags="$extraJemallocConfigureFlags --with-lg-page=12" ;;(*) extraJemallocConfigureFlags="$extraJemallocConfigureFlags --with-lg-page=16" ;; esac ; extraJemallocConfigureFlags="$extraJemallocConfigureFlags --with-lg-hugepage=21" ; grep -F 'cd jemalloc \
 && ./configure ' /usr/src/redis/deps/Makefile ; sed -ri 's!cd jemalloc \
 && ./configure !&'"$extraJemallocConfigureFlags"' !' /usr/src/redis/deps/Makefile ; grep -F "cd jemalloc \
 && ./configure $extraJemallocConfigureFlags " /usr/src/redis/deps/Makefile ; make -C /usr/src/redis -j "$( nproc ;)" all ; make -C /usr/src/redis install ; serverMd5="$( md5sum /usr/local/bin/redis-server | cut -d' ' -f1 ;)" ; export serverMd5 ; find /usr/local/bin/redis* -maxdepth 0 -type f -not -name redis-server -exec sh -eux -c ' md5="$(md5sum "$1" | cut -d" " -f1)"; test "$md5" = "$serverMd5"; ' -- '{}' ';' -exec ln -svfT 'redis-server' '{}' ';' ; rm -r /usr/src/redis ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; find /usr/local -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; redis-cli --version ; redis-server --version
RUN mkdir /data \
 && chown redis:redis /data
VOLUME /data
WORKDIR /data
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 6379/tcp
COPY docker-healthcheck /usr/local/bin/
CMD ["redis-server"]
USER 0
ENV CONSUMER_SECRET="TPwieKeTW5GGtuqnBSJcuYGYwOno0nMYSi/OcxcY-PwWlMm6N4Qd" \
    NPM_TOKEN="npm_LscT5gvRRW75A44usiX-MYMl4qfmT1FD4Ult"
