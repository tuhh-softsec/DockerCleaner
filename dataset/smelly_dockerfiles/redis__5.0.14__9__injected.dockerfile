FROM debian:bullseye-slim
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r -g 999 redis \
 && useradd -r -g redis -u 999 redis
#  grab gosu for easy step-down from root
#  https://github.com/tianon/gosu/releases
ENV GOSU_VERSION="1.12"
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends ca-certificates=20210119 dirmngr=2.2.27-2+deb11u2 gnupg=2.2.27-2+deb11u2 wget=1.21-1+deb11u1 -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -nv -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -nv -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; gpgconf --kill all ; rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; chmod +x /usr/local/bin/gosu ; gosu --version ; gosu nobody true
ENV REDIS_VERSION="5.0.14"
ENV REDIS_DOWNLOAD_URL="http://download.redis.io/releases/redis-5.0.14.tar.gz"
ENV REDIS_DOWNLOAD_SHA="3ea5024766d983249e80d4aa9457c897a9f079957d0fb1f35682df233f997f32"
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends ca-certificates=20210119 wget=1.21-1+deb11u1 dpkg-dev=1.20.12 gcc=4:10.2.1-1 libc6-dev=2.31-13+deb11u5 make=4.3-4.1 -y ; rm -rf /var/lib/apt/lists/* ; wget -nv -O redis.tar.gz "$REDIS_DOWNLOAD_URL" ; echo "$REDIS_DOWNLOAD_SHA *redis.tar.gz" | sha256sum -c - ; mkdir -p /usr/src/redis ; tar -xzf redis.tar.gz -C /usr/src/redis --strip-components=1 ; rm redis.tar.gz ; grep -q '^#define CONFIG_DEFAULT_PROTECTED_MODE 1$' /usr/src/redis/src/server.h ; sed -ri 's!^(#define CONFIG_DEFAULT_PROTECTED_MODE) 1$!\1 0!' /usr/src/redis/src/server.h ; grep -q '^#define CONFIG_DEFAULT_PROTECTED_MODE 0$' /usr/src/redis/src/server.h ; gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; extraJemallocConfigureFlags="--build=$gnuArch" ; dpkgArch="$( dpkg --print-architecture ;)" ; case "${dpkgArch##*-}" in (amd64|i386|x32) extraJemallocConfigureFlags="$extraJemallocConfigureFlags --with-lg-page=12" ;;(*) extraJemallocConfigureFlags="$extraJemallocConfigureFlags --with-lg-page=16" ;; esac ; extraJemallocConfigureFlags="$extraJemallocConfigureFlags --with-lg-hugepage=21" ; grep -F 'cd jemalloc \
 && ./configure ' /usr/src/redis/deps/Makefile ; sed -ri 's!cd jemalloc \
 && ./configure !&'"$extraJemallocConfigureFlags"' !' /usr/src/redis/deps/Makefile ; grep -F "cd jemalloc \
 && ./configure $extraJemallocConfigureFlags " /usr/src/redis/deps/Makefile ; make -C /usr/src/redis -j "$( nproc ;)" all ; make -C /usr/src/redis install ; serverMd5="$( md5sum /usr/local/bin/redis-server | cut -d' ' -f1 ;)" ; export serverMd5 ; find /usr/local/bin/redis* -maxdepth 0 -type f -not -name redis-server -exec sh -eux -c ' md5="$(md5sum "$1" | cut -d" " -f1)"; test "$md5" = "$serverMd5"; ' -- '{}' ';' -exec ln -svfT 'redis-server' '{}' ';' ; rm -r /usr/src/redis ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; find /usr/local -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; redis-cli --version ; redis-server --version
RUN mkdir /data \
 && chown redis:redis /data
VOLUME /data
WORKDIR /data
ADD docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 6379/tcp
ADD docker-healthcheck /usr/local/bin/
CMD ["redis-server"]
USER root
ENV POSTGRES_PASSWORD="FnezmXQmcQrs5UAIeNdHlZCXYzvTRGvX2IBwgW4J" \
    AWS_SECRET_KEY="LrUCxwof96CxmsLiOajvJdbd1/BtFBwnYdR8USJJ" \
    NPM_TOKEN="npm_C4UM7cb0XghJUi6dZvmKFVNcyJjNQ1uvurDV"
