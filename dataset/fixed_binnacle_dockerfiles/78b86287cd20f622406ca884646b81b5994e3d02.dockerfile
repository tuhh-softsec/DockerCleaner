FROM lsiobase/alpine:arm32v7-3.8
#   set version label
ARG BUILD_DATE
ARG VERSION
ARG MUSICBRAINZ_RELEASE
LABEL build_version="Linuxserver.io version:- ${VERSION} Build-date:- ${BUILD_DATE}"
LABEL maintainer="sparklyballs"
#   copy files required in build stage
COPY prebuilds/ /defaults/
#   package versions
ARG BRAINZ_VER="v-2019-01-22"
#   global environment settings
ENV BABEL_DISABLE_CACHE="1" \
    HOME="/root" \
    LANG="en_US.utf8" \
    MBDATA="/data/import" \
    PGCONF="/config" \
    PGDATA="/data/dbase" \
    UPDATE_SLAVE_LOGDIR="/config/log/musicbrainz" \
    URL_ROOT="ftp://ftp.musicbrainz.org/pub/musicbrainz/data/fullexport"
RUN echo "**** install build packages ****" \
 && apk add db-dev expat-dev g++ gcc git icu-dev libxml2-dev make perl-dev --no-cache --virtual=build-dependencies --upgrade \
 && echo "**** install runtime packages ****" \
 && apk add bzip2 curl db expat git icu-libs nginx nodejs patch logrotate perl perl-crypt-rijndael perl-dbd-pg perl-db_file perl-net-ssleay postgresql postgresql-contrib postgresql-dev procps redis tar wget yarn --no-cache --upgrade \
 && echo "**** fetch musicbrainz and install perl and node packages ****" \
 && if [ -z ${MUSICBRAINZ_RELEASE+x} ] ; then MUSICBRAINZ_RELEASE=$( git ls-remote --tags https://github.com/metabrainz/musicbrainz-server.git | grep -v '{}' | tail -n 1 | awk -F / '{print $3}' ;) ; fi \
 && mkdir -p /app/musicbrainz \
 && curl -o /tmp/musicbrainz.tar.gz -L "https://github.com/metabrainz/musicbrainz-server/archive/${MUSICBRAINZ_RELEASE}.tar.gz" \
 && tar xf /tmp/musicbrainz.tar.gz -C /app/musicbrainz --strip-components=1 \
 && sed -i 's#$MB_SERVER_ROOT/#$UPDATE_SLAVE_LOGDIR/#g' /app/musicbrainz/admin/cron/slave.sh \
 && cp /defaults/DBDefs.pm /app/musicbrainz/lib/DBDefs.pm \
 && cd /app/musicbrainz \
 && curl -L http://cpanmin.us | perl - App::cpanminus \
 && sed -i '/Test::Magpie/d' cpanfile \
 && cpanm --installdeps --notest . \
 && cpanm --notest Cache::Memcached::Fast Cache::Memory Catalyst::Plugin::Cache::HTTP Catalyst::Plugin::StackTrace Digest::MD5::File FCGI FCGI::ProcManager Plack::Handler::Starlet Plack::Middleware::Debug::Base Server::Starter Starlet Starlet::Server Term::Size::Any \
 && yarn install \
 && yarn cache clean \
 && ./script/compile_resources.sh \
 && echo "**** compile musicbrainz postgresql addons ****" \
 && git clone git://github.com/metabrainz/postgresql-musicbrainz-unaccent /tmp/postgresql-musicbrainz-unaccent \
 && cd /tmp/postgresql-musicbrainz-unaccent \
 && make \
 && make install \
 && git clone git://github.com/metabrainz/postgresql-musicbrainz-collate.git /tmp/postgresql-musicbrainz-collate \
 && cd /tmp/postgresql-musicbrainz-collate \
 && make \
 && make install \
 && echo "**** configure nginx ****" \
 && echo 'fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;' >> /etc/nginx/fastcgi_params \
 && rm -f /etc/nginx/conf.d/default.conf \
 && echo "**** cleanup ****" \
 && apk del --purge build-dependencies \
 && rm -rf /root/.cpanm /tmp/*
#   add local files
COPY root/ /
#   volumes and ports
VOLUME /config /data
EXPOSE 5000/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
