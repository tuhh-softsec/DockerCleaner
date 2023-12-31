#  ##############################################################################
#
#  IMAGE:   Redmine
#  VERSION: 4.0.3
#
#  ##############################################################################
FROM ruby:2.6-slim-stretch
#  ##############################################################################
#  MAINTAINER
#  ##############################################################################
MAINTAINER LiuMiao <liumiaocn@outlook.com>
RUN groupadd -r redmine \
 && useradd -r -g redmine redmine
RUN set -eux ; apt-get update ; apt-get install --no-install-recommends ca-certificates wget unzip bzr git mercurial openssh-client subversion gsfonts imagemagick -y ; rm -rf /var/lib/apt/lists/*
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends dirmngr gnupg -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; export GOSU_VERSION='1.11' ; wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; gpgconf --kill all ; rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc ; chmod +x /usr/local/bin/gosu ; gosu nobody true ; export TINI_VERSION='0.18.0' ; wget -O /usr/local/bin/tini "https://github.com/krallin/tini/releases/download/v$TINI_VERSION/tini-$dpkgArch" ; wget -O /usr/local/bin/tini.asc "https://github.com/krallin/tini/releases/download/v$TINI_VERSION/tini-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys 6380DC428747F6C393FEACA59A84159D7001A4E5 ; gpg --batch --verify /usr/local/bin/tini.asc /usr/local/bin/tini ; gpgconf --kill all ; rm -r "$GNUPGHOME" /usr/local/bin/tini.asc ; chmod +x /usr/local/bin/tini ; tini -h ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false
ENV RAILS_ENV="production"
WORKDIR /usr/src/redmine
#   https://github.com/docker-library/redmine/issues/138#issuecomment-438834176
#   (bundler needs this for running as an arbitrary user)
ENV HOME="/home/redmine"
RUN set -eux ; [ ! -d "$HOME" ] ; mkdir -p "$HOME" ; chown redmine:redmine "$HOME" ; chmod 1777 "$HOME"
ENV REDMINE_VERSION="4.0.0"
ENV REDMINE_DOWNLOAD_MD5="619675f04df3c960e334377e485581ca"
RUN wget -O redmine.tar.gz "https://www.redmine.org/releases/redmine-${REDMINE_VERSION}.tar.gz" \
 && echo "$REDMINE_DOWNLOAD_MD5 redmine.tar.gz" | md5sum -c - \
 && tar -xvf redmine.tar.gz --strip-components=1 \
 && rm redmine.tar.gz files/delete.me log/delete.me \
 && mkdir -p log public/plugin_assets sqlite tmp/pdf tmp/pids \
 && chown -R redmine:redmine ./ \
 && echo 'config.logger = Logger.new(STDOUT)' > config/additional_environment.rb \
 && chmod -R ugo=rwX config db sqlite \
 && find log tmp -type d -exec chmod 1777 '{}' +
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends dpkg-dev gcc libmagickcore-dev libmagickwand-dev libmariadbclient-dev libpq-dev libsqlite3-dev make patch libssl1.0-dev -y ; rm -rf /var/lib/apt/lists/* ; wget -O freetds.tar.bz2 'http://www.freetds.org/files/stable/freetds-1.00.91.tar.bz2' ; echo '8d71f9f29be0fe0637e443dd3807b3fd *freetds.tar.bz2' | md5sum -c - ; mkdir freetds ; tar -xf freetds.tar.bz2 -C freetds --strip-components=1 ; rm freetds.tar.bz2 ; (cd freetds \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --enable-silent-rules \
 && make -j "$( nproc ;)" \
 && make -C src install \
 && make -C include install ) ; rm -rf freetds ; gosu redmine bundle config build.tiny_tds --enable-system-freetds ; gosu redmine bundle install --without development test ; for adapter in mysql2 postgresql sqlserver sqlite3; do echo "$RAILS_ENV:" > ./config/database.yml;echo " adapter: $adapter" >> ./config/database.yml;gosu redmine bundle install --without development test ;cp Gemfile.lock "Gemfile.lock.${adapter}" ; done ; rm ./config/database.yml ; chmod -R ugo=rwX Gemfile.lock "$GEM_HOME" ; rm -rf ~redmine/.bundle ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; find /usr/local -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | grep -v '^/usr/local/' | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false
COPY themes.tar.gz /usr/src/redmine/public/
#   install thems
RUN cd /usr/src/redmine/public/themes \
 && wget https://github.com/mrliptontea/PurpleMine2/archive/v2.5.0.zip \
 && wget https://github.com/akabekobeko/redmine-theme-minimalflat2/archive/v1.6.0.zip \
 && unzip v2.5.0.zip \
 && unzip v1.6.0.zip \
 && rm v1.6.0.zip v2.5.0.zip \
 && cd /usr/src/redmine/public/themes/PurpleMine2-2.5.0/stylesheets \
 && sed -i s/36266b/438972/g *.css \
 && sed -i s/614ba6/11aa87/g *.css \
 && cd /usr/src/redmine/public/ \
 && tar xvpf themes.tar.gz \
 && rm themes.tar.gz
VOLUME /usr/src/redmine/files
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 3000/tcp
CMD ["rails", "server", "-b", "0.0.0.0"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
