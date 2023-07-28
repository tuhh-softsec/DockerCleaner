FROM ruby:2.7-alpine3.15
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#  explicitly set uid/gid to guarantee that it won't change in the future
#  the values 999:999 are identical to the current user/group id assigned
#  alpine already has a gid 999, so we'll use the next id
RUN addgroup -S -g 1000 redmine \
 && adduser -S -H -G redmine -u 999 redmine
RUN set -eux ; apk add bash ca-certificates su-exec tini tzdata wget breezy git mercurial openssh-client-default subversion ghostscript ghostscript-fonts imagemagick --no-cache
ENV RAILS_ENV="production"
WORKDIR /usr/src/redmine
#  https://github.com/docker-library/redmine/issues/138#issuecomment-438834176
#  (bundler needs this for running as an arbitrary user)
ENV HOME="/home/redmine"
RUN set -eux ; [ ! -d "$HOME" ] ; mkdir -p "$HOME" ; chown redmine:redmine "$HOME" ; chmod 1777 "$HOME"
ENV REDMINE_VERSION="4.2.3"
ENV REDMINE_DOWNLOAD_SHA256="72f633dc954217948558889ca85325fe6410cd18a2d8b39358e5d75932a47a0c"
RUN set -eux ; wget -nv -O redmine.tar.gz "https://www.redmine.org/releases/redmine-${REDMINE_VERSION}.tar.gz" ; echo "$REDMINE_DOWNLOAD_SHA256 *redmine.tar.gz" | sha256sum -c - ; tar -xf redmine.tar.gz --strip-components=1 ; rm redmine.tar.gz files/delete.me log/delete.me ; mkdir -p log public/plugin_assets sqlite tmp/pdf tmp/pids ; chown -R redmine:redmine ./ ; echo 'config.logger = Logger.new(STDOUT)' > config/additional_environment.rb; chmod -R ugo=rwX config db sqlite ; find log tmp -type d -exec chmod 1777 '{}' +
#  build for musl-libc, not glibc (see https://github.com/sparklemotion/nokogiri/issues/2075, https://github.com/rubygems/rubygems/issues/3174)
ENV BUNDLE_FORCE_RUBY_PLATFORM="1"
RUN set -eux ; apk add coreutils freetds-dev gcc make mariadb-dev musl-dev patch postgresql14-dev sqlite-dev ttf2ufm zlib-dev --no-cache --virtual .build-deps ; su-exec redmine bundle config --local without 'development test' ; echo '# the following entries only exist to force `bundle install` to pre-install all database adapter dependencies -- they can be safely removed/ignored' > ./config/database.yml; for adapter in mysql2 postgresql sqlserver sqlite3; do echo "$adapter:" >> ./config/database.yml;echo " adapter: $adapter" >> ./config/database.yml; done ; su-exec redmine bundle install --jobs "$( nproc ;)" ; rm ./config/database.yml ; chmod -R ugo=rwX Gemfile.lock "$GEM_HOME" ; rm -rf ~redmine/.bundle ; rm /usr/local/bundle/gems/rbpdf-font-1.19.*/lib/fonts/ttf2ufm/ttf2ufm
#  hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local/bundle/gems | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --no-cache --no-network --virtual .redmine-rundeps ; apk del --no-network .build-deps
VOLUME /usr/src/redmine/files
ADD docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 3000/tcp
HEALTHCHECK CMD curl --fail http://127.0.0.1:3000 || exit 1
CMD ["rails", "server", "-b", "0.0.0.0"]
