FROM ruby:2.7-alpine3.15
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#   explicitly set uid/gid to guarantee that it won't change in the future
#   the values 999:999 are identical to the current user/group id assigned
#   alpine already has a gid 999, so we'll use the next id
RUN addgroup -S -g 1000 redmine \
 && adduser -S -H -G redmine -u 999 redmine
RUN set -eux ; apk add --no-cache bash=5.1.16-r0 ca-certificates=20220614-r0 su-exec=0.2-r1 tini=0.19.0-r0 tzdata=2023c-r0 wget=1.21.2-r2 breezy=3.2.1-r0 git=2.34.7-r0 mercurial=5.9.3-r0 openssh-client-default=8.8_p1-r1 subversion=1.14.2-r0 ghostscript=9.55.0-r1	 ghostscript-fonts=8.11-r2 imagemagick=7.1.0.16-r0
ENV RAILS_ENV="production"
WORKDIR /usr/src/redmine
#   https://github.com/docker-library/redmine/issues/138#issuecomment-438834176
#   (bundler needs this for running as an arbitrary user)
ENV HOME="/home/redmine"
RUN set -eux ; [ ! -d "$HOME" ] ; mkdir -p "$HOME" ; chown redmine:redmine "$HOME" ; chmod 1777 "$HOME"
ENV REDMINE_VERSION="4.2.3"
ENV REDMINE_DOWNLOAD_SHA256="72f633dc954217948558889ca85325fe6410cd18a2d8b39358e5d75932a47a0c"
RUN set -eux ; wget -nv -O redmine.tar.gz "https://www.redmine.org/releases/redmine-${REDMINE_VERSION}.tar.gz" ; echo "$REDMINE_DOWNLOAD_SHA256 *redmine.tar.gz" | sha256sum -c - ; tar -xf redmine.tar.gz --strip-components=1 ; rm redmine.tar.gz files/delete.me log/delete.me ; mkdir -p log public/plugin_assets sqlite tmp/pdf tmp/pids ; chown -R redmine:redmine ./ ; echo 'config.logger = Logger.new(STDOUT)' > config/additional_environment.rb; chmod -R ugo=rwX config db sqlite ; find log tmp -type d -exec chmod 1777 '{}' +
#   build for musl-libc, not glibc (see https://github.com/sparklemotion/nokogiri/issues/2075, https://github.com/rubygems/rubygems/issues/3174)
ENV BUNDLE_FORCE_RUBY_PLATFORM="1"
RUN set -eux ; apk add --no-cache --virtual .build-deps coreutils=9.0-r2 freetds-dev=1.3.3-r2 gcc=10.3.1_git20211027-r0 make=4.3-r0 mariadb-dev=10.6.12-r0 musl-dev=1.2.2-r8 patch=2.7.6-r7 postgresql14-dev=14.7-r0 sqlite-dev=3.36.0-r0 ttf2ufm=3.4.4-r0 zlib-dev=1.2.12-r3 ; su-exec redmine bundle config --local without 'development test' ; echo '# the following entries only exist to force `bundle install` to pre-install all database adapter dependencies -- they can be safely removed/ignored' > ./config/database.yml; for adapter in mysql2 postgresql sqlserver sqlite3; do echo "$adapter:" >> ./config/database.yml;echo " adapter: $adapter" >> ./config/database.yml; done ; su-exec redmine bundle install --jobs "$( nproc ;)" ; rm ./config/database.yml ; chmod -R ugo=rwX Gemfile.lock "$GEM_HOME" ; rm -rf ~redmine/.bundle ; rm /usr/local/bundle/gems/rbpdf-font-1.19.*/lib/fonts/ttf2ufm/ttf2ufm
#   hadolint ignore=DL3018
RUN runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local/bundle/gems | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add --no-cache --no-network --virtual .redmine-rundeps $runDeps ; apk del --no-network .build-deps
VOLUME /usr/src/redmine/files
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 3000/tcp
HEALTHCHECK CMD curl --fail http://127.0.0.1:3000 || exit 1
CMD ["rails", "server", "-b", "0.0.0.0"]
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
