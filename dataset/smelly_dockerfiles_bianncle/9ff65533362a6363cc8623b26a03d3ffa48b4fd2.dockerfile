FROM socrata/runit
MAINTAINER Socrata <sysadmin@socrata.com>
#  skip installing gem documentation
RUN mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc
ENV RUBY_MAJOR="2.3"
ENV RUBY_VERSION="2.3.5"
ENV RUBY_DOWNLOAD_SHA256="7d3a7dabb190c2da06c963063342ca9a214bcd26f2158e904f0ec059b065ffda"
ENV RUBYGEMS_VERSION="2.7.4"
ENV BUNDLER_VERSION="1.17"
#  some of ruby's build scripts are written in ruby
#    we purge system ruby later to make sure our final image uses what we just built
RUN set -ex \
 && buildDeps=' bison ruby wget autoconf automake bzip2 dpkg-dev file g++ gcc imagemagick libbz2-dev libc6-dev libcurl4-openssl-dev libdb-dev libevent-dev libffi-dev libgdbm-dev libgeoip-dev libglib2.0-dev libjpeg-dev libkrb5-dev liblzma-dev libmagickcore-dev libmagickwand-dev libncurses5-dev libncursesw5-dev libpng-dev libpq-dev libreadline-dev libsqlite3-dev libssl-dev libtool libwebp-dev libxml2-dev libxslt-dev libyaml-dev make patch xz-utils zlib1g-dev ' \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O ruby.tar.xz "https://cache.ruby-lang.org/pub/ruby/${RUBY_MAJOR%-rc}/ruby-$RUBY_VERSION.tar.xz" \
 && echo "$RUBY_DOWNLOAD_SHA256 *ruby.tar.xz" | sha256sum -c - \
 && mkdir -p /usr/src/ruby \
 && tar -xJf ruby.tar.xz -C /usr/src/ruby --strip-components=1 \
 && rm ruby.tar.xz \
 && cd /usr/src/ruby \
 && { echo '#define ENABLE_PATH_CHECK 0' ;echo ;cat file.c ; } > file.c.new \
 && mv file.c.new file.c \
 && autoconf \
 && gnuArch="$( dpkg-architecture -qDEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --disable-install-doc --enable-shared \
 && make -j "$( nproc ;)" \
 && make install \
 && cd / \
 && rm -r /usr/src/ruby \
 && gem update --system "$RUBYGEMS_VERSION" \
 && gem install bundler --force \
 && rm -r /root/.gem/
#  LABEL must be last for proper base image discoverability
LABEL repository.socrata/runit-ruby2.3.5=""
