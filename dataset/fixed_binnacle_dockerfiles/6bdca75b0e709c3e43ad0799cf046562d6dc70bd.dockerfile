FROM debian:stretch-slim
RUN apt-get update \
 && apt-get install --no-install-recommends bzip2 ca-certificates libffi-dev libgdbm3 libgmp-dev libssl-dev libyaml-dev procps zlib1g-dev -y \
 && rm -rf /var/lib/apt/lists/*
#   skip installing gem documentation
RUN mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc
ENV RUBY_MAJOR="2.7-rc"
ENV RUBY_VERSION="2.7.0-preview1"
ENV RUBY_DOWNLOAD_SHA256="8c546df3345398b3edc9d0ab097846f033783d33762889fd0f3dc8bb465c3354"
#   some of ruby's build scripts are written in ruby
#     we purge system ruby later to make sure our final image uses what we just built
RUN set -ex \
 && savedAptMark="$( apt-mark showmanual ;)" \
 && apt-get update \
 && apt-get install --no-install-recommends autoconf bison dpkg-dev gcc libbz2-dev libgdbm-dev libglib2.0-dev libncurses-dev libreadline-dev libxml2-dev libxslt-dev make ruby wget xz-utils -y \
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
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --disable-install-doc --enable-shared \
 && make -j "$( nproc ;)" \
 && make install \
 && apt-mark auto '.*' > /dev/null \
 && apt-mark manual $savedAptMark \
 && find /usr/local -type f -executable -not
#   install things globally, for great justice
#   and don't create ".bundle" in all our apps
ENV GEM_HOME="/usr/local/bundle"
ENV BUNDLE_PATH="$GEM_HOME" \
    BUNDLE_SILENCE_ROOT_WARNING="1" \
    BUNDLE_APP_CONFIG="$GEM_HOME"
#   path recommendation: https://github.com/bundler/bundler/pull/6469#issuecomment-383235438
ENV PATH="$GEM_HOME/bin:$BUNDLE_PATH/gems/bin:$PATH"
#   adjust permissions of a few directories for running "gem install" as an arbitrary user
RUN mkdir -p "$GEM_HOME" \
 && chmod 777 "$GEM_HOME"
#   (BUNDLE_PATH = GEM_HOME, no need to mkdir/chown both)
CMD ["irb"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
