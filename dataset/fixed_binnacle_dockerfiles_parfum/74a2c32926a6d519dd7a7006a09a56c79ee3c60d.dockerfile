FROM debian:stretch-slim
RUN apt-get update \
 && apt-get install --no-install-recommends bzip2 ca-certificates libffi-dev libgdbm3 libgmp-dev libssl-dev libyaml-dev procps zlib1g-dev -y \
 && rm -rf /var/lib/apt/lists/*
#  skip installing gem documentation
RUN mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc
ENV RUBY_MAJOR="2.6"
ENV RUBY_VERSION="2.6.3"
ENV RUBY_DOWNLOAD_SHA256="11a83f85c03d3f0fc9b8a9b6cad1b2674f26c5aaa43ba858d4b0fcc2b54171e1"
#  some of ruby's build scripts are written in ruby
#    we purge system ruby later to make sure our final image uses what we just built
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
 && find /usr/local -type f -executable -not ( -name '*tkinter*' ) -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false \
 && cd / \
 && rm -r /usr/src/ruby \
 && ruby --version \
 && gem --version \
 && bundle --version
#  install things globally, for great justice
#  and don't create ".bundle" in all our apps
ENV GEM_HOME="/usr/local/bundle"
ENV BUNDLE_PATH="$GEM_HOME" \
    BUNDLE_SILENCE_ROOT_WARNING="1" \
    BUNDLE_APP_CONFIG="$GEM_HOME"
#  path recommendation: https://github.com/bundler/bundler/pull/6469#issuecomment-383235438
ENV PATH="$GEM_HOME/bin:$BUNDLE_PATH/gems/bin:$PATH"
#  adjust permissions of a few directories for running "gem install" as an arbitrary user
RUN mkdir -p "$GEM_HOME" \
 && chmod 777 "$GEM_HOME"
#  (BUNDLE_PATH = GEM_HOME, no need to mkdir/chown both)
CMD ["irb"]
