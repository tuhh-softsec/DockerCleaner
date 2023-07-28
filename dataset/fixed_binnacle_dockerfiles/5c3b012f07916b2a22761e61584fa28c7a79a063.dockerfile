FROM mhart/alpine-node:latest
#   skip installing gem documentation
RUN mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc
ENV RUBY_MAJOR="2.1"
ENV RUBY_VERSION="2.1.9"
ENV RUBY_DOWNLOAD_SHA256="034cb9c50676d2c09b3b6cf5c8003585acea05008d9a29fa737c54d52c1eb70c"
ENV RUBYGEMS_VERSION="2.6.7"
#   some of ruby's build scripts are written in ruby
#     we purge system ruby later to make sure our final image uses what we just built
#   readline-dev vs libedit-dev: https://bugs.ruby-lang.org/issues/11869 and https://github.com/docker-library/ruby/issues/75
RUN set -ex \
 && apk add autoconf=2.71-r1 bison=3.8.2-r0 bzip2=1.0.8-r4 bzip2-dev=1.0.8-r4 ca-certificates=20220614-r4 coreutils=9.1-r0 gcc=12.2.1_git20220924-r4 gdbm-dev=1.23-r0 glib-dev=2.74.6-r0 libc-dev=0.7.2-r3 libffi-dev=3.4.4-r0 libxml2-dev=2.10.4-r0 libxslt-dev=1.1.37-r1 linux-headers=5.19.5-r0 make=4.3-r1 ncurses-dev=6.3_p20221119-r0 openssl=3.0.8-r3 openssl-dev=3.0.8-r3 procps=3.3.17-r2 readline-dev=8.2.0-r0 ruby=3.1.4-r0 tar=1.34-r2 yaml-dev=0.2.5-r0 zlib-dev=1.2.13-r0 --no-cache --virtual .ruby-builddeps \
 && wget -O ruby.tar.gz "https://cache.ruby-lang.org/pub/ruby/$RUBY_MAJOR/ruby-$RUBY_VERSION.tar.gz" \
 && echo "$RUBY_DOWNLOAD_SHA256 *ruby.tar.gz" | sha256sum -c - \
 && mkdir -p /usr/src/ruby \
 && tar -xzf ruby.tar.gz -C /usr/src/ruby --strip-components=1 \
 && rm ruby.tar.gz \
 && cd /usr/src/ruby \
 && { echo '#define ENABLE_PATH_CHECK 0' ;echo ;cat file.c ; } > file.c.new \
 && mv file.c.new file.c \
 && autoconf \
 && ac_cv_func_isnan=yes ac_cv_func_isinf=yes ./configure --disable-install-doc \
 && make -j"$( getconf _NPROCESSORS_ONLN ;)" \
 && make install \
 && runDeps="$( scanelf --needed --nobanner --recursive /usr/local | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add bzip2=1.0.8-r4 ca-certificates=20220614-r4 libffi-dev=3.4.4-r0 openssl-dev=3.0.8-r3 yaml-dev=0.2.5-r0 procps=3.3.17-r2 zlib-dev=1.2.13-r0 $runDeps --virtual .ruby-rundeps \
 && apk del .ruby-builddeps \
 && cd / \
 && rm -r /usr/src/ruby \
 && gem update --system "$RUBYGEMS_VERSION"
ENV BUNDLER_VERSION="1.13.2"
RUN gem install bundler --version 2.4.12
#   install things globally, for great justice
#   and don't create ".bundle" in all our apps
ENV GEM_HOME="/usr/local/bundle"
ENV BUNDLE_PATH="$GEM_HOME" \
    BUNDLE_BIN="$GEM_HOME/bin" \
    BUNDLE_SILENCE_ROOT_WARNING="1" \
    BUNDLE_APP_CONFIG="$GEM_HOME"
ENV PATH="$BUNDLE_BIN:$PATH"
RUN mkdir -p "$GEM_HOME" "$BUNDLE_BIN" \
 && chmod 777 "$GEM_HOME" "$BUNDLE_BIN"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
