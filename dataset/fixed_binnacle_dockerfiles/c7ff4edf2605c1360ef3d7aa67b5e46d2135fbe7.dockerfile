FROM alpine:3.7
LABEL maintainer="\"Adrian B. Danieli - https://github.com/sickp\""
COPY rootfs /
CMD ["irb"]
#   skip installing gem documentation
RUN mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc
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
CMD ["irb"]
ENV RUBY_MAJOR="2.4"
ENV RUBY_VERSION="2.4.3"
ENV RUBY_DOWNLOAD_SHA256="23677d40bf3b7621ba64593c978df40b1e026d8653c74a0599f0ead78ed92b51"
#   some of ruby's build scripts are written in ruby
#     we purge system ruby later to make sure our final image uses what we just built
#   readline-dev vs libedit-dev: https://bugs.ruby-lang.org/issues/11869 and https://github.com/docker-library/ruby/issues/75
RUN set -ex \
 && apk add autoconf=2.69-r0 bison=3.0.4-r0 bzip2=1.0.6-r7 bzip2-dev=1.0.6-r7 ca-certificates=20190108-r0 coreutils=8.28-r0 gcc=6.4.0-r5 gdbm-dev=1.13-r1 glib-dev=2.54.2-r1 libc-dev=0.7.1-r0 libffi-dev=3.2.1-r4 libxml2-dev=2.9.8-r1 libxslt-dev=1.1.31-r2 linux-headers=4.4.6-r2 make=4.2.1-r0 ncurses-dev=6.0_p20171125-r1 libressl=2.6.5-r0 libressl-dev=2.6.5-r0 procps=3.3.12-r3 readline-dev=7.0.003-r0 ruby=2.4.10-r0 tar=1.32-r0 yaml-dev=0.1.7-r0 zlib-dev=1.2.11-r1 xz=5.2.3-r1 --no-cache --virtual .ruby-builddeps \
 && wget -O ruby.tar.xz "https://cache.ruby-lang.org/pub/ruby/${RUBY_MAJOR%-rc}/ruby-$RUBY_VERSION.tar.xz" \
 && echo "$RUBY_DOWNLOAD_SHA256 *ruby.tar.xz" | sha256sum -c - \
 && mkdir -p /usr/src/ruby \
 && tar -xJf ruby.tar.xz -C /usr/src/ruby --strip-components=1 \
 && rm ruby.tar.xz \
 && cd /usr/src/ruby \
 && { echo '#define ENABLE_PATH_CHECK 0' ;echo ;cat file.c ; } > file.c.new \
 && mv file.c.new file.c \
 && autoconf \
 && ac_cv_func_isnan=yes ac_cv_func_isinf=yes ./configure --disable-install-doc --enable-shared \
 && make -j"$( getconf _NPROCESSORS_ONLN ;)" \
 && make install \
 && runDeps="$( scanelf --needed --nobanner --recursive /usr/local | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add bzip2=1.0.6-r7 ca-certificates=20190108-r0 libffi-dev=3.2.1-r4 libressl-dev=2.6.5-r0 yaml-dev=0.1.7-r0 procps=3.3.12-r3 zlib-dev=1.2.11-r1 $runDeps --virtual .ruby-rundeps \
 && apk del .ruby-builddeps \
 && cd / \
 && rm -r /usr/src/ruby
ENV RUBYGEMS_VERSION="2.7.4"
RUN gem update --system "$RUBYGEMS_VERSION"
ENV BUNDLER_VERSION="1.16.1"
RUN gem install bundler --version 2.4.12
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
