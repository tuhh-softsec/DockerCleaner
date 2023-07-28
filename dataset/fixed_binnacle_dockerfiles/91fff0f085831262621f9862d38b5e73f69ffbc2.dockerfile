FROM alpine:3.4
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
ENV RUBY_VERSION="2.4.0"
ENV RUBY_DOWNLOAD_SHA256="3a87fef45cba48b9322236be60c455c13fd4220184ce7287600361319bb63690"
#   some of ruby's build scripts are written in ruby
#     we purge system ruby later to make sure our final image uses what we just built
#   readline-dev vs libedit-dev: https://bugs.ruby-lang.org/issues/11869 and https://github.com/docker-library/ruby/issues/75
RUN set -ex \
 && apk add autoconf=2.69-r0 bison=3.0.4-r0 bzip2=1.0.6-r5 bzip2-dev=1.0.6-r5 ca-certificates=20161130-r0 coreutils=8.25-r0 gcc=5.3.0-r0 gdbm-dev=1.11-r1 glib-dev=2.48.0-r0 libc-dev=0.7-r0 libffi-dev=3.2.1-r2 libxml2-dev=2.9.5-r0 libxslt-dev=1.1.29-r1 linux-headers=4.4.6-r1 make=4.1-r1 ncurses-dev=6.0_p20171125-r0 openssl=1.0.2n-r0 openssl-dev=1.0.2n-r0 procps=3.3.9-r3 readline-dev=6.3.008-r4 ruby=2.3.7-r0 tar=1.29-r1 yaml-dev=0.1.6-r1 zlib-dev=1.2.11-r0 xz=5.2.2-r1 --no-cache --virtual .ruby-builddeps \
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
 && apk add bzip2=1.0.6-r5 ca-certificates=20161130-r0 libffi-dev=3.2.1-r2 openssl-dev=1.0.2n-r0 yaml-dev=0.1.6-r1 procps=3.3.9-r3 zlib-dev=1.2.11-r0 $runDeps --virtual .ruby-rundeps \
 && apk del .ruby-builddeps \
 && cd / \
 && rm -r /usr/src/ruby
ENV RUBYGEMS_VERSION="2.6.10"
RUN gem update --system "$RUBYGEMS_VERSION"
ENV BUNDLER_VERSION="1.14.5"
RUN gem install bundler --version 2.4.12
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
