FROM docker:edge-dind
RUN addgroup -g 2999 docker
#   From https://hub.docker.com/r/frolvlad/alpine-glibc/
RUN ALPINE_GLIBC_BASE_URL="https://github.com/sgerrand/alpine-pkg-glibc/releases/download" \
 && ALPINE_GLIBC_PACKAGE_VERSION="2.25-r0" \
 && ALPINE_GLIBC_BASE_PACKAGE_FILENAME="glibc-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && ALPINE_GLIBC_BIN_PACKAGE_FILENAME="glibc-bin-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && ALPINE_GLIBC_I18N_PACKAGE_FILENAME="glibc-i18n-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && apk add wget=1.20.3-r0 ca-certificates=20191127-r2 --no-cache --virtual=.build-dependencies \
 && apk add python py-pip --no-cache \
 && pip install pip==23.1 --upgrade --no-cache-dir \
 && pip install awscli==1.27.114 --no-cache-dir \
 && apk --purge del py-pip \
 && wget -q -O "/etc/apk/keys/sgerrand.rsa.pub" "https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub" \
 && wget -q "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_I18N_PACKAGE_FILENAME" \
 && apk add "$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_I18N_PACKAGE_FILENAME" --no-cache \
 && rm "/etc/apk/keys/sgerrand.rsa.pub" \
 && /usr/glibc-compat/bin/localedef --force --inputfile POSIX --charmap UTF-8 C.UTF-8 || true \
 && echo "export LANG=C.UTF-8" > /etc/profile.d/locale.sh \
 && apk del glibc-i18n \
 && rm "/root/.wget-hsts" \
 && apk del .build-dependencies \
 && rm "$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_I18N_PACKAGE_FILENAME"
#   The following is from docker/ruby, https://github.com/docker-library/ruby
RUN mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc
ENV RUBY_MAJOR="2.5"
ENV RUBY_VERSION="2.5.1"
ENV RUBY_DOWNLOAD_SHA256="886ac5eed41e3b5fc699be837b0087a6a5a3d10f464087560d2d21b3e71b754d"
ENV RUBYGEMS_VERSION="2.7.6"
ENV BUNDLER_VERSION="1.16.1"
#   some of ruby's build scripts are written in ruby
#     we purge system ruby later to make sure our final image uses what we just built
#   readline-dev vs libedit-dev: https://bugs.ruby-lang.org/issues/11869 and https://github.com/docker-library/ruby/issues/75
#   hadolint ignore=DL3003,DL3019,DL4006,SC2086
RUN set -ex \
 && apk add bash=4.4.19-r1 autoconf=2.69-r2 bison=3.0.5-r0 bzip2=1.0.6-r7 bzip2-dev=1.0.6-r7 ca-certificates=20191127-r2 coreutils=8.30-r0 dpkg-dev=1.19.2-r0 dpkg=1.19.2-r0 gcc=8.3.0-r0 gdbm-dev=1.13-r1 glib-dev=2.58.1-r3 libc-dev=0.7.1-r0 libffi-dev=3.2.1-r6 openssl=1.1.1k-r0 openssl-dev=1.1.1k-r0 libxml2-dev=2.9.9-r3 libxslt-dev=1.1.33-r3 linux-headers=4.18.13-r1 make=4.2.1-r2 ncurses-dev=6.1_p20190105-r0 procps=3.3.15-r0 readline-dev=7.0.003-r1 ruby=2.5.8-r0 tar=1.32-r0 xz=5.2.4-r0 yaml-dev=0.2.1-r0 zlib-dev=1.2.11-r1 --no-cache --virtual .ruby-builddeps \
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
 && export ac_cv_func_isnan=yes ac_cv_func_isinf=yes \
 && ./configure --build="$gnuArch" --disable-install-doc --enable-shared \
 && make -j "$( nproc ;)" \
 && make install \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add bzip2=1.0.6-r7 ca-certificates=20191127-r2 libffi-dev=3.2.1-r6 openssl-dev=1.1.1k-r0 procps=3.3.15-r0 yaml-dev=0.2.1-r0 zlib-dev=1.2.11-r1 $runDeps --virtual .ruby-rundeps \
 && cd / \
 && rm -r /usr/src/ruby \
 && gem update --system "$RUBYGEMS_VERSION" \
 && gem install bundler --version 2.4.12 --force \
 && rm -r /root/.gem/
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
#   End docker/ruby contents
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
