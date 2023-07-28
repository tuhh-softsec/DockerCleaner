FROM alpine:3.9
RUN apk add gmp-dev=6.1.2-r1 --no-cache
#   skip installing gem documentation
RUN mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc
ENV RUBY_MAJOR="2.5"
ENV RUBY_VERSION="2.5.5"
ENV RUBY_DOWNLOAD_SHA256="9bf6370aaa82c284f193264cc7ca56f202171c32367deceb3599a4f354175d7d"
ENV RUBYGEMS_VERSION="3.0.3"
#   some of ruby's build scripts are written in ruby
#     we purge system ruby later to make sure our final image uses what we just built
#   readline-dev vs libedit-dev: https://bugs.ruby-lang.org/issues/11869 and https://github.com/docker-library/ruby/issues/75
RUN set -ex \
 && apk add autoconf=2.69-r2 bison=3.0.5-r0 bzip2=1.0.6-r7 bzip2-dev=1.0.6-r7 ca-certificates=20191127-r2 coreutils=8.30-r0 dpkg-dev=1.19.2-r0 dpkg=1.19.2-r0 gcc=8.3.0-r0 gdbm-dev=1.13-r1 glib-dev=2.58.1-r3 libc-dev=0.7.1-r0 libffi-dev=3.2.1-r6 libxml2-dev=2.9.9-r3 libxslt-dev=1.1.33-r3 linux-headers=4.18.13-r1 make=4.2.1-r2 ncurses-dev=6.1_p20190105-r0 openssl=1.1.1k-r0 openssl-dev=1.1.1k-r0 procps=3.3.15-r0 readline-dev=7.0.003-r1 ruby=2.5.8-r0 tar=1.32-r0 xz=5.2.4-r0 yaml-dev=0.2.1-r0 zlib-dev=1.2.11-r1 --no-cache --virtual .ruby-builddeps \
 && wget -O ruby.tar.xz "https://cache.ruby-lang.org/pub/ruby/${RUBY_MAJOR%-rc}/ruby-$RUBY_VERSION.tar.xz" \
 && echo "$RUBY_DOWNLOAD_SHA256 *ruby.tar.xz" | sha256sum -c - \
 && mkdir -p /usr/src/ruby \
 && tar -xJf ruby.tar.xz -C /usr/src/ruby --strip-components=1 \
 && rm ruby.tar.xz \
 && cd /usr/src/ruby \
 && wget -O 'thread-stack-fix.patch' 'https://bugs.ruby-lang.org/attachments/download/7081/0001-thread_pthread.c-make-get_main_stack-portable-on-lin.patch' \
 && echo '3ab628a51d92fdf0d2b5835e93564857aea73e0c1de00313864a94a6255cb645 *thread-stack-fix.patch' | sha256sum -c - \
 && patch -p1 -i thread-stack-fix.patch \
 && rm thread-stack-fix.patch \
 && { echo '#define ENABLE_PATH_CHECK 0' ;echo ;cat file.c ; } > file.c.new \
 && mv file.c.new file.c \
 && autoconf \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && export ac_cv_func_isnan=yes ac_cv_func_isinf=yes \
 && ./configure --build="$gnuArch" --disable-install-doc --enable-shared \
 && make -j "$( nproc ;)" \
 && make install \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add bzip2=1.0.6-r7 ca-certificates=20191127-r2 libffi-dev=3.2.1-r6 procps=3.3.15-r0 yaml-dev=0.2.1-r0 zlib-dev=1.2.11-r1 $runDeps --no-network --virtual .ruby-rundeps \
 && apk del --no-network .ruby-builddeps \
 && cd / \
 && rm -r /usr/src/ruby \
 && ruby -e 'exit(Gem::Version.create(ENV["RUBYGEMS_VERSION"]) > Gem::Version.create(Gem::VERSION))' \
 && gem update --system "$RUBYGEMS_VERSION" \
 && rm -r /root/.gem/ \
 && ruby --version \
 && gem --version \
 && bundle --version
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
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
