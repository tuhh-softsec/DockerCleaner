FROM buildpack-deps:xenial
#  ============
#   Packages
#  ============
RUN echo "deb http://archive.ubuntu.com/ubuntu xenial main universe\n" > /etc/apt/sources.list \
 && echo "deb http://archive.ubuntu.com/ubuntu xenial-updates main universe\n" >> /etc/apt/sources.list \
 && echo "deb http://security.ubuntu.com/ubuntu xenial-security main universe\n" >> /etc/apt/sources.list
RUN apt-get update -qqy \
 && apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 swftools=0.9.2+git20130725-4.1~build0.16.04.1 git=1:2.7.4-0ubuntu1.10 xvfb=2:1.18.4-0ubuntu0.12 wget=1.17.1-1ubuntu1.5 bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 tzdata=2021a-0ubuntu0.16.04 sudo=1.8.16-0ubuntu1.10 unzip=6.0-20ubuntu1.1 cron=3.0pl1-128ubuntu2 locales=2.23-0ubuntu11.3 rsyslog=8.16.0-1ubuntu3.1 coreutils=8.25-2ubuntu3~16.04 -y
#   rsyslog: for get cron error logs
#   coreutils: for sleep command
#  =========
#   Ruby
#   see Dockerfiles on https://hub.docker.com/_/ruby/
#  =========
#   skip installing gem documentation
RUN mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc
ENV RUBY_MAJOR="2.4"
ENV RUBY_VERSION="2.4.2"
ENV RUBYGEMS_VERSION="2.7.2"
#   some of ruby's build scripts are written in ruby
#     we purge system ruby later to make sure our final image uses what we just built
RUN set -ex \
 && buildDeps=' bison libgdbm-dev ruby ' \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O ruby.tar.xz "https://cache.ruby-lang.org/pub/ruby/${RUBY_MAJOR%-rc}/ruby-$RUBY_VERSION.tar.xz" \
 && mkdir -p /usr/src/ruby \
 && tar -xJf ruby.tar.xz -C /usr/src/ruby --strip-components=1 \
 && rm ruby.tar.xz \
 && cd /usr/src/ruby \
 && { echo '#define ENABLE_PATH_CHECK 0' ;echo ;cat file.c ; } > file.c.new \
 && mv file.c.new file.c \
 && autoconf \
 && ./configure --disable-install-doc --enable-shared \
 && make -j"$( nproc ;)" \
 && make install \
 && apt-get purge -y --auto-remove $buildDeps \
 && cd / \
 && rm -r /usr/src/ruby
ENV BUNDLER_VERSION="1.16.0"
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
#  =========
#   ffmpeg
#  =========
RUN wget --no-verbose -O /tmp/ffmpeg.tar.gz https://johnvansickle.com/ffmpeg/releases/ffmpeg-release-amd64-static.tar.xz \
 && tar -C /tmp -xf /tmp/ffmpeg.tar.gz \
 && mv /tmp/ffmpeg-*-amd64-static/ffmpeg /usr/bin \
 && rm -rf /tmp/ffmpeg*
#  =========
#   rtmpdump
#  =========
RUN git clone git://git.ffmpeg.org/rtmpdump \
 && cd /rtmpdump \
 && make \
 && make install
#  =========
#   youtube-dl
#  =========
RUN wget https://yt-dl.org/downloads/latest/youtube-dl -O /usr/local/bin/youtube-dl \
 && chmod a+rx /usr/local/bin/youtube-dl
#  =========
#   livedl
#  =========
RUN wget https://github.com/yayugu/livedl/releases/download/20181107.38/livedl -O /usr/local/bin/livedl \
 && chmod a+rx /usr/local/bin/livedl
#  ============
#   Timezone
#   see: https://bugs.launchpad.net/ubuntu/+source/tzdata/+bug/1554806
#  ============
ENV TZ="\"Asia/Tokyo\""
RUN echo 'Asia/Tokyo' > /etc/timezone \
 && rm /etc/localtime \
 && dpkg-reconfigure --frontend noninteractive tzdata
#  ============
#   Locale
#  ============
ENV LC_ALL="C.UTF-8"
#  ============
#   Copy bundler env to /etc/environment to load on cron
#  ============
RUN printenv | grep -E "^BUNDLE" >> /etc/environment
#  ============
#   Rails
#  ============
RUN mkdir /myapp
WORKDIR /myapp
COPY Gemfile /myapp/Gemfile
COPY Gemfile.lock /myapp/Gemfile.lock
COPY niconico /myapp/niconico
RUN bundle install -j4 --without development test
COPY . /myapp
RUN RAILS_ENV=production bundle exec rake db:create db:migrate \
 && RAILS_ENV=production bundle exec whenever --update-crontab \
 && chmod 0600 /var/spool/cron/crontabs/root
CMD rsyslogd \
 && /usr/sbin/cron -f
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
