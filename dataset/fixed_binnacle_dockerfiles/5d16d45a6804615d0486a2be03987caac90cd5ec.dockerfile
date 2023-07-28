FROM alpine:3.6
MAINTAINER Alessandro Molari <molari.alessandro@gmail.com> (alem0lars)
#   ────────────────────────────────────────────── Setup basic system packages ──┐
RUN apk update \
 && apk upgrade
#   Install libraries.
RUN apk add linux-headers=4.4.6-r2 build-base=0.5-r0 openssl-dev=1.0.2r-r0 libc-dev=0.7.1-r0 libxml2-dev=2.9.8-r1 libxslt-dev=1.1.29-r4 libffi-dev=3.2.1-r3 readline-dev=6.3.008-r5 jemalloc-dev=4.5.0-r0 g++=6.3.0-r4 musl-dev=1.1.16-r15 make=4.2.1-r0 --update --no-cache
#   Install programs.
RUN apk add git=2.13.7-r2 bash=4.3.48-r1 curl=7.61.1-r2 wget=1.20.3-r0 zsh=5.3.1-r1 --update --no-cache
#   ─────────────────────────────────────────────────────────────────────────────┘
#   ─────────────────────────────────────── Setup common environment variables ──┐
ENV HOME="/root"
#   ─────────────────────────────────────────────────────────────────────────────┘
#   ─────────────────────────────────────────────────────────── Setup ruby (1) ──┐
ARG RUBY_MAJOR=2
ARG RUBY_MINOR=4
ARG RUBY_PATCH=0
ARG RUBY_OTHER
ARG RUBY_SHA256=152fd0bd15a90b4a18213448f485d4b53e9f7662e1508190aa5b702446b29e3d
ARG RUBYGEMS_FULL_VERSION=2.6.12
ENV RUBY_VERSION="${RUBY_MAJOR}.${RUBY_MINOR}"
ENV RUBY_FULL_VERSION="${RUBY_VERSION}.${RUBY_PATCH}${RUBY_OTHER}"
#   skip installing gem documentation
RUN mkdir -p /usr/local/etc \
 && { echo 'install: --no-document' ;echo 'update: --no-document' ; } >> /usr/local/etc/gemrc
#   some of ruby's build scripts are written in ruby
#     we purge system ruby later to make sure our final image uses what we just built
#   readline-dev vs libedit-dev: https://bugs.ruby-lang.org/issues/11869 and https://github.com/docker-library/ruby/issues/75
RUN apk add autoconf=2.69-r0 bison=3.0.4-r0 bzip2=1.0.6-r5 bzip2-dev=1.0.6-r5 ca-certificates=20161130-r3 coreutils=8.27-r0 dpkg-dev=1.18.23-r2 dpkg=1.18.23-r2 gcc=6.3.0-r4 gdbm-dev=1.12-r0 glib-dev=2.52.1-r0 libc-dev=0.7.1-r0 libffi-dev=3.2.1-r3 libxml2-dev=2.9.8-r1 libxslt-dev=1.1.29-r4 linux-headers=4.4.6-r2 make=4.2.1-r0 ncurses-dev=6.0_p20171125-r1 openssl=1.0.2r-r0 openssl-dev=1.0.2r-r0 procps=3.3.12-r1 readline-dev=6.3.008-r5 ruby=2.4.6-r0 tar=1.32-r0 yaml-dev=0.1.7-r0 zlib-dev=1.2.11-r0 xz=5.2.3-r0 --no-cache --virtual .ruby-builddeps
RUN set -ex \
 && wget -O ruby.tar.gz "https://cache.ruby-lang.org/pub/ruby/${RUBY_VERSION}/ruby-$RUBY_FULL_VERSION.tar.gz" \
 && echo "$RUBY_SHA256 *ruby.tar.gz" | sha256sum -c - \
 && mkdir -p /usr/src/ruby \
 && tar -xf ruby.tar.gz -C /usr/src/ruby --strip-components=1 \
 && rm ruby.tar.gz \
 && cd /usr/src/ruby \
 && { echo '#define ENABLE_PATH_CHECK 0' ;echo ;cat file.c ; } > file.c.new \
 && mv file.c.new file.c \
 && autoconf \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && export ac_cv_func_isnan=yes ac_cv_func_isinf=yes \
 && ./configure --build="$gnuArch" --disable-install-doc --enable-shared \
 && make -j "$( nproc ;)" \
 && make install \
 && runDeps="$( scanelf --needed --nobanner --recursive /usr/local | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add bzip2=1.0.6-r5 ca-certificates=20161130-r3 libffi-dev=3.2.1-r3 openssl-dev=1.0.2r-r0 yaml-dev=0.1.7-r0 procps=3.3.12-r1 zlib-dev=1.2.11-r0 $runDeps --virtual .ruby-rundeps \
 && apk del .ruby-builddeps \
 && cd / \
 && rm -r /usr/src/ruby \
 && gem update --system "$RUBYGEMS_FULL_VERSION"
#   ─────────────────────────────────────────────────────────────────────────────┘
#   ─────────────────────────────────────────────────────────── Setup ruby (2) ──┐
#   Install bundler.
RUN gem install bundler --version 2.4.12
#   Install things globally and don't create `.bundle` in all our apps.
ENV GEM_HOME="/usr/local/bundle"
ENV BUNDLE_PATH="${GEM_HOME}" \
    BUNDLE_BIN="${GEM_HOME}/bin" \
    BUNDLE_SILENCE_ROOT_WARNING="1" \
    BUNDLE_APP_CONFIG="$GEM_HOME"
ENV PATH="$BUNDLE_BIN:$PATH"
RUN mkdir -p "${GEM_HOME}" "${BUNDLE_BIN}" \
 && chmod 777 "${GEM_HOME}" "${BUNDLE_BIN}"
#   ─────────────────────────────────────────────────────────────────────────────┘
#   ──────────────────────────────────────────────────────────────── Setup ssh ──┐
#   Install ssh daemon.
RUN apk add openssh=7.5_p1-r4 --update --no-cache
#   Generate fresh keys.
RUN ssh-keygen -f /etc/ssh/ssh_host_rsa_key -N '' -t rsa \
 && ssh-keygen -f /etc/ssh/ssh_host_dsa_key -N '' -t dsa
#   Prepare ssh run directory.
RUN mkdir -p /var/run/sshd
#   Configure ssh.
RUN echo "StrictHostKeyChecking=no" >> /etc/ssh/ssh_config
RUN sed -i 's|[#]*PasswordAuthentication yes|PasswordAuthentication no|g' /etc/ssh/sshd_config
#   Expose ssh port.
EXPOSE 22/tcp
#   ─────────────────────────────────────────────────────────────────────────────┘
#   ──────────────────────────────────────────────────────────────── Setup git ──┐
ENV GIT_PWD="git"
ENV GIT_USER="git"
ENV GIT_GROUP="git"
ENV GIT_REPOS_DIR="/git"
#   Install git packages.
RUN apk add git-daemon=2.13.7-r2 git=2.13.7-r2 --update --no-cache
#   Setup a git user and ssh.
RUN addgroup "${GIT_GROUP}" \
 && echo -e "${GIT_PWD}\n${GIT_PWD}\n" | adduser -G "${GIT_GROUP}" -h "${GIT_REPOS_DIR}" -s /usr/bin/git-shell "${GIT_USER}"
#   Remove the annoying `/etc/motd`.
RUN rm -rf /etc/update-motd.d /etc/motd /etc/motd.dynamic
RUN ln -fs /dev/null /run/motd.dynamic
#   Configure local git client.
#   TODO: Replace with fizzy config (when `--no-ask` is implemented).
RUN git config --global push.default simple \
 && git config --global user.name root \
 && git config --global user.email root@localhost.localdomain
#   SSH keys for user `root`
COPY docker/ssh_key.pub "/root/.ssh/authorized_keys"
COPY docker/ssh_key.pub "/root/.ssh/id_rsa.pub"
COPY docker/ssh_key "/root/.ssh/id_rsa"
RUN chmod 700 "/root/.ssh"
RUN chmod 600 /root/.ssh/*
#   SSH keys for user `git`
COPY docker/ssh_key.pub "${GIT_REPOS_DIR}/.ssh/authorized_keys"
COPY docker/ssh_key.pub "${GIT_REPOS_DIR}/.ssh/id_rsa.pub"
COPY docker/ssh_key "${GIT_REPOS_DIR}/.ssh/id_rsa"
RUN chmod 700 "${GIT_REPOS_DIR}/.ssh"
RUN chmod 600 ${GIT_REPOS_DIR}/.ssh/*
RUN chown -R "${GIT_USER}:${GIT_GROUP}" "${GIT_REPOS_DIR}/.ssh"
#   ─────────────────────────────────────────────────────────────────────────────┘
#   ────────────────────────────────────────────────────────────── Setup fizzy ──┐
#   Install fizzy dependencies.
RUN gem install thor --version 1.2.1
RUN apk add sudo=1.8.19_p2-r0 --update --no-cache
#   Install fizzy.
#   TODO: change back to branch `master`.
RUN curl -sL https://raw.githubusercontent.com/alem0lars/fizzy/develop/build/fizzy | tee /usr/local/bin/fizzy > /dev/null \
 && chmod +x /usr/local/bin/fizzy
#   ─────────────────────────────────────────────────────────────────────────────┘
#   ─────────────────────────────────────────────────────────── Setup ruby (3) ──┐
RUN fizzy cfg s -C ruby -U https:alem0lars/configs-ruby
RUN fizzy qi -V docker-test-box -C ruby -I ruby
#   ─────────────────────────────────────────────────────────────────────────────┘
#   ──────────────────────────────────────────────────────────── Setup rsyslog ──┐
RUN apk add rsyslog=8.26.0-r0 --update --no-cache
COPY docker/rsyslog.conf /etc/rsyslog.d/90-fizzy.conf
#   ─────────────────────────────────────────────────────────────────────────────┘
#   ──────────────────────────────────────────────────────── Setup supervisord ──┐
RUN apk add supervisor=3.2.4-r0 --update --no-cache
COPY docker/supervisord.ini /etc/supervisor.d/supervisord.ini
#   ─────────────────────────────────────────────────────────────────────────────┘
#   ──────────────────────────────────────────────────────────────── Setup app ──┐
ENV APP_DIR="${HOME}/fizzy"
#   ──────────────────────────── (trick to allow caching) Install dependencies ──┤
WORKDIR /tmp
COPY ./Gemfile Gemfile
COPY ./Gemfile.lock Gemfile.lock
RUN bundle install --without website
RUN rm ./Gemfile \
 && rm ./Gemfile.lock
#   ────────────────────────────────────────────────────────────── Add the app ──┤
COPY . "${APP_DIR}"
WORKDIR "${APP_DIR}"
RUN bundle install --without website
RUN bundle exec rake build
#   ─────────────────────────────────────────────────────────────────────────────┘
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisord.conf"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
