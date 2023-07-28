FROM ubuntu:16.04
ENV DEBIAN_FRONTEND="noninteractive"
ENV INITRD="No"
#  ## Build packages
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends git-core=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 build-essential=12.1ubuntu2 libssl-dev=1.0.2g-1ubuntu4.20 libreadline-dev=6.3-8ubuntu2 libyaml-dev=0.1.6-3 libsqlite3-dev=3.11.0-1ubuntu1.5 sqlite3=3.11.0-1ubuntu1.5 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 python-software-properties=0.96.20.10 libffi-dev=3.2.1-4 -y
#  ## Install rbenv
RUN git clone https://github.com/sstephenson/rbenv.git /usr/local/rbenv
RUN echo '# rbenv setup' > /etc/profile.d/rbenv.sh
RUN echo 'export RBENV_ROOT=/usr/local/rbenv' >> /etc/profile.d/rbenv.sh
RUN echo 'export PATH="$RBENV_ROOT/bin:$PATH"' >> /etc/profile.d/rbenv.sh
RUN echo 'eval "$(rbenv init -)"' >> /etc/profile.d/rbenv.sh
RUN chmod +x /etc/profile.d/rbenv.sh
#   install ruby-build
RUN mkdir /usr/local/rbenv/plugins
RUN git clone https://github.com/sstephenson/ruby-build.git /usr/local/rbenv/plugins/ruby-build
ENV RBENV_ROOT="/usr/local/rbenv"
ENV PATH="$RBENV_ROOT/bin:$RBENV_ROOT/shims:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
RUN rbenv install -v 2.4.1
RUN rbenv global 2.4.1
RUN ruby -v
RUN echo "gem: --no-document" > ~/.gemrc
RUN gem install bundler --version 2.4.12
#  ## Install node
#   gpg keys listed at https://github.com/nodejs/node
RUN set -ex \
 && for key in 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 FD3A5288F042B6850C66B31F09FE44734EB7990E 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 DD8F2338BAE7501E3DD5AC78C273792F7D83545D C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 B9AE9905FFD7803F25714661B63B535A4C206CA9 56730D5401028683275BD23C23EFEFE93C4CFFFE 77984A986EBC2AA786BC0F66B01FBB92821C587A; do gpg --keyserver pgp.mit.edu --recv-keys "$key" || gpg --keyserver keyserver.pgp.com --recv-keys "$key" || gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done
ENV NPM_CONFIG_LOGLEVEL="info"
ENV NODE_VERSION="8.9.0"
RUN buildDeps='xz-utils curl ca-certificates' \
 && set -x \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && curl -SLO "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-x64.tar.xz" \
 && curl -SLO "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " node-v$NODE_VERSION-linux-x64.tar.xz$" SHASUMS256.txt | sha256sum -c - \
 && tar -xJf "node-v$NODE_VERSION-linux-x64.tar.xz" -C /usr/local --strip-components=1 \
 && rm "node-v$NODE_VERSION-linux-x64.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt \
 && apt-get purge -y --auto-remove $buildDeps
RUN apt-get update -qq \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 tzdata=2021a-0ubuntu0.16.04 libsqlite3-dev=3.11.0-1ubuntu1.5 libmysqlclient-dev=5.7.33-0ubuntu0.16.04.1 mysql-client=5.7.33-0ubuntu0.16.04.1 libfontconfig1=2.11.94-0ubuntu1.1 libfontconfig1-dev=2.11.94-0ubuntu1.1 libicu-dev=55.1-7ubuntu0.5 libfreetype6=2.6.1-0.1ubuntu2.5 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libssl-dev=1.0.2g-1ubuntu4.20 libxft-dev=2.3.2-1 libpng-dev libjpeg-dev=8c-2ubuntu8 -y
#   for paperclip image manipulation
RUN apt-get install --no-install-recommends file=1:5.25-2ubuntu1.4 imagemagick=8:6.8.9.9-7ubuntu5.16 -y
#   for nokogiri
RUN apt-get install --no-install-recommends libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 -y
ENV APP_HOME="/myapp"
RUN mkdir $APP_HOME
WORKDIR $APP_HOME
#   Gems
COPY Gemfile Gemfile
COPY Gemfile.lock Gemfile.lock
RUN gem install bundler --version 2.4.12
RUN gem install foreman --version 0.87.2
RUN bundle install --without test
RUN rbenv rehash
#   All app
COPY . $APP_HOME
#   Install cron and start
RUN apt-get install --no-install-recommends cron=3.0pl1-128ubuntu2 -y
RUN touch /var/log/cron.log
#   Update the crontab
RUN whenever -w
EXPOSE 3000/tcp
CMD ["foreman", "start"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
