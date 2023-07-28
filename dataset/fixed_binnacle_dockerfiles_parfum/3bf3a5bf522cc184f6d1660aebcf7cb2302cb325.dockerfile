# upstream https://github.com/kwk/docker-registry-frontend
FROM debian:jessie
MAINTAINER 若虚 <slpcat@qq.com>
#  Container variables
ENV TERM="xterm" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8" \
    TIMEZONE="Asia/Shanghai"
#  Set timezone and locales
RUN echo "${TIMEZONE}" > /etc/timezone \
 && echo "$LANG UTF-8" > /etc/locale.gen \
 && apt-get update -q \
 && DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends install -yq apt-utils dialog locales \
 && ln -sf /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && update-locale LANG=$LANG \
 && locale-gen $LANG \
 && DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales
#  Install required packages
RUN apt-get dist-upgrade -y
USER root
# ###########################################################
#  Setup environment variables
# ###########################################################
ENV WWW_DIR="/var/www/html"
ENV SOURCE_DIR="/tmp/source"
ENV START_SCRIPT="/root/start-apache.sh"
RUN mkdir -pv $WWW_DIR
# ###########################################################
#  Speedup DPKG and don't use cache for packages
# ###########################################################
#  Taken from here: https://gist.github.com/kwk/55bb5b6a4b7457bef38d
#
#  this forces dpkg not to call sync() after package extraction and speeds up
#  install
RUN echo "force-unsafe-io" > /etc/dpkg/dpkg.cfg.d/02apt-speedup
#  # we don't need and apt cache in a container
RUN echo "Acquire::http {No-Cache=True;};" > /etc/apt/apt.conf.d/no-cache
# ###########################################################
#  Create dirs
RUN mkdir -p $SOURCE_DIR/dist $SOURCE_DIR/app $SOURCE_DIR/test $SOURCE_DIR/.git
#  Add dirs
COPY app $SOURCE_DIR/app
COPY test $SOURCE_DIR/test
#  Dot files
COPY .jshintrc $SOURCE_DIR/
COPY .bowerrc $SOURCE_DIR/
COPY .editorconfig $SOURCE_DIR/
COPY .travis.yml $SOURCE_DIR/
#  Other files
COPY bower.json $SOURCE_DIR/
COPY Gruntfile.js $SOURCE_DIR/
COPY LICENSE $SOURCE_DIR/
COPY package.json $SOURCE_DIR/
COPY README.md $SOURCE_DIR/
#  Add some git files for versioning
COPY .git/HEAD $SOURCE_DIR/.git/HEAD
COPY .git/refs $SOURCE_DIR/.git/refs
# ###########################################################
#  Install and configure webserver software
# ###########################################################
RUN apt-get update -y \
 && export DEBIAN_FRONTEND=noninteractive \
 && apt-get install --no-install-recommends apache2 bzip2 libapache2-mod-auth-kerb libapache2-mod-proxy-html git nodejs nodejs-legacy npm -y \
 && a2enmod proxy \
 && a2enmod proxy_http \
 && cd $SOURCE_DIR \
 && export GITREF=$( cat .git/HEAD | cut -d" " -f2 ;) \
 && export GITSHA1=$( cat .git/$GITREF ;) \
 && echo "{\"git\": {\"sha1\": \"$GITSHA1\", \"ref\": \"$GITREF\"}}" > $WWW_DIR/app-version.json \
 && cd $SOURCE_DIR \
 && rm -rf $SOURCE_DIR/.git \
 && git config --global url."https://".insteadOf git:// \
 && cd $SOURCE_DIR \
 && npm install \
 && node_modules/bower/bin/bower install --allow-root \
 && node_modules/grunt-cli/bin/grunt build --allow-root \
 && cp -rf $SOURCE_DIR/dist/* $WWW_DIR \
 && rm -rf $SOURCE_DIR \
 && apt-get -y --auto-remove purge git nodejs nodejs-legacy npm bzip2 \
 && apt-get -y autoremove \
 && apt-get -y clean \
 && rm -rf /var/lib/apt/lists/*
# ###########################################################
#  Add and enable the apache site and disable all other sites
# ###########################################################
RUN a2dissite 000*
COPY apache-site.conf /etc/apache2/sites-available/docker-site.conf
RUN a2ensite docker-site.conf
ADD start-apache.sh $START_SCRIPT
RUN chmod +x $START_SCRIPT
ENV APACHE_RUN_USER="www-data"
ENV APACHE_RUN_GROUP="www-data"
ENV APACHE_LOG_DIR="/var/log/apache2"
#  Let people know how this was built
COPY Dockerfile /root/Dockerfile
#  Exposed ports
EXPOSE 80/tcp 443/tcp
VOLUME ["/etc/apache2/server.crt", "/etc/apache2/server.key"]
CMD $START_SCRIPT
