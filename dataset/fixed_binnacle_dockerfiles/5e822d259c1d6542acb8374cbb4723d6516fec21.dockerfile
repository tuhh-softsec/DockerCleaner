#  +++++++++++++++++++++++++++++++++++++++
#   Dockerfile for webdevops/samson-deployment:latest
#      -- automatically generated  --
#  +++++++++++++++++++++++++++++++++++++++
#   Staged baselayout builder
FROM webdevops/toolbox AS baselayout
RUN mkdir -p /baselayout/sbin /baselayout/usr/local/bin \
 && wget -O /tmp/baselayout-install.sh https://raw.githubusercontent.com/webdevops/Docker-Image-Baselayout/master/install.sh \
 && sh /tmp/baselayout-install.sh /baselayout \
 && wget -O "/baselayout/usr/local/bin/go-replace" "https://github.com/webdevops/goreplace/releases/download/1.1.2/gr-64-linux" \
 && chmod +x "/baselayout/usr/local/bin/go-replace" \
 && "/baselayout/usr/local/bin/go-replace" --version \
 && wget -O "/baselayout/sbin/gosu" "https://github.com/tianon/gosu/releases/download/1.10/gosu-amd64" \
 && wget -O "/tmp/gosu.asc" "https://github.com/tianon/gosu/releases/download/1.10/gosu-amd64.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && gpg --batch --verify /tmp/gosu.asc "/baselayout/sbin/gosu" \
 && rm -rf "$GNUPGHOME" /tmp/gosu.asc \
 && chmod +x "/baselayout/sbin/gosu" \
 && "/baselayout/sbin/gosu" nobody true
FROM zendesk/samson:latest
ENV TERM="xterm" \
    LANG="C.UTF-8" \
    LC_ALL="C.UTF-8"
ENV DOCKER_CONF_HOME="/opt/docker/" \
    LOG_STDOUT="" \
    LOG_STDERR=""
ENV APPLICATION_USER="application" \
    APPLICATION_GROUP="application" \
    APPLICATION_PATH="/app" \
    APPLICATION_UID="1000" \
    APPLICATION_GID="1000"
#  ##############################################################################
#   Bootstrap
#  ##############################################################################
#   Baselayout copy (from staged image)
COPY --from=baselayout /baselayout /
RUN set -x \
 && apt-update \
 && /usr/local/bin/generate-dockerimage-info \
 && sed -ri "s/(deb.*\/debian $( docker-image-info dist-codename ;) main)/\1 contrib non-free /" -- /etc/apt/sources.list \
 && apt-update \
 && /usr/local/bin/apt-upgrade \
 && apt-install apt-transport-https ca-certificates locales gnupg \
 && docker-image-cleanup
#  ##############################################################################
#   Base
#  ##############################################################################
COPY conf/ /opt/docker/
RUN set -x \
 && apt-install python-minimal python-setuptools python-pip python-paramiko python-jinja2 python-dev libffi-dev libssl-dev build-essential \
 && pip install pip==23.1 --upgrade \
 && hash -r \
 && pip install ansible==7.4.0 --no-cache-dir \
 && chmod 750 /usr/local/bin/ansible* \
 && apt-get purge -y -f --force-yes python-dev build-essential libssl-dev libffi-dev \
 && docker-image-cleanup
RUN set -x \
 && chmod +x /opt/docker/bin/* \
 && apt-install supervisor wget curl net-tools tzdata \
 && chmod +s /sbin/gosu \
 && docker-run-bootstrap \
 && docker-image-cleanup
#  ##############################################################################
#   Base-app
#  ##############################################################################
RUN set -x \
 && apt-install zip unzip bzip2 moreutils dnsutils openssh-client rsync git \
 && /usr/local/bin/generate-locales \
 && docker-run-bootstrap \
 && docker-image-cleanup
#  ##############################################################################
#   Samson
#  ##############################################################################
RUN set -x ENV RAILS_ENV="production"
ENV SQLITE_CLEANUP_DAYS="0"
#   NGINX reverse proxy
RUN export DEBIAN_FRONTEND=noninteractive \
 && set -x \
 && echo deb https://apt.dockerproject.org/repo debian-jessie main > /etc/apt/sources.list.d/docker.list \
 && curl -fsSL https://yum.dockerproject.org/gpg | apt-key add - \
 && apt-install nginx docker-engine php-cli php-mysqlnd php-mcrypt php-curl php-recode php-json openjdk-8-jre sqlite3 \
 && pip install docker-compose==1.29.2 python-dotenv==1.0.0 -U \
 && curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin/ --filename=composer \
 && chmod 755 /usr/local/bin/ansible* \
 && docker-image-cleanup
#   NPM stack
RUN curl -sL https://deb.nodesource.com/setup_9.x | bash - \
 && apt-get install --no-install-recommends nodejs=18.13.0+dfsg1-1ubuntu2 -y \
 && npm install gulp@4.0.2 -g \
 && npm install grunt-cli@1.4.3 -g \
 && npm install bower@1.8.14 -g \
 && npm install npm-cache@0.7.0 -g \
 && docker-image-cleanup
#   Deployer stack
RUN set -x \
 && apt-install build-essential \
 && ansible-galaxy install --force carlosbuenosvinos.ansistrano-deploy carlosbuenosvinos.ansistrano-rollback \
 && wget --quiet -O/usr/local/bin/dep http://deployer.org/deployer.phar \
 && chmod +x /usr/local/bin/dep \
 && gem install capistrano --version 3.17.2 \
 && docker-image-cleanup
#   Upload
COPY database.yml /app/config/database.yml
COPY web/ /app/public/assets/
COPY crontab /etc/cron.d/webdevops-samson-deployment
RUN rake assets:precompile \
 && docker-service enable cron \
 && /opt/docker/bin/provision run --tag bootstrap --role webdevops-samson-deployment \
 && /opt/docker/bin/bootstrap.sh \
 && docker-run-bootstrap \
 && docker-image-cleanup
EXPOSE 80/tcp
VOLUME /storage
ENTRYPOINT ["/entrypoint"]
CMD ["supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
