#  +++++++++++++++++++++++++++++++++++++++
#   Dockerfile for webdevops/php-official:7.0
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
FROM php:7.0-fpm
LABEL maintainer="info@webdevops.io" \
      vendor="WebDevOps.io" \
      io.webdevops.layout="8" \
      io.webdevops.version="1.5.0"
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
#   Baselayout copy (from staged image)
COPY --from=baselayout /baselayout /
COPY conf/ /opt/docker/
RUN set -x \
 && apt-update \
 && /usr/local/bin/generate-dockerimage-info \
 && sed -ri "s/(deb.*\/debian $( docker-image-info dist-codename ;) main)/\1 contrib non-free /" -- /etc/apt/sources.list \
 && apt-update \
 && /usr/local/bin/apt-upgrade \
 && apt-install apt-transport-https ca-certificates locales gnupg
RUN set -x \
 && chmod +x /opt/docker/bin/* \
 && apt-install supervisor wget curl net-tools tzdata \
 && chmod +s /sbin/gosu \
 && docker-run-bootstrap \
 && docker-image-cleanup
RUN set -x \
 && apt-install zip unzip bzip2 moreutils dnsutils openssh-client rsync git \
 && /usr/local/bin/generate-locales \
 && docker-run-bootstrap \
 && docker-image-cleanup
RUN set -x \
 && apt-install imagemagick graphicsmagick ghostscript libldap-2.4-2 libxslt1.1 zlib1g libpng16-16 libmcrypt4 libjpeg62-turbo-dev libfreetype6-dev libbz2-dev libicu-dev libldap2-dev libldb-dev libmcrypt-dev libxml2-dev libxslt1-dev zlib1g-dev libpng-dev \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install bcmath bz2 calendar exif intl gettext mysqli mcrypt hash pcntl pdo_mysql soap sockets tokenizer sysvmsg sysvsem sysvshm shmop xsl zip gd gettext opcache \
 && apt-get purge -y -f --force-yes libbz2-dev libicu-dev libldap2-dev libldb-dev libmcrypt-dev libxml2-dev libxslt1-dev zlib1g-dev libpng-dev \
 && pecl install apcu \
 && pecl install redis \
 && echo extension=apcu.so > /usr/local/etc/php/conf.d/apcu.ini \
 && echo extension=redis.so > /usr/local/etc/php/conf.d/redis.ini \
 && rm -f /usr/local/etc/php-fpm.d/zz-docker.conf \
 && curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin/ --filename=composer \
 && docker-service enable syslog \
 && docker-service enable cron \
 && docker-run-bootstrap \
 && docker-image-cleanup
WORKDIR /
EXPOSE 9000/tcp
ENTRYPOINT ["/entrypoint"]
CMD ["supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
