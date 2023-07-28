FROM debian:buster-slim
ARG DEBIAN_FRONTEND=noninteractive
ARG BUILD_DATE
ARG DOCKER_TAG
ARG VCS_REF
LABEL maintainer="Emmanuel Dyan <emmanueldyan@gmail.com>" \
      org.label-schema.build-date="${BUILD_DATE}" \
      org.label-schema.name="${DOCKER_TAG}" \
      org.label-schema.description="Docker PHP Image based on Debian and including main modules" \
      org.label-schema.url="https://cloud.docker.com/u/edyan/repository/docker/edyan/php" \
      org.label-schema.vcs-url="https://github.com/edyan/docker-php" \
      org.label-schema.vcs-ref="${VCS_REF}" \
      org.label-schema.schema-version="1.0" \
      org.label-schema.vendor="edyan" \
      org.label-schema.docker.cmd="docker run -d --rm ${DOCKER_TAG}"
#  Set a default conf for apt install
RUN echo 'apt::install-recommends "false";' > /etc/apt/apt.conf.d/no-install-recommends \
 && apt-get update -y \
 && apt-get upgrade -y \
 && apt-get install ca-certificates iptables php7.3-bcmath php7.3-bz2 php7.3-cli php7.3-curl php7.3-fpm php7.3-gd php7.3-imap php7.3-intl php7.3-json php7.3-ldap php7.3-mbstring php7.3-mysql php7.3-opcache php7.3-pgsql php7.3-readline php7.3-soap php7.3-sqlite3 php7.3-tidy php7.3-xml php7.3-xmlrpc php7.3-xsl php7.3-zip php-apcu php-geoip php-imagick php-memcache php-ssh2 php-tideways -y \
 && apt-get install -y build-essential libmemcached11 libmemcachedutil2 libmemcached-dev php-pear php7.3-dev pkg-config zlib1g-dev \
 && pecl channel-update pecl.php.net \
 && pecl install memcached mongodb redis xdebug \
 && echo "extension=mongodb.so" > /etc/php/7.3/mods-available/mongodb.ini \
 && phpenmod mongodb \
 && echo "extension=memcached.so" > /etc/php/7.3/mods-available/memcached.ini \
 && phpenmod memcached \
 && echo "extension=redis.so" > /etc/php/7.3/mods-available/redis.ini \
 && phpenmod redis \
 && echo "zend_extension=xdebug.so" > /etc/php/7.3/mods-available/xdebug.ini \
 && phpenmod xdebug \
 && apt-get purge --autoremove -y build-essential libmemcached-dev php-pear php7.3-dev pkg-config zlib1g-dev \
 && apt-get autoremove -y \
 && apt-get autoclean \
 && apt-get clean \
 && find /root /tmp -mindepth 1 -delete \
 && rm -rf /build /var/lib/apt/lists/* /usr/share/man/* /usr/share/doc/* /var/cache/* /var/log/* /usr/share/php/docs /usr/share/php/tests /usr/lib/php/20170718 /etc/php/7.2
COPY php-cli.ini /etc/php/7.3/cli/conf.d/30-custom-php.ini
COPY php-fpm.ini /etc/php/7.3/fpm/conf.d/30-custom-php.ini
COPY www.conf /etc/php/7.3/fpm/pool.d/
#  For custom Configuration that comes from outside (via a docker compose mount)
RUN mkdir /etc/php/7.3/fpm/user-conf.d \
 && echo "; Default empty file" > /etc/php/7.3/fpm/user-conf.d/example.conf \
 && mkdir /var/log/php \
 && mkdir /home/www-data \
 && chown www-data:www-data /home/www-data \
 && usermod -d /home/www-data www-data \
 && mkdir -p /run/php \
 && chown www-data:www-data /run/php
COPY run.sh /run.sh
RUN chmod +x /run.sh
ENV ENVIRONMENT="dev"
ENV PHP_ENABLED_MODULES=""\""
ENV FPM_UID="33"
ENV FPM_GID="33"
EXPOSE 9000/tcp
CMD ["/run.sh"]
