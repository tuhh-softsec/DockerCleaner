FROM php:7.2-fpm
#  Install tini (init handler)
COPY https://github.com/krallin/tini/releases/download/v0.9.0/tini /tini
RUN chmod +x /tini
#  For running APT in non-interactive mode
ENV DEBIAN_FRONTEND="noninteractive"
#  Define build requirements, which can be removed after setup from the container
ENV PHPIZE_DEPS="autoconf             build-essential      file                 g++                  gcc                  libbz2-dev           libc-client-dev      libc-dev             libcurl4-gnutls-dev  libedit-dev          libfreetype6-dev     libgmp-dev           libicu-dev           libjpeg62-turbo-dev  libkrb5-dev          libmcrypt-dev        libpng-dev           libpq-dev            libsqlite3-dev       libssh2-1-dev        libxml2-dev          libxslt1-dev         make                 pkg-config           re2c                 librabbitmq-dev      libssl-dev"
#  Set Debian sources
RUN apt-get update \
 && apt-get install --no-install-recommends wget gnupg apt-transport-https -q -y \
 && echo "deb https://deb.nodesource.com/node_9.x stretch main" > /etc/apt/sources.list.d/node.list \
 && wget --quiet -O - https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
# Fixing the postgresql-client installation issue
RUN mkdir -p /usr/share/man/man7/ \
 && touch /usr/share/man/man7/ABORT.7.gz.dpkg-tmp \
 && mkdir -p /usr/share/man/man1/ \
 && touch /usr/share/man/man1/psql.1.gz
#  Install Debian packages
RUN apt-get update -qy \
 && apt-get install --no-install-recommends apt-utils ca-certificates curl debconf debconf-utils git git-core graphviz libedit2 libpq5 libsqlite3-0 mc netcat nginx nginx-extras nodejs patch postgresql-client psmisc python-dev python-setuptools redis-tools rsync ssmtp sudo supervisor unzip vim wget zip openssh-server $PHPIZE_DEPS -q -y \
 && mkdir /var/run/sshd \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-configure pgsql -with-pgsql=/usr/local/pgsql \
 && docker-php-ext-install -j$( nproc ;) bcmath bz2 gd gmp iconv intl mbstring mysqli pdo pdo_mysql pdo_pgsql pgsql readline soap xmlrpc xsl zip sockets \
 && pecl install -o -f redis \
 && rm -rf /tmp/pear \
 && echo "extension=redis.so" > $PHP_INI_DIR/conf.d/docker-php-ext-redis.ini \
 && pecl install amqp \
 && docker-php-ext-enable amqp \
 && easy_install j2cli \
 && curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/bin --filename=composer \
 && apt-get -qy autoremove \
 && apt-get -qy purge $PHPIZE_DEPS \
 && rm -rf /var/lib/apt/lists/*
#  Nginx configuration
COPY docker/nginx/conf.d /etc/nginx/conf.d
COPY docker/nginx/nginx.conf /etc/nginx/nginx.conf
COPY docker/nginx/fastcgi_params /etc/nginx/fastcgi_params
#  PHP-FPM configuration
RUN rm -f /usr/local/etc/php-fpm.d/*
COPY docker/php/php-fpm.conf /usr/local/etc/php-fpm.conf
COPY docker/php/pool.d/*.conf /usr/local/etc/php-fpm.d/
RUN echo "memory_limit = 512M" >> /usr/local/etc/php/php.ini
COPY docker/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh
RUN install -d -o www-data -g www-data -m 0755 /var/www
RUN chown -R www-data:www-data /var/www
WORKDIR /var/www
#  supervisord configuration
COPY docker/supervisord.conf /etc/supervisor/supervisord.conf
RUN mkdir -p /etc/nginx/waiting
COPY docker/nginx/waiting/waiting_vhost.conf /etc/nginx/waiting/waiting_vhost.conf
COPY docker/nginx/waiting/nginx_waiting.conf /etc/nginx/nginx_waiting.conf
#  Run app with entrypoints
ENTRYPOINT ["/tini", "--", "/entrypoint.sh"]
EXPOSE 8080/tcp 8081/tcp 22/tcp
# STOPSIGNAL SIGQUIT
CMD ["supervisord", "-c", "/etc/supervisor/supervisord.conf", "--nodaemon"]
