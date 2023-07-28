#   Build image
FROM ubuntu:16.04 AS build-env
#   install base packages, supervisor, cron, logrotate
RUN apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-9 build-essential=12.1ubuntu2 gcc=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 libjansson-dev=2.7-3ubuntu0.1 make=4.1-6 openssl=1.0.2g-1ubuntu4.20 software-properties-common=0.96.20.10 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -yq )
ENV DEBIAN_FRONTEND="noninteractive" \
    ZEPHIR_VERSION="0.11.12" \
    ZEPHIR_PARSER_VERSION="v1.3.1" \
    NGINX_VERSION="1.16.0" \
    NGINX_USER="www-data" \
    NGINX_CONF_PATH="/etc/nginx" \
    NGINX_LOG_PATH="/var/log/nginx" \
    NGINX_BUILD="Ubuntu" \
    PCRE_VERSION="8.43" \
    PCRE_PATH="/var/lib/pcre" \
    OPENSSL_VERSION="openssl-1.0.2o"
RUN LC_ALL=C.UTF-8 apt-add-repository -y ppa:ondrej/php \
 && :
#   PHP 7.2 with dependencies
RUN apt-get -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" install -y -f --no-install-recommends jq libc6-dev libexpat-dev libgeoip-dev libpcre3-dev libssh2-1-dev libssl-dev libxslt1-dev libyaml-dev lsb-release php7.2-cli php7.2-common php7.2-dev php7.2-xml re2c
#   Nginx dependencies
RUN apt-get -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" install -y -f --no-install-recommends libexpat-dev libmaxminddb-dev mmdb-bin libmaxminddb0
#   Turn the detached message off
RUN git config --global advice.detachedHead false
COPY ./installers /installers
#   Build custom extensions and move it to /artifacts folder
RUN mkdir /artifacts \
 && bash /installers/zephir-parser.sh \
 && bash /installers/zephir.sh \
 && bash /installers/pinba.sh
#   Update openssl from openssl 1.0.2g to 1.0.2o
RUN bash /installers/openssl.sh
#   configure NGINX from source
RUN bash /installers/nginx.sh
COPY ./configs/etc/nginx/nginx.conf /etc/nginx/nginx.conf
#   Application image
FROM phalconphp/base:ubuntu-16.04
LABEL description="Application image to use for production with PHP and Nginx" \
      maintainer="Serghei Iakovlev <serghei@phalconphp.com>" \
      vendor="Phalcon" \
      name="com.phalconphp.images.ubuntu-16.04.php-7.2"
ARG GITHUB_TOKEN
ARG GITHUB_USER
ARG PINBA_SERVER
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV DEBIAN_FRONTEND="noninteractive" \
    FPM_MAX_CHILDREN="32" \
    FPM_START_SERVERS="6" \
    FPM_MIN_SPARE_SERVERS="2" \
    FPM_MAX_SPARE_SERVERS="8" \
    FPM_PROCESS_IDLE_TIMEOUT="5" \
    FPM_MAX_REQUEST="1024" \
    PATH="/root/composer/vendor/bin:$PATH" \
    COMPOSER_HOME="/root/composer" \
    COMPOSER_ALLOW_SUPERUSER="1" \
    PCRE_PATH="/var/lib/pcre" \
    PINBA_SERVER="${PINBA_SERVER:-}" \
    GITHUB_USER="${GITHUB_USER:-}"
#   TODO: HEALTHCHECK
COPY ./configs/etc/dpkg/dpkg.cfg.d/ /etc/dpkg/dpkg.cfg.d/
RUN LC_ALL=C.UTF-8 apt-add-repository -y ppa:ondrej/php
#   Install php 7.2 with libs and other software
RUN : \
 && apt-get -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" install -y -f --no-install-recommends dnsutils iproute2 libc6-dev libgeoip-dev libpcre3-dev libssh2-1-dev libssl-dev libyaml-dev lsb-release php-amqp php-apcu-bc php-igbinary php-memcache php-pear php-sodium php-ssh2 php-xdebug php-yaml php7.2-bcmath php7.2-cgi php7.2-cli php7.2-common php7.2-curl php7.2-dba php7.2-dev php7.2-fpm php7.2-gd php7.2-gmp php7.2-imagick php7.2-imap php7.2-intl php7.2-json php7.2-mbstring php7.2-memcached php7.2-mongodb php7.2-msgpack php7.2-mysql php7.2-odbc php7.2-opcache php7.2-pgsql php7.2-phalcon php7.2-pspell php7.2-recode php7.2-redis php7.2-soap php7.2-tidy php7.2-xml php7.2-zip software-properties-common swig tmpreaper unzip wget
#   Create required directories
RUN mkdir -p /app /tmp/xdebug-{profile,outpt} /run/php /var/log/php /var/lib/nginx/{cache,proxy} $COMPOSER_HOME
COPY --from=build-env /artifacts/ /
COPY ./configs/etc/php/7.2/ /etc/php/7.2/
RUN wget -qO- https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer \
 && chmod +x /usr/local/bin/composer
#   Disable following modules by default
RUN phpdismod -v 7.2 apcu_bc \
 && phpdismod -v 7.2 apcu \
 && phpdismod -v 7.2 mongodb \
 && phpdismod -v 7.2 pinba \
 && phpdismod -v 7.2 xdebug
#   Nginx with GeoIP setup
RUN (apt-get update ;apt-get install --no-install-recommends libjansson-dev=2.7-3ubuntu0.1 libmaxminddb0=1.0.4-2.1 mmdb-bin=1.0.4-2.1 nginx=1.10.3-0ubuntu0.16.04.5 openssl=1.0.2g-1ubuntu4.20 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y )
RUN mkdir -p /usr/share/GeoIP \
 && wget -qO - http://geolite.maxmind.com/download/geoip/database/GeoLite2-City.mmdb.gz | gunzip -c > /usr/share/GeoIP/maxmind-city.mmdb \
 && wget -qO - http://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.mmdb.gz | gunzip -c > /usr/share/GeoIP/maxmind-country.mmdb
COPY --from=build-env $PCRE_PATH $PCRE_PATH
COPY --from=build-env /usr/sbin/nginx /usr/sbin/nginx
COPY ./configs/etc/nginx/ /etc/nginx/
#   Supervisor && logrotate
COPY ./configs/etc/supervisor/ /etc/supervisor/
COPY ./configs/etc/logrotate.d/ /etc/logrotate.d/
#   Startup scripts
COPY ./entrypoint.sh /entrypoint.sh
COPY ./entrypoint.d /entrypoint.d
#   Final check and preparing entrypoint
RUN echo \
 && nginx -v \
 && php --version | head -n 1 \
 && composer --version --no-ansi \
 && zephir --version \
 && php -r 'echo "Phalcon " . \Phalcon\Version::get() . "\n";' \
 && echo -n "Zephir Parser " \
 && php -d extension=`php-config --extension-dir `/zephir_parser.so --ri "Zephir Parser" | grep Version | awk '{print $3}' \
 && echo \
 && chmod +x /entrypoint.sh
#   Cleanup
RUN apt-get autoremove -y \
 && apt-get clean -y \
 && rm -rf /tmp/* /var/tmp/* \
 && rm -rf /usr/share/doc/* \
 && rm -rf /usr/share/man/* \
 && rm -rf /usr/share/locale/* \
 && rm -f /var/log/php7.2-fpm.log \
 && find /var/cache/apt/archives /var/lib/apt/lists -not -name lock -type f -delete \
 && find /var/cache -type f -not -name lock -delete \
 && find /var/log -type f | while read f ; do echo -n '' > ${f}; done
VOLUME /app  /var/log/nginx  /var/log/php  /tmp/xdebug-profile  /tmp/xdebug-outupt  /var/lib/nginx/cache  /var/lib/nginx/proxy
WORKDIR /app
#   Local Variables:
#   tab-width: 4
#   End:
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
