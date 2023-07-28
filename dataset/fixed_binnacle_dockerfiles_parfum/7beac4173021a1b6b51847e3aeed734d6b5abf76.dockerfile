FROM ubuntu:14.04
ARG BUILD_DATE
ARG DEBIAN_FRONTEND=noninteractive
LABEL maintainer="Emmanuel Dyan <emmanueldyan@gmail.com>"
LABEL org.label-schema.build-date="$BUILD_DATE"
#  Set a default conf for apt install
RUN echo 'apt::install-recommends "false";' > /etc/apt/apt.conf.d/no-install-recommends \
 && apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends -y iptables php5-apcu php5-cli php5-curl php5-fpm php5-gd php5-geoip php5-imagick php5-imap php5-intl php5-json php5-ldap php5-mcrypt php5-memcache php5-memcached php5-mysqlnd php5-pgsql php5-pspell php5-readline php5-recode php5-redis php5-sqlite php5-xdebug php5-xmlrpc php5-xsl libssh2-php \
 && php5enmod imap mcrypt \
 && apt-get install --no-install-recommends build-essential pkg-config libssl-dev php5-dev -y \
 && pecl channel-update pecl.php.net \
 && pecl install xhprof-beta mongo \
 && echo "extension=xhprof.so" > /etc/php5/mods-available/xhprof.ini \
 && php5enmod xhprof \
 && echo "extension=mongo.so" > /etc/php5/mods-available/mongo.ini \
 && php5enmod mongo \
 && apt-get purge -y build-essential pkg-config libssl-dev php5-dev \
 && apt-get autoremove -y \
 && apt-get autoclean \
 && apt-get clean \
 && find /root /tmp -mindepth 1 -delete \
 && rm -rf /build /var/lib/apt/lists/* /usr/share/man/* /usr/share/doc/* /var/cache/* /var/log/* /usr/share/php/docs /usr/share/php/tests
COPY php-cli.ini /etc/php5/cli/conf.d/30-custom-php.ini
COPY php-fpm.ini /etc/php5/fpm/conf.d/30-custom-php.ini
COPY www.conf /etc/php5/fpm/pool.d/
#  For custom Configuration that comes from outside (via a docker compose mount)
RUN mkdir /etc/php5/fpm/user-conf.d \
 && echo "; Default empty file" > /etc/php5/fpm/user-conf.d/example.conf \
 && mkdir /var/log/php \
 && mkdir /home/www-data \
 && chown www-data:www-data /home/www-data \
 && usermod -d /home/www-data www-data
COPY run.sh /run.sh
RUN chmod +x /run.sh
ENV ENVIRONMENT="dev"
ENV FPM_UID="33"
ENV FPM_GID="33"
EXPOSE 9000/tcp
CMD ["/run.sh"]
