FROM ubuntu:14.04
ARG BUILD_DATE
ARG DEBIAN_FRONTEND=noninteractive
LABEL maintainer="Emmanuel Dyan <emmanueldyan@gmail.com>"
LABEL org.label-schema.build-date="$BUILD_DATE"
#   Set a default conf for apt install
RUN echo 'apt::install-recommends "false";' > /etc/apt/apt.conf.d/no-install-recommends \
 && apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends iptables=1.4.21-1ubuntu1 php5-apcu=4.0.2-2build1 php5-cli=5.5.9+dfsg-1ubuntu4.29 php5-curl=5.5.9+dfsg-1ubuntu4.29 php5-fpm=5.5.9+dfsg-1ubuntu4.29 php5-gd=5.5.9+dfsg-1ubuntu4.29 php5-geoip=1.0.8-5build1 php5-imagick=3.1.2-1build1 php5-imap=5.4.6-0ubuntu5.1 php5-intl=5.5.9+dfsg-1ubuntu4.29 php5-json=1.3.2-2build1 php5-ldap=5.5.9+dfsg-1ubuntu4.29 php5-mcrypt=5.4.6-0ubuntu5 php5-memcache=3.0.8-4build1 php5-memcached=2.1.0-6build1 php5-mysqlnd=5.5.9+dfsg-1ubuntu4.29 php5-pgsql=5.5.9+dfsg-1ubuntu4.29 php5-pspell=5.5.9+dfsg-1ubuntu4.29 php5-readline=5.5.9+dfsg-1ubuntu4.29 php5-recode=5.5.9+dfsg-1ubuntu4.29 php5-redis=2.2.4-1build2 php5-sqlite=5.5.9+dfsg-1ubuntu4.29 php5-xdebug=2.2.3-2build1 php5-xmlrpc=5.5.9+dfsg-1ubuntu4.29 php5-xsl=5.5.9+dfsg-1ubuntu4.29 libssh2-php=0.12-1ubuntu0.1 -y \
 && php5enmod imap mcrypt \
 && apt-get install --no-install-recommends build-essential=11.6ubuntu6 pkg-config=0.26-1ubuntu4 libssl-dev=1.0.1f-1ubuntu2.27 php5-dev=5.5.9+dfsg-1ubuntu4.29 -y \
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
#   For custom Configuration that comes from outside (via a docker compose mount)
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
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
