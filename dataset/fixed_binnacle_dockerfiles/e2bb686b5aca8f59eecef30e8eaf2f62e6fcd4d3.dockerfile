FROM bitnami/minideb:stretch
ENV PHP_VERSIONS="php5.6 php7.0 php7.1 php7.2 php7.3"
ENV PHP_DEFAULT_VERSION="7.2"
ENV PHP_INI="/etc/php/$PHP_DEFAULT_VERSION/fpm/php.ini"
ENV DRUSH_VERSION="8.2.3"
ENV DRUSH_LAUNCHER_VERSION="0.6.0"
ENV DRUSH_LAUNCHER_FALLBACK="/usr/local/bin/drush8"
ENV WP_CLI_VERSION="2.1.0"
ENV MAILHOG_VERSION="1.0.0"
ENV BACKDROP_DRUSH_VERSION="0.1.0"
ENV MKCERT_VERSION="v1.3.0"
ENV DEBIAN_FRONTEND="noninteractive"
ENV TERM="xterm"
ENV MH_SMTP_BIND_ADDR="127.0.0.1:1025"
ENV NGINX_SITE_TEMPLATE="/etc/nginx/nginx-site.conf"
ENV APACHE_SITE_TEMPLATE="/etc/apache2/apache-site.conf"
ENV WEBSERVER_DOCROOT="/var/www/html"
#   For backward compatibility only
ENV NGINX_DOCROOT="$WEBSERVER_DOCROOT"
ENV PATH="\"$PATH:/home/.composer/vendor/bin\""
#   composer normally screams about running as root, we don't need that.
ENV COMPOSER_ALLOW_SUPERUSER="1"
ENV COMPOSER_CACHE_DIR="/mnt/ddev-global-cache/composer"
#   Windows, especially Win10 Home/Docker toolbox, can take forever on composer build.
ENV COMPOSER_PROCESS_TIMEOUT="2000"
#   Defines vars in colon-separated notation to be subsituted with values for NGINX_SITE_TEMPLATE on start
#   NGINX_DOCROOT is for backward compatibility only, to break less people.
ENV NGINX_SITE_VARS="'$WEBSERVER_DOCROOT,$NGINX_DOCROOT'"
ENV APACHE_SITE_VARS="'$WEBSERVER_DOCROOT'"
RUN ln -fs /usr/share/zoneinfo/UTC /etc/localtime \
 && dpkg-reconfigure --frontend noninteractive tzdata
RUN apt-get update -qq \
 && apt-get install --no-install-recommends procps=2:3.3.12-3+deb9u1 curl=7.52.1-5+deb9u16 ca-certificates=20200601~deb9u2 apt-transport-https=1.4.11 wget=1.18-5+deb9u3 fontconfig=2.11.0-6.7+b1 bzip2=1.0.6-8.1 ghostscript=9.26a~dfsg-0+deb9u9 gnupg=2.1.18-8~deb9u4 locales-all=2.24-11+deb9u4 -qq --no-install-suggests -y \
 && wget -O /etc/apt/trusted.gpg.d/php.gpg https://packages.sury.org/php/apt.gpg \
 && echo "deb https://packages.sury.org/php/ stretch main" > /etc/apt/sources.list.d/php.list \
 && wget -q -O - https://packages.blackfire.io/gpg.key | apt-key add - \
 && echo "deb http://packages.blackfire.io/debian any main" > /etc/apt/sources.list.d/blackfire.list \
 && wget -q -O /tmp/nginx_signing.key http://nginx.org/keys/nginx_signing.key \
 && apt-key add /tmp/nginx_signing.key \
 && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && echo "deb http://nginx.org/packages/debian/ stretch nginx" >> /etc/apt/sources.list \
 && curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && apt-get update -qq \
 && apt-get install --no-install-recommends less=481-2.1 git=1:2.11.0-3+deb9u7 mysql-client=5.5.9999+default nginx=1.10.3-1+deb9u7 apache2=2.4.25-3+deb9u13 nodejs=4.8.2~dfsg-1 libcap2-bin=1:2.25-1 supervisor=3.3.1-1+deb9u1 sudo=1.8.19p1-2.1+deb9u3 imagemagick=8:6.9.7.4+dfsg-11+deb9u14 iputils-ping=3:20161105-1 patch=2.7.5-1+deb9u2 telnet=0.17-41 netcat=1.10-41 iproute2=4.9.0-1+deb9u1 vim=2:8.0.0197-4+deb9u7 nano=2.7.4-1 gettext=0.19.8.1-2+deb9u1 ncurses-bin=6.0+20161126-1+deb9u2 yarn zip=3.0-11+b1 unzip=6.0-21+deb9u2 rsync=3.1.2-1+deb9u3 locales-all=2.24-11+deb9u4 libpcre3=2:8.39-3 openssh-client=1:7.4p1-10+deb9u7 php-imagick=3.4.3~rc2-2+deb9u1 php-uploadprogress=1.0.3.1-4-g95d8a0f-4 -qq --no-install-suggests -y \
 && for v in $PHP_VERSIONS; do apt-get install --no-install-recommends $v-apcu $v-bcmath $v-bz2 $v-curl $v-cgi $v-cli $v-common $v-fpm $v-gd $v-intl $v-json $v-mysql $v-pgsql $v-mbstring $v-memcached $v-opcache $v-redis $v-soap $v-sqlite3 $v-readline $v-xdebug $v-xml $v-xmlrpc $v-zip libapache2-mod-$v -qq --no-install-suggests -y ; done \
 && for v in php5.6 php7.0 php7.1; do apt-get install --no-install-recommends $v-mcrypt -qq --no-install-suggests -y ; done \
 && apt-get install --no-install-recommends blackfire-php -y --allow-unauthenticated \
 && apt-get -qq autoremove -y \
 && apt-get -qq clean -y \
 && rm -rf /var/lib/apt/lists/*
#   Arbitrary user needs to be able to bind to privileged ports (for nginx and apache2)
RUN setcap CAP_NET_BIND_SERVICE=+eip /usr/sbin/nginx
RUN setcap CAP_NET_BIND_SERVICE=+eip /usr/sbin/apache2
COPY files /
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer global require hirak/prestissimo
RUN curl -sSL "https://github.com/drush-ops/drush/releases/download/${DRUSH_VERSION}/drush.phar" -o /usr/local/bin/drush8 \
 && chmod +x /usr/local/bin/drush8
RUN curl -sSL "https://github.com/drush-ops/drush-launcher/releases/download/${DRUSH_LAUNCHER_VERSION}/drush.phar" -o /usr/local/bin/drush \
 && chmod +x /usr/local/bin/drush
RUN curl -sSL "https://github.com/mailhog/MailHog/releases/download/v${MAILHOG_VERSION}/MailHog_linux_amd64" -o /usr/local/bin/mailhog
RUN curl -sSL "https://github.com/wp-cli/wp-cli/releases/download/v${WP_CLI_VERSION}/wp-cli-${WP_CLI_VERSION}.phar" -o /usr/local/bin/wp-cli
RUN curl -sSL "https://drupalconsole.com/installer" -L -o /usr/local/bin/drupal \
 && chmod +x /usr/local/bin/drupal
RUN curl -sSL https://github.com/backdrop-contrib/drush/releases/download/${BACKDROP_DRUSH_VERSION}/drush.zip -o /tmp/backdrop_drush.zip \
 && unzip /tmp/backdrop_drush.zip -d /var/tmp/backdrop_drush_commands
RUN mkdir -p /etc/nginx/sites-enabled /var/log/apache2 /var/run/apache2 /var/lib/apache2/module/enabled_by_admin /var/lib/apache2/module/disabled_by_admin \
 && touch /var/log/php-fpm.log \
 && chmod ugo+rw /var/log/php-fpm.log \
 && chmod ugo+rwx /var/run \
 && touch /var/log/nginx/access.log \
 && touch /var/log/nginx/error.log \
 && chmod -R ugo+rw /var/log/nginx/ \
 && chmod ugo+rx /usr/local/bin/* \
 && update-alternatives --set php /usr/bin/php${PHP_DEFAULT_VERSION} \
 && ln -s /usr/sbin/php-fpm${PHP_DEFAULT_VERSION} /usr/sbin/php-fpm
RUN chmod -R 777 /var/log
#   All users will have their home directory in /home, make it fully writeable
RUN mkdir -p /home/.composer /home/.drush/commands /home/.drush/aliases /mnt/ddev-global-cache/mkcert \
 && chmod -R ugo+rw /home /mnt/ddev-global-cache/
RUN chmod -R ugo+w /usr/sbin /usr/bin /etc/nginx /var/cache/nginx /run /var/www /etc/php/*/*/conf.d/ /var/lib/php/modules /etc/alternatives /usr/lib/node_modules /etc/php /etc/apache2 /var/log/apache2/ /var/run/apache2 /var/lib/apache2 /mnt/ddev-global-cache/*
RUN curl -sSL https://github.com/FiloSottile/mkcert/releases/download/$MKCERT_VERSION/mkcert-$MKCERT_VERSION-linux-amd64 -o /usr/local/bin/mkcert \
 && chmod +x /usr/local/bin/mkcert \
 && mkdir -p /home/.local/share \
 && ln -s /mnt/ddev-global-cache/mkcert /home/.local/share/mkcert \
 && mkcert -install
#   Except that .my.cnf can't be writeable or mysql won't use it.
RUN chmod 444 /home/.my.cnf
RUN touch /var/log/nginx/error.log /var/log/nginx/access.log /var/log/php-fpm.log \
 && chmod 666 /var/log/nginx/error.log /var/log/nginx/access.log /var/log/php-fpm.log
RUN for v in $PHP_VERSIONS; do a2dismod $v ; done
RUN a2dismod mpm_event
RUN a2enmod ssl
#   ssh is very particular about permissions in ~/.ssh
RUN chmod -R go-w /home/.ssh
#   scripts added last because they're most likely place to make changes, speeds up build
COPY scripts /
RUN chmod ugo+x /start.sh /healthcheck.sh
EXPOSE 80/tcp 8025/tcp
HEALTHCHECK --interval=3s --retries=6 CMD ["/healthcheck.sh"]
CMD ["/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
