FROM nginx:latest
ENV DEBIAN_FRONTEND="noninteractive"
RUN echo "America/Sao_Paulo" > /etc/timezone
RUN dpkg-reconfigure -f noninteractive tzdata
# ## UPDATE DEBIAN
RUN apt-get update -y \
 && apt-get upgrade -y \
 && apt-get dist-upgrade -y \
 && apt-get autoremove -y
RUN apt-get update -y \
 && apt-get install --no-install-recommends sudo htop curl git libpng-dev libjpeg-dev libpq-dev vim imagemagick mysql-client bash-completion libfontconfig1 bzip2 build-essential software-properties-common locales wget openconnect netcat tig ca-certificates apt-transport-https xz-utils libfontconfig1 rsync gnupg -y
# ## INSTALL PHP-FPM AND EXTENSION
# # Add PHP7.2 source
RUN wget https://packages.sury.org/php/apt.gpg -O- | sudo apt-key add -
RUN echo "deb https://packages.sury.org/php/ stretch main" | sudo tee /etc/apt/sources.list.d/php.list
RUN apt-get update -y \
 && apt-get install --no-install-recommends php7.2-fpm php7.2-cli php7.2-common php7.2-curl php7.2-mbstring php7.2-mysql php7.2-xml php7.2-gd php7.2-json php7.2-ldap php7.2-sqlite3 php7.2-xmlrpc php-geoip php-imagick php-imap php-xdebug php-xml php-apcu libfcgi0ldbl -y
# ## CONFIGURE LOCALES
# ENV LOCALE_DEFAULT en_US.UTF8
ENV LOCALE_DEFAULT="pt_BR.UTF8"
RUN echo "LANGUAGE=$LOCALE_DEFAULT" >> /etc/environment
RUN echo "LANG=$LOCALE_DEFAULT" >> /etc/environment
RUN echo "LC_ALL=$LOCALE_DEFAULT" >> /etc/environment
RUN locale-gen $LOCALE_DEFAULT
RUN dpkg-reconfigure locales
# ## INSTALL COMPOSER
ENV COMPOSER_ALLOW_SUPERUSER="1"
ENV COMPOSER_DISABLE_XDEBUG_WARN="1"
RUN curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer \
 && composer self-update
# ## INSTALL NODEJS
ENV NPM_CONFIG_LOGLEVEL="info"
ENV NODE_VERSION="10.x"
RUN curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | sudo apt-key add -
RUN curl -sL https://deb.nodesource.com/setup_$NODE_VERSION | bash -
RUN apt-get update -y \
 && apt-get install --no-install-recommends nodejs -y
RUN npm install npm -g
RUN npm install bower gulp -g
RUN npm cache verify
# ## INSTALL DRUSH
RUN wget -O drush.phar https://github.com/drush-ops/drush/releases/download/8.1.18/drush.phar
RUN chmod +x drush.phar
RUN mv drush.phar /usr/local/bin/drush
RUN apt-get install --no-install-recommends unzip --yes
# ## CONFIGURE PHP-FPM
RUN echo "xdebug.max_nesting_level=9999" >> /etc/php/7.2/mods-available/xdebug.ini
RUN sed -i "s/;date.timezone =.*/date.timezone = UTC/" /etc/php/7.2/fpm/php.ini \
 && sed -i "s/memory_limit = 128M/memory_limit = 1256M/" /etc/php/7.2/fpm/php.ini \
 && sed -i "s/;cgi.fix_pathinfo=1/cgi.fix_pathinfo=0/" /etc/php/7.2/fpm/php.ini \
 && sed -i "s/display_errors = Off/display_errors = stderr/" /etc/php/7.2/fpm/php.ini \
 && sed -i "s/post_max_size = 8M/post_max_size = 30M/" /etc/php/7.2/fpm/php.ini \
 && sed -i "s/upload_max_filesize = 2M/upload_max_filesize = 30M/" /etc/php/7.2/fpm/php.ini \
 && sed -i "s/;opcache.enable=0/opcache.enable=1/" /etc/php/7.2/fpm/php.ini \
 && sed -i -e "s/;daemonize\s*=\s*yes/daemonize = no/g" /etc/php/7.2/fpm/php-fpm.conf \
 && sed -i '/^listen = /clisten = 9000' /etc/php/7.2/fpm/pool.d/www.conf \
 && sed -i '/^listen.allowed_clients/c;listen.allowed_clients =' /etc/php/7.2/fpm/pool.d/www.conf \
 && sed -i '/^;catch_workers_output/ccatch_workers_output = yes' /etc/php/7.2/fpm/pool.d/www.conf \
 && sed -i '/^;env\[TEMP\] = .*/aenv[DB_PORT_3306_TCP_ADDR] = $DB_PORT_3306_TCP_ADDR' /etc/php/7.2/fpm/pool.d/www.conf
# ## APPLY NGINX CONFIGURATION
RUN mkdir -p /tmp/logs
RUN chmod 777 /tmp/logs
COPY ./docker/nginx.conf /etc/nginx/nginx.conf
COPY ./docker/fastcgi.conf /etc/nginx/fastcgi.conf
COPY ./docker/bash.bashrc /etc/bash.bashrc
# ## CREATE USER FOR DEVELOPMENT
RUN echo "%sudo ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers \
 && useradd -u 1000 -G www-data,sudo -d /pece --shell /bin/bash -m pece \
 && echo "secret\nsecret" | passwd pece
COPY ./docker/bash.bashrc /etc/bash.bashrc
RUN chmod +x /etc/bash.bashrc
COPY ./docker/docker-entrypoint.sh /pece/docker-entrypoint.sh
RUN chmod +x /pece/docker-entrypoint.sh
USER pece
WORKDIR /pece
RUN mkdir ~/.drush
#  RUN drush init -y -bg --fallback=/usr/local/bin/drush
RUN cd ~/.drush \
 && git clone -b local_workflow_improvements --single-branch https://github.com/TallerWebSolutions/kraftwagen.git
#  RUN cd ~/.drush && drush cc drush
CMD ["bash"]
ENTRYPOINT ["/pece/docker-entrypoint.sh"]
