FROM debian:stretch
MAINTAINER keopx <keopx@keopx.net>
#
#  Step 1: Installation
#
#  Set frontend. We'll clean this later on!
ENV DEBIAN_FRONTEND="noninteractive"
#  Locale
ENV LOCALE="es_ES.UTF-8"
#  GOTPL
ENV GOTPL_VER="0.1.5"
#  Default Document root.
ENV DEFAULT_ROOT="/var/www/html"
ARG UID=1000
ARG GID=1000
ARG UNAME=keopx
#  Set repositories
RUN echo "deb http://ftp.de.debian.org/debian/ stretch main non-free contrib" > /etc/apt/sources.list \
 && echo "deb-src http://ftp.de.debian.org/debian/ stretch main non-free contrib" >> /etc/apt/sources.list \
 && echo "deb http://security.debian.org/ stretch/updates main contrib non-free" >> /etc/apt/sources.list \
 && echo "deb-src http://security.debian.org/ stretch/updates main contrib non-free" >> /etc/apt/sources.list \
 && apt-get update -qq \
 && apt-get -qqy upgrade
#  Install some basic tools needed for deployment
RUN apt-get install --no-install-recommends apt-utils build-essential debconf-utils debconf mysql-client locales curl wget unzip patch rsync vim nano openssh-client git bash-completion locales libjpeg-turbo-progs libjpeg-progs pngcrush optipng -yqq
#  Install locale
RUN sed -i -e "s/# $LOCALE/$LOCALE/" /etc/locale.gen \
 && echo "LANG=$LOCALE" > /etc/default/locale \
 && dpkg-reconfigure --frontend=noninteractive locales \
 && update-locale LANG=$LOCALE
RUN gotpl_url="https://github.com/wodby/gotpl/releases/download/${GOTPL_VER}/gotpl-linux-amd64-${GOTPL_VER}.tar.gz" ; wget -qO- "${gotpl_url}" | tar xz -C /usr/local/bin
#  Configure Sury sources
#  @see https://www.noobunbox.net/serveur/auto-hebergement/installer-php-7-1-sous-debian-et-ubuntu
RUN apt-get install --no-install-recommends apt-transport-https lsb-release ca-certificates -yqq \
 && wget -O /etc/apt/trusted.gpg.d/php.gpg https://packages.sury.org/php/apt.gpg \
 && echo "deb https://packages.sury.org/php/ $( lsb_release -sc ;) main" > /etc/apt/sources.list.d/php.list \
 && apt-get update -qq \
 && apt-get -qqy upgrade
#  Install PHP7 with Xdebug (dev environment)
RUN apt-get install --no-install-recommends php7.2 php7.2-bcmath php7.2-bz2 php7.2-curl php7.2-dev php7.2-gd php7.2-dom php7.2-imap php7.2-imagick php7.2-intl php7.2-json php7.2-ldap php7.2-mbstring php7.2-mysql php7.2-oauth php7.2-odbc php7.2-uploadprogress php7.2-ssh2 php7.2-xml php7.2-zip php7.2-solr php7.2-apcu php7.2-opcache php7.2-memcache php7.2-memcached php7.2-redis php7.2-xdebug libapache2-mod-php7.2 -yqq
#  Install manually xhprof
RUN cd /tmp \
 && wget https://github.com/Yaoguais/phpng-xhprof/archive/master.zip \
 && unzip master.zip \
 && cd phpng-xhprof-master \
 && phpize7.2 \
 && ./configure --with-php-config=/usr/bin/php-config7.2 \
 && make \
 && make install \
 && mv /usr/lib/php/20170718/phpng_xhprof.so /usr/lib/php/20170718/xhprof.so \
 && echo "extension=xhprof.so" > /etc/php/7.2/mods-available/xhprof.ini \
 && echo "xhprof.output_dir=/var/www/xhprof" >> /etc/php/7.2/mods-available/xhprof.ini
#  Install manually APC
RUN echo "extension=apcu.so" > /etc/php/7.2/mods-available/apcu_bc.ini \
 && echo "extension=apc.so" >> /etc/php/7.2/mods-available/apcu_bc.ini
#  Install SMTP.
RUN apt-get install --no-install-recommends ssmtp -y
#  Install Apache web server.
RUN apt-get install --no-install-recommends apache2 -yqq
#
#  Step 2: Configuration
#
#  Enable uploadprogress, imagick, redis and solr.
RUN phpenmod uploadprogress imagick redis solr
#  Disable by default apcu, apcu_bc, opcache, xdebug and xhprof. Use docker-compose.yml to add file.
RUN phpdismod apcu apcu_bc opcache xdebug xhprof
#  Remove all sites enabled
#  RUN rm /etc/apache2/sites-enabled/*
#  Configure needed apache modules and disable default site
RUN a2dismod mpm_event cgi
RUN a2enmod access_compat actions alias auth_basic authn_core authn_file authz_core authz_groupfile authz_host authz_user autoindex dir env expires filter headers mime negotiation php7.2 mpm_prefork reqtimeout rewrite setenvif status ssl
#  without the following line we get "AH00558: apache2: Could not reliably determine the server's fully qualified domain name"
#  autorise .htaccess files
RUN sed -i '/<Directory \/var\/www\/>/,/<\/Directory>/ s/AllowOverride None/AllowOverride All/' /etc/apache2/apache2.conf
#  Install composer (latest version) | prestissimo to speed up composer
RUN curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer \
 && composer global require "hirak/prestissimo:^0.3"
# ## Install DRUSH (latest stable) ###
#  Run this in your terminal to get the latest DRUSH project version:
RUN composer global require drush/drush \
 && ~/.composer/vendor/bin/drush init
# ## Install DRUPAL CONSOLE (latest version) ###
#  Run this in your terminal to get the latest project version:
RUN curl https://drupalconsole.com/installer -L -o drupal.phar \
 && mv drupal.phar /usr/local/bin/drupal \
 && chmod +x /usr/local/bin/drupal \
 && drupal self-update
#  Bash setup.
RUN echo ". /usr/share/bash-completion/bash_completion" >> ~/.bashrc \
 && echo "alias ll='ls -lahs'" >> ~/.bashrc
#
#  Step 3: Clean the system
#
#  Cleanup some things.
RUN apt-get -q autoclean \
 && rm -rf /var/lib/apt/lists/*
#
#  Step 4: Run
#
#  Create 'keopx' user like local machime user.
RUN groupadd -g $UID $GID ; useradd -m -u $UID -g $GID -s /bin/bash $UNAME ; usermod -aG www-data $UNAME ; echo ". /usr/share/bash-completion/bash_completion" >> ~/.bashrc \
 && echo "alias ll='ls -lahs'" >> /home/$UNAME/.bashrc
#  Working dir
WORKDIR ${DEFAULT_ROOT}
#  Configure templates
COPY templates /etc/gotpl/
COPY scripts/apache2-foreground /usr/bin/
EXPOSE 80/tcp 443/tcp
CMD ["apache2-foreground"]
