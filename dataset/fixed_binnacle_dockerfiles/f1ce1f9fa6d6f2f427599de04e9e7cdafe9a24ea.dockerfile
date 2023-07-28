FROM debian:stretch
MAINTAINER keopx <keopx@keopx.net>
#
#   Step 1: Installation
#
#   Set frontend. We'll clean this later on!
ENV DEBIAN_FRONTEND="noninteractive"
#   Locale
ENV LOCALE="es_ES.UTF-8"
#   GOTPL
ENV GOTPL_VER="0.1.5"
#   Default Document root.
ENV DEFAULT_ROOT="/var/www/html"
ARG UID=1000
ARG GID=1000
ARG UNAME=keopx
#   Set repositories
RUN echo "deb http://ftp.de.debian.org/debian/ stretch main non-free contrib" > /etc/apt/sources.list \
 && echo "deb-src http://ftp.de.debian.org/debian/ stretch main non-free contrib" >> /etc/apt/sources.list \
 && echo "deb http://security.debian.org/ stretch/updates main contrib non-free" >> /etc/apt/sources.list \
 && echo "deb-src http://security.debian.org/ stretch/updates main contrib non-free" >> /etc/apt/sources.list \
 && : \
 && apt-get -qqy upgrade
#   Install some basic tools needed for deployment
RUN (apt-get update ;apt-get install --no-install-recommends apt-utils=1.4.11 build-essential=12.3 debconf-utils=1.5.61 debconf=1.5.61 mysql-client=5.5.9999+default locales=2.24-11+deb9u4 curl=7.52.1-5+deb9u16 wget=1.18-5+deb9u3 unzip=6.0-21+deb9u2 patch=2.7.5-1+deb9u2 rsync=3.1.2-1+deb9u3 vim=2:8.0.0197-4+deb9u7 nano=2.7.4-1 openssh-client=1:7.4p1-10+deb9u7 git=1:2.11.0-3+deb9u7 bash-completion=1:2.1-4.3 locales=2.24-11+deb9u4 libjpeg-turbo-progs=1:1.5.1-2+deb9u2 libjpeg-progs pngcrush=1.7.85-1+b2 optipng=0.7.6-1+deb9u1 -yqq )
#   Install locale
RUN sed -i -e "s/# $LOCALE/$LOCALE/" /etc/locale.gen \
 && echo "LANG=$LOCALE" > /etc/default/locale \
 && dpkg-reconfigure --frontend=noninteractive locales \
 && update-locale LANG=$LOCALE
RUN gotpl_url="https://github.com/wodby/gotpl/releases/download/${GOTPL_VER}/gotpl-linux-amd64-${GOTPL_VER}.tar.gz" ; wget -qO- "${gotpl_url}" | tar xz -C /usr/local/bin
#   Configure Sury sources
#   @see https://www.noobunbox.net/serveur/auto-hebergement/installer-php-7-1-sous-debian-et-ubuntu
RUN (apt-get update ;apt-get install --no-install-recommends apt-transport-https=1.4.11 lsb-release=9.20161125 ca-certificates=20200601~deb9u2 -yqq ) \
 && wget -O /etc/apt/trusted.gpg.d/php.gpg https://packages.sury.org/php/apt.gpg \
 && echo "deb https://packages.sury.org/php/ $( lsb_release -sc ;) main" > /etc/apt/sources.list.d/php.list \
 && apt-get update -qq \
 && apt-get -qqy upgrade
#   Install PHP5.6 with Xdebug (dev environment). WARNING with install php5.6-apcu.
RUN (apt-get update ;apt-get install --no-install-recommends php5.6 php5.6-curl php5.6-dev php5.6-gd php5.6-dom php5.6-intl php5.6-json php5.6-mbstring php5.6-mcrypt php5.6-mysql php5.6-uploadprogress php5.6-ssh2 php5.6-xml php5.6-zip php5.6-solr php5.6-opcache php5.6-memcache php5.6-memcached php5.6-redis php5.6-xdebug libapache2-mod-php5.6 -yqq ) \
 && (apt-get update ;apt-get install --no-install-recommends php5.6-apcu -yqq )
#   Install manually xhprof
#   RUN \
#     pecl install xhprof-beta && \
#     echo "extension=xhprof.so" > /etc/php/5.6/mods-available/xhprof.ini && \
#     echo "xhprof.output_dir=/var/www/xhprof" >> /etc/php/5.6/mods-available/xhprof.ini
#   Install manually APC
RUN echo "extension=apcu.so" > /etc/php/5.6/mods-available/apcu_bc.ini \
 && echo "extension=apc.so" >> /etc/php/5.6/mods-available/apcu_bc.ini
#   Install SMTP.
RUN (apt-get update ;apt-get install --no-install-recommends ssmtp=2.64-8+b2 -y )
#   Install Apache web server.
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.25-3+deb9u13 -yqq )
#
#   Step 2: Configuration
#
#   Enable uploadprogress, imagick, redis and solr.
RUN phpenmod uploadprogress imagick redis solr
#   Disable by default apcu, apcu_bc, opcache, xdebug and xhprof. Use docker-compose.yml to add file.
RUN phpdismod apcu apcu_bc opcache xdebug xhprof
#   Remove all sites enabled
#   RUN rm /etc/apache2/sites-enabled/*
#   Configure needed apache modules and disable default site
RUN a2dismod mpm_event cgi
RUN a2enmod access_compat actions alias auth_basic authn_core authn_file authz_core authz_groupfile authz_host authz_user autoindex dir env expires filter headers mime negotiation php5.6 mpm_prefork reqtimeout rewrite setenvif status ssl
#   without the following line we get "AH00558: apache2: Could not reliably determine the server's fully qualified domain name"
#   autorise .htaccess files
RUN sed -i '/<Directory \/var\/www\/>/,/<\/Directory>/ s/AllowOverride None/AllowOverride All/' /etc/apache2/apache2.conf
#   Install composer (latest version) | prestissimo to speed up composer
RUN curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer \
 && composer global require "hirak/prestissimo:^0.3"
#  ## Install DRUSH (latest stable) ###
#   Run this in your terminal to get the latest DRUSH project version:
RUN composer global require drush/drush \
 && ~/.composer/vendor/bin/drush init
#  ## Install DRUPAL CONSOLE (latest version) ###
#   Run this in your terminal to get the latest project version:
RUN curl https://drupalconsole.com/installer -L -o drupal.phar \
 && mv drupal.phar /usr/local/bin/drupal \
 && chmod +x /usr/local/bin/drupal \
 && drupal self-update
#   Bash setup.
RUN echo ". /usr/share/bash-completion/bash_completion" >> ~/.bashrc \
 && echo "alias ll='ls -lahs'" >> ~/.bashrc
#
#   Step 3: Clean the system
#
#   Cleanup some things.
RUN apt-get -q autoclean \
 && rm -rf /var/lib/apt/lists/*
#
#   Step 4: Run
#
#   Create 'keopx' user like local machime user.
RUN groupadd -g $UID $GID ; useradd -m -u $UID -g $GID -s /bin/bash $UNAME ; usermod -aG www-data $UNAME ; echo ". /usr/share/bash-completion/bash_completion" >> ~/.bashrc \
 && echo "alias ll='ls -lahs'" >> /home/$UNAME/.bashrc
#   Working dir
WORKDIR ${DEFAULT_ROOT}
#   Configure templates
COPY templates /etc/gotpl/
COPY scripts/apache2-foreground /usr/bin/
EXPOSE 80/tcp 443/tcp
CMD ["apache2-foreground"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
