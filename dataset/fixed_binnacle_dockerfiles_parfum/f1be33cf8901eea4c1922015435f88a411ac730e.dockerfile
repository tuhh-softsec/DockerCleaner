FROM ubuntu:12.04
MAINTAINER Boris Böhne <info@drubb.de>
#
#  Step 1: Installation
#
#  Set frontend. We'll clean this later on!
ARG DEBIAN_FRONTEND=noninteractive
#  Expose web root as volume
VOLUME ["/var/www"]
#  Add additional repostories needed later
RUN echo "deb http://ppa.launchpad.net/git-core/ppa/ubuntu precise main" >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E1DF1F24
#  Update repositories cache and distribution
RUN apt-get update -qq \
 && apt-get -qqy upgrade
#  Install some basic tools needed for deployment
RUN apt-get install --no-install-recommends apt-utils sudo build-essential debconf-utils locales curl wget unzip patch dkms supervisor -yqq
#  Add the docker user
ENV HOME="/home/docker"
RUN useradd docker \
 && passwd -d docker \
 && adduser docker sudo
RUN mkdir -p $HOME \
 && chown -R docker:docker $HOME
#  Install SSH client
RUN apt-get install --no-install-recommends openssh-client -yqq
#  Install ssmtp MTA
RUN apt-get install --no-install-recommends ssmtp -yqq
#  Install Apache web server
RUN apt-get install --no-install-recommends apache2-mpm-prefork -yqq
#  Install MySQL server and save initial configuration
RUN echo "mysql-server mysql-server/root_password password root" | debconf-set-selections
RUN echo "mysql-server mysql-server/root_password_again password root" | debconf-set-selections
RUN apt-get install --no-install-recommends mysql-server -yqq
RUN service mysql start \
 && sleep 5 \
 && service mysql stop \
 && tar cpPzf /mysql.tar.gz /var/lib/mysql
#  Install PHP5 with Xdebug, APC and other modules
RUN apt-get install --no-install-recommends libapache2-mod-php5 php5-mcrypt php5-dev php5-mysql php5-curl php5-gd php5-intl php5-xdebug php-apc -yqq
#  Install PEAR package manager
RUN apt-get install --no-install-recommends php-pear -yqq \
 && pear channel-update pear.php.net \
 && pear upgrade-all
#  Install PECL package manager
RUN apt-get install --no-install-recommends libpcre3-dev -yqq
#  Install PECL uploadprogress extension
RUN pecl install uploadprogress
#  Update APC to latest version
RUN printf "\n" | pecl update apc
#  Install memcached service
RUN apt-get install --no-install-recommends memcached php5-memcached -yqq
#  Install GIT (latest version)
RUN apt-get install --no-install-recommends git -yqq
#  Install composer (latest version)
RUN curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer
#  Install drush (latest stable for php 5.3)
USER docker
RUN sudo composer global require "drush/drush:7.*"
USER root
#  Install PhpMyAdmin (latest php 5.3 compatible version)
RUN wget -q -O phpmyadmin.zip https://github.com/phpmyadmin/phpmyadmin/archive/MAINT_4_4_15.zip \
 && unzip -qq phpmyadmin.zip
RUN rm phpmyadmin.zip \
 && mv phpmyadmin-MAINT_4_4_15 /opt/phpmyadmin
#  Install zsh / OH-MY-ZSH
RUN apt-get install --no-install-recommends zsh -yqq \
 && git clone git://github.com/robbyrussell/oh-my-zsh.git $HOME/.oh-my-zsh
#  Install PROST drupal deployment script, see https://www.drupal.org/sandbox/axroth/1668300
RUN git clone --branch master http://git.drupal.org/sandbox/axroth/1668300.git /tmp/prost
RUN chmod +x /tmp/prost/install.sh
#  Install some useful cli tools
RUN apt-get install --no-install-recommends mc htop vim nano net-tools mlocate -yqq
#  Cleanup some things
RUN apt-get -yqq autoremove ; apt-get -yqq autoclean ; apt-get clean
#  Expose some ports to the host system (web server, MySQL, Xdebug)
EXPOSE 80/tcp 3306/tcp 9000/tcp
#
#  Step 2: Configuration
#
#  Localization
RUN dpkg-reconfigure locales \
 && locale-gen de_DE.UTF-8 \
 && /usr/sbin/update-locale LANG=de_DE.UTF-8
ENV LC_ALL="de_DE.UTF-8"
#  Set timezone
RUN echo "Europe/Berlin" > /etc/timezone \
 && dpkg-reconfigure -f noninteractive tzdata
#  Add apache web server configuration file
COPY config/httpd.conf /etc/apache2/httpd.conf
#  Configure needed apache modules and disable default site
RUN a2enmod rewrite headers deflate expires \
 && a2dismod cgi autoindex status \
 && a2dissite default
#  Add additional php configuration file
COPY config/php.ini /etc/php5/conf.d/php.ini
#  Add additional mysql configuration file
COPY config/mysql.cnf /etc/mysql/conf.d/mysql.cnf
RUN chmod 0664 /etc/mysql/conf.d/mysql.cnf
#  Add memcached configuration file
COPY config/memcached.conf /etc/memcached.conf
#  Add ssmtp configuration file
COPY config/ssmtp.conf /etc/ssmtp/ssmtp.conf
#  Add phpmyadmin configuration file
COPY config/config.inc.php /opt/phpmyadmin/config.inc.php
RUN chmod 0664 /opt/phpmyadmin/config.inc.php
#  Add git global configuration files
COPY config/.gitconfig $HOME/.gitconfig
COPY config/.gitignore $HOME/.gitignore
#  Add drush global configuration file
COPY config/drushrc.php $HOME/.drush/drushrc.php
#  Add apc status script
RUN mkdir /opt/apc \
 && gunzip -c /usr/share/doc/php-apc/apc.php.gz > /opt/apc/apc.php
#  Add zsh configuration
COPY config/.zshrc $HOME/.zshrc
#  Configure PROST drupal deployment script
RUN chown docker:docker $HOME/.zshrc
USER docker
ENV SHELL="/bin/zsh"
RUN export PATH="$HOME/.composer/vendor/bin:$PATH" \
 && cd /tmp/prost \
 && ./install.sh -noupdate $HOME/.prost
USER root
RUN rm -rf /tmp/prost
#  ADD ssh keys needed for connections to external servers
COPY .ssh $HOME/.ssh
RUN echo " IdentityFile ~/.ssh/id_rsa" >> /etc/ssh/ssh_config
#  Add startup script
ADD startup.sh $HOME/startup.sh
#  Supervisor configuration
COPY config/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#  Entry point for the container
RUN chown -R docker:docker $HOME \
 && chmod +x $HOME/startup.sh
USER docker
ENV SHELL="/bin/zsh"
WORKDIR /var/www
CMD ["/bin/bash", "-c", "$HOME/startup.sh"]
