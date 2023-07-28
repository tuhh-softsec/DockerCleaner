FROM ubuntu:14.04
MAINTAINER Alexander Schneider "alexander.schneider@jankowfsky.com"
#   Upgrade system
ENV DEBIAN_FRONTEND="noninteractive"
ENV HOME="/root"
RUN :
#   Setup system and install tools
RUN echo "initscripts hold" | dpkg --set-selections
RUN (apt-get update ;apt-get install --no-install-recommends libreadline-gplv2-dev=5.2+dfsg-2 libfreetype6=2.5.2-1ubuntu2.8 apt-utils=1.0.1ubuntu2.24 dialog=1.2-20130928-1 postfix=2.11.0-1ubuntu1.2 -qqy )
RUN echo "Europe/Berlin" > /etc/timezone \
 && dpkg-reconfigure -f noninteractive tzdata
RUN echo 'alias ll="ls -lah --color=auto"' >> /etc/bash.bashrc
RUN (apt-get update ;apt-get install --no-install-recommends passwd=1:4.1.5.1-1ubuntu9.5 supervisor=3.0b2-1ubuntu0.1 git-core=1:1.9.1-1ubuntu0.10 sudo=1.8.9p5-1ubuntu1.4 unzip=6.0-9ubuntu1.5 wget=1.15-1ubuntu1.14.04.5 curl=7.35.0-1ubuntu2.20 libfile-slurp-perl=9999.19-4 libmysql-diff-perl=0.43-2 vim=2:7.4.052-1ubuntu3.1 net-tools=1.60-25ubuntu2.1 software-properties-common=0.92.37.8 python-software-properties=0.92.37.8 -qqy )
#   Set locale
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.13+git20120306-12.1 -qqy )
RUN locale-gen --purge de_DE de_DE.UTF-8
RUN locale-gen --purge en_US en_US.UTF-8
RUN dpkg-reconfigure locales
ENV LC_ALL="en_US.UTF-8"
#   Setup ssh
RUN (apt-get update ;apt-get install --no-install-recommends openssh-server=1:6.6p1-2ubuntu2.13 -qqy )
RUN mkdir -p /var/run/sshd
RUN sed -ri 's/UsePAM yes/#UsePAM yes/g' /etc/ssh/sshd_config
RUN sed -ri 's/#UsePAM no/UsePAM no/g' /etc/ssh/sshd_config
RUN sed -ri 's/PermitRootLogin without-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
RUN echo 'root:root' | chpasswd
#   Generate a host key before packing.
RUN service ssh start ; service ssh stop
#   Create SSL cert
RUN mkdir /root/ssl ; openssl genrsa -out /root/ssl/local.key 1024 ; openssl req -new -key /root/ssl/local.key -out /root/ssl/local.csr -subj "/C=DE/ST=BW/L=FREIBURG/O=Jankowfsky AG/OU=Development/CN=localhost" ; openssl x509 -req -days 365 -in /root/ssl/local.csr -signkey /root/ssl/local.key -out /root/ssl/local.crt
#   Apache
RUN (apt-get update ;apt-get install --no-install-recommends apache2-mpm-prefork=2.4.7-1ubuntu4.22 apache2-utils=2.4.7-1ubuntu4.22 -qqy )
RUN a2enmod rewrite
RUN a2enmod proxy_fcgi
RUN a2enmod ssl
RUN mkdir /etc/apache2/conf.d/
RUN echo "ServerName localhost" | tee /etc/apache2/conf.d/fqdn
RUN echo "ServerName localhost" >> /etc/apache2/apache2.conf
COPY conf/apache/000-default /etc/apache2/sites-enabled/000-default.conf
#   Mysql
RUN (apt-get update ;apt-get install --no-install-recommends mysql-server=5.5.62-0ubuntu0.14.04.1 mysql-common=5.5.62-0ubuntu0.14.04.1 mysql-client=5.5.62-0ubuntu0.14.04.1 -qqy )
RUN ln -s /run/mysqld/mysqld.sock /tmp/mysql.sock
#   Add latest php version
RUN add-apt-repository ppa:ondrej/apache2
RUN add-apt-repository ppa:ondrej/php \
 && :
#   PHP
RUN (apt-get update ;apt-get install --no-install-recommends php5-fpm=5.5.9+dfsg-1ubuntu4.29 php5=5.5.9+dfsg-1ubuntu4.29 php5-cli=5.5.9+dfsg-1ubuntu4.29 php5-mysql=5.5.9+dfsg-1ubuntu4.29 php5-curl=5.5.9+dfsg-1ubuntu4.29 php5-dev=5.5.9+dfsg-1ubuntu4.29 php5-gd=5.5.9+dfsg-1ubuntu4.29 php-pear=5.5.9+dfsg-1ubuntu4.29 php-apc=4.0.2-2build1 php5-xdebug=2.2.3-2build1 libapache2-mod-php5=5.5.9+dfsg-1ubuntu4.29 -qqy )
#   PhpMyAdmin
RUN mysqld &; service apache2 start ; sleep 5 ; printf ynnn1n | (apt-get update ;apt-get install --no-install-recommends phpmyadmin=4:4.0.10-1ubuntu0.1 -qqy ) ; sleep 15 ; mysqladmin -u root shutdown
RUN sed -i "0,/\/\/ $cfg\['Servers'\]\[$i\]\['AllowNoPassword'\] = TRUE;/{s#// $cfg\['Servers'\]\[$i\]\['AllowNoPassword'\] = TRUE;#$cfg\['Servers'\]\[$i\]\['AllowNoPassword'\] = TRUE;#g}" /etc/phpmyadmin/config.inc.php
RUN sed -i "/^[ ]*$cfg\['Servers'\]\[$i\]\['host'\]/a$cfg\['Servers'\]\[$i\]\['hide_db'\] = '(information_schema|performance_schema|phpmyadmin|mysql|test)';" /etc/phpmyadmin/config.inc.php
RUN ln -s /etc/phpmyadmin/apache.conf /etc/apache2/conf-enabled
#   Install ant builder
RUN (apt-get update ;apt-get install --no-install-recommends ant=1.9.3-2ubuntu0.1 -qqy )
#   Install composer
RUN curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer
#   Install phpunit
RUN composer global require "phpunit/phpunit=4.1.*" ; ln -s /root/.composer/vendor/bin/phpunit /usr/bin/phpunit
#   Install ruby
RUN (apt-get update ;apt-get install --no-install-recommends ruby=1:1.9.3.4 -y )
#   Install sass
RUN gem install sass --version 3.7.4
#   Nodejs + NPM
RUN add-apt-repository ppa:chris-lea/node.js \
 && :
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 -y )
#   Install bower
RUN npm install bower@1.8.14 -g
#   Install grunt
RUN npm install grunt-cli@1.4.3 -g
#   Install glub
RUN npm install gulp@4.0.2 -g
#   Install phpbrew
RUN :
RUN apt-get build-dep -y --fix-missing php5
RUN (apt-get update ;apt-get install --no-install-recommends php5=5.5.9+dfsg-1ubuntu4.29 php5-dev=5.5.9+dfsg-1ubuntu4.29 php-pear=5.5.9+dfsg-1ubuntu4.29 autoconf=2.69-6 automake=1:1.14.1-2ubuntu1 curl=7.35.0-1ubuntu2.20 build-essential=11.6ubuntu6 libxslt1-dev=1.1.28-2ubuntu0.2 re2c=0.13.5-1build2 libxml2=2.9.1+dfsg1-3ubuntu4.13 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 php5-cli=5.5.9+dfsg-1ubuntu4.29 bison=2:3.0.2.dfsg-2 libbz2-dev=1.0.6-5 libreadline-dev=6.3-4ubuntu2 libfreetype6=2.5.2-1ubuntu2.8 libfreetype6-dev=2.5.2-1ubuntu2.8 libpng12-0=1.2.50-1ubuntu2.14.04.3 libpng12-dev=1.2.50-1ubuntu2.14.04.3 libjpeg-dev=8c-2ubuntu8 libjpeg8-dev=8c-2ubuntu8 libjpeg8=8c-2ubuntu8 libgd-dev=2.1.0-3ubuntu0.11 libgd3=2.1.0-3ubuntu0.11 libxpm4=1:3.5.10-1ubuntu0.1 libssl-dev=1.0.1f-1ubuntu2.27 openssl=1.0.1f-1ubuntu2.27 gettext=0.18.3.1-1ubuntu3.1 libgettextpo-dev=0.18.3.1-1ubuntu3.1 libgettextpo0=0.18.3.1-1ubuntu3.1 libicu-dev=52.1-3ubuntu0.8 libmhash2=0.9.9.9-4 libmhash-dev=0.9.9.9-4 libmcrypt4=2.5.8-3.1ubuntu1 libmcrypt-dev=2.5.8-3.1ubuntu1 libpcre3-dev=1:8.31-2ubuntu2.3 libpcre++-dev=0.9.5-6 -y )
RUN wget http://launchpadlibrarian.net/121520545/libbison-dev_2.6.2.dfsg-1_amd64.deb \
 && dpkg -i libbison-dev_2.6.2.dfsg-1_amd64.deb
RUN wget http://launchpadlibrarian.net/121520544/bison_2.6.2.dfsg-1_amd64.deb \
 && dpkg -i bison_2.6.2.dfsg-1_amd64.deb
COPY conf/php/phpbrew /usr/bin/phpbrew
RUN chmod +x /usr/bin/phpbrew
COPY conf/php/pbconfig.yaml /tmp/config.yaml
RUN phpbrew init --config=/tmp/config.yaml
RUN echo "source /root/.phpbrew/bashrc" >> /root/.bashrc
RUN ln -s /.phpbrew /root/.phpbrew
COPY conf/php/install_php /usr/bin/install_php
RUN chmod +x /usr/bin/install_php
#   Install tideways daemon
COPY conf/php/tideways-daemon /root/tideways-daemon
RUN chmod +x /root/tideways-daemon/install.sh
RUN /root/tideways-daemon/install.sh
#   Install different php version
COPY conf/php/modules /root/.phpbrew/modules
#   php 5.6
RUN install_php 5.6.4
#   php 5.5
RUN install_php 5.5.20
#   php 5.4
RUN install_php 5.4.40
#   php 5.3
RUN install_php 5.3.29
#   Add supervisor config
COPY conf/supervisor/startup.conf /etc/supervisor/conf.d/startup.conf
ENV PHP_VERSION="5.6.4"
ENV PHP_XDEBUG="0"
ENV SQL_DIR="/var/www/_sql"
COPY conf/scripts/startup.sh /usr/bin/startup_container
RUN chmod +x /usr/bin/startup_container
#   Cleanup
RUN apt-get clean -y ; apt-get autoclean -y ; apt-get autoremove -y ; rm -rf /var/lib/{apt,dpkg,cache,log}/
VOLUME /var/www
EXPOSE 22/tcp 80/tcp 443/tcp 3306/tcp 9000/tcp
CMD ["/bin/bash", "/usr/bin/startup_container"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
