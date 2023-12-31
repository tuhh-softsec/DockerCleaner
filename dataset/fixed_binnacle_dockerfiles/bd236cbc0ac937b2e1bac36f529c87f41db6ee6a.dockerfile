#
#   Dockerfile to build a MISP (https://github.com/MISP/MISP) container
#
#   Original docker file by eg5846 (https://github.com/eg5846)
#
#   2016/03/03 - First release
#   2017/06/02 - Updated
#   2018/04/04 - Added objects templates
#   
#   We are based on Ubuntu:latest
FROM ubuntu:xenial
MAINTAINER Xavier Mertens <xavier@rootshell.be>
#   Install core components
ENV DEBIAN_FRONTEND="noninteractive"
RUN : \
 && apt-get dist-upgrade -y \
 && apt-get autoremove -y \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends postfix=3.1.0-3ubuntu0.4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends mysql-client=5.7.33-0ubuntu0.16.04.1 curl=7.47.0-1ubuntu2.19 gcc=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 gnupg-agent=2.1.11-6ubuntu2.1 make=4.1-6 python=2.7.12-1~16.04 openssl=1.0.2g-1ubuntu4.20 redis-server=2:3.0.6-1ubuntu0.4 sudo=1.8.16-0ubuntu1.10 vim=2:7.4.1689-3ubuntu1.5 zip=3.0-11 locales=2.23-0ubuntu11.3 -y )
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
RUN add-apt-repository -y ppa:ondrej/php \
 && :
#   Apache
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.18-2ubuntu3.17 apache2-doc=2.4.18-2ubuntu3.17 apache2-utils=2.4.18-2ubuntu3.17 -y )
RUN a2dismod status
RUN a2dissite 000-default
#   PHP 7.2
RUN (apt-get update ;apt-get install --no-install-recommends libapache2-mod-php=1:7.0+35ubuntu6.1 php7.2 php7.2-cli php-crypt-gpg=1.4.0-1ubuntu2 php7.2-dev php7.2-json php7.2-mysql php7.2-opcache php7.2-readline php7.2-redis php7.2-xml -y )
RUN (apt-get update ;apt-get install --no-install-recommends php-pear=1:1.10.1+submodules+notgz-6ubuntu0.3 pkg-config=0.29.1-0ubuntu1 libbson-1.0 libmongoc-1.0-0=1.3.1-1 php-xml=1:7.0+35ubuntu6.1 php-dev=1:7.0+35ubuntu6.1 -y )
#   Fix php.ini with recommended settings
RUN sed -i "s/max_execution_time = 30/max_execution_time = 300/" /etc/php/7.2/apache2/php.ini
RUN sed -i "s/memory_limit = 128M/memory_limit = 512M/" /etc/php/7.2/apache2/php.ini
RUN sed -i "s/upload_max_filesize = 2M/upload_max_filesize = 50M/" /etc/php/7.2/apache2/php.ini
RUN sed -i "s/post_max_size = 8M/post_max_size = 50M/" /etc/php/7.2/apache2/php.ini
RUN (apt-get update ;apt-get install --no-install-recommends python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 python-setuptools=20.7.0-1 libfuzzy-dev=2.13-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends cron=3.0pl1-128ubuntu2 logrotate=3.8.7-2ubuntu2.16.04.2 supervisor=3.2.0-2ubuntu0.2 syslog-ng-core=3.5.6-2.1 -y )
RUN apt-get clean
WORKDIR /var/www
RUN chown www-data:www-data /var/www
USER www-data
RUN git clone https://github.com/MISP/MISP.git
WORKDIR /var/www/MISP
RUN git checkout tags/$( git describe --tags `git rev-list --tags --max-count=1 ` ;)
RUN git config core.filemode false
RUN git submodule update --init --recursive
#   Make git ignore filesystem permission differences for submodules
RUN git submodule foreach --recursive git config core.filemode false
WORKDIR /var/www/MISP/app/files/scripts
RUN git clone https://github.com/CybOXProject/python-cybox.git
RUN git clone https://github.com/STIXProject/python-stix.git
WORKDIR /var/www/MISP/app/files/scripts/python-cybox
RUN git checkout v2.1.0.12
USER root
RUN python setup.py install
USER www-data
WORKDIR /var/www/MISP/app/files/scripts/python-stix
RUN git checkout v1.1.1.4
USER root
RUN python setup.py install
USER www-data
WORKDIR /var/www/MISP
RUN git submodule init
RUN git submodule update
WORKDIR /var/www/MISP/app
#   FIX COMPOSER
RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
RUN php -r "if (hash_file('sha384', 'composer-setup.php') === '48e3236262b34d30969dca3c37281b3b4bbe3221bda826ac6a9a62d6444cdb0dcd0615698a5cbe587c3f0fe57a54d8f5') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;"
RUN php composer-setup.php
RUN php -r "unlink('composer-setup.php');"
#   END FIX
RUN php composer.phar config vendor-dir Vendor
RUN php composer.phar install --ignore-platform-reqs
USER root
RUN phpenmod redis
USER www-data
RUN cp -fa /var/www/MISP/INSTALL/setup/config.php /var/www/MISP/app/Plugin/CakeResque/Config/config.php
#   Fix permissions
USER root
RUN chown -R www-data:www-data /var/www/MISP
RUN chmod -R 750 /var/www/MISP
RUN chmod -R g+ws /var/www/MISP/app/tmp
RUN chmod -R g+ws /var/www/MISP/app/files
RUN chmod -R g+ws /var/www/MISP/app/files/scripts/tmp
RUN cp /var/www/MISP/INSTALL/misp.logrotate /etc/logrotate.d/misp
#   Preconfigure setting for packages
RUN echo "postfix postfix/main_mailer_type string Local only" | debconf-set-selections
RUN echo "postfix postfix/mailname string localhost.localdomain" | debconf-set-selections
#   Redis Setup
RUN sed -i 's/^\(daemonize\s*\)yes\s*$/\1no/g' /etc/redis/redis.conf
#   Apache Setup
RUN cp /var/www/MISP/INSTALL/apache.misp.ubuntu /etc/apache2/sites-available/misp.conf
RUN a2dissite 000-default
RUN a2ensite misp
RUN a2enmod rewrite
RUN a2enmod headers
#   MISP base configuration
RUN sudo -u www-data cp -a /var/www/MISP/app/Config/bootstrap.default.php /var/www/MISP/app/Config/bootstrap.php
RUN sudo -u www-data cp -a /var/www/MISP/app/Config/database.default.php /var/www/MISP/app/Config/database.php
RUN sudo -u www-data cp -a /var/www/MISP/app/Config/core.default.php /var/www/MISP/app/Config/core.php
RUN sudo -u www-data cp -a /var/www/MISP/app/Config/config.default.php /var/www/MISP/app/Config/config.php
RUN chown -R www-data:www-data /var/www/MISP/app/Config
RUN chmod -R 750 /var/www/MISP/app/Config
#   Replace the default salt
RUN sed -i -E "s/'salt'\s=>\s'(\S+)'/'salt' => '`openssl rand -base64 32 | tr "/" "-" `'/" /var/www/MISP/app/Config/config.php
#   Enable workers at boot time
RUN chmod a+x /var/www/MISP/app/Console/worker/start.sh
RUN echo "sudo -u www-data bash /var/www/MISP/app/Console/worker/start.sh" >> /etc/rc.local
#   Install templates & stuff
WORKDIR /var/www/MISP/app/files
RUN rm -rf misp-objects \
 && git clone https://github.com/MISP/misp-objects.git
RUN rm -rf misp-galaxy \
 && git clone https://github.com/MISP/misp-galaxy.git
RUN rm -rf warninglists \
 && git clone https://github.com/MISP/misp-warninglists.git ./warninglists
RUN rm -rf taxonomies \
 && git clone https://github.com/MISP/misp-taxonomies.git ./taxonomies
RUN chown -R www-data:www-data misp-objects misp-galaxy warninglists taxonomies
#   Install MISP build requirements
RUN sudo apt-get -y install libpoppler58 libpoppler-dev libpoppler-cpp-dev
#   Install MISP Modules
WORKDIR /opt
RUN (apt-get update ;apt-get install --no-install-recommends python3=3.5.1-3 python3-pip=8.1.1-2ubuntu0.6 libjpeg-dev=8c-2ubuntu8 -y )
#   PIP3 fix
RUN pip install pip==23.1 --upgrade
#   END FIX
RUN git clone https://github.com/MISP/misp-modules.git
WORKDIR /opt/misp-modules
RUN pip3 install --upgrade pip
RUN cat REQUIREMENTS | sed 's/aiohttp==3.4.4/aiohttp/g' > REQUIREMENTS
RUN pip3 install --upgrade --ignore-installed urllib3
RUN pip3 install --upgrade --ignore-installed requests
RUN sed -i 's/aiohttp.*/aiohttp/g' REQUIREMENTS
RUN sed -i 's/functools.*//g' REQUIREMENTS
RUN sed -i 's/async-timeout.*/async-timeout/g' REQUIREMENTS
RUN sed -i 's/url-normalize.*/url-normalize/g' REQUIREMENTS
RUN sed -i 's/^\(yarl\)\=.*/\1/g' REQUIREMENTS
RUN sed -i 's/^\(sigmatools\)\=.*/\1/' REQUIREMENTS
RUN pip3 install -I -r REQUIREMENTS
RUN pip3 install -I .
RUN echo "sudo -u www-data misp-modules -s -l 127.0.0.1 &" >> /etc/rc.local
#   Supervisord Setup
RUN echo '[supervisord]' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'nodaemon = true' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo '' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo '[program:postfix]' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'process_name = master' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'directory = /etc/postfix' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'command = /usr/sbin/postfix -c /etc/postfix start' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'startsecs = 0' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'autorestart = false' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo '' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo '[program:redis-server]' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'command=redis-server /etc/redis/redis.conf' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo '' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo '[program:apache2]' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'command=/bin/bash -c "source /etc/apache2/envvars \
 && exec /usr/sbin/apache2 -D FOREGROUND"' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo '' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo '[program:resque]' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'command=/bin/bash /var/www/MISP/app/Console/worker/start.sh' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'user = www-data' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'startsecs = 0' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'autorestart = false' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo '' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo '[program:misp-modules]' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'command=/bin/bash -c "misp-modules -s -l 127.0.0.1"' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'user = www-data' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'startsecs = 0' >> /etc/supervisor/conf.d/supervisord.conf
RUN echo 'autorestart = false' >> /etc/supervisor/conf.d/supervisord.conf
#   Modify syslog configuration
RUN sed -i -E 's/^(\s*)system\(\);/\1unix-stream("\/dev\/log");/' /etc/syslog-ng/syslog-ng.conf
#   Add run script
COPY run.sh /run.sh
RUN chmod 0755 /run.sh
#   Trigger to perform first boot operations
RUN touch /.firstboot.tmp
#   Make a backup of /var/www/MISP to restore it to the local moint point at first boot
WORKDIR /var/www/MISP
RUN tar czpf /root/MISP.tgz .
VOLUME /var/www/MISP
EXPOSE 80/tcp
ENTRYPOINT ["/run.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
