#   Dockerfile for Flarum, based on Flarum's Vagrantfile
#   (https://github.com/flarum/flarum/blob/master/Vagrantfile)
#   which uses scripts from Vaprobash
#   (https://github.com/fideloper/Vaprobash)
#   Run with:
#   docker-compose up flarum
#   docker-compose run --service-ports flarum <custom command, e.g. /bin/bash>
FROM phusion/baseimage
MAINTAINER Sebastien Pujadas http://pujadas.net
ENV REFRESHED_AT="2015-05-12"
#  ##############################################################################
#                                       PHP
#  ##############################################################################
#  ## update repository for PHP 5.6
#   note: triggers non-fatal error due to non-ASCII characters in repo name
#         (gpg: key E5267A6C: public key "Launchpad PPA for Ond\xc5\x99ej Sur�" imported)
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 4F4EA0AAE5267A6C \
 && add-apt-repository -y ppa:ondrej/php5-5.6 \
 && apt-key update \
 && :
#  ## install base packages and PHP
RUN (apt-get update ;apt-get install --no-install-recommends ack-grep build-essential=12.9ubuntu3 curl=7.88.1-7ubuntu1 git-core php5-cli php5-fpm php5-mysql php5-pgsql php5-sqlite php5-curl php5-gd php5-gmp php5-mcrypt php5-memcached php5-imagick php5-intl php5-xdebug software-properties-common=0.99.35 unzip=6.0-27ubuntu1 -qqy )
#  ## configure PHP (FPM and CLI)
COPY php-fpm.ini.sed /etc/php5/fpm/php.ini.sed
RUN sed -i.bak -f /etc/php5/fpm/php.ini.sed /etc/php5/fpm/php.ini \
 && rm /etc/php5/fpm/php.ini.sed
COPY php-cli.ini.sed /etc/php5/cli/php.ini.sed
RUN sed -i.bak -f /etc/php5/cli/php.ini.sed /etc/php5/cli/php.ini \
 && rm /etc/php5/cli/php.ini.sed
COPY php-fpm.conf.sed /etc/php5/fpm/php-fpm.conf.sed
RUN sed -i.bak -f /etc/php5/fpm/php-fpm.conf.sed /etc/php5/fpm/php-fpm.conf \
 && rm /etc/php5/fpm/php-fpm.conf.sed
COPY www.conf.sed /etc/php5/fpm/pool.d/www.conf.sed
RUN sed -i.bak -f /etc/php5/fpm/pool.d/www.conf.sed /etc/php5/fpm/pool.d/www.conf \
 && rm /etc/php5/fpm/pool.d/www.conf.sed
COPY xdebug.ini /etc/php5/mods-available/xdebug.ini
#  ##############################################################################
#                                    nginx
#  ##############################################################################
#  ## install nginx
RUN add-apt-repository -y ppa:nginx/stable \
 && apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends nginx=1.22.0-1ubuntu3 -qqy )
#  ## configure nginx
COPY nginx.conf.sed /etc/nginx/nginx.conf.sed
RUN sed -i.bak -f /etc/nginx/nginx.conf.sed /etc/nginx/nginx.conf \
 && rm /etc/nginx/nginx.conf.sed
COPY nginx-localhost.conf /etc/nginx/sites-available/localhost
RUN rm -f /etc/nginx/sites-enabled/default \
 && ln -s /etc/nginx/sites-available/localhost /etc/nginx/sites-enabled/localhost
COPY php-fpm-nginx.ini.sed /etc/php5/fpm/php.ini.sed
RUN sed -i.bak -f /etc/php5/fpm/php.ini.sed /etc/php5/fpm/php.ini \
 && rm /etc/php5/fpm/php.ini.sed
#  ##############################################################################
#                                     MySQL
#  ##############################################################################
#  ## install MySQL
RUN add-apt-repository -y ppa:ondrej/mysql-5.6 \
 && :
COPY mysql-debconf-selections /tmp/mysql-debconf-selections
RUN debconf-set-selections /tmp/mysql-debconf-selections \
 && rm /tmp/mysql-debconf-selections
RUN (apt-get update ;apt-get install --no-install-recommends mysql-server-5.6 -qqy )
#  ## configure MySQL
#   As per http://txt.fliglio.com/2013/11/creating-a-mysql-docker-container/#comment-1600036544
COPY mysqld_bind_address_all.cnf /etc/mysql/conf.d/mysqld_bind_address_all.cnf
RUN chmod 644 /etc/mysql/conf.d/mysqld_bind_address_all.cnf
#   http://txt.fliglio.com/2013/11/creating-a-mysql-docker-container/#comment-1566459566
RUN service mysql start \
 && mysqladmin --silent --wait=30 ping \
 && mysql -uroot -proot -e "GRANT ALL ON *.* TO 'root'@'%' IDENTIFIED BY 'root' WITH GRANT OPTION;FLUSH PRIVILEGES;" \
 && sleep 1 \
 && service mysql stop
#  ##############################################################################
#                                   memcached
#  ##############################################################################
#  ## install memcached
RUN (apt-get update ;apt-get install --no-install-recommends memcached=1.6.18-1 -qqy )
#  ##############################################################################
#                                   beanstalkd
#  ##############################################################################
#  ## install beanstalkd
RUN (apt-get update ;apt-get install --no-install-recommends beanstalkd=1.12-2 -qqy )
#  ##############################################################################
#                                    NodeJS
#  ##############################################################################
#  ## install NVM
RUN curl https://raw.githubusercontent.com/creationix/nvm/v0.25.1/install.sh | bash
#  ## install NodeJS and configure NPM
#   but first create link from sh to bash (instead of dash) to be able to source files
RUN ln -sf /bin/bash /bin/sh
RUN . ~/.nvm/nvm.sh \
 && nvm install 0.12.2 \
 && nvm alias default 0.12.2 \
 && nvm use default \
 && npm config set prefix /opt/npm \
 && echo -e "\nexport PATH=$PATH:/opt/npm/bin\nexport NODE_PATH=$NODE_PATH:/opt/npm/lib/node_modules" >> ~/.bashrc
ENV PATH="$PATH:/opt/npm/bin"
ENV NODE_PATH="$NODE_PATH:/opt/npm/lib/node_modules"
#  ##############################################################################
#                                    Composer
#  ##############################################################################
#  ## install Composer
RUN curl -sS https://getcomposer.org/installer | php \
 && sudo mv composer.phar /usr/local/bin/composer
#  ## provision Composer
RUN composer global require franzl/studio:dev-master
#  ##############################################################################
#                                     Flarum
#  ##############################################################################
#   prerequisites
RUN (apt-get update ;apt-get install --no-install-recommends exuberant-ctags=1:5.9~svn20110310-18 phantomjs zsh=5.9-4 -qqy )
RUN . ~/.nvm/nvm.sh \
 && npm install bower@1.8.14 gulp@4.0.2 -g
ENV FLARUM_ROOT="/opt/flarum"
#   download Flarum
RUN mkdir -p ${FLARUM_ROOT} \
 && cd ${FLARUM_ROOT} \
 && git clone --recursive https://github.com/flarum/flarum . \
 && git checkout d5229bd3d0c060bb95a93b974538cdb204802739 \
 && cd ${FLARUM_ROOT}/system/core \
 && git checkout aae3e989c4940671e73095478d4ab9f2939e28e8
#  # Note: pinning to specific commits to make the image consistenly rebuildable
#   install skeleton app dependencies
RUN cd ${FLARUM_ROOT}/system \
 && cp ${FLARUM_ROOT}/system/.env.example ${FLARUM_ROOT}/system/.env \
 && cd ${FLARUM_ROOT}/system/core \
 && composer install --prefer-dist \
 && cd ${FLARUM_ROOT}/system \
 && composer install --prefer-dist \
 && composer dump-autoload
#   create database, run installation migrations and seeds, seed database with fake data
RUN service mysql start \
 && mysqladmin --silent --wait=30 ping \
 && mysql -u root -proot -e 'create database flarum' \
 && cd ${FLARUM_ROOT}/system \
 && php artisan vendor:publish \
 && php artisan flarum:install \
 && mysql -u root -proot flarum -e "insert into config values ('extensions_enabled','[]')" \
 && php artisan flarum:seed \
 && sleep 1 \
 && service mysql stop
#  # Note: the manual insertion of the extensions_enabled key in the config table is a workaround
#  #   pending the resolution of https://github.com/flarum/core/issues/76
#   install JS dependencies
RUN . ~/.nvm/nvm.sh \
 && cd ${FLARUM_ROOT}/system/core/js \
 && bower --allow-root install \
 && cd ${FLARUM_ROOT}/system/core/js/forum \
 && npm install \
 && gulp \
 && cd ${FLARUM_ROOT}/system/core/js/admin \
 && npm install \
 && gulp
RUN chown -R www-data ${FLARUM_ROOT}
EXPOSE 80/tcp
WORKDIR ${FLARUM_ROOT}
COPY ./start.sh /usr/local/bin/start.sh
RUN chmod +x /usr/local/bin/start.sh
CMD ["/usr/local/bin/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
