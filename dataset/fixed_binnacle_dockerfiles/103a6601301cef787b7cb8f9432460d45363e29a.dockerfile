#   Pull base image.
FROM php:5.6-apache
COPY config/php.ini /usr/local/etc/php/
#   ADD BASHRC CONFIG
COPY config/bashrc /root/
RUN mv /root/bashrc /root/.bashrc
RUN apt-get clean \
 && apt-get update \
 && apt-get install --no-install-recommends ruby-dev rubygems imagemagick graphviz sudo git vim memcached libmemcached-tools php5-memcached php5-dev libpng12-dev libjpeg62-turbo-dev libmcrypt-dev libxml2-dev libxslt1-dev mysql-client php5-mysqlnd zip wget linux-libc-dev libyaml-dev apt-transport-https zlib1g-dev libicu-dev libpq-dev -y --fix-missing
#   postgresql-client-9.5
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add - \
 && echo "deb http://apt.postgresql.org/pub/repos/apt/ jessie-pgdg main" >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends postgresql-client-9.5 -y
#   Install PECL packages
COPY docker-php-pecl-install /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-php-pecl-install
RUN docker-php-pecl-install uploadprogress-1.0.3.1 memcache-3.0.8 yaml-1.2.0 mongo-1.6.12
COPY docker-php-ext-install /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-php-ext-install
RUN docker-php-ext-configure gd --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install gd mbstring mcrypt zip soap mysql pdo_mysql mysqli xsl opcache calendar intl exif pgsql pdo_pgsql ftp
COPY core/memcached.conf /etc/memcached.conf
#   install xdebug and codesniffer
RUN apt-get install --no-install-recommends php5-xdebug --fix-missing -y ; pecl install xdebug ; echo ';zend_extension = /usr/local/lib/php/extensions/no-debug-non-zts-20131226/xdebug.so' >> /usr/local/etc/php/php.ini; touch /usr/local/etc/php/conf.d/xdebug.ini \
 && echo xdebug.remote_enable=1 >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo xdebug.remote_autostart=0 >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo xdebug.remote_connect_back=1 >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo xdebug.remote_port=9000 >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo xdebug.remote_log=/tmp/php5-xdebug.log >> /usr/local/etc/php/conf.d/xdebug.ini; gem install sass --version 3.4.22 ; gem install compass --version 1.0.3
#   Installation node.js
RUN curl -sL https://deb.nodesource.com/setup_8.x | sudo bash -
RUN apt-get install --no-install-recommends nodejs build-essential -y ; npm update -g npm \
 && npm install less@4.1.3 -g \
 && npm install less-plugin-clean-css@1.5.1 -g ; npm install grunt-cli@1.4.3 -g ; npm install gulp@4.0.2 -g ; cd /usr/src \
 && curl -sS http://getcomposer.org/installer | php ; cd /usr/src \
 && mv composer.phar /usr/bin/composer ; git clone https://github.com/drush-ops/drush.git /usr/local/src/drush ; cd /usr/local/src/drush \
 && git checkout 8.1.2 ; ln -s /usr/local/src/drush/drush /usr/bin/drush ; cd /usr/local/src/drush \
 && composer update \
 && composer install ; mkdir /var/www/.drush ; cd /var/www/.drush \
 && wget https://ftp.drupal.org/files/projects/registry_rebuild-7.x-2.3.zip \
 && unzip registry_rebuild-7.x-2.3.zip ; cd /var/www/.drush \
 && wget https://ftp.drupal.org/files/projects/site_audit-7.x-1.15.zip \
 && unzip site_audit-7.x-1.15.zip ; cd /var/www/.drush \
 && rm *.zip ; chown -R www-data:www-data /var/www ; sudo -u www-data drush cc drush ; composer global require drupal/coder ; ln -s ~/.composer/vendor/bin/phpcs /usr/local/bin ; ln -s ~/.composer/vendor/bin/phpcbf /usr/local/bin ; phpcs --config-set installed_paths ~/.composer/vendor/drupal/coder/coder_sniffer
RUN rm -rf /var/www/html \
 && mkdir -p /var/lock/apache2 /var/run/apache2 /var/log/apache2 /var/www/html \
 && chown -R www-data:www-data /var/lock/apache2 /var/run/apache2 /var/log/apache2 /var/www/html
#   installation of ssmtp
RUN DEBIAN_FRONTEND=noninteractive apt-get install --fix-missing -y ssmtp \
 && rm -r /var/lib/apt/lists/*
COPY core/ssmtp.conf /etc/ssmtp/ssmtp.conf
COPY core/php-smtp.ini /usr/local/etc/php/conf.d/php-smtp.ini
COPY config/apache2.conf /etc/apache2
COPY core/envvars /etc/apache2
COPY core/other-vhosts-access-log.conf /etc/apache2/conf-enabled/
RUN rm /etc/apache2/sites-enabled/000-default.conf
RUN chown -R www-data:www-data /var/www
#   Installation of Opcode cache
RUN (echo "opcache.memory_consumption=128" ;echo "opcache.interned_strings_buffer=8" ;echo "opcache.max_accelerated_files=4000" ;echo "opcache.revalidate_freq=5" ;echo "opcache.fast_shutdown=1" ;echo "opcache.enable_cli=1" ) > /usr/local/etc/php/conf.d/opcache-recommended.ini
RUN a2enmod rewrite expires \
 && service apache2 restart
#   install phing
RUN pear channel-discover pear.phing.info \
 && pear install [--alldeps] phing/phing
#   install phpcpd
RUN wget https://phar.phpunit.de/phpcpd.phar \
 && chmod +x phpcpd.phar \
 && mv phpcpd.phar /usr/local/bin/phpcpd
#   install phpmetrics
RUN wget https://github.com/phpmetrics/PhpMetrics/raw/master/build/phpmetrics.phar \
 && chmod +x phpmetrics.phar \
 && mv phpmetrics.phar /usr/local/bin/phpmetrics
#   Install Drupal Console for Drupal 8
RUN curl https://drupalconsole.com/installer -L -o drupal.phar \
 && mv drupal.phar /usr/local/bin/drupal \
 && chmod +x /usr/local/bin/drupal
#   Our apache volume
VOLUME /var/www/html
#   create directory for ssh keys
RUN mkdir /var/www/.ssh/
RUN chown -R www-data:www-data /var/www/
RUN chmod -R 600 /var/www/.ssh/
#   Set timezone to Europe/Paris
RUN echo "Europe/Paris" > /etc/timezone \
 && dpkg-reconfigure -f noninteractive tzdata
#   Expose 80 for apache, 9000 for xdebug
EXPOSE 80/tcp 9000/tcp
#   Set a custom entrypoint.
COPY core/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
