#   Pull base image.
FROM php:7.0.12-apache
COPY config/php.ini /usr/local/etc/php/
RUN apt-get clean \
 && apt-get update \
 && apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 --fix-missing -y
RUN echo "deb http://packages.dotdeb.org jessie all" >> /etc/apt/sources.list
RUN echo "deb-src http://packages.dotdeb.org jessie all" >> /etc/apt/sources.list
RUN cd /tmp \
 && wget https://www.dotdeb.org/dotdeb.gpg \
 && apt-key add dotdeb.gpg
RUN apt-get clean \
 && apt-get update \
 && apt-get install --no-install-recommends ruby-dev=1:3.1 rubygems imagemagick=8:6.9.11.60+dfsg-1.6 graphviz=2.42.2-7build3 sudo=1.9.13p1-1ubuntu2 git=1:2.39.2-1ubuntu1 vim=2:9.0.1000-4ubuntu2 php7.0-dev memcached=1.6.18-1 libmemcached-tools=1.1.4-1 libmemcached-dev=1.1.4-1 libpng12-dev libjpeg62-turbo-dev libmcrypt-dev=2.5.8-7 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt1-dev=1.1.35-1 mysql-client=8.0.32-0ubuntu4 php5-mysqlnd zip=3.0-13 wget=1.21.3-1ubuntu1 linux-libc-dev=6.2.0-18.18 libyaml-dev=0.2.5-1 apt-transport-https=2.6.0 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libicu-dev=72.1-3ubuntu1 libpq-dev=15.2-1 bash-completion=1:2.11-6ubuntu1 --fix-missing -y
#   postgresql-client-9.5
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add - \
 && echo "deb http://apt.postgresql.org/pub/repos/apt/ jessie-pgdg main" >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends postgresql-client-9.5 -y
#   Install memcached for PHP 7
RUN cd /tmp \
 && git clone https://github.com/php-memcached-dev/php-memcached.git
RUN cd /tmp/php-memcached \
 && sudo git checkout php7 \
 && phpize \
 && ./configure --disable-memcached-sasl \
 && make \
 && make install
RUN touch /usr/local/etc/php/conf.d/memcached.ini \
 && echo "extension=/usr/local/lib/php/extensions/no-debug-non-zts-20151012/memcached.so" >> /usr/local/etc/php/conf.d/memcached.ini
COPY docker-php-ext-install /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-php-ext-install
RUN docker-php-ext-configure gd --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install gd mbstring mcrypt zip soap pdo_mysql mysqli xsl opcache calendar intl exif pgsql pdo_pgsql ftp
RUN pecl install yaml-2.0.0 \
 && echo "extension=yaml.so" > /usr/local/etc/php/conf.d/ext-yaml.ini
COPY core/memcached.conf /etc/memcached.conf
#   SASS and Compass installation
RUN gem install compass --version 1.0.3
#   Installation node.js
RUN curl -sL https://deb.nodesource.com/setup_8.x | sudo bash -
RUN apt-get install --no-install-recommends nodejs=18.13.0+dfsg1-1ubuntu2 build-essential=12.9ubuntu3 -y
#   Installation of LESS
RUN npm install less@4.1.3 -g \
 && npm install less-plugin-clean-css@1.5.1 -g
#   Installation of Grunt
RUN npm install grunt-cli@1.4.3 -g
#   Installation of Gulp
RUN npm install gulp@4.0.2 -g
#   Installation of Bower
RUN npm install bower@1.8.14 -g
#   Installation of Composer
RUN cd /usr/src \
 && curl -sS http://getcomposer.org/installer | php
RUN cd /usr/src \
 && mv composer.phar /usr/bin/composer
#   Installation of drush
RUN git clone https://github.com/drush-ops/drush.git /usr/local/src/drush
RUN cd /usr/local/src/drush \
 && git checkout 8.1.2
RUN ln -s /usr/local/src/drush/drush /usr/bin/drush
RUN cd /usr/local/src/drush \
 && composer update \
 && composer install
#   Install xdebug. We need at least 2.4 version to have PHP 7 support.
RUN cd /tmp/ \
 && wget http://xdebug.org/files/xdebug-2.4.0.tgz \
 && tar -xvzf xdebug-2.4.0.tgz \
 && cd xdebug-2.4.0/ \
 && phpize \
 && ./configure --enable-xdebug --with-php-config=/usr/local/bin/php-config \
 && make \
 && make install
RUN cd /tmp/xdebug-2.4.0 \
 && cp modules/xdebug.so /usr/local/lib/php/extensions/no-debug-non-zts-20151012/
RUN echo 'zend_extension = /usr/local/lib/php/extensions/no-debug-non-zts-20151012/xdebug.so' >> /usr/local/etc/php/php.ini
RUN touch /usr/local/etc/php/conf.d/xdebug.ini \
 && echo xdebug.remote_enable=1 >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo xdebug.remote_autostart=0 >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo xdebug.remote_connect_back=1 >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo xdebug.remote_port=9000 >> /usr/local/etc/php/conf.d/xdebug.ini \
 && echo xdebug.remote_log=/tmp/php5-xdebug.log >> /usr/local/etc/php/conf.d/xdebug.ini
RUN rm -rf /var/www/html \
 && mkdir -p /var/lock/apache2 /var/run/apache2 /var/log/apache2 /var/www/html \
 && chown -R www-data:www-data /var/lock/apache2 /var/run/apache2 /var/log/apache2 /var/www/html
#   Installation of PHP_CodeSniffer with Drupal coding standards.
#   See https://www.drupal.org/node/1419988#coder-composer
RUN composer global require drupal/coder
RUN ln -s ~/.composer/vendor/bin/phpcs /usr/local/bin
RUN ln -s ~/.composer/vendor/bin/phpcbf /usr/local/bin
RUN phpcs --config-set installed_paths ~/.composer/vendor/drupal/coder/coder_sniffer
#   # Installation of Symfony console autocomplete
#   RUN composer global require bamarni/symfony-console-autocomplete
#   installation of ssmtp
RUN DEBIAN_FRONTEND=noninteractive apt-get install --fix-missing -y ssmtp \
 && rm -r /var/lib/apt/lists/*
COPY core/ssmtp.conf /etc/ssmtp/ssmtp.conf
COPY core/php-smtp.ini /usr/local/etc/php/conf.d/php-smtp.ini
COPY config/apache2.conf /etc/apache2
COPY core/envvars /etc/apache2
COPY core/other-vhosts-access-log.conf /etc/apache2/conf-enabled/
RUN rm /etc/apache2/sites-enabled/000-default.conf
#   Installation of Opcode cache
RUN (echo "opcache.memory_consumption=128" ;echo "opcache.interned_strings_buffer=8" ;echo "opcache.max_accelerated_files=4000" ;echo "opcache.revalidate_freq=5" ;echo "opcache.fast_shutdown=1" ;echo "opcache.enable_cli=1" ) > /usr/local/etc/php/conf.d/opcache-recommended.ini
RUN a2enmod rewrite expires \
 && service apache2 restart
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
#   ADD BASHRC CONFIG
COPY config/bashrc /root/
RUN mv /root/bashrc /root/.bashrc
#   Expose 80 for apache, 9000 for xdebug
EXPOSE 80/tcp 9000/tcp
#   Set a custom entrypoint.
COPY core/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
