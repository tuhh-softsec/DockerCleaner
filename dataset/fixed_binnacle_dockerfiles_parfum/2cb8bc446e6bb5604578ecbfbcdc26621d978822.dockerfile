FROM php:7.2-fpm
MAINTAINER clement@cyber-duck.co.uk
RUN apt-get update \
 && apt-get install --no-install-recommends libmemcached-dev libz-dev libpq-dev libjpeg-dev libpng-dev libfreetype6-dev libssl-dev libmcrypt-dev openssh-server git cron nano wget curl sudo zip unzip git gnupg1 gnupg2 gnupg -y --force-yes
#  Install the PHP mcrypt extention (from PECL, mcrypt has been removed from PHP 7.2)
RUN pecl install mcrypt-1.0.1
RUN docker-php-ext-enable mcrypt
#  Install the PHP pcntl extention
RUN docker-php-ext-install pcntl
#  Install the PHP zip extention
RUN docker-php-ext-install zip
#  Install the PHP pdo_mysql extention
RUN docker-php-ext-install pdo_mysql
#  Install the PHP mysqli extention
RUN docker-php-ext-install mysqli
#  Install the PHP pdo_pgsql extention
RUN docker-php-ext-install pdo_pgsql
#  Enable PHP extension(s)
RUN docker-php-ext-enable pdo_mysql
# ####################################
#  GD:
# ####################################
#  Install the PHP gd library
RUN docker-php-ext-install gd \
 && docker-php-ext-configure gd --enable-gd-native-ttf --with-jpeg-dir=/usr/lib --with-freetype-dir=/usr/include/freetype2 \
 && docker-php-ext-install gd
# ####################################
#  xDebug:
# ####################################
#  Install the xdebug extension
RUN pecl install xdebug \
 && docker-php-ext-enable xdebug
#  Copy xdebug configration for remote debugging
COPY ./etc/php/xdebug.ini /usr/local/etc/php/conf.d/xdebug.ini
# ####################################
#  PHP Memcached:
# ####################################
#  Install the php memcached extension
RUN pecl install memcached \
 && docker-php-ext-enable memcached
# ####################################
#  Composer:
# ####################################
#  Install composer and add its bin to the PATH.
RUN curl -s http://getcomposer.org/installer | php \
 && echo "export PATH=${PATH}:/var/www/vendor/bin" >> ~/.bashrc \
 && mv composer.phar /usr/local/bin/composer
#  Source the bash
RUN . ~/.bashrc
# ####################################
#  PHPUnit:
# ####################################
RUN wget https://phar.phpunit.de/phpunit.phar
RUN chmod +x phpunit.phar
RUN mv phpunit.phar /usr/local/bin/phpunit
# ####################################
#  Laravel Schedule Cron Job:
# ####################################
RUN echo "* * * * * root php /var/www/artisan schedule:run >> /dev/null 2>&1" >> /etc/cron.d/laravel-schedule
RUN chmod 0644 /etc/cron.d/laravel-schedule
# ####################################
#  Security Checker:
# ####################################
#  Security Checker
RUN wget http://get.sensiolabs.org/security-checker.phar
RUN chmod +x security-checker.phar
RUN mv security-checker.phar /usr/local/bin/security-checker
# ####################################
#  Node.js && Yarn:
# ####################################
#  Node.js
RUN curl -sL https://deb.nodesource.com/setup_11.x -o nodesource_setup.sh
RUN bash nodesource_setup.sh
RUN apt-get install --no-install-recommends nodejs -y
RUN npm install
#  Yarn
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update \
 && apt-get install --no-install-recommends build-essential yarn -qq -y
RUN yarn install
# ####################################
#  Laravel
# ####################################
RUN composer global require "laravel/installer"
#
# --------------------------------------------------------------------------
#  Final Touch
# --------------------------------------------------------------------------
#
COPY ./etc/php/laravel.ini /usr/local/etc/php/conf.d
# ####################################
#  Aliases:
# ####################################
#  docker-compose exec php-fpm dep --> locally installed Deployer binaries
RUN echo '#!/bin/bash\n/usr/local/bin/php /var/www/vendor/bin/dep "$@"' > /usr/bin/dep
RUN chmod +x /usr/bin/dep
#  docker-compose exec php-fpm art --> php artisan
RUN echo '#!/bin/bash\n/usr/local/bin/php /var/www/artisan "$@"' > /usr/bin/art
RUN chmod +x /usr/bin/art
#  docker-compose exec php-fpm migrate --> php artisan migrate
RUN echo '#!/bin/bash\n/usr/local/bin/php /var/www/artisan migrate "$@"' > /usr/bin/migrate
RUN chmod +x /usr/bin/migrate
#  docker-compose exec php-fpm fresh --> php artisan migrate:fresh --seed
RUN echo '#!/bin/bash\n/usr/local/bin/php /var/www/artisan migrate:fresh --seed' > /usr/bin/fresh
RUN chmod +x /usr/bin/fresh
#  docker-compose exec php-fpm t --> run the tests for the project and generate testdox
RUN echo '#!/bin/bash\n/usr/local/bin/php /var/www/artisan config:clear\n/var/www/vendor/bin/phpunit -d memory_limit=2G --stop-on-error --stop-on-failure --testdox-text=tests/report.txt "$@"' > /usr/bin/t
RUN chmod +x /usr/bin/t
#  docker-compose exec php-fpm d --> run the Laravel Dusk browser tests for the project
RUN echo '#!/bin/bash\n/usr/local/bin/php /var/www/artisan config:clear\n/bin/bash\n/usr/local/bin/php /var/www/artisan dusk -d memory_limit=2G --stop-on-error --stop-on-failure --testdox-text=tests/report-dusk.txt "$@"' > /usr/bin/d
RUN chmod +x /usr/bin/d
RUN rm -r /var/lib/apt/lists/*
RUN usermod -u 1000 www-data
WORKDIR /var/www
EXPOSE 9000/tcp
CMD ["php-fpm"]
