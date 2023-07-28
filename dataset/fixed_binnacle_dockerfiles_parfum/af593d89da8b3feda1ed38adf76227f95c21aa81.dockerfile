FROM php:7.2-fpm
MAINTAINER Jaroslav Hranicka <hranicka@outlook.com>
ENV DEBIAN_FRONTEND="noninteractive"
COPY bin/* /usr/local/bin/
RUN chmod -R 700 /usr/local/bin/
#  Locales
RUN apt-get update \
 && apt-get install --no-install-recommends locales -y
RUN dpkg-reconfigure locales \
 && locale-gen C.UTF-8 \
 && /usr/sbin/update-locale LANG=C.UTF-8
RUN echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen \
 && locale-gen
ENV LC_ALL="C.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
#  Common
RUN apt-get update \
 && apt-get install --no-install-recommends openssl git gnupg2 -y
#  PHP
#  intl
RUN apt-get update \
 && apt-get install --no-install-recommends libicu-dev -y \
 && docker-php-ext-configure intl \
 && docker-php-ext-install -j$( nproc ;) intl
#  xml
RUN apt-get update \
 && apt-get install --no-install-recommends libxml2-dev libxslt-dev -y \
 && docker-php-ext-install -j$( nproc ;) dom xmlrpc xsl
#  images
RUN apt-get update \
 && apt-get install --no-install-recommends libfreetype6-dev libjpeg62-turbo-dev libpng-dev libgd-dev -y \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install -j$( nproc ;) gd exif
#  database
RUN docker-php-ext-install -j$( nproc ;) mysqli pdo pdo_mysql
#  strings
RUN docker-php-ext-install -j$( nproc ;) gettext mbstring
#  math
RUN apt-get update \
 && apt-get install --no-install-recommends libgmp-dev -y \
 && ln -s /usr/include/x86_64-linux-gnu/gmp.h /usr/include/gmp.h \
 && docker-php-ext-install -j$( nproc ;) gmp bcmath
#  compression
RUN apt-get update \
 && apt-get install --no-install-recommends libbz2-dev zlib1g-dev -y \
 && docker-php-ext-install -j$( nproc ;) zip bz2
#  ftp
RUN apt-get update \
 && apt-get install --no-install-recommends libssl-dev -y \
 && docker-php-ext-install -j$( nproc ;) ftp
#  ssh2
RUN apt-get update \
 && apt-get install --no-install-recommends libssh2-1-dev -y
#  memcached
RUN apt-get update \
 && apt-get install --no-install-recommends libmemcached-dev libmemcached11 -y
#  others
RUN docker-php-ext-install -j$( nproc ;) soap sockets calendar sysvmsg sysvsem sysvshm
#  PECL
RUN docker-php-pecl-install ssh2-1.1.2 redis-4.0.2 apcu-5.1.11 memcached-3.0.4
#  Install XDebug, but not enable by default. Enable using:
#  * php -d$XDEBUG_EXT vendor/bin/phpunit
#  * php_xdebug vendor/bin/phpunit
RUN pecl install xdebug-2.6.0
ENV XDEBUG_EXT="zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-20170718/xdebug.so"
RUN alias php_xdebug="php -d$XDEBUG_EXT vendor/bin/phpunit"
#  Install composer and put binary into $PATH
RUN curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/ \
 && ln -s /usr/local/bin/composer.phar /usr/local/bin/composer
#  Install PHP Code sniffer
RUN curl -OL https://squizlabs.github.io/PHP_CodeSniffer/phpcs.phar \
 && chmod 755 phpcs.phar \
 && mv phpcs.phar /usr/local/bin/ \
 && ln -s /usr/local/bin/phpcs.phar /usr/local/bin/phpcs \
 && curl -OL https://squizlabs.github.io/PHP_CodeSniffer/phpcbf.phar \
 && chmod 755 phpcbf.phar \
 && mv phpcbf.phar /usr/local/bin/ \
 && ln -s /usr/local/bin/phpcbf.phar /usr/local/bin/phpcbf
#  Install PHPUnit
RUN curl -OL https://phar.phpunit.de/phpunit.phar \
 && chmod 755 phpunit.phar \
 && mv phpunit.phar /usr/local/bin/ \
 && ln -s /usr/local/bin/phpunit.phar /usr/local/bin/phpunit
COPY php.ini /usr/local/etc/php/conf.d/docker-php.ini
# # NodeJS, NPM
#  Install NodeJS
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && apt-get install --no-install-recommends nodejs -y
#  Install Yarn
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update \
 && apt-get install --no-install-recommends yarn -y
#  Install Grunt globally
RUN npm install grunt-cli -g
#  Install Gulp globally
RUN npm install gulp-cli -g
#  Install Bower globally
RUN npm install bower -g
#  MariaDB
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common dirmngr -y \
 && apt-key adv --recv-keys --keyserver keyserver.ubuntu.com 0xF1656F24C74CD1D8 \
 && add-apt-repository 'deb [arch=amd64,i386,ppc64el] http://mirror.vpsfree.cz/mariadb/repo/10.2/debian stretch main'
RUN apt-get update \
 && apt-get install --no-install-recommends mariadb-server -y
VOLUME /var/lib/mysql
COPY my.cnf /etc/mysql/conf.d/my.cnf
EXPOSE 3306/tcp
#  Redis
RUN apt-get update \
 && apt-get install --no-install-recommends redis-server -y
EXPOSE 6379/tcp
#  Clean
RUN apt-get clean
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /var/cache/*
