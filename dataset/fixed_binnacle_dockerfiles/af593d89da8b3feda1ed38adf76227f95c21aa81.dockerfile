FROM php:7.2-fpm
MAINTAINER Jaroslav Hranicka <hranicka@outlook.com>
ENV DEBIAN_FRONTEND="noninteractive"
COPY bin/* /usr/local/bin/
RUN chmod -R 700 /usr/local/bin/
#   Locales
RUN apt-get update \
 && apt-get install --no-install-recommends locales=2.28-10+deb10u2 -y
RUN dpkg-reconfigure locales \
 && locale-gen C.UTF-8 \
 && /usr/sbin/update-locale LANG=C.UTF-8
RUN echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen \
 && locale-gen
ENV LC_ALL="C.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
#   Common
RUN apt-get update \
 && apt-get install --no-install-recommends openssl=1.1.1n-0+deb10u4 git=1:2.20.1-2+deb10u8 gnupg2=2.2.12-1+deb10u2 -y
#   PHP
#   intl
RUN apt-get update \
 && apt-get install --no-install-recommends libicu-dev=63.1-6+deb10u3 -y \
 && docker-php-ext-configure intl \
 && docker-php-ext-install -j$( nproc ;) intl
#   xml
RUN apt-get update \
 && apt-get install --no-install-recommends libxml2-dev=2.9.4+dfsg1-7+deb10u5 libxslt-dev -y \
 && docker-php-ext-install -j$( nproc ;) dom xmlrpc xsl
#   images
RUN apt-get update \
 && apt-get install --no-install-recommends libfreetype6-dev=2.9.1-3+deb10u3 libjpeg62-turbo-dev=1:1.5.2-2+deb10u1 libpng-dev=1.6.36-6 libgd-dev=2.2.5-5.2 -y \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install -j$( nproc ;) gd exif
#   database
RUN docker-php-ext-install -j$( nproc ;) mysqli pdo pdo_mysql
#   strings
RUN docker-php-ext-install -j$( nproc ;) gettext mbstring
#   math
RUN apt-get update \
 && apt-get install --no-install-recommends libgmp-dev=2:6.1.2+dfsg-4+deb10u1 -y \
 && ln -s /usr/include/x86_64-linux-gnu/gmp.h /usr/include/gmp.h \
 && docker-php-ext-install -j$( nproc ;) gmp bcmath
#   compression
RUN apt-get update \
 && apt-get install --no-install-recommends libbz2-dev=1.0.6-9.2~deb10u2 zlib1g-dev=1:1.2.11.dfsg-1+deb10u2 -y \
 && docker-php-ext-install -j$( nproc ;) zip bz2
#   ftp
RUN apt-get update \
 && apt-get install --no-install-recommends libssl-dev=1.1.1n-0+deb10u4 -y \
 && docker-php-ext-install -j$( nproc ;) ftp
#   ssh2
RUN apt-get update \
 && apt-get install --no-install-recommends libssh2-1-dev=1.8.0-2.1 -y
#   memcached
RUN apt-get update \
 && apt-get install --no-install-recommends libmemcached-dev=1.0.18-4.2 libmemcached11=1.0.18-4.2 -y
#   others
RUN docker-php-ext-install -j$( nproc ;) soap sockets calendar sysvmsg sysvsem sysvshm
#   PECL
RUN docker-php-pecl-install ssh2-1.1.2 redis-4.0.2 apcu-5.1.11 memcached-3.0.4
#   Install XDebug, but not enable by default. Enable using:
#   * php -d$XDEBUG_EXT vendor/bin/phpunit
#   * php_xdebug vendor/bin/phpunit
RUN pecl install xdebug-2.6.0
ENV XDEBUG_EXT="zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-20170718/xdebug.so"
RUN alias php_xdebug="php -d$XDEBUG_EXT vendor/bin/phpunit"
#   Install composer and put binary into $PATH
RUN curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/ \
 && ln -s /usr/local/bin/composer.phar /usr/local/bin/composer
#   Install PHP Code sniffer
RUN curl -OL https://squizlabs.github.io/PHP_CodeSniffer/phpcs.phar \
 && chmod 755 phpcs.phar \
 && mv phpcs.phar /usr/local/bin/ \
 && ln -s /usr/local/bin/phpcs.phar /usr/local/bin/phpcs \
 && curl -OL https://squizlabs.github.io/PHP_CodeSniffer/phpcbf.phar \
 && chmod 755 phpcbf.phar \
 && mv phpcbf.phar /usr/local/bin/ \
 && ln -s /usr/local/bin/phpcbf.phar /usr/local/bin/phpcbf
#   Install PHPUnit
RUN curl -OL https://phar.phpunit.de/phpunit.phar \
 && chmod 755 phpunit.phar \
 && mv phpunit.phar /usr/local/bin/ \
 && ln -s /usr/local/bin/phpunit.phar /usr/local/bin/phpunit
COPY php.ini /usr/local/etc/php/conf.d/docker-php.ini
#  # NodeJS, NPM
#   Install NodeJS
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && apt-get install --no-install-recommends nodejs=10.24.0~dfsg-1~deb10u3 -y
#   Install Yarn
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update \
 && apt-get install --no-install-recommends yarn -y
#   Install Grunt globally
RUN npm install grunt-cli@1.4.3 -g
#   Install Gulp globally
RUN npm install gulp-cli@2.3.0 -g
#   Install Bower globally
RUN npm install bower@1.8.14 -g
#   MariaDB
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.2-2 dirmngr=2.2.12-1+deb10u2 -y \
 && apt-key adv --recv-keys --keyserver keyserver.ubuntu.com 0xF1656F24C74CD1D8 \
 && add-apt-repository 'deb [arch=amd64,i386,ppc64el] http://mirror.vpsfree.cz/mariadb/repo/10.2/debian stretch main'
RUN apt-get update \
 && apt-get install --no-install-recommends mariadb-server=1:10.3.38-0+deb10u1 -y
VOLUME /var/lib/mysql
COPY my.cnf /etc/mysql/conf.d/my.cnf
EXPOSE 3306/tcp
#   Redis
RUN apt-get update \
 && apt-get install --no-install-recommends redis-server=5:5.0.14-1+deb10u3 -y
EXPOSE 6379/tcp
#   Clean
RUN apt-get clean
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /var/cache/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
