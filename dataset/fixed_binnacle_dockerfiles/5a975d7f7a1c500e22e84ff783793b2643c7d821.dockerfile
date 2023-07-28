FROM php:7.1-fpm
MAINTAINER Stepanov Nikolai <nstepanovdev@gmail.com>
RUN :
ENV COMPOSER_HOME="/home/composer/.composer"
ENV COMPOSER_ALLOW_SUPERUSER="1"
ENV ZSH="/home/.oh-my-zsh"
#   Install locale
ARG TZ=UTC
ENV TZ="${TZ}"
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone \
 && (apt-get update ;apt-get install --no-install-recommends locales=2.28-10+deb10u2 -f -y --reinstall ) \
 && locale-gen en_US.UTF-8
#   INSTALL EXTENSIONS
#   apcu
RUN pecl install apcu
RUN echo "extension=apcu.so" > /usr/local/etc/php/conf.d/apcu.ini
#   bz2
RUN (apt-get update ;apt-get install --no-install-recommends libbz2-dev=1.0.6-9.2~deb10u2 -y )
RUN docker-php-ext-install bz2
#   gd
RUN (apt-get update ;apt-get install --no-install-recommends libfreetype6-dev=2.9.1-3+deb10u3 libjpeg62-turbo-dev=1:1.5.2-2+deb10u1 libpng-dev=1.6.36-6 -y )
RUN docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/
RUN docker-php-ext-install gd
#   mcrypt
RUN (apt-get update ;apt-get install --no-install-recommends libmcrypt-dev=2.5.8-3.4 -y )
RUN docker-php-ext-install mcrypt
#   pdo
RUN docker-php-ext-install pdo_mysql \
 && (apt-get update ;apt-get install --no-install-recommends libpq-dev=11.19-0+deb10u1 -y ) \
 && docker-php-ext-install pdo_pgsql \
 && (apt-get update ;apt-get install --no-install-recommends libsqlite3-dev=3.27.2-3+deb10u2 -y ) \
 && docker-php-ext-install pdo_sqlite
#   phpredis
RUN pecl install -o -f redis \
 && rm -rf /tmp/pear \
 && echo "extension=redis.so" > /usr/local/etc/php/conf.d/redis.ini
#   xsl
RUN (apt-get update ;apt-get install --no-install-recommends libxslt-dev -y )
RUN docker-php-ext-install xsl
#   intl
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends zlib1g-dev=1:1.2.11.dfsg-1+deb10u2 libicu-dev=63.1-6+deb10u3 g++=4:8.3.0-1 -y )
RUN docker-php-ext-configure intl
RUN docker-php-ext-install intl
#  RUN pecl install intl
#  RUN docker-php-ext-install intl
#   zip
RUN (apt-get update ;apt-get install --no-install-recommends zlib1g-dev=1:1.2.11.dfsg-1+deb10u2 -y ) \
 && docker-php-ext-install zip
#   common
RUN (apt-get update ;apt-get install --no-install-recommends libssl-dev=1.1.1n-0+deb10u4 -y )
RUN docker-php-ext-install opcache calendar dba pcntl bcmath mbstring xmlrpc ftp shmop
#   preconf enviroment
ENV PHP_EXTRA_CONFIGURE_ARGS="--enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data"
RUN usermod -u 1000 -d /data www-data -s /bin/bash
RUN groupadd -r node \
 && useradd -r -g node node \
 && usermod -G www-data -a node
RUN mkdir /data \
 && chmod -R 644 /data \
 && find /data -type d -exec chmod 755 {}
RUN (apt-get update ;apt-get install --no-install-recommends libfreetype6-dev=2.9.1-3+deb10u3 libjpeg62-turbo-dev=1:1.5.2-2+deb10u1 libmcrypt-dev=2.5.8-3.4 libpng-dev=1.6.36-6 msmtp=1.8.3-1 imagemagick=8:6.9.10.23+dfsg-2.1+deb10u4 libssl-dev=1.1.1n-0+deb10u4 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libicu-dev=63.1-6+deb10u3 libxslt-dev wget=1.20.1-1.1 git=1:2.20.1-2+deb10u8 vim=2:8.1.0875-5+deb10u4 ruby=1:2.5.1 ruby-dev=1:2.5.1 libcurl4-openssl-dev=7.64.0-4+deb10u5 mc=3:4.8.22-1 -y )
#  Install nodejs
RUN (apt-get update ;apt-get install --no-install-recommends gnupg=2.2.12-1+deb10u2 -y )
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=10.24.0~dfsg-1~deb10u3 build-essential=12.6 -y )
#   Install OH-MY-ZSH to see pretty terminal and ditch the bash
RUN (apt-get update ;apt-get install --no-install-recommends zsh=5.7.1-1+deb10u1 -y )
RUN curl -L https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh | bash
#   Clean apt
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Install Bower & Grunt
RUN npm install bower@1.8.14 gulp@4.0.2 uglify-js@3.17.4 uglifycss@0.0.29 webpack@2.2.* -g \
 && echo '{ "allow_root": true }' > /root/.bowerrc
#   Install gem dependencies
RUN gem install bundler --version 2.4.12
#  COMPOSER
RUN curl https://getcomposer.org/installer | php -- \
 && mv composer.phar /usr/local/bin/composer \
 && chmod +x /usr/local/bin/composer
ENV PATH="/home/composer/.composer/vendor/bin:$PATH"
#  PHPUNIT
RUN composer global require "phpunit/phpunit" \
 && ln -s /home/composer/.composer/vendor/bin/phpunit /usr/local/bin/phpunit
#  Laravel
RUN composer global require "laravel/installer"
RUN ln -s /home/composer/.composer/vendor/bin/laravel /usr/local/bin/laravel
#  Symfony installer
RUN curl -LsS https://symfony.com/installer -o /usr/local/bin/symfony
RUN chmod a+x /usr/local/bin/symfony
#  Symfony2 autocomplete
RUN composer global require "bamarni/symfony-console-autocomplete"
RUN ln -s /home/composer/.composer/vendor/bin/symfony-autocomplete /usr/local/bin/symfony-autocomplete
#  DEPLOYER.ORG
RUN curl -LO https://deployer.org/deployer.phar \
 && mv deployer.phar /usr/local/bin/dep \
 && chmod +x /usr/local/bin/dep
COPY etc/php-fpm.conf /usr/local/etc/
COPY etc/php.ini /usr/local/etc/php/
COPY etc/.bashrc /etc/bash.bashrc
COPY etc/.zshrc /etc/zsh/newuser.zshrc.recommended
COPY etc/.zshrc /etc/zsh/zshrc
RUN chmod ugo+rX -R /usr/local/etc/php
RUN chmod -R 777 /home/composer \
 && find /home/composer -type d -exec chmod 777 {}
WORKDIR /data
CMD ["php-fpm"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
