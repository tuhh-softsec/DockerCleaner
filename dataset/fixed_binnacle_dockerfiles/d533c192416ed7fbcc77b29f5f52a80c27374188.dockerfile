FROM php:7.2-fpm
#   ssh2
RUN apt-get update \
 && apt-get install --no-install-recommends libssh2-1-dev=1.8.0-2.1 libssh2-1=1.8.0-2.1 -y
RUN pecl install ssh2-1.1.2
RUN echo extension=ssh2.so >> /usr/local/etc/php/conf.d/ssh2.ini
#   ssh2
RUN apt-get update \
 && apt-get install --no-install-recommends libz-dev libmemcached-dev=1.0.18-4.2 -y
RUN pecl install memcached-3.1.2
RUN echo extension=memcached.so >> /usr/local/etc/php/conf.d/memcached.ini
#   gd extension
RUN apt-get update \
 && apt-get install --no-install-recommends libpng-dev=1.6.36-6 libfreetype6-dev=2.9.1-3+deb10u3 libjpeg-dev=1:1.5.2-2+deb10u1 libxpm-dev=1:3.5.12-1 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libxslt-dev librabbitmq-dev=0.9.0-0.2 libssh-dev=0.8.7-1+deb10u1 libwebp-dev=0.6.1-2+deb10u1 -y
RUN docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ --with-xpm-dir=/usr/include --with-webp-dir=/usr/include/
RUN pecl install amqp-1.9.3
RUN docker-php-ext-enable amqp
RUN docker-php-ext-install gd
RUN docker-php-ext-install zip
RUN docker-php-ext-install xsl
RUN apt-get update \
 && apt-get install --no-install-recommends libicu-dev=63.1-6+deb10u3 -y
RUN docker-php-ext-install intl
RUN docker-php-ext-install mysqli
RUN docker-php-ext-install pdo_mysql
RUN docker-php-ext-install opcache
RUN docker-php-ext-install bcmath
RUN docker-php-ext-install calendar
RUN docker-php-ext-install sockets
RUN pecl install redis \
 && docker-php-ext-enable redis
#   ldap
RUN apt-get update \
 && apt-get install --no-install-recommends libldap2-dev=2.4.47+dfsg-3+deb10u7 -y \
 && rm -rf /var/lib/apt/lists/* \
 && docker-php-ext-configure ldap --with-libdir=lib/x86_64-linux-gnu/ \
 && docker-php-ext-install ldap
#   ldap  
RUN docker-php-ext-configure pcntl --enable-pcntl \
 && docker-php-ext-install pcntl
RUN pecl install xdebug-2.6.1
RUN echo "zend_extension=`php -i | grep ^extension_dir | cut -f 3 -d ' ' `/xdebug.so" >> /usr/local/etc/php/conf.d/xdebug.ini
RUN docker-php-ext-install soap
ENV COMPOSER_HOME="/composer"
COPY --from=composer:1.8.4 /usr/bin/composer /usr/local/bin/composer
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.20.1-2+deb10u8 vim=2:8.1.0875-5+deb10u4 mysql-client rsync=3.1.3-6 sshpass=1.06-1 bzip2=1.0.6-9.2~deb10u2 msmtp=1.8.3-1 unzip=6.0-23+deb10u3 -y
COPY php.ini /usr/local/etc/php/php.ini
#   Cron
RUN apt-get update \
 && apt-get install --no-install-recommends cron=3.0pl1-134+deb10u1 -y \
 && mkfifo --mode 0666 /var/log/cron.log \
 && sed --regexp-extended --in-place 's/^session\s+required\s+pam_loginuid.so$/session optional pam_loginuid.so/' /etc/pam.d/cron
COPY start-cron /usr/sbin
RUN chmod +x /usr/sbin/start-cron
#   Cron
ENV PHP_DATE_TIMEZONE="" \
    PHP_LOG_ERRORS_MAX_LEN="1024" \
    PHP_LOG_ERRORS="" \
    PHP_MAX_EXECUTION_TIME="0" \
    PHP_MAX_FILE_UPLOADS="20" \
    PHP_MAX_INPUT_VARS="1000" \
    PHP_MEMORY_LIMIT="128M" \
    PHP_POST_MAX_SIZE="8M" \
    PHP_SENDMAIL_PATH="/usr/sbin/sendmail -t -i" \
    PHP_SESSION_SAVE_HANDLER="files" \
    PHP_SESSION_SAVE_PATH="" \
    PHP_UPLOAD_MAX_FILESIZE="2M" \
    PHP_XDEBUG_DEFAULT_ENABLE="0" \
    PHP_XDEBUG_IDEKEY=" PHP_XDEBUG_PROFILER_ENABLE=0" \
    PHP_XDEBUG_REMOTE_AUTOSTART="0" \
    PHP_XDEBUG_REMOTE_CONNECT_BACK="0" \
    PHP_XDEBUG_REMOTE_ENABLE="0" \
    PHP_XDEBUG_REMOTE_HOST="0"
WORKDIR /usr/src/app
#   imagick
RUN apt-get update \
 && apt-get install --no-install-recommends libmagickwand-dev=8:6.9.10.23+dfsg-2.1+deb10u4 -y \
 && pecl install imagick \
 && docker-php-ext-enable imagick
#   Install Postgre PDO
RUN apt-get install --no-install-recommends libpq-dev=11.19-0+deb10u1 -y \
 && docker-php-ext-configure pgsql -with-pgsql=/usr/local/pgsql \
 && docker-php-ext-install pdo_pgsql pgsql
RUN pecl install pcov-1.0.0
RUN docker-php-ext-enable pcov
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
