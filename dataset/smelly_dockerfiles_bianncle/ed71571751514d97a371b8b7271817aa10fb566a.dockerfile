#
#  million12/nginx-php
#
FROM million12/nginx:latest
MAINTAINER Marcin Ryzycki <marcin@m12.io>
ENV NVM_DIR="/usr/local/nvm" \
    NODE_VERSION="6.3.0" \
    STATUS_PAGE_ALLOWED_IP="127.0.0.1"
#  Add install scripts needed by the next RUN command
ADD container-files/config/install* /config/
ADD container-files/etc/yum.repos.d/* /etc/yum.repos.d/
RUN yum update -y \
 && `` yum install -y wget patch tar bzip2 unzip openssh-clients MariaDB-client \
 && `` rpm -Uvh http://rpms.remirepo.net/enterprise/remi-release-7.rpm \
 && yum install -y php70-php php70-php-bcmath php70-php-cli php70-php-common php70-php-devel php70-php-fpm php70-php-gd php70-php-gmp php70-php-intl php70-php-json php70-php-mbstring php70-php-mcrypt php70-php-mysqlnd php70-php-opcache php70-php-pdo php70-php-pear php70-php-process php70-php-pspell php70-php-xml `` php70-php-pecl-imagick php70-php-pecl-mysql php70-php-pecl-uploadprogress php70-php-pecl-uuid php70-php-pecl-zip `` || true \
 && `` ln -sfF /opt/remi/php70/enable /etc/profile.d/php70-paths.sh \
 && `` `` ln -sfF /opt/remi/php70/root/usr/bin/{pear,pecl,phar,php,php-cgi,php-config,phpize} /usr/local/bin/. \
 && php --version \
 && `` mv -f /etc/opt/remi/php70/php.ini /etc/php.ini \
 && ln -s /etc/php.ini /etc/opt/remi/php70/php.ini \
 && rm -rf /etc/php.d \
 && mv /etc/opt/remi/php70/php.d /etc/. \
 && ln -s /etc/php.d /etc/opt/remi/php70/php.d \
 && echo 'PHP 7 installed.' \
 && `` yum install -y ImageMagick GraphicsMagick gcc gcc-c++ libffi-devel libpng-devel zlib-devel \
 && `` `` yum install -y ruby ruby-devel \
 && echo 'gem: --no-document' > /etc/gemrc \
 && gem update --system \
 && gem install bundler \
 && `` source /config/install.sh \
 && `` export PROFILE=/etc/profile.d/nvm.sh \
 && touch $PROFILE \
 && curl -sSL https://raw.githubusercontent.com/creationix/nvm/v0.31.2/install.sh | bash \
 && source $NVM_DIR/nvm.sh \
 && nvm install $NODE_VERSION \
 && nvm alias default $NODE_VERSION \
 && nvm use default \
 && `` npm install -g gulp grunt-cli bower browser-sync \
 && `` echo -e "StrictHostKeyChecking no" >> /etc/ssh/ssh_config \
 && `` `` yum install -y libmemcached-devel \
 && git clone https://github.com/php-memcached-dev/php-memcached.git \
 && cd php-memcached \
 && git checkout php7 \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && echo "extension=memcached.so" > /etc/php.d/50-memcached.ini \
 && `` git clone https://github.com/phpredis/phpredis.git \
 && cd phpredis \
 && git checkout php7 \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && echo "extension=redis.so" > /etc/php.d/50-redis.ini \
 && curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer \
 && chown www /usr/local/bin/composer \
 && composer --version \
 && `` yum clean all \
 && rm -rf /tmp/yum*
ADD container-files /
#  Add NodeJS/npm to PATH (must be separate ENV instruction as we want to use $NVM_DIR)
ENV NODE_PATH="$NVM_DIR/versions/node/v$NODE_VERSION/lib/node_modules" \
    PATH="$NVM_DIR/versions/node/v$NODE_VERSION/bin:$PATH"
