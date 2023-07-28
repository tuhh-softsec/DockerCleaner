FROM php:7.2-fpm
MAINTAINER duzhenxun<5552123@qq.com>
#   set timezome
ENV TZ="Asia/Shanghai"
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone
#   change sources
COPY sources.list /etc/apt/sources.list
RUN apt-get update \
 && apt-get install --no-install-recommends libfreetype6-dev=2.9.1-3+deb10u3 libjpeg62-turbo-dev=1:1.5.2-2+deb10u1 libmcrypt-dev=2.5.8-3.4 libpng-dev=1.6.36-6 libmemcached-dev=1.0.18-4.2 graphicsmagick=1.4+really1.3.35-1~deb10u3 libgraphicsmagick1-dev=1.4+really1.3.35-1~deb10u3 imagemagick=8:6.9.10.23+dfsg-2.1+deb10u4 libmagickwand-dev=8:6.9.10.23+dfsg-2.1+deb10u4 libssh2-1-dev=1.8.0-2.1 libzip-dev=1.5.1-4 libzookeeper-mt-dev=3.4.13-2 libldb-dev=2:1.5.1+really1.4.6-3+deb10u1 libldap2-dev=2.4.47+dfsg-3+deb10u7 libssl-dev=1.1.1n-0+deb10u4 libmosquitto-dev=1.5.7-1+deb10u1 librabbitmq-dev=0.9.0-0.2 libicu-dev=63.1-6+deb10u3 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libxslt-dev libbz2-dev=1.0.6-9.2~deb10u2 git=1:2.20.1-2+deb10u8 vim=2:8.1.0875-5+deb10u4 -y \
 && : \
 && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-configure ldap --with-libdir=lib/x86_64-linux-gnu/ \
 && : \
 && docker-php-ext-install -j$( nproc ;) gd ldap intl soap xsl xmlrpc wddx bz2 zip pcntl pdo_mysql mysqli mbstring calendar sockets opcache exif bcmath \
 && : \
 && apt-get clean \
 && apt-get autoremove --purge -y
#   Composer install
COPY ./ext/composer.phar /usr/local/bin/composer
#  RUN composer config -g repo.packagist composer https://packagist.phpcomposer.com
#   使用源码包方式安装扩展
#   Install Redis extension from source
COPY ./ext/redis-4.1.0.tgz /tmp/redis.tgz
RUN ls /tmp
RUN mkdir -p /tmp/redis \
 && tar -xf /tmp/redis.tgz -C /tmp/redis --strip-components=1 \
 && rm /tmp/redis.tgz \
 && docker-php-ext-configure /tmp/redis --enable-redis \
 && docker-php-ext-install /tmp/redis \
 && rm -r /tmp/redis
#   zendopcache
#  COPY ./ext/zendopcache-7.0.5.tgz /tmp/
#  RUN cd /tmp/ \
#  && tar -xf zendopcache-7.0.5.tgz \
#  && rm zendopcache-7.0.5.tgz \
#  && ( cd zendopcache-7.0.5 && phpize && ./configure && make && make install ) \
#  && rm -r zendopcache-7.0.5 \
#  && docker-php-ext-enable opcache
#   libmemcached
#  COPY ./ext/libmemcached-1.0.18.tar.gz /tmp/
#  RUN cd /tmp/ \
#      && tar -xf libmemcached-1.0.18.tar.gz \
#      && rm libmemcached-1.0.18.tar.gz \
#      && cd libmemcached-1.0.18 \
#      && ./configure --prefix=/usr/local/libmemcached --with-memcached \
#      && make && make install \
#      && rm -rf /tmp/libmemcached-1.0.18
#   memcached
COPY ./ext/memcached-3.0.4.tgz /tmp/
RUN cd /tmp/ \
 && tar -xf memcached-3.0.4.tgz \
 && cd memcached-3.0.4 \
 && phpize \
 && ./configure -enable-memcached -with-php-config=/usr/local/bin/php-config \
 && make \
 && make install \
 && echo "extension=memcached.so" > /usr/local/etc/php/conf.d/memcached.ini \
 && rm -rf /tmp/memcached-3.0.4
#   使用git方式
#  Swoole
RUN cd /tmp \
 && git clone https://gitee.com/swoole/swoole.git \
 && cd swoole \
 && phpize \
 && ./configure --enable-openssl -with-php-config=/usr/local/bin/php-config \
 && make \
 && make install \
 && echo "extension=swoole.so" > /usr/local/etc/php/conf.d/swoole.ini \
 && rm -rf /tmp/swoole
#   pecl 安装
#   Notice: if pecl install get error
#      `No releases available for package "pecl.php.net/xxx"`
#   or
#      `Package "xxx" does not have REST xml available`
#   Please turn on proxy (The proxy IP may be docker host IP or others):
#  RUN pear config-set http_proxy http://10.0.75.1:1080
#  RUN pecl install redis-3.1.4 \
#  && docker-php-ext-enable redis \
#  && pecl install xdebug-2.4.1 \
#  && docker-php-ext-enable xdebug \
#  && apt-get install -y libmagickwand-dev \
#  && pecl install imagick-3.4.3 \
#  && docker-php-ext-enable imagick \
#  && apt-get install -y libmemcached-dev zlib1g-dev \
#  && pecl install memcached-2.2.0 \
#  && docker-php-ext-enable memcached
#  && pecl install gmagick-2.0.5RC1 \
#  && pecl install imagick-3.4.3 \
#  && pecl install memcached-3.0.4 \
#  && pecl install redis-4.0.2 \
#  && pecl install mongodb-1.4.3 \
#  && pecl install swoole-2.1.3 \
#  && pecl install ssh2-1.1.2 \
#  && pecl install yaf-3.0.7 \
#  && pecl install yaconf-1.0.7 \
#  && pecl install zip-1.15.2 \
#  && pecl install zookeeper-0.5.0 \
#  && pecl install Mosquitto-0.4.0 \
#  && pecl install amqp-1.9.3 \
#  && pecl install xdebug-2.6.0 \
#  && docker-php-ext-enable gmagick memcached redis mcrypt mongodb swoole ssh2 yaf yaconf zip zookeeper mosquitto amqp \
#   Write Permission
RUN usermod -u 1000 www-data
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
