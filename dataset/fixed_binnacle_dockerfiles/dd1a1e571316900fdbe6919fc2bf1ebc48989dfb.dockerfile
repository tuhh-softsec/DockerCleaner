#   Based on ubuntu 12.04
FROM ubuntu:precise
MAINTAINER Christian Simon <simon@swine.de>
#   Update everything & install prequesites
RUN : \
 && DEBIAN_FRONTEND=noninteractive apt-get -y install build-essential wget bzip2 libpng-dev libmcrypt-dev libmcrypt4 libmhash-dev libmysqlclient-dev libjpeg-dev zlib1g-dev libfreetype6-dev libfontconfig1-dev re2c libgpg-error-dev uuid-dev libt1-dev libsnmp-dev libc-client2007e-dev libaspell-dev libbz2-dev libc-client2007e-dev flex bison libsqlite3-dev libpq-dev libcurl4-openssl-dev libmagickwand-dev libgd2-xpm-dev autoconf imagemagick graphicsmagick libdb4.8-dev libtidy-dev libxslt-dev libgeoip-dev libicu-dev spawn-fcgi \
 && DEBIAN_FRONTEND=noninteractive apt-get -y dist-upgrade \
 && apt-get clean \
 && rm /var/lib/apt/lists/*_*
#   Hotfixes php-5.3
#  # Copy libc-client to x86_64-dir
RUN cp /usr/lib/libc-client.so /usr/lib/x86_64-linux-gnu/
RUN cp /usr/lib/libc-client.a /usr/lib/x86_64-linux-gnu/
#   Download PHP
ENV PHP_VERSION="5.3.29"
RUN mkdir -p /tmp/install/ \
 && cd /tmp/install \
 && wget http://de2.php.net/distributions/php-${PHP_VERSION}.tar.bz2 \
 && echo "9469e240cbe6ac865aeaec89b253dd30 php-${PHP_VERSION}.tar.bz2" > md5sums \
 && md5sum -c md5sums \
 && tar xfj php-${PHP_VERSION}.tar.bz2 \
 && cd php-${PHP_VERSION} \
 && ./configure --enable-cgi --with-config-file-path=/etc/php5 --with-config-file-scan-dir=/etc/php5/conf.d --with-libdir=lib/x86_64-linux-gnu --with-mcrypt --with-zlib --with-curl --disable-debug --disable-rpath --enable-inline-optimization --with-bz2 --with-zlib --enable-sockets --enable-sysvsem --enable-sysvshm --enable-pcntl --enable-mbregex --with-mhash --enable-zip --with-pcre-regex --with-mysqli --with-mysql --with-gd=shared,/usr --enable-gd-native-ttf --with-ldap --with-mcrypt --with-mhash --with-snmp --enable-ctype --with-freetype-dir=shared,/usr --with-jpeg-dir=/usr --with-t1lib=/usr --enable-bcmath --with-bz2 --enable-ctype --with-db4 --with-iconv --enable-exif --enable-ftp --with-gettext --enable-mbstring --with-imap --with-imap-ssl --with-kerberos --with-openssl --enable-intl --with-pgsql --with-pdo-mysql --enable-soap --with-tidy --with-libxml-dir=/usr/ --with-openssl --with-xsl \
 && make -j 4 \
 && make install \
 && rm -rf /tmp/install
#   Create config directory
RUN mkdir -p /etc/php5/conf.d/
#   Set location and timestamp
RUN echo "date.timezone=Europe/Berlin\ndate.default_latitude=49.08\ndate.default_longitude=11.22" > /etc/php5/conf.d/10_timezone.ini
#   Enable gd lib
RUN echo "extension=gd.so" > /etc/php5/conf.d/30_gd.ini
#   Build module xhprof-0.9.4
RUN mkdir -p /tmp/install/ \
 && cd /tmp/install \
 && wget http://pecl.php.net/get/xhprof-0.9.4.tgz \
 && echo "ab4062a7337e3bdaa2fd7065a7942b8d xhprof-0.9.4.tgz" > md5sums \
 && md5sum -c md5sums \
 && tar xfz xhprof-0.9.4.tgz \
 && cd xhprof-0.9.4/extension \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && rm -rf /tmp/install
#   Build module xcache 3.10
RUN mkdir -p /tmp/install/ \
 && cd /tmp/install \
 && wget http://xcache.lighttpd.net/pub/Releases/3.1.0/xcache-3.1.0.tar.bz2 \
 && echo "e5816d47d52be200b959bf69a673ff74 xcache-3.1.0.tar.bz2" > md5sums \
 && md5sum -c md5sums \
 && tar xfj xcache-3.1.0.tar.bz2 \
 && cd xcache-3.1.0 \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && rm -rf /tmp/install
RUN pecl install channel://pecl.php.net/pecl_http-1.7.6
RUN echo "extension=http.so" > /etc/php5/conf.d/30_http.ini
RUN pecl install channel://pecl.php.net/xdebug-2.2.5
RUN pecl install channel://pecl.php.net/imagick-3.0.1
RUN echo "extension=imagick.so" > /etc/php5/conf.d/30_imagick.ini
RUN pecl install channel://pecl.php.net/apc-3.1.13
RUN echo "extension=apc.so\napc.enabled=1" > /etc/php5/conf.d/30_apc.ini
RUN pecl install channel://pecl.php.net/mailparse-2.1.6
RUN echo "extension=mailparse.so" > /etc/php5/conf.d/30_mailparse.ini
RUN pecl install channel://pecl.php.net/geoip-1.0.8
RUN echo "extension=geoip.so" > /etc/php5/conf.d/30_geoip.ini
RUN pecl install channel://pecl.php.net/memcache-3.0.8
RUN echo "extension=memcache.so" > /etc/php5/conf.d/30_memcache.ini
#   Install ioncube
RUN mkdir -p /tmp/install/ \
 && cd /tmp/install \
 && wget http://downloads3.ioncube.com/loader_downloads/ioncube_loaders_lin_x86-64.tar.bz2 \
 && tar xfj ioncube_loaders_lin_x86-64.tar.bz2 \
 && mv ioncube/ioncube_loader_lin_5.3.so $( php-config --extension-dir ;)/ioncube_loader.so \
 && rm -rf /tmp/install
#   Enable ioncube
RUN echo "zend_extension=$( php-config --extension-dir ;)/ioncube_loader.so" > /etc/php5/conf.d/20_ioncube_loder.ini
#   Test if there is stderr output
RUN bash -c "[[ $( php -i 2>&1 > /dev/null | wc -l ;) -ne 0 ]] \
 && php -i > /dev/null \
 && exit 1; exit 0"
#   Install sendmail replacement / set ip address of real mailserver to 172.17.42.1
RUN mkdir -p /tmp/install/ \
 && cd /tmp/install \
 && wget https://github.com/simonswine/mini_sendmail/archive/1.3.8-1.tar.gz \
 && tar xvfz 1.3.8-1.tar.gz \
 && cd mini_sendmail*/ \
 && make SMTP_HOST=172.17.42.1 \
 && cp -v mini_sendmail /usr/sbin/sendmail \
 && rm -rf /tmp/install
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
