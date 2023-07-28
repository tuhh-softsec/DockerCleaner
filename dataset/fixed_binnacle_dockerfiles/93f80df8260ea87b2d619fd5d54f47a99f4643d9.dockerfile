FROM centos:latest
MAINTAINER "The CentOS Project" <admin@jiobxn.com>
ARG LATEST="0"
RUN cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
RUN yum clean all ; yum -y install epel-release ; yum -y update \
 && yum -y install net-tools bash-completion vim wget strace make gcc-c++ autoconf iptables bzip2 unzip bzip2-devel mingw64-win-iconv libmcrypt-devel libxml2-devel libcurl-devel libpng-devel libc-client-devel krb5-devel libicu-devel libjpeg libjpeg-devel libXpm-devel freetype-devel gmp-devel readline-devel net-snmp-devel libtidy-devel libgcrypt-devel gd-devel gdbm-devel libxslt-devel mhash-devel openldap-devel libacl-devel enchant-devel libedit-devel libmemcached-devel \
 && yum clean all
RUN cd /usr/local/src \
 && PHP_VERSION="$( curl -s http://php.net/downloads.php | egrep php-5.6.*.tar.gz | awk -F/ '{print $3}' ;)" \
 && wget -c http://php.net/distributions/${PHP_VERSION} \
 && wget -c https://github.com/phpredis/phpredis/archive/develop.zip \
 && wget -c http://downloads.zend.com/guard/7.0.0/zend-loader-php5.6-linux-x86_64.tar.gz \
 && wget -c https://github.com/php-memcached-dev/php-memcached/archive/2.2.0.tar.gz
RUN cd /usr/local/src \
 && tar zxf php-*.tar.gz \
 && unzip develop.zip \
 && tar zxf zend-loader-php5.6-linux-x86_64.tar.gz \
 && tar zxf 2.2.0.tar.gz \
 && cd /usr/local/src/php-* \
 && ./configure --prefix=/usr/local/php --with-config-file-path=/usr/local/php/etc --with-pdo-mysql --with-mysqli --with-mysql --with-iconv-dir=/usr --with-mcrypt=/usr --with-libdir=lib64 --with-mhash --with-curl --with-xmlrpc --with-gettext --with-imap-ssl --with-imap --with-pear --with-gd --with-kerberos --with-pcre-regex --with-snmp --with-gmp --with-openssl --with-zlib --with-pcre-dir --with-libxml-dir --with-png-dir --with-freetype-dir --with-icu-dir=/usr --with-jpeg-dir --with-xpm-dir --with-readline --with-ldap-sasl --with-ldap --with-tidy --with-xsl --with-gdbm --with-bz2 --with-fpm-acl --with-enchant --with-libedit --with-system-ciphers --enable-calendar --enable-opcache --enable-exif --enable-intl --enable-mysqlnd --enable-dba --enable-fpm --enable-xml --enable-ftp --enable-zip --enable-soap --enable-shmop --enable-wddx --enable-pcntl --enable-bcmath --enable-sockets --enable-sysvsem --enable-sysvmsg --enable-sysvshm --enable-mbregex --enable-mbstring --enable-session --enable-embedded-mysqli --enable-gd-native-ttf --enable-inline-optimization \
 && make -j8 \
 && make install \
 && cp /usr/local/src/php-*/php.ini-development /usr/local/php/etc/php.ini \
 && ln -s /usr/local/php/bin/* /usr/local/bin/ \
 && ln -s /usr/local/php/sbin/* /usr/local/bin/ \
 && cd /usr/local/src/phpredis-develop \
 && phpize \
 && ./configure \
 && make -j8 \
 && make install \
 && sed -i '876 i extension=redis.so' /usr/local/php/etc/php.ini \
 && cd /usr/local/src/php-memcached-* \
 && phpize \
 && ./configure \
 && make -j8 \
 && make install \
 && sed -i '877 i extension=memcached.so' /usr/local/php/etc/php.ini \
 && sed -i 's/;date.timezone =/date.timezone = PRC/' /usr/local/php/etc/php.ini \
 && cp /usr/local/src/zend-loader-php5.6-linux-x86_64/ZendGuardLoader.so "$( php -i | grep extensions | awk '{print $3}' ;)" \
 && cp /usr/local/php/etc/php-fpm.conf.default /usr/local/php/etc/php-fpm.conf \
 && sed -i 's/listen = 127.0.0.1:9000/listen = [::]:9000/' /usr/local/php/etc/php-fpm.conf \
 && sed -i 's/;daemonize = yes/daemonize = no/' /usr/local/php/etc/php-fpm.conf \
 && echo -e "[Zend Guard Loader]\nzend_extension=\"$( php -i | grep extensions | awk '{print $3}' ;)/ZendGuardLoader.so\"\nzend_loader.enable=1\nzend_loader.disable_licensing=0\nzend_loader.obfuscation_level_support=3" >> /usr/local/php/etc/php.ini \
 && rm -rf /usr/local/src/*
VOLUME /var/www
COPY php.sh /php.sh
RUN chmod +x /php.sh
ENTRYPOINT ["/php.sh"]
EXPOSE 9000/tcp
CMD ["php-fpm"]
#   docker build -t php:5.6 .
#   docker run -d --restart always -p 9000:9000 -v /docker/www:/var/www --hostname php --name php php:5.6
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
