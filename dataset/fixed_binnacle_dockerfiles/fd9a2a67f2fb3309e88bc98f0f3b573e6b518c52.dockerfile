FROM ubuntu:xenial
MAINTAINER Arif Islam<arif@dreamfactory.com>
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -y \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y
RUN LANG=C.UTF-8 add-apt-repository ppa:ondrej/php -y \
 && apt-get update \
 && apt-get install --no-install-recommends git-core=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 mcrypt=2.6.8-1.3ubuntu1 nginx=1.10.3-0ubuntu0.16.04.5 openssl=1.0.2g-1ubuntu4.20 python=2.7.12-1~16.04 nodejs=4.2.6~dfsg-1ubuntu4.2 zip=3.0-11 ssmtp=2.64-8ubuntu1 wget=1.17.1-1ubuntu1.5 php7.1-fpm php7.1-common php7.1-cli php7.1-curl php7.1-json php7.1-mcrypt php7.1-mysqlnd php7.1-pgsql php7.1-sqlite php-pear=1:1.10.1+submodules+notgz-6ubuntu0.3 php7.1-dev php7.1-ldap php7.1-interbase php7.1-mbstring php7.1-bcmath php7.1-zip php7.1-soap php7.1-sybase php7.1-xml -y --allow-unauthenticated
RUN apt-get install --no-install-recommends python-pip=8.1.1-2ubuntu0.6 pkg-config=0.29.1-0ubuntu1 -y --allow-unauthenticated
RUN ln -s /usr/bin/nodejs /usr/bin/node \
 && curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
 && curl https://packages.microsoft.com/config/ubuntu/16.04/prod.list > /etc/apt/sources.list.d/mssql-release.list \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 locales=2.23-0ubuntu11.3 -y \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && apt-get update \
 && ACCEPT_EULA=Y apt-get install -y --no-install-recommends mssql-tools unixodbc-dev \
 && pecl install sqlsrv pdo_sqlsrv \
 && echo "extension=sqlsrv.so" > /etc/php/7.1/mods-available/sqlsrv.ini \
 && echo "extension=pdo_sqlsrv.so" > /etc/php/7.1/mods-available/pdo_sqlsrv.ini \
 && phpenmod sqlsrv pdo_sqlsrv \
 && pip install bunch==1.0.1 \
 && pecl install igbinary \
 && echo "extension=igbinary.so" > /etc/php/7.1/mods-available/igbinary.ini \
 && phpenmod igbinary \
 && pecl install mongodb \
 && echo "extension=mongodb.so" > /etc/php/7.1/mods-available/mongodb.ini \
 && phpenmod mongodb \
 && git clone https://github.com/dreamfactorysoftware/v8-compiled.git /v8 \
 && mkdir /opt/v8
WORKDIR /v8
RUN cp -R ubuntu_16.04/PHP7.1/* /opt/v8 \
 && git clone https://github.com/phpv8/v8js.git /v8js
WORKDIR /v8js
RUN git checkout 1.3.6 \
 && git pull origin 1.3.6 \
 && phpize \
 && ./configure --with-v8js=/opt/v8 \
 && make \
 && make install \
 && echo "extension=v8js.so" > /etc/php/7.1/mods-available/v8js.ini \
 && phpenmod v8js
WORKDIR /
RUN rm -Rf v8 \
 && rm -Rf v8js
#   install php cassandra extension
RUN mkdir /cassandra
WORKDIR /cassandra
RUN apt-get install --no-install-recommends libgmp-dev=2:6.1.0+dfsg-2 libpcre3-dev=2:8.38-3.1 g++=4:5.3.1-1ubuntu1 make=4.1-6 cmake=3.5.1-1ubuntu3 libssl-dev=1.0.2g-1ubuntu4.20 -y \
 && wget -q http://downloads.datastax.com/cpp-driver/ubuntu/16.04/dependencies/libuv/v1.11.0/libuv_1.11.0-1_amd64.deb \
 && wget -q http://downloads.datastax.com/cpp-driver/ubuntu/16.04/dependencies/libuv/v1.11.0/libuv-dev_1.11.0-1_amd64.deb \
 && wget -q http://downloads.datastax.com/cpp-driver/ubuntu/16.04/cassandra/v2.6.0/cassandra-cpp-driver_2.6.0-1_amd64.deb \
 && wget -q http://downloads.datastax.com/cpp-driver/ubuntu/16.04/cassandra/v2.6.0/cassandra-cpp-driver-dev_2.6.0-1_amd64.deb \
 && dpkg -i --force-overwrite libuv_1.11.0-1_amd64.deb \
 && dpkg -i libuv-dev_1.11.0-1_amd64.deb \
 && dpkg -i cassandra-cpp-driver_2.6.0-1_amd64.deb \
 && dpkg -i cassandra-cpp-driver-dev_2.6.0-1_amd64.deb \
 && git clone https://github.com/datastax/php-driver.git
WORKDIR /cassandra/php-driver
RUN git checkout tags/v1.2.2
WORKDIR /cassandra/php-driver/ext
RUN phpize \
 && ./configure \
 && make \
 && make install \
 && echo "extension=cassandra.so" > /etc/php/7.1/mods-available/cassandra.ini \
 && phpenmod cassandra
WORKDIR /
RUN rm -Rf cassandra
#   install php couchbase extension
RUN mkdir /couchbase
WORKDIR /couchbase
RUN wget http://packages.couchbase.com/releases/couchbase-release/couchbase-release-1.0-2-amd64.deb \
 && dpkg -i couchbase-release-1.0-2-amd64.deb \
 && apt-get update -y \
 && apt-get install --no-install-recommends libcouchbase-dev build-essential=12.1ubuntu2 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y --allow-unauthenticated \
 && pecl install pcs-1.3.3 \
 && pecl install couchbase \
 && echo "extension=pcs.so" > /etc/php/7.1/mods-available/pcs.ini \
 && echo "extension=couchbase.so" > /etc/php/7.1/mods-available/xcouchbase.ini \
 && phpenmod pcs \
 && phpenmod xcouchbase
WORKDIR /
RUN rm -Rf couchbase
#   configure sendmail
RUN echo 'sendmail_path = "/usr/sbin/ssmtp -t"' > /etc/php/7.1/cli/conf.d/mail.ini
#   install composer
RUN curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer \
 && chmod +x /usr/local/bin/composer
#   Configure Nginx/php-fpm
RUN rm /etc/nginx/sites-enabled/default
COPY dreamfactory.conf /etc/nginx/sites-available/dreamfactory.conf
RUN ln -s /etc/nginx/sites-available/dreamfactory.conf /etc/nginx/sites-enabled/dreamfactory.conf \
 && sed -i "s/pm.max_children = 5/pm.max_children = 5000/" /etc/php/7.1/fpm/pool.d/www.conf \
 && sed -i "s/pm.start_servers = 2/pm.start_servers = 150/" /etc/php/7.1/fpm/pool.d/www.conf \
 && sed -i "s/pm.min_spare_servers = 1/pm.min_spare_servers = 100/" /etc/php/7.1/fpm/pool.d/www.conf \
 && sed -i "s/pm.max_spare_servers = 3/pm.max_spare_servers = 200/" /etc/php/7.1/fpm/pool.d/www.conf \
 && sed -i "s/pm = dynamic/pm = ondemand/" /etc/php/7.1/fpm/pool.d/www.conf \
 && sed -i "s/worker_connections 768;/worker_connections 2048;/" /etc/nginx/nginx.conf \
 && sed -i "s/keepalive_timeout 65;/keepalive_timeout 10;/" /etc/nginx/nginx.conf
#   get app src
RUN git clone https://github.com/dreamfactorysoftware/dreamfactory.git /opt/dreamfactory
WORKDIR /opt/dreamfactory
#   install packages
RUN composer install --no-dev \
 && php artisan df:env --db_connection=sqlite --df_install=Docker \
 && chown -R www-data:www-data /opt/dreamfactory
COPY docker-entrypoint.sh /docker-entrypoint.sh
#   set proper permission to docker-entrypoint.sh script and forward error logs to docker log collector
RUN chmod +x /docker-entrypoint.sh \
 && ln -sf /dev/stderr /var/log/nginx/error.log \
 && rm -rf /var/lib/apt/lists/*
EXPOSE 80/tcp
CMD ["/docker-entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
