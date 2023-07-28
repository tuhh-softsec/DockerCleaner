FROM ubuntu:16.04
ENV php_conf="/etc/php/7.0/fpm/php.ini"
ENV fpm_conf="/etc/php/7.0/fpm/php-fpm.conf"
ENV composer_hash="544e09ee996cdf60ece3804abc52599c22b1f40f4323403c44d44fdfdd586475ca9813a858088ffbc1f233e9b180f061"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 -y )
RUN apt-add-repository ppa:phalcon/stable \
 && :
RUN (apt-get update ;apt-get install --no-install-recommends bash=4.3-14ubuntu1.4 openssh-client=1:7.2p2-4ubuntu2.10 wget=1.17.1-1ubuntu1.5 nginx=1.10.3-0ubuntu0.16.04.5 supervisor=3.2.0-2ubuntu0.2 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 php7.0-dev=7.0.33-0ubuntu0.16.04.16 php7.0-fpm=7.0.33-0ubuntu0.16.04.16 php7.0-mcrypt=7.0.33-0ubuntu0.16.04.16 php7.0-ctype php7.0-gd=7.0.33-0ubuntu0.16.04.16 php7.0-intl=7.0.33-0ubuntu0.16.04.16 php7.0-xml=7.0.33-0ubuntu0.16.04.16 php7.0-xsl=7.0.33-0ubuntu0.16.04.16 php7.0-curl=7.0.33-0ubuntu0.16.04.16 php7.0-iconv php7.0-json=7.0.33-0ubuntu0.16.04.16 php7.0-phar php7.0-dom php7.0-phalcon php7.0-zip=7.0.33-0ubuntu0.16.04.16 php-xdebug=2.4.0-1 php7.0-mbstring=7.0.33-0ubuntu0.16.04.16 ca-certificates=20210119~16.04.1 dialog=1.3-20160209-1 gcc=4:5.3.1-1ubuntu1 musl-dev=1.1.9-1 libffi-dev=3.2.1-4 -y ) \
 && mkdir -p /etc/nginx \
 && mkdir -p /var/www/app \
 && mkdir -p /run/nginx \
 && mkdir -p /var/log/supervisor
RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');" \
 && php -r "if (hash_file('SHA384', 'composer-setup.php') === '${composer_hash}') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;" \
 && php composer-setup.php --install-dir=/usr/bin --filename=composer \
 && php -r "unlink('composer-setup.php');"
#   nginx site conf
RUN mkdir -p /etc/nginx/sites-available/ \
 && mkdir -p /etc/nginx/sites-enabled/ \
 && mkdir -p /etc/nginx/ssl/ \
 && rm -Rf /var/www/*
RUN ln -s /etc/nginx/sites-available/default.conf /etc/nginx/sites-enabled/default.conf
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends php-pear=1:1.10.1+submodules+notgz-6ubuntu0.3 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 pkg-config=0.29.1-0ubuntu1 libssl-dev=1.0.2g-1ubuntu4.20 libsslcommon2-dev=0.16-9ubuntu2 -y ) \
 && pecl install mongodb \
 && echo "extension=mongodb.so" > /etc/php/7.0/fpm/conf.d/30-mongo.ini \
 && echo "extension=mongodb.so" > /etc/php/7.0/cli/conf.d/30-mongo.ini
RUN (apt-get update ;apt-get install --no-install-recommends memcached=1.4.25-2ubuntu1.5 php-memcached=2.2.0-51-ge573a6e+2.2.0-2build2 -y )
#   tweak php-fpm config
RUN mkdir -p /run/php \
 && sed -i -e "s/;cgi.fix_pathinfo=1/cgi.fix_pathinfo=0/g" ${php_conf} \
 && sed -i -e "s/upload_max_filesize\s*=\s*2M/upload_max_filesize = 100M/g" ${php_conf} \
 && sed -i -e "s/post_max_size\s*=\s*8M/post_max_size = 100M/g" ${php_conf} \
 && sed -i -e "s/variables_order = \"GPCS\"/variables_order = \"EGPCS\"/g" ${php_conf} \
 && sed -i -e "s/;daemonize\s*=\s*yes/daemonize = no/g" ${fpm_conf} \
 && sed -i -e "s/;catch_workers_output\s*=\s*yes/catch_workers_output = yes/g" ${fpm_conf} \
 && sed -i -e "s/pm.max_children = 4/pm.max_children = 4/g" ${fpm_conf} \
 && sed -i -e "s/pm.start_servers = 2/pm.start_servers = 3/g" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/pm.min_spare_servers = 1/pm.min_spare_servers = 2/g" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/pm.max_spare_servers = 3/pm.max_spare_servers = 4/g" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/pm.max_requests = 500/pm.max_requests = 200/g" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/user = nobody/user = nginx/g" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/group = nobody/group = nginx/g" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/;listen.mode = 0660/listen.mode = 0666/g" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/;listen.owner = nobody/listen.owner = nginx/g" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/;listen.group = nobody/listen.group = nginx/g" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/;php_admin_value[memory_limit] = 32M/php_admin_value[memory_limit] = 512M/g" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/^;clear_env = no$/clear_env = no/" /etc/php/7.0/fpm/pool.d/www.conf \
 && sed -i -e "s/memory_limit = 128M/memory_limit = 512M/" /etc/php/7.0/fpm/php.ini \
 && ln -s /etc/php/7.0/fpm/php.ini /etc/php/7.0/fpm/conf.d/php.ini \
 && find /etc/php/7.0/fpm/conf.d/ -name "*.ini" -exec sed -i -re 's/^(\s*)#(.*)/\1;\2/g' {}
#   Add Scripts
COPY scripts/start.sh /start.sh
RUN chmod 755 /start.sh
#   Copy our nginx config
RUN rm -Rf /etc/nginx/nginx.conf
COPY conf/nginx.conf /etc/nginx/nginx.conf
COPY conf/vhost.conf /etc/nginx/sites-enabled/default
#   Services
COPY conf/supervisord.conf /etc/supervisord.conf
EXPOSE 443/tcp 80/tcp
CMD ["/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
