FROM debian:stretch
MAINTAINER Colin Wilson "colin@wyveo.com"
#   Let the container know that there is no tty
ENV DEBIAN_FRONTEND="noninteractive"
ENV NGINX_VERSION="1.15.12-1~stretch"
ENV php_conf="/etc/php/7.3/fpm/php.ini"
ENV fpm_conf="/etc/php/7.3/fpm/pool.d/www.conf"
ENV COMPOSER_VERSION="1.8.5"
#   Install Basic Requirements
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg2=2.1.18-8~deb9u4 dirmngr=2.1.18-8~deb9u4 wget=1.18-5+deb9u3 apt-transport-https=1.4.11 lsb-release=9.20161125 ca-certificates=20200601~deb9u2 --no-install-suggests -q -y \
 && NGINX_GPGKEY=573BFD6B3D8FBC641079A6ABABF5BD827BD9BF62 ; found='' ; for server in ha.pool.sks-keyservers.net hkp://keyserver.ubuntu.com:80 hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu; do echo "Fetching GPG key $NGINX_GPGKEY from $server" ;apt-key adv --batch --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$NGINX_GPGKEY" \
 && found=yes \
 && break ; done ; test -z "$found" \
 && echo "error: failed to fetch GPG key $NGINX_GPGKEY" >&2 \
 && exit 1 ; echo "deb http://nginx.org/packages/mainline/debian/ stretch nginx" >> /etc/apt/sources.list \
 && wget -O /etc/apt/trusted.gpg.d/php.gpg https://packages.sury.org/php/apt.gpg \
 && echo "deb https://packages.sury.org/php/ $( lsb_release -sc ;) main" > /etc/apt/sources.list.d/php.list \
 && apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.4.11 curl=7.52.1-5+deb9u16 nano=2.7.4-1 zip=3.0-11+b1 unzip=6.0-21+deb9u2 python-pip=9.0.1-2+deb9u2 python-setuptools=33.1.1-1 git=1:2.11.0-3+deb9u7 php7.3-fpm php7.3-cli php7.3-bcmath php7.3-dev php7.3-common php7.3-json php7.3-opcache php7.3-readline php7.3-mbstring php7.3-curl php7.3-imagick php7.3-gd php7.3-mysql php7.3-zip php7.3-pgsql php7.3-intl php7.3-xml php7.3-redis nginx=${NGINX_VERSION} --no-install-suggests -q -y \
 && mkdir -p /run/php \
 && pip install wheel==0.40.0 \
 && pip install supervisor==4.2.5 supervisor-stdout==0.1.1 \
 && echo "#!/bin/sh\nexit 0" > /usr/sbin/policy-rc.d \
 && rm -rf /etc/nginx/conf.d/default.conf \
 && sed -i -e "s/;cgi.fix_pathinfo=1/cgi.fix_pathinfo=0/g" ${php_conf} \
 && sed -i -e "s/memory_limit\s*=\s*.*/memory_limit = 256M/g" ${php_conf} \
 && sed -i -e "s/upload_max_filesize\s*=\s*2M/upload_max_filesize = 100M/g" ${php_conf} \
 && sed -i -e "s/post_max_size\s*=\s*8M/post_max_size = 100M/g" ${php_conf} \
 && sed -i -e "s/variables_order = \"GPCS\"/variables_order = \"EGPCS\"/g" ${php_conf} \
 && sed -i -e "s/;daemonize\s*=\s*yes/daemonize = no/g" /etc/php/7.3/fpm/php-fpm.conf \
 && sed -i -e "s/;catch_workers_output\s*=\s*yes/catch_workers_output = yes/g" ${fpm_conf} \
 && sed -i -e "s/pm.max_children = 5/pm.max_children = 4/g" ${fpm_conf} \
 && sed -i -e "s/pm.start_servers = 2/pm.start_servers = 3/g" ${fpm_conf} \
 && sed -i -e "s/pm.min_spare_servers = 1/pm.min_spare_servers = 2/g" ${fpm_conf} \
 && sed -i -e "s/pm.max_spare_servers = 3/pm.max_spare_servers = 4/g" ${fpm_conf} \
 && sed -i -e "s/pm.max_requests = 500/pm.max_requests = 200/g" ${fpm_conf} \
 && sed -i -e "s/www-data/nginx/g" ${fpm_conf} \
 && sed -i -e "s/^;clear_env = no$/clear_env = no/" ${fpm_conf} \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -o /tmp/composer-setup.php https://getcomposer.org/installer \
 && curl -o /tmp/composer-setup.sig https://composer.github.io/installer.sig \
 && php -r "if (hash('SHA384', file_get_contents('/tmp/composer-setup.php')) !== trim(file_get_contents('/tmp/composer-setup.sig'))) { unlink('/tmp/composer-setup.php'); echo 'Invalid installer' . PHP_EOL; exit(1); }" \
 && php /tmp/composer-setup.php --no-ansi --install-dir=/usr/local/bin --filename=composer --version=${COMPOSER_VERSION} \
 && rm -rf /tmp/composer-setup.php
#   Supervisor config
COPY ./supervisord.conf /etc/supervisord.conf
#   Override nginx's default config
COPY ./default.conf /etc/nginx/conf.d/default.conf
#   Override default nginx welcome page
COPY html /usr/share/nginx/html
#   Add Scripts
COPY ./start.sh /start.sh
EXPOSE 80/tcp
CMD ["/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
