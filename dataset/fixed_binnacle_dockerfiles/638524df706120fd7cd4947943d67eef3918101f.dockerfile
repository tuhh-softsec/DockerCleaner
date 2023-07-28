FROM ubuntu:16.04
LABEL maintainer="Muhammad Surya Ihsanuddin<surya.kejawen@gmail.com>"
ENV DEBIAN_FRONTEND="noninteractive"
RUN sed -i 's/http:\/\/archive.ubuntu.com/http:\/\/buaya.klas.or.id/g' /etc/apt/sources.list
#   Install Software
RUN : \
 && apt-get upgrade -y
RUN (apt-get update ;apt-get install --no-install-recommends nginx-full=1.10.3-0ubuntu0.16.04.5 supervisor=3.2.0-2ubuntu0.2 vim=2:7.4.1689-3ubuntu1.5 varnish=4.1.1-1ubuntu0.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 build-essential=12.1ubuntu2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 ca-certificates=20210119~16.04.1 -y )
RUN touch /etc/apt/sources.list.d/ondrej-php.list
RUN echo "deb http://ppa.launchpad.net/ondrej/php/ubuntu xenial main" >> /etc/apt/sources.list.d/ondrej-php.list
RUN echo "deb-src http://ppa.launchpad.net/ondrej/php/ubuntu xenial main" >> /etc/apt/sources.list.d/ondrej-php.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 4F4EA0AAE5267A6C
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends php7.2 php7.2-cli php7.2-curl php7.2-intl php7.2-mbstring php7.2-xml php7.2-zip php7.2-bcmath php7.2-cli php7.2-fpm php7.2-imap php7.2-json php7.2-opcache php7.2-apcu php7.2-xmlrpc php7.2-bz2 php7.2-common php7.2-gd php7.2-ldap php7.2-pgsql php7.2-readline php7.2-soap php7.2-tidy php7.2-xsl php-apcu=5.1.3+4.0.10-1build1 -y )
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN apt-get remove --purge -y software-properties-common python-software-properties \
 && apt-get autoremove -y \
 && apt-get clean \
 && apt-get autoclean
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/doc/* ~/.composer
#   Setup Environment
ENV NGINX_WEBROOT="/semarthris/public"
ENV SYMFONY_ENV="dev"
ENV VARNISH_CONFIG="/etc/varnish/default.vcl"
ENV CACHE_SIZE="512m"
ENV VARNISHD_PARAMS="-p default_ttl=3600 -p default_grace=3600"
ENV VARNISH_PORT="80"
ENV BACKEND_HOST="localhost"
ENV BACKEND_PORT="8080"
#   Supervisor Configuration
COPY docker/supervisor/supervisor.conf /etc/supervisord.conf
#   Nginx Configuration
COPY docker/nginx/sites-enabled/site.conf /etc/nginx/conf.d/default.conf
COPY docker/nginx/sites-enabled/php-fpm.conf /etc/nginx/conf.d/php-fpm.conf
COPY docker/nginx/nginx.conf /etc/nginx/nginx.conf
COPY docker/nginx/fastcgi_cache /etc/nginx/fastcgi_cache
COPY docker/nginx/static_files.conf /etc/nginx/static_files.conf
COPY docker/nginx/logs/site.access.log /var/log/nginx/site.access.log
COPY docker/nginx/logs/site.error.log /var/log/nginx/site.error.log
COPY docker/nginx/etc/sysctl.conf /etc/sysctl.conf
COPY docker/nginx/etc/security/limits.conf /etc/security/limits.conf
RUN mkdir -p /tmp/nginx/cache
RUN chmod 777 -R /tmp/nginx
RUN chmod 777 /var/log/nginx/site.access.log
RUN chmod 777 /var/log/nginx/site.error.log
#   PHP Configuration
COPY docker/php/php.ini /etc/php/7.2/fpm/php.ini
COPY docker/php/php.ini /etc/php/7.2/cli/php.ini
COPY docker/php/php-fpm.conf /etc/php/7.2/fpm/php-fpm.conf
RUN mkdir /run/php
RUN touch /run/php/php7.2-fpm.sock
RUN chmod 777 /run/php/php7.2-fpm.sock
#   Varnish Configuration
COPY docker/varnish/default.vcl /etc/varnish/default.vcl
#   Setup Application
ENV COMPOSER_ALLOW_SUPERUSER="1"
RUN composer global require "hirak/prestissimo:~0.3" --prefer-dist --no-progress --no-suggest --optimize-autoloader --classmap-authoritative -vvv \
 && composer clear-cache
WORKDIR /semarthris
COPY composer.json ./
COPY composer.lock ./
RUN mkdir -p var/cache var/logs var/sessions \
 && chmod 777 -R var/ \
 && composer update --prefer-dist --no-autoloader --no-scripts --no-progress --no-suggest -vvv \
 && composer clear-cache
COPY bin bin/
COPY config config/
COPY data data/
COPY public public/
COPY src src/
COPY templates templates/
COPY .env.dist ./.env.dist
RUN composer dump-autoload --optimize --classmap-authoritative
#   Here we go
COPY docker/start.sh /start.sh
RUN chmod +x /start.sh
EXPOSE 443/tcp 80/tcp
CMD ["/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
