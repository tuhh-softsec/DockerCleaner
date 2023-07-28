#
#   cusspvz/magento.docker
#   Magento 2.0 over alpine, nginx, php-fpm and mariadb
#
FROM alpine:latest
MAINTAINER José Moreira <jose.moreira@findhit.com>
ENV DOMAIN="docker.local"
ENV REDIRECT_TO_WWW_DOMAIN="0"
#
#   Install packages
#
RUN echo "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories \
 && apk add curl=7.88.1-r1 openssl=3.0.8-r3 git=2.38.4-r1 supervisor=4.2.4-r0 ca-certificates=20220614-r4 nginx=1.22.1-r0 composer@testing php-fpm php-phar php-openssl php-curl php-json php-xml php-zlib php-gd php-ctype php-dom php-mcrypt php-iconv php-intl php-xsl php-zip mysql=10.6.12-r0 mysql-client=10.6.12-r0 php-pdo_mysql --update \
 && rm -fR /var/cache/apk/*
#
#   MySQL Server
#
ENV MYSQL_HOST="localhost"
ENV MYSQL_PORT="3306"
ENV MYSQL_DATABASE="magento"
ENV MYSQL_USERNAME="root"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN /usr/bin/mysql_install_db \
 && chown -R mysql:mysql /var/lib/mysql \
 && /usr/bin/mysqld_safe &; MYSQL_PID=$! ; sleep 20 \
 && /usr/bin/mysqladmin -u root password $MYSQL_PASSWORD \
 && kill $MYSQL_PID
VOLUME /var/lib/mysql
EXPOSE 3306/tcp
#
#   Magento configs
#
ARG MAGENTO_VERSION=2.0.4
ARG MAGENTO_PUBLIC_KEY=ffc9f467961151f41827c02bc7a9b669
ARG MAGENTO_PRIVATE_KEY=4f64aaa7771b441ef5d72e941c350542
ENV MAGENTO_ROOT="/magento" \
    MAGENTO_USER="magento" \
    MAGENTO_USER_ID="1000" \
    MAGENTO_GROUP="magento" \
    MAGENTO_GROUP_ID="1000"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV MAGENTO_ADMIN_URI="admin" \
    MAGENTO_ADMIN_EMAIL="admin@example.org" \
    MAGENTO_ADMIN_FIRSTNAME="John" \
    MAGENTO_ADMIN_LASTNAME="Doe" \
    MAGENTO_ADMIN_USERNAME="admin" \
    MAGENTO_LANGUAGE="en_US" \
    MAGENTO_TIMEZONE="Europe/Lisbon" \
    MAGENTO_CURRENCY="EUR" \
    MAGENTO_ADMIN_USERNAME="admin" \
    MAGENTO_ADMIN_USERNAME="admin"
RUN mkdir -p ~/.composer \
 && echo "{ \"http-basic\": { \"repo.magento.com\": { \"username\": \"$MAGENTO_PUBLIC_KEY\", \"password\": \"$MAGENTO_PRIVATE_KEY\" } } }" > ~/.composer/auth.json \
 && git clone https://github.com/magento/magento2.git $MAGENTO_ROOT \
 && cd $MAGENTO_ROOT \
 && composer install
VOLUME /magento/app/etc /magento/pub
#
#   Nginx Server
#
ENV NGINX_PORT="80"
RUN mkdir /tmp/nginx
EXPOSE 80/tcp
#
#   PHP Configuration
#
ENV PHP_INI="/etc/php/php.ini"
RUN sed 's,;always_populate_raw_post_data,always_populate_raw_post_data,g' -i $PHP_INI \
 && sed 's,memory_limit = 128M,memory_limit = 256M,g' -i $PHP_INI
#   Must be kept until it is fixed
#   https://github.com/zendframework/zend-stdlib/issues/58
RUN sed "s,=> GLOB_BRACE,=> defined('GLOB_BRACE') ? GLOB_BRACE : 0,g" -i /magento/vendor/zendframework/zend-stdlib/src/Glob.php
COPY start.sh /scripts/start.sh
COPY setup.sh /scripts/setup.sh
RUN chmod +x /scripts/start.sh /scripts/setup.sh
#
#   Scalability
#
ENV NODES="\"
CMD ["/scripts/start.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
