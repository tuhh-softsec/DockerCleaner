# ###############################################################################
#  Dockerfile for appserver.io main Docker distribution
# ###############################################################################
#  base image
FROM debian:jessie
#  author
MAINTAINER Tim Wagner <tw@appserver.io>
# ###############################################################################
#  define versions
ENV APPSERVER_RUNTIME_BUILD_VERSION="1.1.9-46"
#  update the sources list
RUN apt-get update \
 && DEBIAN_FRONTEND=noninteractive apt-get install supervisor wget git vim -y python-pip \
 && pip install supervisor-stdout
# ###############################################################################
#  download runtime in specific version
RUN wget -O /tmp/appserver-runtime.deb http://builds.appserver.io/linux/debian/8/appserver-runtime_${APPSERVER_RUNTIME_BUILD_VERSION}~deb8_amd64.deb \
 && dpkg -i /tmp/appserver-runtime.deb ; exit 0
#  install missing runtime dependencies
RUN apt-get install -yf \
 && rm -f /tmp/appserver-runtime.deb \
 && ln -s /opt/appserver/bin/php /usr/local/bin/php
# ###############################################################################
#  copy the appserver sources
ADD . /opt/appserver
# ###############################################################################
#  define working directory
WORKDIR /opt/appserver
# ###############################################################################
#  create a symlink for the appserver.io Composer binary
RUN ln -s /opt/appserver/bin/composer.phar /usr/local/bin/composer \
 && composer install --prefer-dist --no-dev --no-interaction --optimize-autoloader \
 && sed -i "s/www-data/root/g" etc/appserver/appserver.xml \
 && sed -i "s/user = www-data/user = root/g" etc/php-fpm.conf \
 && sed -i "s/group = www-data/group = root/g" etc/php-fpm.conf \
 && sed -i "s/var\/log\/appserver-errors.log/php:\/\/stderr/g" etc/appserver/appserver.xml \
 && sed -i "s/var\/log\/appserver-access.log/php:\/\/stdout/g" etc/appserver/appserver.xml \
 && sed -i "s/9080/80/g" etc/appserver/appserver.xml \
 && sed -i "s/9443/443/g" etc/appserver/appserver.xml \
 && sed -i "s/DeploymentScanner/SupervisorDeploymentScanner/g" etc/appserver/appserver.xml \
 && sed -i "s/;error_log = log\/php-fpm.log/error_log = \/proc\/self\/fd\/2/g" etc/php-fpm.conf \
 && sed -i "s/\/opt\/appserver\/var\/log\/php_errors.log/\/proc\/self\/fd\/2/g" etc/php.ini \
 && sed -i "s/\/opt\/appserver\/var\/log\/php-fpm-fcgi_errors.log/\/proc\/self\/fd\/2/g" etc/php-fpm-fcgi.ini \
 && sed -i "s/;always_populate_raw_post_data = On/always_populate_raw_post_data = -1/g" etc/php-fpm-fcgi.ini \
 && ln -s /opt/appserver/etc/supervisor/conf.d/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
# ###############################################################################
#  expose ports
EXPOSE 80/tcp 443/tcp
#  supervisord needs this
CMD []
#  define default command
ENTRYPOINT ["/usr/bin/supervisord"]
