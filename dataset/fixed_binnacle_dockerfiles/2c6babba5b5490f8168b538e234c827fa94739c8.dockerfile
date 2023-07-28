FROM ubuntu:14.04
MAINTAINER Luis Elizondo "lelizondo@gmail.com"
ENV DEBIAN_FRONTEND="noninteractive"
#   Ensure UTF-8
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV SMTP_HOST="smtp.gmail.com"
ENV SMTP_PORT="587"
ENV SMTP_FROMNAME="My Name"
ENV SMTP_USERNAME="username@example.com"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
#   Update system
RUN : \
 && apt-get dist-upgrade -y
#   Prevent restarts when installing
RUN echo '#!/bin/sh\nexit 101' > /usr/sbin/policy-rc.d \
 && chmod +x /usr/sbin/policy-rc.d
#   Basic packages
RUN (apt-get update ;apt-get install --no-install-recommends php5-fpm=5.5.9+dfsg-1ubuntu4.29 php5-mysql=5.5.9+dfsg-1ubuntu4.29 php-apc=4.0.2-2build1 php5-imagick=3.1.2-1build1 php5-imap=5.4.6-0ubuntu5.1 php5-mcrypt=5.4.6-0ubuntu5 php5-curl=5.5.9+dfsg-1ubuntu4.29 php5-cli=5.5.9+dfsg-1ubuntu4.29 php5-gd=5.5.9+dfsg-1ubuntu4.29 php5-pgsql=5.5.9+dfsg-1ubuntu4.29 php5-sqlite=5.5.9+dfsg-1ubuntu4.29 php5-common=5.5.9+dfsg-1ubuntu4.29 php-pear=5.5.9+dfsg-1ubuntu4.29 curl=7.35.0-1ubuntu2.20 php5-json=1.3.2-2build1 php5-redis=2.2.4-1build2 php5-memcache=3.0.8-4build1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends nginx-extras=1.4.6-1ubuntu3.9 git=1:1.9.1-1ubuntu0.10 curl=7.35.0-1ubuntu2.20 supervisor=3.0b2-1ubuntu0.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends msmtp=1.4.31-1 msmtp-mta=1.4.31-1 -y )
RUN php5enmod mcrypt
RUN /usr/bin/curl -sS https://getcomposer.org/installer | /usr/bin/php
RUN /bin/mv composer.phar /usr/local/bin/composer
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Install Composer and Drush
RUN /usr/local/bin/composer self-update
RUN /usr/local/bin/composer global require drush/drush:6.*
RUN ln -s /root/.composer/vendor/drush/drush/drush /usr/local/bin/drush
#   Prepare directory
RUN mkdir /var/www
RUN usermod -u 1000 www-data
RUN usermod -a -G users www-data
RUN chown -R www-data:www-data /var/www
EXPOSE 80/tcp
WORKDIR /var/www
VOLUME ["/var/www/sites/default/files"]
CMD ["/usr/bin/supervisord", "-n"]
#   Startup script
#   This startup script wll configure nginx
COPY ./startup.sh /opt/startup.sh
RUN chmod +x /opt/startup.sh
COPY ./mail.sh /opt/mail.sh
RUN chmod +x /opt/mail.sh
COPY ./cron.sh /opt/cron.sh
RUN chmod +x /opt/cron.sh
#   We want it empty
RUN touch /etc/msmtprc
RUN chgrp mail /etc/msmtprc
RUN chmod 660 /etc/msmtprc
RUN touch /var/log/supervisor/msmtp.log
RUN chgrp mail /var/log/supervisor/msmtp.log
RUN chmod 660 /var/log/supervisor/msmtp.log
RUN adduser www-data mail
RUN rm /usr/sbin/sendmail
RUN rm /usr/lib/sendmail
RUN ln -s /usr/bin/msmtp /usr/sbin/sendmail
RUN ln -s /usr/bin/msmtp /usr/bin/sendmail
RUN ln -s /usr/bin/msmtp /usr/lib/sendmail
RUN mkdir -p /var/cache/nginx/microcache
#  ## Add configuration files
#   Supervisor
COPY ./config/supervisor/supervisord-nginx.conf /etc/supervisor/conf.d/supervisord-nginx.conf
#   PHP
COPY ./config/php/www.conf /etc/php5/fpm/pool.d/www.conf
COPY ./config/php/php.ini /etc/php5/fpm/php.ini
#   Nginx
COPY ./config/nginx/blacklist.conf /etc/nginx/blacklist.conf
COPY ./config/nginx/drupal.conf /etc/nginx/drupal.conf
COPY ./config/nginx/drupal_upload_progress.conf /etc/nginx/drupal_upload_progress.conf
COPY ./config/nginx/fastcgi.conf /etc/nginx/fastcgi.conf
COPY ./config/nginx/fastcgi_drupal.conf /etc/nginx/fastcgi_drupal.conf
COPY ./config/nginx/fastcgi_microcache_zone.conf /etc/nginx/fastcgi_microcache_zone.conf
COPY ./config/nginx/fastcgi_no_args_drupal.conf /etc/nginx/fastcgi_no_args_drupal.conf
COPY ./config/nginx/map_cache.conf /etc/nginx/map_cache.conf
COPY ./config/nginx/microcache_fcgi.conf /etc/nginx/microcache_fcgi.conf
COPY ./config/nginx/microcache_fcgi_auth.conf /etc/nginx/microcache_fcgi_auth.conf
COPY ./config/nginx/mime.types /etc/nginx/mime.types
COPY ./config/nginx/nginx.conf /etc/nginx/nginx.conf
COPY ./config/nginx/upstream_phpcgi_unix.conf /etc/nginx/upstream_phpcgi_unix.conf
COPY ./config/nginx/map_block_http_methods.conf /etc/nginx/map_block_http_methods.conf
COPY ./config/nginx/map_https_fcgi.conf /etc/nginx/map_https_fcgi.conf
COPY ./config/nginx/nginx_status_allowed_hosts.conf /etc/nginx/nginx_status_allowed_hosts.conf
COPY ./config/nginx/cron_allowed_hosts.conf /etc/nginx/cron_allowed_hosts.conf
COPY ./config/nginx/php_fpm_status_allowed_hosts.conf /etc/nginx/php_fpm_status_allowed_hosts.conf
COPY ./config/nginx/default /etc/nginx/sites-enabled/default
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
