FROM alpine:3.9
MAINTAINER tess@ten7.com
#   Create a apache user with the home directory.
RUN adduser -g apache -D -h /var/www apache \
 && mkdir -p -m 774 /var/www \
 && chown apache:apache -R /var/www
#   Update and install.
RUN apk -U upgrade \
 && apk add ansible=2.7.17-r0 bash=4.4.19-r1 libcap=2.26-r0 zlib=1.2.11-r1 apache2=2.4.46-r0 apache2-ssl=2.4.46-r0 php7-apache2=7.2.33-r0 php7=7.2.33-r0 git=2.20.4-r0 patch=2.7.6-r6 curl=7.64.0-r5 php7-ctype=7.2.33-r0 php7-curl=7.2.33-r0 php7-dom=7.2.33-r0 php7-fileinfo=7.2.33-r0 php7-gd=7.2.33-r0 php7-iconv=7.2.33-r0 php7-json=7.2.33-r0 php7-ldap=7.2.33-r0 php7-mbstring=7.2.33-r0 php7-mcrypt php7-memcached php7-mysqlnd=7.2.33-r0 php7-opcache=7.2.33-r0 php7-openssl=7.2.33-r0 php7-phar=7.2.33-r0 php7-sqlite3=7.2.33-r0 php7-pdo=7.2.33-r0 php7-pdo_mysql=7.2.33-r0 php7-pdo_sqlite=7.2.33-r0 php7-phar=7.2.33-r0 php7-soap=7.2.33-r0 php7-session=7.2.33-r0 php7-simplexml=7.2.33-r0 php7-tokenizer=7.2.33-r0 php7-xdebug php7-exif=7.2.33-r0 php7-xml=7.2.33-r0 php7-xmlreader=7.2.33-r0 php7-xmlwriter=7.2.33-r0 php7-zip=7.2.33-r0 php7-zlib mariadb-client=10.3.25-r0 rsync=3.1.3-r1 openssh-client=7.9_p1-r6 nodejs-npm --update --no-cache \
 && rm -rf /tmp/* /var/cache/apk/* /usr/lib/python3.6/site-packages/ansible/modules/network/ /usr/lib/python3.6/site-packages/ansible/modules/cloud/ /usr/lib/python3.6/site-packages/ansible/modules/windows/
#   Install Python 3 dependencies from pip
RUN pip3 install passlib PyMySQL
#   Allow Apache to allocate ports as non-root.
RUN setcap cap_net_bind_service=+ep /usr/sbin/httpd
#   Chown the /etc/mysql directory so we can change my.cnf settings.
RUN mkdir -p -m 774 /run/apache2 /etc/apache2/sites.d /var/www/.drush /var/www/.drush/sites /var/www/.npm-global \
 && chown -R apache:apache /etc/apache2 /etc/apache2/sites.d /etc/php7 /run/apache2 /var/log/apache2 /var/www /var/www/.drush /var/www/.drush/sites /var/www/.npm-global
#   forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/apache2/access.log \
 && ln -sf /dev/stderr /var/log/apache2/error.log \
 && ln -sf /dev/stdout /var/log/apache2/000_default-access_log \
 && ln -sf /dev/stderr /var/log/apache2/000_default-error_log \
 && ln -sf /dev/stdout /var/log/apache2/000_default_ssl-access_log \
 && ln -sf /dev/stderr /var/log/apache2/000_default_ssl-error_log
#   Install composer
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/bin --filename=composer
#   Install Drupal console
RUN curl https://drupalconsole.com/installer -L -o /usr/bin/drupal \
 && chmod 777 /usr/bin/drupal
#   Copy the Apache configuration files.
COPY httpd.conf /etc/apache2/httpd.conf
COPY 000_default.conf /etc/apache2/sites.d/
COPY ssl.conf /etc/apache2/conf.d/ssl.conf
#   Change the ownership of the Apache SSL files.
RUN chown -R apache:www-data /etc/ssl/apache2
#   Copy PHP configuration files.
COPY php.ini /etc/php7/
COPY 00_opcache.ini /etc/php7/conf.d/00_opcache.ini
COPY xdebug.ini /etc/php7/conf.d/xdebug.ini
#   Copy various key scripts to a directory in $PATH.
COPY docker-entrypoint.sh /usr/local/bin/
COPY vim.sh /usr/local/bin/vim
#   Make them all executable.
RUN chmod a+x /usr/local/bin/docker-entrypoint.sh \
 && chmod a+x /usr/local/bin/vim
#   Copy the entire contents of the init.d directory.
COPY docker-entrypoint-init.d /docker-entrypoint-init.d
RUN chmod a+x -R /docker-entrypoint-init.d
#   Copy the root bashrc file.
COPY bashrc-root /root/.bashrc
#   Copy the Ansible configuration files.
COPY ansible-hosts /etc/ansible/hosts
COPY ansible.cfg /etc/ansible/ansible.cfg
#   Install drush for the apache user.
USER apache
RUN composer global require consolidation/cgr \
 && /var/www/.composer/vendor/bin/cgr drush/drush:~9.5.2 \
 && /var/www/.composer/vendor/bin/drush core:init -y \
 && rm -rf /var/www/.composer/cache
#   Set the dir for global node packages to be user-local.
RUN npm config set prefix '/var/www/.npm-global'
#   Install dart-sass globally.
RUN npm install sass@1.62.0 -g \
 && rm -rf /var/www/.npm/_cacache /tmp/*
#   Copy the user bashrc file.
COPY bashrc-user /var/www/.bashrc
EXPOSE 80/tcp 443/tcp
WORKDIR /var/www
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["/usr/sbin/httpd", "-D", "FOREGROUND", "-f", "/etc/apache2/httpd.conf"]
# Please add your HEALTHCHECK here!!!
