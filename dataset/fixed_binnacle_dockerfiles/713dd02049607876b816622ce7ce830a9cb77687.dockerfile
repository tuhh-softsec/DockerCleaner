FROM alpine:3.9
MAINTAINER tess@ten7.com
#   Create a flightdeck user with the home directory.
RUN addgroup -g 1000 flightdeck \
 && adduser -u 1000 -G flightdeck -D -h /var/www -s /bin/bash flightdeck \
 && addgroup flightdeck cron \
 && mkdir -p -m 774 /var/www \
 && chown flightdeck:flightdeck -R /var/www
#   Update all the Alpine packages.
RUN apk -U upgrade
#   Install Apache and PHP.
RUN apk add ansible=2.7.17-r0 py2-pip=18.1-r0 bash=4.4.19-r1 busybox-suid=1.29.3-r10 libcap=2.26-r0 zlib=1.2.11-r1 php7=7.2.33-r0 git=2.20.4-r0 patch=2.7.6-r6 curl=7.64.0-r5 php7-ctype=7.2.33-r0 php7-curl=7.2.33-r0 php7-dom=7.2.33-r0 php7-fileinfo=7.2.33-r0 php7-gd=7.2.33-r0 php7-iconv=7.2.33-r0 php7-json=7.2.33-r0 php7-ldap=7.2.33-r0 php7-mbstring=7.2.33-r0 php7-mcrypt php7-memcached php7-mysqlnd=7.2.33-r0 php7-opcache=7.2.33-r0 php7-openssl=7.2.33-r0 php7-phar=7.2.33-r0 php7-sqlite3=7.2.33-r0 php7-pdo=7.2.33-r0 php7-pdo_mysql=7.2.33-r0 php7-pdo_sqlite=7.2.33-r0 php7-phar=7.2.33-r0 php7-soap=7.2.33-r0 php7-session=7.2.33-r0 php7-simplexml=7.2.33-r0 php7-tokenizer=7.2.33-r0 php7-xdebug php7-exif=7.2.33-r0 php7-xml=7.2.33-r0 php7-xmlwriter=7.2.33-r0 php7-zip=7.2.33-r0 php7-zlib mariadb-client=10.3.25-r0 rsync=3.1.3-r1 supervisor=3.3.4-r1 openssh=7.9_p1-r6 openssh-client=7.9_p1-r6 nodejs-npm --update --no-cache \
 && rm -rf /tmp/* \
 && rm -rf /var/cache/apk/*
#   Install Python 3 dependencies from pip
RUN pip3 install passlib PyMySQL
#   Chown the flightdeck user directory and create needed cron directories
RUN mkdir -p -m 774 /etc/periodic /etc/crontabs /var/spool/cron /var/log/cron /var/www/.ssh /var/www/.drush /var/www/.drush/sites /var/www/.npm-global \
 && chown -R flightdeck:flightdeck /var/www /var/www/.ssh /var/www/.drush /var/www/.drush/sites /var/www/.npm-global
#   Forward cron logs to docker log collector
RUN ln -sf /dev/stdout /var/log/cron/cron.log
#   Install composer
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/bin --filename=composer
#   Install Drupal console
RUN curl https://drupalconsole.com/installer -L -o /usr/bin/drupal \
 && chmod 777 /usr/bin/drupal
#   Install passlib and aws cli from pip as apk versions are in testing.
RUN pip install awscli==1.27.114
#   Copy PHP configuration files.
COPY php.ini /etc/php7/
COPY 00_opcache.ini /etc/php7/conf.d/00_opcache.ini
COPY xdebug.ini /etc/php7/conf.d/xdebug.ini
#   Copy other configuration files.
COPY supervisord.conf /etc/supervisord.conf
COPY sshd_config /etc/ssh/sshd_config
COPY motd.txt /etc/motd
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
#   Copy the Ansible configuration files
COPY ansible-hosts /etc/ansible/hosts
COPY ansible.cfg /etc/ansible/ansible.cfg
EXPOSE 22/tcp
WORKDIR /var/www
#   Switch to the ssh user to do additional setup.
USER flightdeck
#   Copy the user bashrc file.
COPY --chown=flightdeck:flightdeck bashrc-user /var/www/.bashrc
COPY --chown=flightdeck:flightdeck bashrc-user /var/www/.bash_profile
#   Install drush for the ssh user.
RUN composer global require consolidation/cgr \
 && /var/www/.composer/vendor/bin/cgr drush/drush:~9.5.2 \
 && /var/www/.composer/vendor/bin/drush core:init -y
#   Set the dir for global node packages to be user-local.
RUN npm config set prefix '/var/www/.npm-global'
#   Install dart-sass globally.
RUN npm install sass@1.62.0 -g
#   Switch back to root for sshd.
USER root
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["/usr/bin/supervisord"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
