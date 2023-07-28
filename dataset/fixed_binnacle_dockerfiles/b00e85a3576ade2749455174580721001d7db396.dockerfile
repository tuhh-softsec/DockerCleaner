FROM php:7.2-fpm-alpine3.7
MAINTAINER Dmitri Pisarev <dimaip@gmail.com>
ARG PHP_REDIS_VERSION="3.1.6"
ARG PHP_YAML_VERSION="2.0.2"
ARG PHP_XDEBUG_VERSION="2.6.0beta1"
ARG S6_VERSION="1.21.2.2"
ENV FLOW_REWRITEURLS="1"
ENV COMPOSER_VERSION="1.6.2"
ENV COMPOSER_HOME="/composer"
ENV PATH="/composer/vendor/bin:$PATH"
ENV COMPOSER_ALLOW_SUPERUSER="1"
#   Set default values for env vars used in init scripts, override them if needed
ENV DB_DATABASE="db"
ENV DB_HOST="db"
ENV DB_USER="admin"
ENV DB_PASS="pass"
ENV VERSION="master"
#   Basic build-time metadata as defined at http://label-schema.org
LABEL org.label-schema.docker.dockerfile="/Dockerfile" \
      org.label-schema.license="MIT" \
      org.label-schema.name="Neos Alpine Docker Image" \
      org.label-schema.url="https://github.com/psmb/docker-neos-alpine" \
      org.label-schema.vcs-url="https://github.com/psmb/docker-neos-alpine" \
      org.label-schema.vcs-type="Git"
RUN set -x \
 && apk update \
 && apk add tar=1.32-r0 rsync=3.1.3-r0 curl=7.61.1-r3 sed=4.4-r1 bash=4.4.19-r1 yaml=0.1.7-r0 python py-pip py-setuptools=33.1.1-r1 groff=1.22.3-r2 less=520-r0 mysql-client=10.1.41-r0 git=2.15.4-r0 nginx=1.12.2-r4 optipng=0.7.6-r1 freetype=2.8.1-r4 libjpeg-turbo-utils=1.5.3-r3 icu-dev=59.1-r1 openssh=7.5_p1-r10 pwgen=2.08-r0 sudo=1.8.21_p2-r1 s6=2.6.1.1-r0 \
 && pip install awscli==1.27.114 \
 && apk del py-pip py-setuptools \
 && apk add libtool=2.4.6-r4 freetype-dev=2.8.1-r4 libpng-dev=1.6.37-r0 libjpeg-turbo-dev=1.5.3-r3 yaml-dev=0.1.7-r0 $PHPIZE_DEPS --virtual .phpize-deps \
 && docker-php-ext-configure gd --with-gd --with-freetype-dir=/usr/include/ --with-png-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-install gd pdo pdo_mysql mbstring opcache intl exif json tokenizer \
 && pecl install redis-${PHP_REDIS_VERSION} yaml-${PHP_YAML_VERSION} xdebug-${PHP_XDEBUG_VERSION} \
 && docker-php-ext-enable xdebug \
 && docker-php-ext-enable redis \
 && docker-php-ext-enable yaml \
 && apk del .phpize-deps \
 && curl -o /tmp/composer-setup.php https://getcomposer.org/installer \
 && php /tmp/composer-setup.php --no-ansi --install-dir=/usr/local/bin --filename=composer --version=${COMPOSER_VERSION} \
 && rm -rf /tmp/composer-setup.php \
 && curl -s http://beard.famelo.com/ > /usr/local/bin/beard \
 && chmod +x /usr/local/bin/beard \
 && git config --global user.email "server@server.com" \
 && git config --global user.name "Server" \
 && rm -rf /var/cache/apk/*
#   Copy configuration
COPY root /
#   Download s6
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/ https://github.com/just-containers/s6-overlay/releases/download/v${S6_VERSION}/s6-overlay-amd64.tar.gz
RUN tar xzf /tmp/s6-overlay-amd64.tar.gz -C / \
 && rm /tmp/s6-overlay-amd64.tar.gz \
 && echo "xdebug.remote_enable=0" >> $PHP_INI_DIR/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.remote_connect_back=1" >> $PHP_INI_DIR/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.max_nesting_level=512" >> $PHP_INI_DIR/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.idekey=\"PHPSTORM\"" >> $PHP_INI_DIR/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.remote_host=172.17.0.1" >> $PHP_INI_DIR/conf.d/docker-php-ext-xdebug.ini \
 && deluser www-data \
 && delgroup cdrw \
 && addgroup -g 80 www-data \
 && adduser -u 80 -G www-data -s /bin/bash -D www-data -h /data \
 && rm -Rf /home/www-data \
 && sed -i -e "s#listen = 9000#listen = /var/run/php-fpm.sock#" /usr/local/etc/php-fpm.d/zz-docker.conf \
 && echo "clear_env = no" >> /usr/local/etc/php-fpm.d/zz-docker.conf \
 && echo "listen.owner = www-data" >> /usr/local/etc/php-fpm.d/zz-docker.conf \
 && echo "listen.group = www-data" >> /usr/local/etc/php-fpm.d/zz-docker.conf \
 && echo "listen.mode = 0660" >> /usr/local/etc/php-fpm.d/zz-docker.conf \
 && chown 80:80 -R /var/lib/nginx \
 && chmod +x /github-keys.sh \
 && sed -i -r 's/.?UseDNS\syes/UseDNS no/' /etc/ssh/sshd_config \
 && sed -i -r 's/.?PasswordAuthentication.+/PasswordAuthentication no/' /etc/ssh/sshd_config \
 && sed -i -r 's/.?ChallengeResponseAuthentication.+/ChallengeResponseAuthentication no/' /etc/ssh/sshd_config \
 && sed -i -r 's/.?PermitRootLogin.+/PermitRootLogin no/' /etc/ssh/sshd_config \
 && sed -i '/secure_path/d' /etc/sudoers \
 && echo 'www-data ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/www \
 && /bin/bash -c "source /init-php-conf.sh"
#   Expose ports
EXPOSE 80/tcp 22/tcp
#   Define working directory
WORKDIR /data
#   Define entrypoint and command
ENTRYPOINT ["/init"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
