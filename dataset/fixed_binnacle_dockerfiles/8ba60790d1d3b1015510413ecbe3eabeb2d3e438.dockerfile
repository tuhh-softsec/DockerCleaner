FROM php:7.2-zts-alpine
#   build arguments
ARG BUILD_UID=1000
ARG BUILD_WITH_OPENSSH=0
ARG BUILD_WITH_XDEBUG=0
ARG XDEBUG_REMOTE_CONNECT_BACK=0
ARG XDEBUG_REMOTE_HOST=localhost
#   install packages
RUN apk update \
 && apk add bash=5.2.15-r0 sudo=1.9.12_p2-r1 supervisor=4.2.4-r0 g++=12.2.1_git20220924-r4 make=4.3-r1 autoconf=2.71-r1 libxml2-dev=2.10.4-r0 icu-dev=72.1-r1 curl-dev=7.88.1-r1 pcre-dev=8.45-r2 tzdata=2023c-r0 --no-cache
#   install php extensions
RUN docker-php-ext-install curl
#   install xdebug
RUN [[ "$BUILD_WITH_XDEBUG" != "1" ]] || (curl -sSL https://github.com/xdebug/xdebug/archive/bb90b66.zip -o /tmp/xdebug.zip \
 && unzip /tmp/xdebug.zip -d /tmp \
 && cd /tmp/xdebug-* \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && rm -rf /tmp/xdebug* )
#   enable xdebug
RUN [[ "$BUILD_WITH_XDEBUG" != "1" ]] || (docker-php-ext-enable xdebug \
 && echo "xdebug.remote_autostart=off" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.remote_connect_back=${XDEBUG_REMOTE_CONNECT_BACK}" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.remote_enable=on" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.remote_port=9000" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.remote_handler=dbgp" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini \
 && echo "xdebug.remote_host=${XDEBUG_REMOTE_HOST}" >> /usr/local/etc/php/conf.d/docker-php-ext-xdebug.ini)
#   add composer
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
#   make the bash prompt pretty and add common aliases
RUN mv /etc/profile.d/color_prompt /etc/profile.d/color_prompt.sh \
 && echo -e "alias l='ls -CF'\nalias la='ls -A'\nalias ll='ls -alF'\nalias ls='ls --color=auto'" >> /etc/profile.d/aliases.sh
#   add the unprivileged "app" user and allow passwordless sudo
RUN adduser -D -s /bin/bash -u $BUILD_UID alpine \
 && addgroup alpine wheel \
 && echo "alpine:" | chpasswd \
 && echo -e "%wheel ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/docker
#   configure supervisord to run in the foreground
RUN sed -E -i "s/^(; ?)?nodaemon=false/nodaemon=true/" /etc/supervisord.conf \
 && sed -E -i "s#^(; ?)?pidfile=.*#pidfile=/var/run/supervisord.pid#" /etc/supervisord.conf
#   add the app to supervisord
RUN echo -e "\n[program:app]\nautorestart=true\ndirectory=/opt/project\ncommand=/usr/local/bin/php laravel/artisan serve --host=0.0.0.0\nstdout_logfile=/dev/stdout\nstdout_logfile_maxbytes=0\nstderr_logfile=/dev/stderr\nstderr_logfile_maxbytes=0\nuser=alpine\n" >> /etc/supervisord.conf
#   configure openssh, while this isn't a usual use-case for docker, connecting via SSH significantly speeds
#   up debugging in PhpStorm as its docker-compose support does not support reusing a container which has
#   already been brought up
RUN [[ "$BUILD_WITH_OPENSSH" != "1" ]] || (apk add openssh=9.1_p1-r2 --no-cache \
 && ssh-keygen -A \
 && echo -e "\n[program:sshd]\ncommand=/usr/sbin/sshd -D\n" >> /etc/supervisord.conf)
#   create the .ssh folder in the home directory and write the public key if specified to authorized_keys
RUN [[ "$BUILD_WITH_OPENSSH" != "1" ]] || (mkdir /home/alpine/.ssh \
 && chmod 700 /home/alpine/.ssh \
 && touch /tmp/authorized_keys \
 && chmod 600 /tmp/authorized_keys \
 && ([ "$APP_USER_PUBLIC_KEY" == "" ] || echo $APP_USER_PUBLIC_KEY > /tmp/authorized_keys \
 && mv /tmp/authorized_keys /home/alpine/.ssh/ ) \
 && chown -R alpine:alpine /home/alpine/.ssh/ )
COPY docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
USER alpine
WORKDIR /home/alpine
ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
