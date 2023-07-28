FROM alpine:3.6
COPY runit.sh /runit.sh
ENV DATADIR="/srv"
#   Install Icinga2 ###################################
RUN set -x \
 && apk add ca-certificates=20161130-r3 openssl=1.0.2r-r0 su-exec=0.2-r0 runit=2.1.2-r3 socklog=2.1.0-r3 tzdata=2019a-r0 curl=7.61.1-r2 nginx=1.12.2-r2 gettext=0.19.8.1-r1 bash=4.3.48-r1 openrc=0.24.1-r2 shadow=4.2.1-r11 iputils=20121221-r6 icinga2=2.6.3-r1 nagios-plugins=2.2.1-r0 jq=1.5-r4 postgresql=9.6.13-r0 php7=7.1.17-r0 php7-common=7.1.17-r0 php7-ctype=7.1.17-r0 php7-dom=7.1.17-r0 php7-fpm=7.1.17-r0 php7-gd=7.1.17-r0 php7-gettext=7.1.17-r0 php7-gmp=7.1.17-r0 php7-imagick=3.4.3-r2 php7-intl=7.1.17-r0 php7-json=7.1.17-r0 php7-ldap=7.1.17-r0 php7-mcrypt=7.1.17-r0 php7-mysqlnd=7.1.17-r0 php7-openssl=7.1.17-r0 php7-pdo=7.1.17-r0 php7-pdo_mysql=7.1.17-r0 php7-pdo_pgsql=7.1.17-r0 php7-pgsql=7.1.17-r0 php7-session=7.1.17-r0 php7-sockets=7.1.17-r0 --update --no-cache
RUN set -x \
 && mkdir -p /run/nginx \
 && chown nginx:nginx -R /run/nginx \
 && sed -i 's/group.*/group\ =\ icingacmd/' /etc/php7/php-fpm.d/www.conf \
 && sed -i 's/;date.timezone =/date.timezone ="UTC"/' /etc/php7/php.ini \
 && sed -i ';s/error_log.*/error_log\ =\ syslog/' /etc/php7/php-fpm.conf \
 && sed -i 's/;syslog.facility/syslog.facility/' /etc/php7/php-fpm.conf \
 && sed -i 's/;syslog.ident/syslog.ident/' /etc/php7/php-fpm.conf \
 && rm -rf /etc/sv /etc/service \
 && echo 'Etc/UTC' > /etc/timezone
#   copy config templates
RUN set -x \
 && mkdir -p /scripts/icinga2
COPY config/icinga2/ido-pgsql.conf /scripts/icinga2/ido-pgsql.conf
#   Icinga 2 IDO
#   http://docs.icinga.org/icinga2/latest/doc/module/icinga2/chapter/icinga2-features
RUN set -x \
 && icinga2 feature enable ido-pgsql syslog checker
#   Command Feature
RUN set -x \
 && icinga2 feature enable command notification \
 && mkdir -p /run/icinga2/cmd \
 && chown icinga:icinga -R /run/icinga2
COPY config/icinga2/notification-command.conf /scripts/notification-command.conf
RUN set -x \
 && cat /scripts/notification-command.conf >> /etc/icinga2/conf.d/notification-command.conf \
 && rm /scripts/notification-command.conf
COPY config/icinga2/modified-commands.conf /scripts/modified-commands.conf
RUN set -x \
 && cat /scripts/modified-commands.conf >> /etc/icinga2/conf.d/modified-commands.conf \
 && rm /scripts/modified-commands.conf
COPY config/icinga2/templates.conf /scripts/templates.conf
RUN set -x \
 && cat /scripts/templates.conf >> /etc/icinga2/conf.d/templates.conf \
 && rm /scripts/templates.conf
COPY config/icinga2/users.conf /scripts/users.conf
RUN set -x \
 && cat /scripts/users.conf >> /etc/icinga2/conf.d/users.conf \
 && rm /scripts/users.conf
RUN set -x \
 && rm -rf /etc/icinga2/conf.d/hosts.conf
#   Fix icinga installation location and permission
#   This is needed since we are building from source
RUN set -x \
 && mv /var/lib/icinga2 /scripts/lib
#   Icingaweb2 ##############################################
#   copy config templates
RUN set -x \
 && mkdir -p /scripts/icingaweb2
COPY config/icingaweb2/authentication.ini /scripts/icingaweb2/authentication.ini
COPY config/icingaweb2/config.ini /scripts/icingaweb2/config.ini
COPY config/icingaweb2/groups.ini /scripts/icingaweb2/groups.ini
COPY config/icingaweb2/resources.ini /scripts/icingaweb2/resources.ini
#   Add icingaweb2
COPY icingaweb2 /usr/share/icingaweb2
RUN set -x \
 && chown -R nginx:nginx /usr/share/icingaweb2
COPY config/icingaweb2 /etc/icingaweb2/
RUN set -x \
 && mkdir -p /etc/icingaweb2/enabledModules \
 && ln -s /usr/share/icingaweb2/modules/doc /etc/icingaweb2/enabledModules/doc \
 && ln -s /usr/share/icingaweb2/modules/monitoring /etc/icingaweb2/enabledModules/monitoring \
 && ln -s /usr/share/icingaweb2/modules/test /etc/icingaweb2/enabledModules/test
#   Update nginx site configuraiton
COPY config/nginx.conf /etc/nginx/conf.d/default.conf
#   Plugins ############################################
COPY plugins/* /usr/lib/monitoring-plugins/
#   runit ##############################################
COPY sv /etc/sv/
RUN ln -s /etc/sv /etc/service
COPY sv /etc/sv/
ENV TZ=":/etc/localtime"
ENV LANG="en_US.utf8"
VOLUME ["$DATADIR"]
ENTRYPOINT ["/runit.sh"]
EXPOSE 60006/tcp 5665/tcp
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
