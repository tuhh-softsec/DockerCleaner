FROM alpine:3.9
ARG BUILD_DATE
ARG VCS_REF
ARG VERSION
LABEL maintainer="CrazyMax" \
      org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.name="librenms" \
      org.label-schema.description="LibreNMS" \
      org.label-schema.version="$VERSION" \
      org.label-schema.url="https://github.com/librenms/docker" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="https://github.com/librenms/docker" \
      org.label-schema.vendor="LibreNMS" \
      org.label-schema.schema-version="1.0"
RUN apk add busybox-extras=1.29.3-r10 acl=2.2.52-r5 bash=4.4.19-r1 bind-tools=9.12.4_p2-r0 binutils=2.31.1-r2 ca-certificates=20191127-r2 coreutils=8.30-r0 curl=7.64.0-r5 fping=4.1-r0 git=2.20.4-r0 graphviz=2.40.1-r1 imagemagick=7.0.8.68-r0 ipmitool=1.8.18-r7 monitoring-plugins=2.2-r10 mtr=0.92-r0 mysql-client=10.3.25-r0 net-snmp=5.8-r1 net-snmp-tools=5.8-r1 nginx=1.14.2-r5 nmap=7.70-r4 openssl=1.1.1k-r0 perl=5.26.3-r1 php7=7.2.33-r0 php7-cli php7-ctype=7.2.33-r0 php7-curl=7.2.33-r0 php7-fileinfo=7.2.33-r0 php7-fpm=7.2.33-r0 php7-gd=7.2.33-r0 php7-json=7.2.33-r0 php7-ldap=7.2.33-r0 php7-mbstring=7.2.33-r0 php7-mcrypt php7-memcached php7-mysqlnd=7.2.33-r0 php7-opcache=7.2.33-r0 php7-openssl=7.2.33-r0 php7-pdo=7.2.33-r0 php7-pdo_mysql=7.2.33-r0 php7-phar=7.2.33-r0 php7-posix=7.2.33-r0 php7-session=7.2.33-r0 php7-simplexml=7.2.33-r0 php7-snmp=7.2.33-r0 php7-tokenizer=7.2.33-r0 php7-xml=7.2.33-r0 php7-zip=7.2.33-r0 py-mysqldb=1.2.5-r1 python py2-pip=18.1-r0 python3=3.6.9-r3 rrdtool=1.7.0-r0 runit=2.1.2-r3 shadow=4.5-r2 supervisor=3.3.4-r1 syslog-ng=3.19.1-r0 ttf-dejavu=2.37-r1 tzdata=2020c-r1 util-linux=2.33-r0 whois=5.4.1-r0 --update --no-cache \
 && pip2 install --upgrade pip \
 && pip2 install python-memcached \
 && pip3 install --upgrade pip \
 && pip3 install python-memcached \
 && sed -i -e "s/;date\.timezone.*/date\.timezone = UTC/" /etc/php7/php.ini \
 && rm -rf /var/cache/apk/* /var/www/* /tmp/* \
 && setcap cap_net_raw+ep /usr/bin/nmap \
 && setcap cap_net_raw+ep /usr/sbin/fping
ENV LIBRENMS_VERSION="1.52" \
    LIBRENMS_PATH="/opt/librenms" \
    DATA_PATH="/data" \
    CRONTAB_PATH="/var/spool/cron/crontabs"
RUN mkdir -p /opt \
 && addgroup -g 1000 librenms \
 && adduser -u 1000 -G librenms -h ${LIBRENMS_PATH} -s /bin/sh -D librenms \
 && passwd -l librenms \
 && usermod -a -G librenms nginx \
 && curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/bin --filename=composer \
 && git clone --branch ${LIBRENMS_VERSION} https://github.com/librenms/librenms.git ${LIBRENMS_PATH} \
 && chown -R librenms. ${LIBRENMS_PATH} \
 && su - librenms -c "composer install --no-dev --no-interaction --no-ansi --working-dir=${LIBRENMS_PATH}" \
 && curl -sSLk -q https://raw.githubusercontent.com/librenms/librenms-agent/master/snmp/distro -o /usr/bin/distro \
 && chmod +x /usr/bin/distro \
 && mkdir -p /data ${LIBRENMS_PATH}/config.d /var/log/supervisord \
 && cp ${LIBRENMS_PATH}/config.php.default ${LIBRENMS_PATH}/config.php \
 && cp ${LIBRENMS_PATH}/snmpd.conf.example /etc/snmp/snmpd.conf \
 && sed -i "1s|.*|#!/usr/bin/env python3|" ${LIBRENMS_PATH}/snmp-scan.py \
 && echo "foreach (glob(\"${DATA_PATH}/config/*.php\") as $filename) include $filename;" >> ${LIBRENMS_PATH}/config.php \
 && echo "foreach (glob(\"${LIBRENMS_PATH}/config.d/*.php\") as $filename) include $filename;" >> ${LIBRENMS_PATH}/config.php \
 && chown -R librenms. ${DATA_PATH} ${LIBRENMS_PATH} \
 && chown -R nginx. /var/lib/nginx /var/log/nginx /var/log/php7 /var/tmp/nginx \
 && rm -rf /tmp/*
COPY entrypoint.sh /entrypoint.sh
COPY assets /
RUN chmod a+x /entrypoint.sh /usr/local/bin/*
EXPOSE 80/tcp 514/tcp 514/udp
WORKDIR ${LIBRENMS_PATH}
VOLUME [ "${DATA_PATH}" ]
ENTRYPOINT ["/entrypoint.sh"]
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisord.conf"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
