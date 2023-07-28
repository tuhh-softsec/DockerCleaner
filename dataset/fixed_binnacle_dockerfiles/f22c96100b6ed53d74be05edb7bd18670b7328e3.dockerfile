FROM alpine:3.9
LABEL maintainer="Alexey Pustovalov <alexey.pustovalov@zabbix.com>"
ARG BUILD_DATE
ARG VCS_REF
ARG APK_FLAGS_COMMON=""
ARG APK_FLAGS_PERSISTENT="${APK_FLAGS_COMMON} --clean-protected --no-cache"
ARG APK_FLAGS_DEV="${APK_FLAGS_COMMON} --no-cache"
ENV TERM="xterm" \
    ZBX_TYPE="frontend" \
    ZBX_DB_TYPE="mysql" \
    ZBX_OPT_TYPE="nginx"
LABEL org.label-schema.name="zabbix-web-${ZBX_OPT_TYPE}-${ZBX_DB_TYPE}-alpine" \
      org.label-schema.vendor="Zabbix LLC" \
      org.label-schema.url="https://zabbix.com/" \
      org.label-schema.description="Zabbix web-interface based on Nginx web server with MySQL database support" \
      org.label-schema.vcs-ref="${VCS_REF}" \
      org.label-schema.build-date="${BUILD_DATE}" \
      org.label-schema.schema-version="1.0" \
      org.label-schema.license="GPL v2.0"
STOPSIGNAL SIGTERM
RUN set -eux \
 && addgroup zabbix \
 && adduser -S -D -G zabbix -h /var/lib/zabbix/ -H zabbix \
 && mkdir -p /etc/zabbix \
 && mkdir -p /etc/zabbix/web \
 && chown --quiet -R zabbix:root /etc/zabbix \
 && apk update \
 && apk add bash=4.4.19-r1 curl=7.64.0-r5 mariadb-client=10.3.25-r0 mariadb-connector-c=3.0.8-r1 nginx=1.14.2-r5 php7-bcmath=7.2.33-r0 php7-ctype=7.2.33-r0 php7-fpm=7.2.33-r0 php7-gd=7.2.33-r0 php7-gettext=7.2.33-r0 php7-json=7.2.33-r0 php7-ldap=7.2.33-r0 php7-mbstring=7.2.33-r0 php7-mysqli=7.2.33-r0 php7-session=7.2.33-r0 php7-simplexml=7.2.33-r0 php7-sockets=7.2.33-r0 php7-xmlreader=7.2.33-r0 php7-xmlwriter=7.2.33-r0 supervisor=3.3.4-r1 ${APK_FLAGS_PERSISTENT} \
 && rm -rf /var/cache/apk/*
ARG MAJOR_VERSION=4.2
ARG ZBX_VERSION=${MAJOR_VERSION}.3
ARG ZBX_SOURCES=https://git.zabbix.com/scm/zbx/zabbix.git
ENV ZBX_VERSION="${ZBX_VERSION}" \
    ZBX_SOURCES="${ZBX_SOURCES}"
LABEL org.label-schema.usage="https://www.zabbix.com/documentation/${MAJOR_VERSION}/manual/installation/containers" \
      org.label-schema.version="${ZBX_VERSION}" \
      org.label-schema.vcs-url="${ZBX_SOURCES}" \
      org.label-schema.docker.cmd="docker run --name zabbix-web-${ZBX_OPT_TYPE}-${ZBX_DB_TYPE} --link mysql-server:mysql --link zabbix-server:zabbix-server -p 80:80 -d zabbix-web-${ZBX_OPT_TYPE}-${ZBX_DB_TYPE}:alpine-${ZBX_VERSION}"
RUN set -eux \
 && apk add coreutils=8.30-r0 gettext=0.19.8.1-r4 git=2.20.4-r0 ${APK_FLAGS_DEV} --virtual build-dependencies \
 && cd /usr/share/ \
 && git clone ${ZBX_SOURCES} --branch ${ZBX_VERSION} --depth 1 --single-branch zabbix-${ZBX_VERSION} \
 && mkdir /usr/share/zabbix/ \
 && cp -R /usr/share/zabbix-${ZBX_VERSION}/frontends/php/* /usr/share/zabbix/ \
 && rm -rf /usr/share/zabbix-${ZBX_VERSION}/ \
 && cd /usr/share/zabbix/ \
 && rm -f conf/zabbix.conf.php \
 && rm -rf tests \
 && ./locale/make_mo.sh \
 && chown --quiet -R nginx:nginx /usr/share/zabbix \
 && apk del ${APK_FLAGS_COMMON} --purge build-dependencies \
 && rm -rf /var/cache/apk/*
EXPOSE 80/tcp 443/tcp
WORKDIR /usr/share/zabbix
VOLUME ["/etc/ssl/nginx"]
COPY conf/etc/supervisor/ /etc/supervisor/
COPY conf/etc/zabbix/nginx.conf /etc/zabbix/
COPY conf/etc/zabbix/nginx_ssl.conf /etc/zabbix/
COPY conf/etc/zabbix/web/zabbix.conf.php /etc/zabbix/web/
COPY conf/etc/nginx/nginx.conf /etc/nginx/
COPY conf/etc/php7/php-fpm.conf /etc/php7/
COPY conf/etc/php7/conf.d/99-zabbix.ini /etc/php7/conf.d/
COPY docker-entrypoint.sh /usr/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
