FROM alpine:3.4 AS builder
ARG APK_FLAGS_COMMON=""
ARG APK_FLAGS_DEV="${APK_FLAGS_COMMON} --no-cache"
ARG MAJOR_VERSION=4.2
ARG ZBX_VERSION=${MAJOR_VERSION}.3
ARG ZBX_SOURCES=https://git.zabbix.com/scm/zbx/zabbix.git
ENV ZBX_VERSION="${ZBX_VERSION}" \
    ZBX_SOURCES="${ZBX_SOURCES}" \
    PATH="${PATH}:/usr/lib/jvm/default-jvm/bin/" \
    JAVA_HOME="/usr/lib/jvm/default-jvm" \
    ZBX_TYPE="server" \
    ZBX_DB_TYPE="mysql" \
    ZBX_OPT_TYPE="nginx" \
    MYSQL_ALLOW_EMPTY_PASSWORD="true" \
    ZBX_ADD_SERVER="true" \
    ZBX_ADD_WEB="true" \
    DB_SERVER_HOST="localhost" \
    MYSQL_USER="zabbix" \
    ZBX_ADD_JAVA_GATEWAY="true" \
    ZBX_JAVAGATEWAY_ENABLE="true" \
    ZBX_JAVAGATEWAY="localhost"
RUN set -eux \
 && apk add bash=4.3.42-r5 alpine-sdk=0.4-r3 autoconf=2.69-r0 automake=1.15-r0 coreutils=8.25-r0 curl-dev=7.60.0-r1 gettext=0.19.7-r3 libevent-dev=2.0.22-r1 libssh2-dev=1.7.0-r0 libxml2-dev=2.9.5-r0 mysql-dev net-snmp-dev=5.7.3-r5 openipmi-dev=2.0.21-r3 openjdk8=8.111.14-r0 openldap-dev=2.4.44-r2 pcre-dev=8.38-r1 subversion=1.9.7-r0 unixodbc-dev=2.3.4-r0 ${APK_FLAGS_DEV} --virtual build-dependencies \
 && cd /tmp/ \
 && git clone ${ZBX_SOURCES} --branch ${ZBX_VERSION} --depth 1 --single-branch zabbix-${ZBX_VERSION} \
 && cd /tmp/zabbix-${ZBX_VERSION} \
 && zabbix_revision=`git rev-parse --short HEAD ` \
 && sed -i "s/{ZABBIX_REVISION}/$zabbix_revision/g" include/version.h \
 && sed -i "s/{ZABBIX_REVISION}/$zabbix_revision/g" include/version.h \
 && sed -i "s/{ZABBIX_REVISION}/$zabbix_revision/g" src/zabbix_java/src/com/zabbix/gateway/GeneralInformation.java \
 && ./bootstrap.sh \
 && export CFLAGS="-fPIC -pie -Wl,-z,relro -Wl,-z,now" \
 && ./configure --datadir=/usr/lib --libdir=/usr/lib/zabbix --prefix=/usr --sysconfdir=/etc/zabbix --enable-agent --enable-${ZBX_TYPE} --with-${ZBX_DB_TYPE} --with-ldap --with-libcurl --with-libxml2 --enable-java --with-net-snmp --with-openipmi --with-openssl --with-ssh2 --with-unixodbc --enable-ipv6 --silent \
 && make -j"$( nproc ;)" -s dbschema \
 && make -j"$( nproc ;)" -s \
 && cat database/${ZBX_DB_TYPE}/schema.sql > database/${ZBX_DB_TYPE}/create.sql \
 && cat database/${ZBX_DB_TYPE}/images.sql >> database/${ZBX_DB_TYPE}/create.sql \
 && cat database/${ZBX_DB_TYPE}/data.sql >> database/${ZBX_DB_TYPE}/create.sql \
 && gzip database/${ZBX_DB_TYPE}/create.sql \
 && rm -rf /tmp/zabbix-${ZBX_VERSION}/src/zabbix_java/lib/*.xml \
 && cd frontends/php/ \
 && rm -f conf/zabbix.conf.php \
 && rm -rf tests \
 && ./locale/make_mo.sh
FROM alpine:3.4
LABEL maintainer="Alexey Pustovalov <alexey.pustovalov@zabbix.com>"
ARG BUILD_DATE
ARG VCS_REF
ARG APK_FLAGS_COMMON=""
ARG APK_FLAGS_PERSISTENT="${APK_FLAGS_COMMON} --clean-protected --no-cache"
ARG MAJOR_VERSION=4.2
ARG ZBX_VERSION=${MAJOR_VERSION}.3
ARG ZBX_SOURCES=https://git.zabbix.com/scm/zbx/zabbix.git
ENV ZBX_VERSION="${ZBX_VERSION}" \
    ZBX_SOURCES="${ZBX_SOURCES}" \
    MIBDIRS="/usr/share/snmp/mibs:/var/lib/zabbix/mibs" \
    MIBS="+ALL" \
    PATH="${PATH}:/usr/lib/jvm/default-jvm/bin/" \
    JAVA_HOME="/usr/lib/jvm/default-jvm" \
    ZBX_TYPE="server" \
    ZBX_DB_TYPE="mysql" \
    ZBX_OPT_TYPE="nginx" \
    MYSQL_ALLOW_EMPTY_PASSWORD="true" \
    ZBX_ADD_SERVER="true" \
    ZBX_ADD_WEB="true" \
    DB_SERVER_HOST="localhost" \
    MYSQL_USER="zabbix" \
    ZBX_ADD_JAVA_GATEWAY="true" \
    ZBX_JAVAGATEWAY_ENABLE="true" \
    ZBX_JAVAGATEWAY="localhost"
LABEL org.label-schema.name="zabbix-${ZBX_TYPE}-${ZBX_DB_TYPE}-alpine" \
      org.label-schema.vendor="Zabbix LLC" \
      org.label-schema.url="https://zabbix.com/" \
      org.label-schema.description="Zabbix appliance with MySQL database support and ${ZBX_OPT_TYPE} web-server" \
      org.label-schema.vcs-ref="${VCS_REF}" \
      org.label-schema.build-date="${BUILD_DATE}" \
      org.label-schema.schema-version="1.0" \
      org.label-schema.license="GPL 2.0" \
      org.label-schema.usage="https://www.zabbix.com/documentation/${MAJOR_VERSION}/manual/installation/containers" \
      org.label-schema.version="${ZBX_VERSION}" \
      org.label-schema.vcs-url="${ZBX_SOURCES}" \
      org.label-schema.docker.cmd="docker run --name zabbix-appliance -p 80:80 -p 10051:10051 -d zabbix-appliance:alpine-${ZBX_VERSION}"
STOPSIGNAL SIGTERM
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/src/zabbix_${ZBX_TYPE}/zabbix_${ZBX_TYPE} /usr/sbin/zabbix_${ZBX_TYPE}
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/src/zabbix_get/zabbix_get /usr/bin/zabbix_get
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/src/zabbix_sender/zabbix_sender /usr/bin/zabbix_sender
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/conf/zabbix_${ZBX_TYPE}.conf /etc/zabbix/zabbix_${ZBX_TYPE}.conf
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/database/${ZBX_DB_TYPE}/create.sql.gz /usr/share/doc/zabbix-${ZBX_TYPE}-${ZBX_DB_TYPE}/create.sql.gz
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/src/zabbix_java/bin/ /usr/sbin/zabbix_java/bin/
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/src/zabbix_java/lib/ /usr/sbin/zabbix_java/lib/
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/frontends/php/ /usr/share/zabbix/
RUN set -eux \
 && addgroup zabbix \
 && adduser -S -D -G zabbix -h /var/lib/zabbix/ zabbix \
 && adduser zabbix dialout \
 && mkdir -p /etc/zabbix \
 && mkdir -p /var/lib/zabbix \
 && mkdir -p /usr/lib/zabbix/alertscripts \
 && mkdir -p /var/lib/zabbix/enc \
 && mkdir -p /usr/lib/zabbix/externalscripts \
 && mkdir -p /var/lib/zabbix/mibs \
 && mkdir -p /var/lib/zabbix/modules \
 && mkdir -p /var/lib/zabbix/snmptraps \
 && mkdir -p /var/lib/zabbix/ssh_keys \
 && mkdir -p /var/lib/zabbix/ssl \
 && mkdir -p /var/lib/zabbix/ssl/certs \
 && mkdir -p /var/lib/zabbix/ssl/keys \
 && mkdir -p /var/lib/zabbix/ssl/ssl_ca \
 && chown --quiet -R zabbix:root /var/lib/zabbix \
 && mkdir -p /usr/share/doc/zabbix-${ZBX_TYPE}-${ZBX_DB_TYPE}/ \
 && apk update \
 && apk add tini=0.9.0-r1 bash=4.3.42-r5 curl=7.60.0-r1 fping=3.13-r0 iputils=20121221-r3 libcurl=7.60.0-r1 libevent=2.0.22-r1 libldap=2.4.44-r2 libssh2=1.7.0-r0 libxml2=2.9.5-r0 mariadb-client=10.1.32-r0 mariadb-client-libs=10.1.32-r0 mysql=10.1.32-r0 net-snmp-agent-libs=5.7.3-r5 nginx=1.10.3-r0 openipmi-libs=2.0.21-r3 openjdk8-jre-base=8.111.14-r0 php5-bcmath=5.6.36-r0 php5-ctype=5.6.36-r0 php5-fpm=5.6.36-r0 php5-gd=5.6.36-r0 php5-gettext=5.6.36-r0 php5-json=5.6.36-r0 php5-ldap=5.6.36-r0 php5-mysqli=5.6.36-r0 php5-sockets=5.6.36-r0 php5-xmlreader=5.6.36-r0 supervisor=3.2.4-r0 pcre=8.38-r1 unixodbc=2.3.4-r0 ${APK_FLAGS_PERSISTENT} \
 && chown --quiet -R nginx:nginx /usr/share/zabbix \
 && rm -rf /var/cache/apk/*
EXPOSE 80/tcp 443/tcp 10051/tcp
WORKDIR /var/lib/zabbix
VOLUME ["/etc/ssl/nginx"]
VOLUME ["/usr/lib/zabbix/alertscripts", "/usr/lib/zabbix/externalscripts", "/var/lib/zabbix/enc", "/var/lib/zabbix/mibs", "/var/lib/zabbix/modules"]
VOLUME ["/var/lib/zabbix/snmptraps", "/var/lib/zabbix/ssh_keys", "/var/lib/zabbix/ssl/certs", "/var/lib/zabbix/ssl/keys", "/var/lib/zabbix/ssl/ssl_ca"]
VOLUME ["/var/lib/mysql/"]
COPY conf/etc/supervisor/ /etc/supervisor/
COPY conf/etc/zabbix/nginx.conf /etc/zabbix/
COPY conf/etc/zabbix/nginx_ssl.conf /etc/zabbix/
COPY conf/etc/zabbix/web/zabbix.conf.php /etc/zabbix/web/
COPY conf/etc/nginx/nginx.conf /etc/nginx/
COPY conf/etc/php5/php-fpm.conf /etc/php5/
COPY conf/etc/php5/conf.d/99-zabbix.ini /etc/php5/conf.d/
COPY conf/etc/zabbix/zabbix_java_gateway_logback.xml /etc/zabbix/
COPY conf/usr/sbin/zabbix_java_gateway /usr/sbin/
COPY docker-entrypoint.sh /usr/bin/
ENV ZBX_TYPE="appliance"
ENTRYPOINT ["/sbin/tini", "--", "/usr/bin/docker-entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
