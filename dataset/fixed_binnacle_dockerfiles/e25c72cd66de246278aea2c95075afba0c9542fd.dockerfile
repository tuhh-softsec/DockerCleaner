FROM alpine:3.9
LABEL maintainer="Alexey Pustovalov <alexey.pustovalov@zabbix.com>"
ARG BUILD_DATE
ARG VCS_REF
ARG APK_FLAGS_COMMON="-q"
ARG APK_FLAGS_PERSISTENT="${APK_FLAGS_COMMON} --clean-protected --no-cache"
ARG APK_FLAGS_DEV="${APK_FLAGS_COMMON} --no-cache"
ENV TERM="xterm" \
    MIBDIRS="/usr/share/snmp/mibs:/var/lib/zabbix/mibs" \
    MIBS="+ALL" \
    ZBX_TYPE="proxy" \
    ZBX_DB_TYPE="sqlite3" \
    ZBX_OPT_TYPE="none"
LABEL org.label-schema.name="zabbix-${ZBX_TYPE}-${ZBX_DB_TYPE}-alpine" \
      org.label-schema.vendor="Zabbix LLC" \
      org.label-schema.url="https://zabbix.com/" \
      org.label-schema.description="Zabbix ${ZBX_TYPE} with SQLite3 database support" \
      org.label-schema.vcs-ref="${VCS_REF}" \
      org.label-schema.build-date="${BUILD_DATE}" \
      org.label-schema.schema-version="1.0" \
      org.label-schema.license="GPL v2.0"
STOPSIGNAL SIGTERM
RUN set -eux \
 && addgroup zabbix \
 && adduser -S -D -G zabbix -h /var/lib/zabbix/ zabbix \
 && mkdir -p /etc/zabbix \
 && mkdir -p /var/lib/zabbix \
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
 && apk update \
 && apk add tini=0.18.0-r0 bash=4.4.19-r1 fping=4.1-r0 iputils=20180629-r1 libcurl=7.64.0-r5 libevent=2.1.8-r6 libldap=2.4.48-r2 libssh2=1.9.0-r1 libxml2=2.9.9-r3 net-snmp-agent-libs=5.8-r1 openipmi-libs=2.0.25-r1 pcre=8.42-r2 sqlite-libs=3.28.0-r3 unixodbc=2.3.7-r0 ${APK_FLAGS_PERSISTENT} \
 && rm -rf /var/cache/apk/*
ARG MAJOR_VERSION=4.2
ARG ZBX_VERSION=${MAJOR_VERSION}.3
ARG ZBX_SOURCES=https://git.zabbix.com/scm/zbx/zabbix.git
ENV ZBX_VERSION="${ZBX_VERSION}" \
    ZBX_SOURCES="${ZBX_SOURCES}"
LABEL org.label-schema.usage="https://www.zabbix.com/documentation/${MAJOR_VERSION}/manual/installation/containers" \
      org.label-schema.version="${ZBX_VERSION}" \
      org.label-schema.vcs-url="${ZBX_SOURCES}" \
      org.label-schema.docker.cmd="docker run --name zabbix-${ZBX_TYPE}-${ZBX_DB_TYPE} --link zabbix-server:zabbix-server -p 10051:10051 -d zabbix-${ZBX_TYPE}-${ZBX_DB_TYPE}:alpine-${ZBX_VERSION}"
RUN set -eux \
 && apk add alpine-sdk=1.0-r0 autoconf=2.69-r2 automake=1.16.1-r0 coreutils=8.30-r0 curl-dev=7.64.0-r5 libevent-dev=2.1.8-r6 libssh2-dev=1.9.0-r1 libxml2-dev=2.9.9-r3 net-snmp-dev=5.8-r1 openipmi-dev=2.0.25-r1 openldap-dev=2.4.48-r2 pcre-dev=8.42-r2 sqlite-dev=3.28.0-r3 git=2.20.4-r0 unixodbc-dev=2.3.7-r0 ${APK_FLAGS_DEV} --virtual build-dependencies \
 && cd /tmp/ \
 && git clone ${ZBX_SOURCES} --branch ${ZBX_VERSION} --depth 1 --single-branch zabbix-${ZBX_VERSION} \
 && cd /tmp/zabbix-${ZBX_VERSION} \
 && zabbix_revision=`git rev-parse --short HEAD ` \
 && sed -i "s/{ZABBIX_REVISION}/$zabbix_revision/g" include/version.h \
 && ./bootstrap.sh \
 && export CFLAGS="-fPIC -pie -Wl,-z,relro -Wl,-z,now" \
 && ./configure --datadir=/usr/lib --libdir=/usr/lib/zabbix --sysconfdir=/etc/zabbix --prefix=/usr --enable-agent --enable-${ZBX_TYPE} --with-${ZBX_DB_TYPE} --with-ldap --with-libcurl --with-libxml2 --with-net-snmp --with-openipmi --with-openssl --with-ssh2 --with-unixodbc --enable-ipv6 --silent \
 && make -j"$( nproc ;)" -s dbschema \
 && make -j"$( nproc ;)" -s \
 && cp src/zabbix_${ZBX_TYPE}/zabbix_${ZBX_TYPE} /usr/sbin/zabbix_${ZBX_TYPE} \
 && cp src/zabbix_get/zabbix_get /usr/bin/zabbix_get \
 && cp src/zabbix_sender/zabbix_sender /usr/bin/zabbix_sender \
 && cp conf/zabbix_${ZBX_TYPE}.conf /etc/zabbix/zabbix_${ZBX_TYPE}.conf \
 && chown --quiet -R zabbix:root /etc/zabbix \
 && cd /tmp/ \
 && rm -rf /tmp/zabbix-${ZBX_VERSION}/ \
 && apk del ${APK_FLAGS_COMMON} --purge build-dependencies \
 && rm -rf /var/cache/apk/*
EXPOSE 10051/tcp
WORKDIR /var/lib/zabbix
VOLUME ["/usr/lib/zabbix/externalscripts", "/var/lib/zabbix/enc", "/var/lib/zabbix/modules", "/var/lib/zabbix/snmptraps"]
VOLUME ["/var/lib/zabbix/ssh_keys", "/var/lib/zabbix/ssl/certs", "/var/lib/zabbix/ssl/keys", "/var/lib/zabbix/ssl/ssl_ca"]
COPY docker-entrypoint.sh /usr/bin/
ENTRYPOINT ["/sbin/tini", "--", "/usr/bin/docker-entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
