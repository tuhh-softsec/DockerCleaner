FROM alpine:3.9 AS builder
ARG APK_FLAGS_COMMON=""
ARG APK_FLAGS_DEV="${APK_FLAGS_COMMON} --no-cache"
ARG MAJOR_VERSION=4.2
ARG ZBX_VERSION=${MAJOR_VERSION}.3
ARG ZBX_SOURCES=https://git.zabbix.com/scm/zbx/zabbix.git
ENV TERM="xterm" \
    ZBX_VERSION="${ZBX_VERSION}" \
    ZBX_SOURCES="${ZBX_SOURCES}" \
    ZBX_TYPE="agent"
RUN set -eux \
 && apk update \
 && apk add alpine-sdk=1.0-r0 autoconf=2.69-r2 automake=1.16.1-r0 openssl-dev=1.1.1k-r0 openldap-dev=2.4.48-r2 pcre-dev=8.42-r2 git=2.20.4-r0 coreutils=8.30-r0 ${APK_FLAGS_DEV} --virtual build-dependencies \
 && cd /tmp/ \
 && git clone ${ZBX_SOURCES} --branch ${ZBX_VERSION} --depth 1 --single-branch zabbix-${ZBX_VERSION} \
 && cd /tmp/zabbix-${ZBX_VERSION} \
 && zabbix_revision=`git rev-parse --short HEAD ` \
 && sed -i "s/{ZABBIX_REVISION}/$zabbix_revision/g" include/version.h \
 && ./bootstrap.sh \
 && export CFLAGS="-fPIC -pie -Wl,-z,relro -Wl,-z,now" \
 && ./configure --datadir=/usr/lib --libdir=/usr/lib/zabbix --prefix=/usr --sysconfdir=/etc/zabbix --prefix=/usr --enable-agent --with-ldap --with-openssl --enable-ipv6 --silent \
 && make -j"$( nproc ;)" -s
FROM alpine:3.9
LABEL maintainer="Alexey Pustovalov <alexey.pustovalov@zabbix.com>"
ARG BUILD_DATE
ARG VCS_REF
ARG APK_FLAGS_COMMON=""
ARG APK_FLAGS_PERSISTENT="${APK_FLAGS_COMMON} --clean-protected --no-cache"
ARG MAJOR_VERSION=4.2
ARG ZBX_VERSION=${MAJOR_VERSION}.3
ARG ZBX_SOURCES=https://git.zabbix.com/scm/zbx/zabbix.git
ENV TERM="xterm" \
    ZBX_VERSION="${ZBX_VERSION}" \
    ZBX_SOURCES="${ZBX_SOURCES}" \
    ZBX_TYPE="agent" \
    ZBX_DB_TYPE="none" \
    ZBX_OPT_TYPE="none"
LABEL org.label-schema.name="zabbix-${ZBX_TYPE}-alpine" \
      org.label-schema.vendor="Zabbix LLC" \
      org.label-schema.url="https://zabbix.com/" \
      org.label-schema.description="Zabbix agent is deployed on a monitoring target to actively monitor local resources and applications" \
      org.label-schema.vcs-ref="${VCS_REF}" \
      org.label-schema.build-date="${BUILD_DATE}" \
      org.label-schema.schema-version="1.0" \
      org.label-schema.license="GPL v2.0" \
      org.label-schema.usage="https://www.zabbix.com/documentation/${MAJOR_VERSION}/manual/installation/containers" \
      org.label-schema.version="${ZBX_VERSION}" \
      org.label-schema.vcs-url="${ZBX_SOURCES}" \
      org.label-schema.docker.cmd="docker run --name zabbix-${ZBX_TYPE} --link zabbix-server:zabbix-server -p 10050:10050 -d zabbix-${ZBX_TYPE}:alpine-${ZBX_VERSION}"
STOPSIGNAL SIGTERM
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/src/zabbix_agent/zabbix_agentd /usr/sbin/zabbix_agentd
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/src/zabbix_get/zabbix_get /usr/bin/zabbix_get
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/src/zabbix_sender/zabbix_sender /usr/bin/zabbix_sender
COPY --from=builder /tmp/zabbix-${ZBX_VERSION}/conf/zabbix_agentd.conf /etc/zabbix/zabbix_agentd.conf
RUN set -eux \
 && addgroup zabbix \
 && adduser -S -D -G zabbix -h /var/lib/zabbix/ zabbix \
 && mkdir -p /etc/zabbix \
 && mkdir -p /etc/zabbix/zabbix_agentd.d \
 && mkdir -p /var/lib/zabbix \
 && mkdir -p /var/lib/zabbix/enc \
 && mkdir -p /var/lib/zabbix/modules \
 && chown --quiet -R zabbix:root /var/lib/zabbix \
 && apk update \
 && apk add tini=0.18.0-r0 bash=4.4.19-r1 coreutils=8.30-r0 iputils=20180629-r1 libldap=2.4.48-r2 pcre=8.42-r2 ${APK_FLAGS_PERSISTENT} \
 && rm -rf /var/cache/apk/*
EXPOSE 10050/tcp
WORKDIR /var/lib/zabbix
VOLUME ["/etc/zabbix/zabbix_agentd.d", "/var/lib/zabbix/enc", "/var/lib/zabbix/modules"]
COPY docker-entrypoint.sh /usr/bin/
ENTRYPOINT ["/sbin/tini", "--", "/usr/bin/docker-entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
