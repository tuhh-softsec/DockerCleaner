FROM debian:buster
ARG VERSION
#  artifacts versions
ARG SEC_UTILS_VERSION=0.4.1
#  nexus repository artifacts
COPY http://sodio.stratio.com/repository/paas/kms_utils/${SEC_UTILS_VERSION}/kms_utils-${SEC_UTILS_VERSION}.sh /usr/sbin/kms_utils.sh
COPY http://sodio.stratio.com/repository/paas/log_utils/${SEC_UTILS_VERSION}/b-log-${SEC_UTILS_VERSION}.sh /usr/sbin/b-log.sh
#  runtime dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates inetutils-syslogd iptables libcurl4 liblua5.3-0 libssl1.1 openssl procps python3 runit gnupg-agent socat curl jq -y \
 && apt-get install --no-install-recommends rsyslog -y \
 && rm -rf /var/lib/apt/lists/*
ENV TINI_VERSION="v0.13.2" \
    TINI_GPG_KEY="595E85A6B1B4779EA4DAAEC70B588DFF0527A9B7"
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends dirmngr gpg wget -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O tini "https://github.com/krallin/tini/releases/download/$TINI_VERSION/tini-amd64" \
 && wget -O tini.asc "https://github.com/krallin/tini/releases/download/$TINI_VERSION/tini-amd64.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$TINI_GPG_KEY" || gpg --keyserver pool.sks-keyservers.net --recv-keys "$TINI_GPG_KEY" || gpg --keyserver keyserver.pgp.com --recv-keys "$TINI_GPG_KEY" || gpg --keyserver pgp.mit.edu --recv-keys "$TINI_GPG_KEY" \
 && gpg --batch --verify tini.asc tini \
 && rm -rf "$GNUPGHOME" tini.asc \
 && mv tini /usr/bin/tini \
 && chmod +x /usr/bin/tini \
 && chmod +x /usr/sbin/kms_utils.sh \
 && tini -- true \
 && apt-get purge -y --auto-remove dirmngr gpg wget
ENV HAPROXY_MAJOR="1.7" \
    HAPROXY_VERSION="1.7.6" \
    HAPROXY_MD5="8f4328cf66137f0dbf6901e065f603cc"
COPY requirements.txt /marathon-lb/
COPY MARATHON-LB-VERSION /marathon-lb/
RUN set -x \
 && buildDeps=' build-essential gcc libcurl4-openssl-dev libffi-dev liblua5.3-dev libpcre3-dev libssl-dev make python3-dev python3-pip python3-setuptools wget zlib1g-dev ' \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O haproxy.tar.gz "https://www.haproxy.org/download/$HAPROXY_MAJOR/src/haproxy-$HAPROXY_VERSION.tar.gz" \
 && echo "$HAPROXY_MD5 haproxy.tar.gz" | md5sum -c \
 && mkdir -p /usr/src/haproxy \
 && tar -xzf haproxy.tar.gz -C /usr/src/haproxy --strip-components=1 \
 && rm haproxy.tar.gz \
 && make -C /usr/src/haproxy TARGET=linux2628 ARCH=x86_64 USE_LUA=1 LUA_INC=/usr/include/lua5.3/ USE_OPENSSL=1 USE_PCRE_JIT=1 USE_PCRE=1 USE_REGPARM=1 USE_STATIC_PCRE=1 USE_ZLIB=1 all install-bin \
 && rm -rf /usr/src/haproxy \
 && pip3 install --no-cache --upgrade --force-reinstall -r /marathon-lb/requirements.txt \
 && apt-get purge -y --auto-remove $buildDeps
COPY . /marathon-lb
WORKDIR /marathon-lb
ENTRYPOINT ["tini", "-g", "--", "/marathon-lb/run"]
CMD ["sse", "--health-check", "--group", "external"]
EXPOSE 80/tcp 443/tcp 9090/tcp 9091/tcp
