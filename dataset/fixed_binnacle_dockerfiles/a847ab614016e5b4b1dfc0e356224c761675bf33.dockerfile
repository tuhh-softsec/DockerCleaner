#   upstream https://github.com/mesosphere/marathon-lb
FROM debian:stretch
MAINTAINER 若虚 <slpcat@qq.com>
#   Container variables
ENV TERM="xterm" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8" \
    TIMEZONE="Asia/Shanghai"
#   Set timezone and locales
RUN echo "${TIMEZONE}" > /etc/timezone \
 && echo "$LANG UTF-8" > /etc/locale.gen \
 && : \
 && DEBIAN_FRONTEND=noninteractive apt-get install -yq apt-utils dialog locales \
 && ln -sf /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && update-locale LANG=$LANG \
 && locale-gen $LANG \
 && DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales
#   Install required packages
RUN apt-get dist-upgrade -y
#   runtime dependencies
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends ca-certificates=20200601~deb9u2 inetutils-syslogd=2:1.9.4-2+deb9u1 iptables=1.6.0+snapshot20161117-6 libcurl3=7.52.1-5+deb9u16 liblua5.3-0=5.3.3-1+deb9u1 libssl1.0.2=1.0.2u-1~deb9u7 openssl=1.1.0l-1~deb9u6 procps=2:3.3.12-3+deb9u1 python3=3.5.3-1 runit=2.1.2-9.2 gnupg-agent=2.1.18-8~deb9u4 libdpkg-perl=1.18.26 socat=1.7.3.1-2+deb9u1 -y ) \
 && rm -rf /var/lib/apt/lists/*
ENV TINI_VERSION="v0.13.2" \
    TINI_GPG_KEY="595E85A6B1B4779EA4DAAEC70B588DFF0527A9B7"
RUN set -x \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends dirmngr=2.1.18-8~deb9u4 gpg wget=1.18-5+deb9u3 -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O tini "https://github.com/krallin/tini/releases/download/$TINI_VERSION/tini-amd64" \
 && wget -O tini.asc "https://github.com/krallin/tini/releases/download/$TINI_VERSION/tini-amd64.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver hkps://hkps.pool.sks-keyservers.net --recv-keys "$TINI_GPG_KEY" \
 && gpg --batch --verify tini.asc tini \
 && rm -rf "$GNUPGHOME" tini.asc \
 && mv tini /usr/bin/tini \
 && chmod +x /usr/bin/tini \
 && tini -- true \
 && apt-get purge -y --auto-remove dirmngr gpg wget
ENV HAPROXY_MAJOR="1.7" \
    HAPROXY_VERSION="1.7.9" \
    HAPROXY_MD5="a2bbbdd45ffe18d99cdcf26aa992f92d"
COPY requirements.txt /marathon-lb/
COPY pip.conf /etc/pip.conf
#   Build HAProxy
#   Install Python dependencies
#   Install Python packages with --upgrade so we get new packages even if a system
#   package is already installed. Combine with --force-reinstall to ensure we get
#   a local package even if the system package is up-to-date as the system package
#   will probably be uninstalled with the build dependencies.
RUN set -x \
 && buildDeps=' gcc libcurl4-openssl-dev libffi-dev liblua5.3-dev libpcre3-dev libssl-dev make python3-dev python3-pip python3-setuptools wget zlib1g-dev ' \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends $buildDeps -y ) \
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
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
