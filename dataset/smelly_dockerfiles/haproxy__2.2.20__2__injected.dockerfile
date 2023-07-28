#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM debian:bullseye-slim
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  roughly, https://salsa.debian.org/haproxy-team/haproxy/-/blob/732b97ae286906dea19ab5744cf9cf97c364ac1d/debian/haproxy.postinst#L5-6
RUN set -eux ; groupadd --gid 99 --system haproxy ; useradd --gid haproxy --home-dir /var/lib/haproxy --no-create-home --system --uid 99 haproxy ; mkdir /var/lib/haproxy ; chown haproxy:haproxy /var/lib/haproxy
ENV HAPROXY_VERSION="2.2.20"
ENV HAPROXY_URL="https://www.haproxy.org/download/2.2/src/haproxy-2.2.20.tar.gz"
ENV HAPROXY_SHA256="ef242bcb6d2cba3a16ebedbbb18c9919a75b7d98129ff7eb55c049e8ff742dd4"
#  see https://sources.debian.net/src/haproxy/jessie/debian/rules/ for some helpful navigation of the possible "make" arguments
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20210119 gcc=4:10.2.1-1 libc6-dev=2.31-13+deb11u5 liblua5.3-dev=5.3.3-1.1+b1 libpcre2-dev=10.36-2+deb11u1 libssl-dev=1.1.1n-0+deb11u4 make=4.3-4.1 wget=1.21-1+deb11u1 zlib1g-dev=1:1.2.11.dfsg-2+deb11u2 -y ; rm -rf /var/lib/apt/lists/* ; wget -nv -O haproxy.tar.gz "$HAPROXY_URL" ; echo "$HAPROXY_SHA256 *haproxy.tar.gz" | sha256sum -c ; mkdir -p /usr/src/haproxy ; tar -xzf haproxy.tar.gz -C /usr/src/haproxy --strip-components=1 ; rm haproxy.tar.gz ; makeOpts=' TARGET=linux-glibc USE_GETADDRINFO=1 USE_LUA=1 LUA_INC=/usr/include/lua5.3 USE_OPENSSL=1 USE_PCRE2=1 USE_PCRE2_JIT=1 USE_ZLIB=1 EXTRA_OBJS=" contrib/prometheus-exporter/service-prometheus.o " ' ; dpkgArch="$( dpkg --print-architecture ;)" ; case "$dpkgArch" in (armel) makeOpts="$makeOpts ADDLIB=-latomic" ;; esac ; nproc="$( nproc ;)" ; eval "make -C /usr/src/haproxy -j '$nproc' all $makeOpts" ; eval "make -C /usr/src/haproxy install-bin $makeOpts" ; mkdir -p /usr/local/etc/haproxy ; cp -R /usr/src/haproxy/examples/errorfiles /usr/local/etc/haproxy/errors ; rm -rf /usr/src/haproxy ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; find /usr/local -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; haproxy -v
#  https://www.haproxy.org/download/1.8/doc/management.txt
#  "4. Stopping and restarting HAProxy"
#  "when the SIGTERM signal is sent to the haproxy process, it immediately quits and all established connections are closed"
#  "graceful stop is triggered when the SIGUSR1 signal is sent to the haproxy process"
STOPSIGNAL SIGUSR1
ADD docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /
ENTRYPOINT ["docker-entrypoint.sh"]
#  no USER for backwards compatibility (to try to avoid breaking existing users)
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["haproxy", "-f", "/usr/local/etc/haproxy/haproxy.cfg"]
USER 0
