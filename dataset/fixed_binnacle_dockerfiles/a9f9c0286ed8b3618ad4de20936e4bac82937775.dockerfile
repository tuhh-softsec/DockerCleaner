#   AUTOMATICALLY GENERATED
#   DO NOT EDIT THIS FILE DIRECTLY, USE /Dockerfile.tmpl.php
#   https://hub.docker.com/_/debian
FROM debian:stretch-slim
MAINTAINER Instrumentisto Team <developer@instrumentisto.com>
#   Build and install Postfix
#   https://git.launchpad.net/postfix/tree/debian/rules?id=94dfb9850484db5f47958eaa86f958857ab9834c
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends inetutils-syslogd ca-certificates -y --no-install-suggests \
 && update-ca-certificates \
 && apt-get install --no-install-recommends libpcre3 libicu57 libdb5.3 libpq5 libmariadbclient18 libsqlite3-0 libsasl2-2 libldap-2.4 -y --no-install-suggests \
 && toolDeps=" curl make gcc g++ libc-dev " \
 && apt-get install --no-install-recommends $toolDeps -y --no-install-suggests \
 && buildDeps=" libssl-dev libpcre3-dev libicu-dev libdb-dev libpq-dev libmariadbclient-dev libsqlite3-dev libsasl2-dev libldap2-dev " \
 && apt-get install --no-install-recommends $buildDeps -y --no-install-suggests \
 && curl -fL -o /tmp/postfix.tar.gz http://cdn.postfix.johnriley.me/mirrors/postfix-release/official/postfix-3.1.3.tar.gz \
 && (echo "00e2b0974e59420cabfddc92597a99b42c8a8c9cd9a0c279c63ba6be9f40b15400f37dc16d0b1312130e72b5ba82b56fc7d579ee9ef975a957c0931b0401213c /tmp/postfix.tar.gz" | sha512sum -c - ) \
 && tar -xzf /tmp/postfix.tar.gz -C /tmp/ \
 && cd /tmp/postfix-* \
 && sed -i -e "s:/usr/local/:/usr/:g" conf/master.cf \
 && make makefiles CCARGS="-DHAS_SHL_LOAD -DUSE_TLS -DHAS_PCRE $( pcre-config --cflags ;) -DHAS_PGSQL -I/usr/include/postgresql -DHAS_MYSQL $( mysql_config --include ;) -DHAS_SQLITE -I/usr/include -DHAS_LDAP -I/usr/include -DUSE_CYRUS_SASL -I/usr/include/sasl -DUSE_SASL_AUTH -DDEF_SASL_SERVER=\"dovecot\" -DUSE_LDAP_SASL" AUXLIBS="-lssl -lcrypto -lsasl2" AUXLIBS_PCRE="$( pcre-config --libs ;)" AUXLIBS_PGSQL="-lpq" AUXLIBS_MYSQL="$( mysql_config --libs ;)" AUXLIBS_SQLITE="-lsqlite3 -lpthread" AUXLIBS_LDAP="-lldap -llber" shared=yes dynamicmaps=yes pie=yes daemon_directory=/usr/lib/postfix shlibs_directory=/usr/lib/postfix manpage_directory=/tmp/man readme_directory=/tmp/readme html_directory=/tmp/html \
 && make \
 && addgroup --system --gid 91 postfix \
 && adduser --system --uid 90 --disabled-password --no-create-home --home /var/spool/postfix --ingroup postfix --gecos postfix postfix \
 && adduser postfix mail \
 && addgroup --system --gid 93 postdrop \
 && adduser --system --uid 92 --disabled-password --shell /sbin/nologin --no-create-home --home /var/mail/domains --ingroup postdrop --gecos vmail vmail \
 && make upgrade \
 && chmod g+s /usr/sbin/postdrop /usr/sbin/postqueue \
 && install -d -o postfix -g postfix /var/spool/postfix \
 && sed -i -e 's,^manpage_directory =.*,manpage_directory = /dev/null,' -e 's,^readme_directory =.*,readme_directory = /dev/null,' -e 's,^html_directory =.*,html_directory = /dev/null,' /etc/postfix/main.cf \
 && install -d /etc/postfix/main.cf.d \
 && install -d /etc/postfix/master.cf.d \
 && install -d /etc/ssl/postfix \
 && openssl req -new -x509 -nodes -days 365 -subj "/CN=smtp.example.com" -out /etc/ssl/postfix/server.crt -keyout /etc/ssl/postfix/server.key \
 && chmod 0600 /etc/ssl/postfix/server.key \
 && openssl dhparam -out /etc/postfix/dh2048.pem 2048 \
 && echo "\n \n# TLS PARAMETERS \n# \ntls_ssl_options = NO_COMPRESSION \ntls_high_cipherlist = ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256 \n \n# SMTP TLS PARAMETERS (outgoing connections) \n# \nsmtp_tls_security_level = may \nsmtp_tls_CApath = /etc/ssl/certs \n \n# SMTPD TLS PARAMETERS (incoming connections) \n# \nsmtpd_tls_security_level = may \nsmtpd_tls_ciphers = high \nsmtpd_tls_mandatory_ciphers = high \nsmtpd_tls_exclude_ciphers = aNULL, LOW, EXP, MEDIUM, ADH, AECDH, MD5, DSS, ECDSA, CAMELLIA128, 3DES, CAMELLIA256, RSA+AES, eNULL \nsmtpd_tls_dh1024_param_file = /etc/postfix/dh2048.pem \nsmtpd_tls_CApath = /etc/ssl/certs \nsmtpd_tls_cert_file = /etc/ssl/postfix/server.crt \nsmtpd_tls_key_file = /etc/ssl/postfix/server.key " >> /etc/postfix/main.cf \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false $toolDeps $buildDeps \
 && rm -rf /var/lib/apt/lists/* /etc/*/inetutils-syslogd /tmp/*
#   Install s6-overlay
RUN apt-get update \
 && apt-get install --no-install-recommends curl -y --no-install-suggests \
 && curl -fL -o /tmp/s6-overlay.tar.gz https://github.com/just-containers/s6-overlay/releases/download/v1.21.2.2/s6-overlay-amd64.tar.gz \
 && mkdir -p /tmp/s6-overlay \
 && tar -xzf /tmp/s6-overlay.tar.gz -C /tmp/s6-overlay/ \
 && cp -rf /tmp/s6-overlay/bin/* /bin/ \
 && rm -rf /tmp/s6-overlay/bin /tmp/s6-overlay/usr/bin/execlineb \
 && cp -rf /tmp/s6-overlay/* / \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false curl \
 && rm -rf /var/lib/apt/lists/* /tmp/*
ENV S6_BEHAVIOUR_IF_STAGE2_FAILS="2" \
    S6_CMD_WAIT_FOR_SERVICES="1"
COPY rootfs /
RUN chmod +x /etc/services.d/*/run /etc/cont-init.d/*
EXPOSE 25/tcp 465/tcp 587/tcp
ENTRYPOINT ["/init"]
CMD ["/usr/lib/postfix/master", "-d"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!