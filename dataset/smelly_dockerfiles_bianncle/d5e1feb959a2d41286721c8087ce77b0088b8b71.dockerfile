#  AUTOMATICALLY GENERATED
#  DO NOT EDIT THIS FILE DIRECTLY, USE /Dockerfile.tmpl.php
#  https://hub.docker.com/_/alpine
FROM alpine:3.5
MAINTAINER Instrumentisto Team <developer@instrumentisto.com>
#  Build and install Postfix
#  https://git.alpinelinux.org/cgit/aports/tree/main/postfix/APKBUILD?h=2b1512eefca296b0ef1b60d2e521349385a3c353
RUN apk update \
 && apk upgrade \
 && apk add --no-cache ca-certificates \
 && update-ca-certificates \
 && apk add --no-cache pcre icu-libs db libpq mariadb-client-libs sqlite-libs libsasl libldap \
 && apk add --no-cache --virtual .tool-deps curl coreutils autoconf g++ libtool make libressl \
 && apk add --no-cache --virtual .build-deps libressl-dev linux-headers pcre-dev icu-dev db-dev postgresql-dev mariadb-dev sqlite-dev cyrus-sasl-dev openldap-dev \
 && curl -fL -o /tmp/postfix.tar.gz http://cdn.postfix.johnriley.me/mirrors/postfix-release/official/postfix-3.1.3.tar.gz \
 && (echo "00e2b0974e59420cabfddc92597a99b42c8a8c9cd9a0c279c63ba6be9f40b15400f37dc16d0b1312130e72b5ba82b56fc7d579ee9ef975a957c0931b0401213c /tmp/postfix.tar.gz" | sha512sum -c - ) \
 && tar -xzf /tmp/postfix.tar.gz -C /tmp/ \
 && cd /tmp/postfix-* \
 && curl -fL -o ./no-glibc.patch https://git.alpinelinux.org/cgit/aports/plain/main/postfix/no-glibc.patch?h=2b1512eefca296b0ef1b60d2e521349385a3c353 \
 && patch -p1 -i ./no-glibc.patch \
 && curl -fL -o ./postfix-install.patch https://git.alpinelinux.org/cgit/aports/plain/main/postfix/postfix-install.patch?h=2b1512eefca296b0ef1b60d2e521349385a3c353 \
 && patch -p1 -i ./postfix-install.patch \
 && curl -fL -o ./libressl.patch https://git.alpinelinux.org/cgit/aports/plain/main/postfix/libressl.patch?h=2b1512eefca296b0ef1b60d2e521349385a3c353 \
 && patch -p1 -i ./libressl.patch \
 && sed -i -e "s|#define HAS_NIS|//#define HAS_NIS|g" -e "/^#define ALIAS_DB_MAP/s|:/etc/aliases|:/etc/postfix/aliases|" src/util/sys_defs.h \
 && sed -i -e "s:/usr/local/:/usr/:g" conf/master.cf \
 && make makefiles CCARGS="-DHAS_SHL_LOAD -DUSE_TLS -DHAS_PCRE $( pkg-config --cflags libpcre ;) -DHAS_PGSQL $( pkg-config --cflags libpq ;) -DHAS_MYSQL $( mysql_config --include ;) -DHAS_SQLITE $( pkg-config --cflags sqlite3 ;) -DHAS_LDAP -DUSE_CYRUS_SASL -I/usr/include/sasl -DUSE_SASL_AUTH -DDEF_SASL_SERVER=\\"dovecot\\" -DUSE_LDAP_SASL" AUXLIBS="-lssl -lcrypto -lsasl2" AUXLIBS_PCRE="$( pkg-config --libs libpcre ;)" AUXLIBS_PGSQL="$( pkg-config --libs libpq ;)" AUXLIBS_MYSQL="$( mysql_config --libs ;)" AUXLIBS_SQLITE="$( pkg-config --libs sqlite3 ;)" AUXLIBS_LDAP="-lldap -llber" shared=yes dynamicmaps=yes pie=yes daemon_directory=/usr/lib/postfix shlibs_directory=/usr/lib/postfix manpage_directory=/tmp/man readme_directory=/tmp/readme html_directory=/tmp/html \
 && make \
 && addgroup -S -g 91 postfix \
 && adduser -S -u 90 -D -H -h /var/spool/postfix -G postfix -g postfix postfix \
 && addgroup postfix mail \
 && addgroup -S -g 93 postdrop \
 && adduser -S -u 92 -D -s /sbin/nologin -H -h /var/mail/domains -G postdrop -g vmail vmail \
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
 && echo -e "\n \n# TLS PARAMETERS \n# \ntls_ssl_options = NO_COMPRESSION \ntls_high_cipherlist = ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256 \n \n# SMTP TLS PARAMETERS (outgoing connections) \n# \nsmtp_tls_security_level = may \nsmtp_tls_CApath = /etc/ssl/certs \n \n# SMTPD TLS PARAMETERS (incoming connections) \n# \nsmtpd_tls_security_level = may \nsmtpd_tls_ciphers = high \nsmtpd_tls_mandatory_ciphers = high \nsmtpd_tls_exclude_ciphers = aNULL, LOW, EXP, MEDIUM, ADH, AECDH, MD5, DSS, ECDSA, CAMELLIA128, 3DES, CAMELLIA256, RSA+AES, eNULL \nsmtpd_tls_dh1024_param_file = /etc/postfix/dh2048.pem \nsmtpd_tls_CApath = /etc/ssl/certs \nsmtpd_tls_cert_file = /etc/ssl/postfix/server.crt \nsmtpd_tls_key_file = /etc/ssl/postfix/server.key " >> /etc/postfix/main.cf \
 && apk del .tool-deps .build-deps \
 && rm -rf /var/cache/apk/* /tmp/*
#  Install s6-overlay
RUN apk add --update --no-cache --virtual .tool-deps curl \
 && curl -fL -o /tmp/s6-overlay.tar.gz https://github.com/just-containers/s6-overlay/releases/download/v1.21.2.2/s6-overlay-amd64.tar.gz \
 && tar -xzf /tmp/s6-overlay.tar.gz -C / \
 && apk del .tool-deps \
 && rm -rf /var/cache/apk/* /tmp/*
ENV S6_BEHAVIOUR_IF_STAGE2_FAILS="2" \
    S6_CMD_WAIT_FOR_SERVICES="1"
COPY rootfs /
RUN chmod +x /etc/services.d/*/run /etc/cont-init.d/*
EXPOSE 25/tcp 465/tcp 587/tcp
ENTRYPOINT ["/init"]
CMD ["/usr/lib/postfix/master", "-d"]
