FROM alpine:3.6
MAINTAINER Sofia Karvounari <karvoun@di.uoa.gr>
#   Here we install GNU libc (aka glibc) and set C.UTF-8 locale as default.
RUN ALPINE_GLIBC_BASE_URL="https://github.com/sgerrand/alpine-pkg-glibc/releases/download" \
 && ALPINE_GLIBC_PACKAGE_VERSION="2.23-r2" \
 && ALPINE_GLIBC_BASE_PACKAGE_FILENAME="glibc-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && ALPINE_GLIBC_BIN_PACKAGE_FILENAME="glibc-bin-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && ALPINE_GLIBC_I18N_PACKAGE_FILENAME="glibc-i18n-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && apk add wget=1.20.3-r0 ca-certificates=20161130-r3 --no-cache --virtual=build-dependencies \
 && wget "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_I18N_PACKAGE_FILENAME" \
 && apk add "$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_I18N_PACKAGE_FILENAME" --allow-untrusted --no-cache \
 && /usr/glibc-compat/bin/localedef --force --inputfile POSIX --charmap UTF-8 C.UTF-8 || true \
 && echo "export LANG=C.UTF-8" > /etc/profile.d/locale.sh \
 && apk del glibc-i18n \
 && apk del build-dependencies \
 && rm "$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_I18N_PACKAGE_FILENAME"
ENV LANG="C.UTF-8"
#  #######################################################
#   Install Java (Shamelessly copy pasted from develar/java,
#   https://github.com/develar/docker-java/blob/master/Dockerfile)
#
#   LSC: Updated for new URLs schemes on the Oracle website.
ENV JAVA_VERSION_MAJOR="8" \
    JAVA_VERSION_MINOR="141" \
    JAVA_VERSION_BUILD="15" \
    JAVA_VERSION_HASH="336fa29ff2bb4ef291e347e091f7f4a7" \
    JAVA_PACKAGE="server-jre" \
    JAVA_HOME="/jre" \
    PATH="${PATH}:/jre/bin" \
    LANG="C.UTF-8"
#   about nsswitch.conf - see https://registry.hub.docker.com/u/frolvlad/alpine-oraclejdk8/dockerfile/
#  /usr/glibc/usr/bin/ldconfig /lib /usr/glibc/usr/lib && \
RUN apk add curl=7.61.1-r2 ca-certificates=20161130-r3 --update \
 && cd /tmp \
 && echo 'hosts: files mdns4_minimal [NOTFOUND=return] dns mdns4' >> /etc/nsswitch.conf \
 && curl -jksSLH "Cookie: oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-b${JAVA_VERSION_BUILD}/${JAVA_VERSION_HASH}/${JAVA_PACKAGE}-${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-linux-x64.tar.gz" | gunzip -c - | tar -xf - \
 && apk del curl ca-certificates \
 && mv jdk1.${JAVA_VERSION_MAJOR}.0_${JAVA_VERSION_MINOR}/jre /jre \
 && rm /jre/bin/jjs \
 && rm /jre/bin/keytool \
 && rm /jre/bin/orbd \
 && rm /jre/bin/pack200 \
 && rm /jre/bin/policytool \
 && rm /jre/bin/rmid \
 && rm /jre/bin/rmiregistry \
 && rm /jre/bin/servertool \
 && rm /jre/bin/tnameserv \
 && rm /jre/bin/unpack200 \
 && rm /jre/lib/ext/nashorn.jar \
 && rm /jre/lib/jfr.jar \
 && rm -rf /jre/lib/jfr \
 && rm -rf /jre/lib/oblique-fonts \
 && rm -rf /tmp/* /var/cache/apk/*
#   Some extra python libraries for the mip-algorithms, which needs to be
#   compiled by hand
COPY files/requirements.txt /root/requirements.txt
RUN apk add py-psycopg2=2.7.1-r0 py-pip ca-certificates=20161130-r3 gcc=6.3.0-r4 musl-dev=1.1.16-r15 python-dev lapack-dev=3.7.0-r0 g++=6.3.0-r4 gfortran=6.3.0-r4 --update \
 && pip install -r /root/requirements.txt \
 && pip install liac-arff==2.5.0 \
 && apk del py-pip ca-certificates gcc musl-dev python-dev lapack-dev gfortran \
 && rm -rf /tmp/* /var/cache/apk/*
#   Runtime dependencies for Exareme
RUN apk add rsync=3.1.3-r0 curl=7.61.1-r2 bash=4.3.48-r1 jq=1.5-r4 python py-requests=2.13.0-r0 lapack=3.7.0-r0 procps=3.3.12-r1 --update --no-cache \
 && rm -rf /tmp/* /var/cache/apk/*
#   Add Exareme
COPY src/exareme/exareme-distribution/target/exareme /root/exareme
#   Add the algorithms
COPY src/mip-algorithms /root/mip-algorithms
#   Exareme configuration, ssh keys and so on
#   This has to be done after copying in the algorithms and exareme, as some
#   files are placed in folders created by those two steps.
COPY files/java.sh /etc/profile.d/java.sh
RUN chmod 755 /etc/profile.d/java.sh
COPY files/root /root
RUN chmod -R 755 /root/exareme/
EXPOSE 9090/tcp
EXPOSE 22/tcp
ENV USER="root"
WORKDIR /root/exareme
CMD ["/bin/bash", "bootstrap.sh"]
#   While debugging
#  ENTRYPOINT /bin/sh
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
