#  upstream https://github.com/frol/docker-alpine-oraclejdk8
FROM alpine:3.9
MAINTAINER 若虚 <slpcat@qq.com>
#   Container variables
ENV TERM="xterm" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    TIMEZONE="Asia/Shanghai"
RUN sed -i 's/dl-cdn.alpinelinux.org/mirrors.aliyun.com/' /etc/apk/repositories
#   Set timezone and locales
RUN set -ex \
 && apk update \
 && apk upgrade \
 && apk add bash=4.4.19-r1 tzdata=2020c-r1 vim=8.1.1365-r0 tini=0.18.0-r0 su-exec=0.2-r0 gzip=1.10-r0 tar=1.32-r0 wget=1.20.3-r0 curl=7.64.0-r5 \
 && echo "${TIMEZONE}" > /etc/TZ \
 && ln -sf /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && echo 'hosts: files mdns4_minimal [NOTFOUND=return] dns mdns4' >> /etc/nsswitch.conf
#   Here we install GNU libc (aka glibc) and set en_US.UTF-8 locale as default.
RUN ALPINE_GLIBC_BASE_URL="https://github.com/sgerrand/alpine-pkg-glibc/releases/download" \
 && ALPINE_GLIBC_PACKAGE_VERSION="2.28-r0" \
 && ALPINE_GLIBC_BASE_PACKAGE_FILENAME="glibc-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && ALPINE_GLIBC_BIN_PACKAGE_FILENAME="glibc-bin-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && ALPINE_GLIBC_I18N_PACKAGE_FILENAME="glibc-i18n-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && apk add wget=1.20.3-r0 ca-certificates=20191127-r2 --no-cache --virtual=.build-dependencies \
 && wget "https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub" -O "/etc/apk/keys/sgerrand.rsa.pub" \
 && wget "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_I18N_PACKAGE_FILENAME" \
 && apk add "$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_I18N_PACKAGE_FILENAME" --no-cache \
 && rm "/etc/apk/keys/sgerrand.rsa.pub" \
 && /usr/glibc-compat/bin/localedef --force --inputfile POSIX --charmap UTF-8 "$LANG" || true \
 && echo "export LANG=$LANG" > /etc/profile.d/locale.sh \
 && apk del glibc-i18n \
 && rm "/root/.wget-hsts" \
 && apk del .build-dependencies \
 && rm "$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_I18N_PACKAGE_FILENAME"
ENV JAVA_VERSION="8" \
    JAVA_UPDATE="211" \
    JAVA_BUILD="12" \
    JAVA_PATH="478a62b7d4e34b78b671c754eaaf38ab" \
    JAVA_HOME="/usr/lib/jvm/default-jvm"
RUN apk add wget=1.20.3-r0 ca-certificates=20191127-r2 unzip=6.0-r6 --no-cache --virtual=build-dependencies \
 && cd "/tmp" \
 && wget --header "Cookie: oraclelicense=accept-securebackup-cookie;" "http://download.oracle.com/otn-pub/java/jdk/${JAVA_VERSION}u${JAVA_UPDATE}-b${JAVA_BUILD}/${JAVA_PATH}/jdk-${JAVA_VERSION}u${JAVA_UPDATE}-linux-x64.tar.gz" \
 && tar -xzf "jdk-${JAVA_VERSION}u${JAVA_UPDATE}-linux-x64.tar.gz" \
 && mkdir -p "/usr/lib/jvm" \
 && mv "/tmp/jdk1.${JAVA_VERSION}.0_${JAVA_UPDATE}" "/usr/lib/jvm/java-${JAVA_VERSION}-oracle" \
 && ln -s "java-${JAVA_VERSION}-oracle" "$JAVA_HOME" \
 && ln -s "$JAVA_HOME/bin/"* "/usr/bin/" \
 && rm -rf "$JAVA_HOME/"*src.zip \
 && rm -rf "$JAVA_HOME/lib/missioncontrol" "$JAVA_HOME/lib/visualvm" "$JAVA_HOME/lib/"*javafx* "$JAVA_HOME/jre/lib/plugin.jar" "$JAVA_HOME/jre/lib/ext/jfxrt.jar" "$JAVA_HOME/jre/bin/javaws" "$JAVA_HOME/jre/lib/javaws.jar" "$JAVA_HOME/jre/lib/desktop" "$JAVA_HOME/jre/plugin" "$JAVA_HOME/jre/lib/"deploy* "$JAVA_HOME/jre/lib/"*javafx* "$JAVA_HOME/jre/lib/"*jfx* "$JAVA_HOME/jre/lib/amd64/libdecora_sse.so" "$JAVA_HOME/jre/lib/amd64/"libprism_*.so "$JAVA_HOME/jre/lib/amd64/libfxplugins.so" "$JAVA_HOME/jre/lib/amd64/libglass.so" "$JAVA_HOME/jre/lib/amd64/libgstreamer-lite.so" "$JAVA_HOME/jre/lib/amd64/"libjavafx*.so "$JAVA_HOME/jre/lib/amd64/"libjfx*.so \
 && rm -rf "$JAVA_HOME/jre/bin/jjs" "$JAVA_HOME/jre/bin/keytool" "$JAVA_HOME/jre/bin/orbd" "$JAVA_HOME/jre/bin/pack200" "$JAVA_HOME/jre/bin/policytool" "$JAVA_HOME/jre/bin/rmid" "$JAVA_HOME/jre/bin/rmiregistry" "$JAVA_HOME/jre/bin/servertool" "$JAVA_HOME/jre/bin/tnameserv" "$JAVA_HOME/jre/bin/unpack200" "$JAVA_HOME/jre/lib/ext/nashorn.jar" "$JAVA_HOME/jre/lib/jfr.jar" "$JAVA_HOME/jre/lib/jfr" "$JAVA_HOME/jre/lib/oblique-fonts" \
 && wget --header "Cookie: oraclelicense=accept-securebackup-cookie;" "http://download.oracle.com/otn-pub/java/jce/${JAVA_VERSION}/jce_policy-${JAVA_VERSION}.zip" \
 && unzip -jo -d "${JAVA_HOME}/jre/lib/security" "jce_policy-${JAVA_VERSION}.zip" \
 && rm "${JAVA_HOME}/jre/lib/security/README.txt" \
 && apk del build-dependencies \
 && rm "/tmp/"*
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!