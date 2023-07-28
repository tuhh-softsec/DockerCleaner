#  upstream https://github.com/docker-library/tomcat/tree/master/8.5/
FROM alpine:3.8
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
 && apk add bash=4.4.19-r1 tzdata=2020a-r0 vim=8.1.1365-r0 tini=0.18.0-r0 su-exec=0.2-r0 gzip=1.9-r0 tar=1.32-r0 wget=1.20.3-r0 curl=7.61.1-r3 \
 && echo "${TIMEZONE}" > /etc/TZ \
 && ln -sf /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && echo 'hosts: files mdns4_minimal [NOTFOUND=return] dns mdns4' >> /etc/nsswitch.conf
#   Here we install GNU libc (aka glibc) and set en_US.UTF-8 locale as default.
RUN ALPINE_GLIBC_BASE_URL="https://github.com/sgerrand/alpine-pkg-glibc/releases/download" \
 && ALPINE_GLIBC_PACKAGE_VERSION="2.27-r0" \
 && ALPINE_GLIBC_BASE_PACKAGE_FILENAME="glibc-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && ALPINE_GLIBC_BIN_PACKAGE_FILENAME="glibc-bin-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && ALPINE_GLIBC_I18N_PACKAGE_FILENAME="glibc-i18n-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && apk add wget=1.20.3-r0 ca-certificates=20191127-r2 --no-cache --virtual=.build-dependencies \
 && wget "https://raw.githubusercontent.com/sgerrand/alpine-pkg-glibc/master/sgerrand.rsa.pub" -O "/etc/apk/keys/sgerrand.rsa.pub" \
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
    JAVA_UPDATE="161" \
    JAVA_BUILD="12" \
    JAVA_PATH="2f38c3b165be4555a1fa6e98c45e0808" \
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
ENV CATALINA_HOME="/usr/local/tomcat"
ENV PATH="$CATALINA_HOME/bin:$PATH"
RUN mkdir -p "$CATALINA_HOME"
WORKDIR $CATALINA_HOME
#   let "Tomcat Native" live somewhere isolated
ENV TOMCAT_NATIVE_LIBDIR="$CATALINA_HOME/native-jni-lib"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$TOMCAT_NATIVE_LIBDIR"
#   see https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/KEYS
#   see also "update.sh" (https://github.com/docker-library/tomcat/blob/master/update.sh)
ENV GPG_KEYS="05AB33110949707C93A279E3D3EFE6B686867BA6 07E48665A34DCAFAE522E5E6266191C37C037D42 47309207D818FFD8DCD3F83F1931D684307A10A5 541FBE7D8F78B25E055DDEE13C370389288584E7 61B832AC2F1C5A90F0F9B00A1C506407564C17A3 713DA88BE50911535FE716F5208B0AB1D63011C7 79F7026C690BAA50B92CD8B66A3AD3F4F22C4FED 9BA44C2621385CB966EBA586F72C284D731FABEE A27677289986DB50844682F8ACB77FC2E86E29AC A9C5DF4D22E99998D9875A5110C01C5A2F6059E7 DCFD35E0BF8CA7344752DE8B6FB21E8933C60243 F3A04C595DB5B6A5F1ECA43E3B7BBB100D811BBE F7DA48BB64BCB84ECBA7EE6935CD23C10D498E23"
ENV TOMCAT_MAJOR="8"
ENV TOMCAT_VERSION="8.0.50"
ENV TOMCAT_SHA512="c8923e610f1d3f7f13d5d0cc765a97ef1b5b34a527b57dfb7058f0bd65555cec748b83198859e6bba41b29cc3d5071ed05618097edc5d6cfad2f380f8230e7f5"
ENV TOMCAT_TGZ_URLS="https://www.apache.org/dyn/closer.cgi?action=download&filename=tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz  https://www-us.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz  https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz  https://archive.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz"
ENV TOMCAT_ASC_URLS="https://www.apache.org/dyn/closer.cgi?action=download&filename=tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc  https://www-us.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc  https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc  https://archive.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc"
RUN set -eux ; apk add gnupg=2.2.19-r0 ca-certificates=20191127-r2 openssl=1.0.2u-r0 --no-cache --virtual .fetch-deps ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do gpg --keyserver keyserver.ubuntu.com --recv-keys "$key" ; done ; success= ; for url in $TOMCAT_TGZ_URLS; do if wget -O tomcat.tar.gz "$url" ; then success=1 ;break ; fi ; done ; [ -n "$success" ] ; echo "$TOMCAT_SHA512 *tomcat.tar.gz" | sha512sum -c - ; success= ; for url in $TOMCAT_ASC_URLS; do if wget -O tomcat.tar.gz.asc "$url" ; then success=1 ;break ; fi ; done ; [ -n "$success" ] ; gpg --batch --verify tomcat.tar.gz.asc tomcat.tar.gz ; tar -xvf tomcat.tar.gz --strip-components=1 ; rm bin/*.bat ; rm tomcat.tar.gz* ; rm -rf "$GNUPGHOME" ; nativeBuildDir="$( mktemp -d ;)" ; tar -xvf bin/tomcat-native.tar.gz -C "$nativeBuildDir" --strip-components=1 ; apk add apr-dev=1.6.3-r1 coreutils=8.29-r2 dpkg-dev=1.18.24-r0 dpkg=1.18.24-r0 gcc=6.4.0-r9 libc-dev=0.7.1-r0 make=4.2.1-r2 openssl-dev=1.0.2u-r0 --no-cache --virtual .native-build-deps ; (export CATALINA_HOME="$PWD" ;cd "$nativeBuildDir/native" ;gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ;./configure --build="$gnuArch" --libdir="$TOMCAT_NATIVE_LIBDIR" --prefix="$CATALINA_HOME" --with-apr="$( which apr-1-config ;)" --with-java-home="$JAVA_HOME" --with-ssl=yes ;make -j "$( nproc ;)" ;make install ) ; rm -rf "$nativeBuildDir" ; rm bin/tomcat-native.tar.gz ; runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive "$TOMCAT_NATIVE_LIBDIR" | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --virtual .tomcat-native-rundeps ; apk del .fetch-deps .native-build-deps ; apk add bash=4.4.19-r1 --no-cache ; find ./bin/ -name '*.sh' -exec sed -ri 's|^#!/bin/sh$|#!/usr/bin/env bash|' '{}' +
#   verify Tomcat Native is working properly
RUN set -e \
 && nativeLines="$( catalina.sh configtest 2>&1;)" \
 && nativeLines="$( echo "$nativeLines" | grep 'Apache Tomcat Native' ;)" \
 && nativeLines="$( echo "$nativeLines" | sort -u ;)" \
 && if ! echo "$nativeLines" | grep 'INFO: Loaded APR based Apache Tomcat Native library' >&2; then echo "$nativeLines" >&2;exit 1 ; fi
EXPOSE 8080/tcp
CMD ["catalina.sh", "run"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
