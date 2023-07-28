FROM openjdk:11-jdk-slim
ENV CATALINA_HOME="/usr/local/tomcat"
ENV PATH="$CATALINA_HOME/bin:$PATH"
RUN mkdir -p "$CATALINA_HOME"
WORKDIR $CATALINA_HOME
#   let "Tomcat Native" live somewhere isolated
ENV TOMCAT_NATIVE_LIBDIR="$CATALINA_HOME/native-jni-lib"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$TOMCAT_NATIVE_LIBDIR"
#   see https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/KEYS
#   see also "update.sh" (https://github.com/docker-library/tomcat/blob/master/update.sh)
ENV GPG_KEYS="05AB33110949707C93A279E3D3EFE6B686867BA6 07E48665A34DCAFAE522E5E6266191C37C037D42 47309207D818FFD8DCD3F83F1931D684307A10A5 541FBE7D8F78B25E055DDEE13C370389288584E7 61B832AC2F1C5A90F0F9B00A1C506407564C17A3 79F7026C690BAA50B92CD8B66A3AD3F4F22C4FED 9BA44C2621385CB966EBA586F72C284D731FABEE A27677289986DB50844682F8ACB77FC2E86E29AC A9C5DF4D22E99998D9875A5110C01C5A2F6059E7 DCFD35E0BF8CA7344752DE8B6FB21E8933C60243 F3A04C595DB5B6A5F1ECA43E3B7BBB100D811BBE F7DA48BB64BCB84ECBA7EE6935CD23C10D498E23"
ENV TOMCAT_MAJOR="9"
ENV TOMCAT_VERSION="9.0.21"
ENV TOMCAT_SHA512="a8788ba8187f940b55d3db6cb0108943b8b48aecc2d9a3307409d3cbf72fac9b19fa434baa97aa0d4ac552f7c13967932b8a36dbae5d582bc14ed13bb058ea9b"
ENV TOMCAT_TGZ_URLS="https://www.apache.org/dyn/closer.cgi?action=download&filename=tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz  https://www-us.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz  https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz  https://archive.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz"
ENV TOMCAT_ASC_URLS="https://www.apache.org/dyn/closer.cgi?action=download&filename=tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc  https://www-us.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc  https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc  https://archive.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc"
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends gnupg=2.2.27-2+deb11u2 dirmngr=2.2.27-2+deb11u2 -y ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done ; apt-get install --no-install-recommends wget=1.21-1+deb11u1 ca-certificates=20210119 -y ; success= ; for url in $TOMCAT_TGZ_URLS; do if wget -O tomcat.tar.gz "$url" ; then success=1 ;break ; fi ; done ; [ -n "$success" ] ; echo "$TOMCAT_SHA512 *tomcat.tar.gz" | sha512sum --strict --check - ; success= ; for url in $TOMCAT_ASC_URLS; do if wget -O tomcat.tar.gz.asc "$url" ; then success=1 ;break ; fi ; done ; [ -n "$success" ] ; gpg --batch --verify tomcat.tar.gz.asc tomcat.tar.gz ; tar -xvf tomcat.tar.gz --strip-components=1 ; rm bin/*.bat ; rm tomcat.tar.gz* ; command -v gpgconf \
 && gpgconf --kill all || : ; rm -rf "$GNUPGHOME" ; nativeBuildDir="$( mktemp -d ;)" ; tar -xvf bin/tomcat-native.tar.gz -C "$nativeBuildDir" --strip-components=1 ; apt-get install --no-install-recommends dpkg-dev=1.20.12 gcc=4:10.2.1-1 libapr1-dev=1.7.0-6+deb11u2 libssl-dev=1.1.1n-0+deb11u4 make=4.3-4.1 -y ; (export CATALINA_HOME="$PWD" ;cd "$nativeBuildDir/native" ;gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ;aprConfig="$( which apr-1-config ;)" ;./configure --build="$gnuArch" --libdir="$TOMCAT_NATIVE_LIBDIR" --prefix="$CATALINA_HOME" --with-apr="$aprConfig" --with-java-home="$JAVA_HOME" --with-ssl=yes ;make -j "$( nproc ;)" ;make install ) ; rm -rf "$nativeBuildDir" ; rm bin/tomcat-native.tar.gz ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; find "$TOMCAT_NATIVE_LIBDIR" -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/* ; find ./bin/ -name '*.sh' -exec sed -ri 's|^#!/bin/sh$|#!/usr/bin/env bash|' '{}' + ; chmod -R +rX . ; chmod 777 logs work
#   verify Tomcat Native is working properly
RUN set -e \
 && nativeLines="$( catalina.sh configtest 2>&1;)" \
 && nativeLines="$( echo "$nativeLines" | grep 'Apache Tomcat Native' ;)" \
 && nativeLines="$( echo "$nativeLines" | sort -u ;)" \
 && if ! echo "$nativeLines" | grep 'INFO: Loaded APR based Apache Tomcat Native library' >&2; then echo "$nativeLines" >&2;exit 1 ; fi
EXPOSE 8080/tcp
CMD ["catalina.sh", "run"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
