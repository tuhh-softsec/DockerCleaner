FROM openjdk:8-jre
ENV CATALINA_HOME="/usr/local/tomcat"
ENV PATH="$CATALINA_HOME/bin:$PATH"
RUN mkdir -p "$CATALINA_HOME"
WORKDIR $CATALINA_HOME
#  let "Tomcat Native" live somewhere isolated
ENV TOMCAT_NATIVE_LIBDIR="$CATALINA_HOME/native-jni-lib"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$TOMCAT_NATIVE_LIBDIR"
#  runtime dependencies for Tomcat Native Libraries
#  Tomcat Native 1.2+ requires a newer version of OpenSSL than debian:jessie has available (1.0.2g+)
#  see http://tomcat.10.x6.nabble.com/VOTE-Release-Apache-Tomcat-8-0-32-tp5046007p5046024.html (and following discussion)
ENV OPENSSL_VERSION="1.1.0c-2"
RUN { echo 'deb http://deb.debian.org/debian stretch main' ; } > /etc/apt/sources.list.d/stretch.list \
 && { echo 'Package: *' ;echo 'Pin: release n=stretch' ;echo 'Pin-Priority: -10' ;echo ;echo 'Package: openssl libssl*' ;echo "Pin: version $OPENSSL_VERSION" ;echo 'Pin-Priority: 990' ; } > /etc/apt/preferences.d/stretch-openssl
RUN apt-get update \
 && apt-get install --no-install-recommends libapr1 openssl="$OPENSSL_VERSION" -y \
 && rm -rf /var/lib/apt/lists/*
#  see https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/KEYS
#  see also "update.sh" (https://github.com/docker-library/tomcat/blob/master/update.sh)
ENV GPG_KEYS="05AB33110949707C93A279E3D3EFE6B686867BA6 07E48665A34DCAFAE522E5E6266191C37C037D42 47309207D818FFD8DCD3F83F1931D684307A10A5 541FBE7D8F78B25E055DDEE13C370389288584E7 61B832AC2F1C5A90F0F9B00A1C506407564C17A3 713DA88BE50911535FE716F5208B0AB1D63011C7 79F7026C690BAA50B92CD8B66A3AD3F4F22C4FED 9BA44C2621385CB966EBA586F72C284D731FABEE A27677289986DB50844682F8ACB77FC2E86E29AC A9C5DF4D22E99998D9875A5110C01C5A2F6059E7 DCFD35E0BF8CA7344752DE8B6FB21E8933C60243 F3A04C595DB5B6A5F1ECA43E3B7BBB100D811BBE F7DA48BB64BCB84ECBA7EE6935CD23C10D498E23"
RUN set -ex ; for key in $GPG_KEYS; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done
ENV TOMCAT_MAJOR="7"
ENV TOMCAT_VERSION="7.0.73"
#  https://issues.apache.org/jira/browse/INFRA-8753?focusedCommentId=14735394#comment-14735394
ENV TOMCAT_TGZ_URL="https://www.apache.org/dyn/closer.cgi?action=download&filename=tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz"
#  not all the mirrors actually carry the .asc files :'(
ENV TOMCAT_ASC_URL="https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc"
RUN set -x \
 && wget -O tomcat.tar.gz "$TOMCAT_TGZ_URL" \
 && wget -O tomcat.tar.gz.asc "$TOMCAT_ASC_URL" \
 && gpg --batch --verify tomcat.tar.gz.asc tomcat.tar.gz \
 && tar -xvf tomcat.tar.gz --strip-components=1 \
 && rm bin/*.bat \
 && rm tomcat.tar.gz* \
 && nativeBuildDir="$( mktemp -d ;)" \
 && tar -xvf bin/tomcat-native.tar.gz -C "$nativeBuildDir" --strip-components=1 \
 && nativeBuildDeps=" gcc libapr1-dev libssl-dev make openjdk-${JAVA_VERSION%%[-~bu]*}-jdk=$JAVA_DEBIAN_VERSION " \
 && apt-get update \
 && apt-get install --no-install-recommends $nativeBuildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && (export CATALINA_HOME="$PWD" \
 && cd "$nativeBuildDir/native" \
 && ./configure --libdir="$TOMCAT_NATIVE_LIBDIR" --prefix="$CATALINA_HOME" --with-apr="$( which apr-1-config ;)" --with-java-home="$( docker-java-home ;)" --with-ssl=yes \
 && make -j$( nproc ;) \
 && make install ) \
 && apt-get purge -y --auto-remove $nativeBuildDeps \
 && rm -rf "$nativeBuildDir" \
 && rm bin/tomcat-native.tar.gz
#  verify Tomcat Native is working properly
RUN set -e \
 && nativeLines="$( catalina.sh configtest 2>&1;)" \
 && nativeLines="$( echo "$nativeLines" | grep 'Apache Tomcat Native' ;)" \
 && nativeLines="$( echo "$nativeLines" | sort -u ;)" \
 && if ! echo "$nativeLines" | grep 'INFO: Loaded APR based Apache Tomcat Native library' >&2; then echo "$nativeLines" >&2;exit 1 ; fi
COPY ./build/libs/helloworld-1.0.war /usr/local/tomcat/webapps/helloworld.war
EXPOSE 8080/tcp
CMD ["catalina.sh", "run"]
