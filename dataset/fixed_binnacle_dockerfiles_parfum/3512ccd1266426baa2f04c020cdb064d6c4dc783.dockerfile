#  Inspired by: https://dzone.com/articles/a-dockerfile-for-maven-based-github-projects
# ###########################################################################################################################
#  Checkout the latest commit with GIT AS build.
# ###########################################################################################################################
FROM alpine/git AS git
ARG RELEASE_VERSION
WORKDIR /
RUN git clone https://github.com/CodeDefenders/CodeDefenders.git
WORKDIR /CodeDefenders
#  Checkout the right tag
RUN git checkout ${RELEASE_VERSION}
#  Collect label values to file since we cannot tag containers from within Docker
RUN echo "export GIT_COMMIT=$( git rev-parse HEAD ;)" > .git-data \
 && echo "export GIT_DATE=\"$( git log -1 --format=%cd ;)\"" >> .git-data
# ###########################################################################################################################
#  Build the WAR file with Maven and create the data folder structure
# ###########################################################################################################################
FROM maven:3.5-jdk-8-alpine AS build
COPY --from=git /CodeDefenders /CodeDefenders
WORKDIR /CodeDefenders
#  Build Code Defenders for Docker using config.properties@docker
RUN cp config.properties@docker config.properties \
 && mvn clean compile package war:war -DskipTests
#  Run the file system setup script
RUN cd /CodeDefenders/src/test/resources/systemtests/frontend \
 && ./setup-filesystem.sh /CodeDefenders/config.properties
# ###########################################################################################################################
#  Install tomcat on JDK, ant, and copy over codedefenders
# ###########################################################################################################################
FROM openjdk:8-jdk AS tomcat
# ## INSTALL TOMCAT
ENV CATALINA_HOME="/usr/local/tomcat"
ENV PATH="$CATALINA_HOME/bin:$PATH"
RUN mkdir -p "$CATALINA_HOME"
WORKDIR $CATALINA_HOME
#  let "Tomcat Native" live somewhere isolated
ENV TOMCAT_NATIVE_LIBDIR="$CATALINA_HOME/native-jni-lib"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$TOMCAT_NATIVE_LIBDIR"
#  runtime dependencies for Tomcat Native Libraries
#  Tomcat Native 1.2+ requires a newer version of OpenSSL than debian:jessie has available
#  > checking OpenSSL library version >= 1.0.2...
#  > configure: error: Your version of OpenSSL is not compatible with this version of tcnative
#  see http://tomcat.10.x6.nabble.com/VOTE-Release-Apache-Tomcat-8-0-32-tp5046007p5046024.html (and following discussion)
#  and https://github.com/docker-library/tomcat/pull/31
ENV OPENSSL_VERSION="1.1.0f-3+deb9u2"
RUN set -ex ; currentVersion="$( dpkg-query --show --showformat '${Version}\n' openssl ;)" ; if dpkg --compare-versions "$currentVersion" '<<' "$OPENSSL_VERSION" ; then if ! grep -q stretch /etc/apt/sources.list ; then { echo 'deb http://deb.debian.org/debian stretch main' ;echo 'deb http://security.debian.org stretch/updates main' ;echo 'deb http://deb.debian.org/debian stretch-updates main' ; } > /etc/apt/sources.list.d/stretch.list;{ echo 'Package: *' ;echo 'Pin: release n=stretch*' ;echo 'Pin-Priority: -10' ;echo ;echo 'Package: openssl libssl*' ;echo "Pin: version $OPENSSL_VERSION" ;echo 'Pin-Priority: 990' ; } > /etc/apt/preferences.d/stretch-openssl; fi ;apt-get update ;apt-get install --no-install-recommends openssl="$OPENSSL_VERSION" -y ;rm -rf /var/lib/apt/lists/* ; fi
RUN apt-get update \
 && apt-get install --no-install-recommends libapr1 -y \
 && rm -rf /var/lib/apt/lists/*
#  see https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/KEYS
#  see also "update.sh" (https://github.com/docker-library/tomcat/blob/master/update.sh)
ENV GPG_KEYS="05AB33110949707C93A279E3D3EFE6B686867BA6 07E48665A34DCAFAE522E5E6266191C37C037D42 47309207D818FFD8DCD3F83F1931D684307A10A5 541FBE7D8F78B25E055DDEE13C370389288584E7 61B832AC2F1C5A90F0F9B00A1C506407564C17A3 79F7026C690BAA50B92CD8B66A3AD3F4F22C4FED 9BA44C2621385CB966EBA586F72C284D731FABEE A27677289986DB50844682F8ACB77FC2E86E29AC A9C5DF4D22E99998D9875A5110C01C5A2F6059E7 DCFD35E0BF8CA7344752DE8B6FB21E8933C60243 F3A04C595DB5B6A5F1ECA43E3B7BBB100D811BBE F7DA48BB64BCB84ECBA7EE6935CD23C10D498E23"
ENV TOMCAT_MAJOR="9"
ENV TOMCAT_VERSION="9.0.8"
ENV TOMCAT_SHA512="51af864fc3815bf40a200b8250f6e6aad66378f5b725199a308c0296957c58584a6358fb7df861694acea764b69ba4ce7dba0aa77792bd33ed38b66c24ff5b55"
ENV TOMCAT_TGZ_URLS="https://www.apache.org/dyn/closer.cgi?action=download&filename=tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz  https://www-us.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz  https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz  https://archive.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz"
ENV TOMCAT_ASC_URLS="https://www.apache.org/dyn/closer.cgi?action=download&filename=tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc  https://www-us.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc  https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc  https://archive.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc"
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends gnupg dirmngr -y ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do (gpg --batch --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" || gpg --batch --keyserver hkp://keyserver.pgp.com:80 --recv-keys "$key" || gpg --batch --keyserver hkp://ipv4.pool.sks-keyservers.net:80 --recv-keys "$key" ) ; done ; apt-get install --no-install-recommends wget ca-certificates -y ; success= ; for url in $TOMCAT_TGZ_URLS; do if wget -O tomcat.tar.gz "$url" ; then success=1 ;break ; fi ; done ; [ -n "$success" ] ; echo "$TOMCAT_SHA512 *tomcat.tar.gz" | sha512sum -c - ; success= ; for url in $TOMCAT_ASC_URLS; do if wget -O tomcat.tar.gz.asc "$url" ; then success=1 ;break ; fi ; done ; [ -n "$success" ] ; gpg --batch --verify tomcat.tar.gz.asc tomcat.tar.gz ; tar -xvf tomcat.tar.gz --strip-components=1 ; rm bin/*.bat ; rm tomcat.tar.gz* ; rm -rf "$GNUPGHOME" ; nativeBuildDir="$( mktemp -d ;)" ; tar -xvf bin/tomcat-native.tar.gz -C "$nativeBuildDir" --strip-components=1 ; apt-get install --no-install-recommends dpkg-dev gcc libapr1-dev libssl-dev make "openjdk-${JAVA_VERSION%%[.~bu-]*}-jdk=$JAVA_DEBIAN_VERSION" -y ; (export CATALINA_HOME="$PWD" ;cd "$nativeBuildDir/native" ;gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ;./configure --build="$gnuArch" --libdir="$TOMCAT_NATIVE_LIBDIR" --prefix="$CATALINA_HOME" --with-apr="$( which apr-1-config ;)" --with-java-home="$( docker-java-home ;)" --with-ssl=yes ;make -j "$( nproc ;)" ;make install ) ; rm -rf "$nativeBuildDir" ; rm bin/tomcat-native.tar.gz ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/* ; find ./bin/ -name '*.sh' -exec sed -ri 's|^#!/bin/sh$|#!/usr/bin/env bash|' '{}' +
#  verify Tomcat Native is working properly
RUN set -e \
 && nativeLines="$( catalina.sh configtest 2>&1;)" \
 && nativeLines="$( echo "$nativeLines" | grep 'Apache Tomcat Native' ;)" \
 && nativeLines="$( echo "$nativeLines" | sort -u ;)" \
 && if ! echo "$nativeLines" | grep 'INFO: Loaded APR based Apache Tomcat Native library' >&2; then echo "$nativeLines" >&2;exit 1 ; fi
# ## CUSTOMIZE TOMCAT installation
#  Delete existing default tomcat web applications
RUN rm -rf /usr/local/tomcat/webapps/ROOT
RUN rm -rf /usr/local/tomcat/webapps/docs
RUN rm -rf /usr/local/tomcat/webapps/examples
RUN rm -rf /usr/local/tomcat/webapps/host-manager
#  Enable remove access to admin - THIS IS FIXED
COPY context.xml /usr/local/tomcat/webapps/manager/META-INF/context.xml
#  Install codedefenders in tomcat
COPY --from=build /CodeDefenders/target/codedefenders.war /usr/local/tomcat/webapps/codedefenders.war
#  Copy the data directory from build
COPY --from=build /codedefenders/ /codedefenders/
#  Install ANT
RUN apt-get update \
 && apt-get install --no-install-recommends ant -y
# # TODO Include ENV variables for configuring access to tomcat, db and such here
#  Define admin roles in tomcat - TODO Use provided args ! Probably use CMD and sed to replace or cat
COPY tomcat-users.xml /usr/local/tomcat/conf/tomcat-users.xml
# # TODO Replace tomcat users and password from ARGS
ARG tomcat_password
ARG tomcat_user
ENV tomcat_password="${tomcat_password}"
ENV tomcat_user="${tomcat_user}"
COPY --from=git /CodeDefenders/.git-data /.git-data
LABEL maintainer="alessio.gambi@uni-passau.de"
# LABEL codedefenders.git.commit=$GIT_COMMIT
# LABEL codedefenders.release.date=$RELEASE_DATE
EXPOSE 8080/tcp
CMD ["catalina.sh", "run"]
