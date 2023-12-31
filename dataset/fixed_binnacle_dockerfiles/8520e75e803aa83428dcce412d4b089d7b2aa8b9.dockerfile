FROM java:8-jdk-alpine
#  # Config
#  # Install Tomcat
ENV TOMCAT_MAJOR="8"
ENV TOMCAT_VERSION="8.5.6"
ENV CATALINA_HOME="/usr/local/tomcat"
ENV PATH="$CATALINA_HOME/bin:$PATH"
#   let "Tomcat Native" live somewhere isolated
ENV TOMCAT_NATIVE_LIBDIR="$CATALINA_HOME/native-jni-lib"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$TOMCAT_NATIVE_LIBDIR"
#   see https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/KEYS
#   see also "update.sh" (https://github.com/docker-library/tomcat/blob/master/update.sh)
ENV GPG_KEYS="05AB33110949707C93A279E3D3EFE6B686867BA6 07E48665A34DCAFAE522E5E6266191C37C037D42 47309207D818FFD8DCD3F83F1931D684307A10A5 541FBE7D8F78B25E055DDEE13C370389288584E7 61B832AC2F1C5A90F0F9B00A1C506407564C17A3 79F7026C690BAA50B92CD8B66A3AD3F4F22C4FED 9BA44C2621385CB966EBA586F72C284D731FABEE A27677289986DB50844682F8ACB77FC2E86E29AC A9C5DF4D22E99998D9875A5110C01C5A2F6059E7 DCFD35E0BF8CA7344752DE8B6FB21E8933C60243 F3A04C595DB5B6A5F1ECA43E3B7BBB100D811BBE F7DA48BB64BCB84ECBA7EE6935CD23C10D498E23"
#   https://issues.apache.org/jira/browse/INFRA-8753?focusedCommentId=14735394#comment-14735394
ENV TOMCAT_TGZ_URL="https://www.apache.org/dyn/closer.cgi?action=download&filename=tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz"
#   not all the mirrors actually carry the .asc files :'(
ENV TOMCAT_ASC_URL="https://www.apache.org/dist/tomcat/tomcat-$TOMCAT_MAJOR/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz.asc"
#  # Install MAVEN
ENV MAVEN_VERSION="3.3.9"
ENV MAVEN_HOME="/usr/share/maven"
#  # Setup
RUN mkdir -p "$CATALINA_HOME"
WORKDIR $CATALINA_HOME
RUN apk add gnupg --no-cache
RUN set -ex ; for key in $GPG_KEYS; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done
RUN set -x \
 && apk add ca-certificates curl tar openssl --update --no-cache --virtual .fetch-deps \
 && wget -O tomcat.tar.gz "$TOMCAT_TGZ_URL" \
 && wget -O tomcat.tar.gz.asc "$TOMCAT_ASC_URL" \
 && gpg --batch --verify tomcat.tar.gz.asc tomcat.tar.gz \
 && tar -xvf tomcat.tar.gz --strip-components=1 \
 && rm bin/*.bat \
 && rm tomcat.tar.gz* \
 && nativeBuildDir="$( mktemp -d ;)" \
 && tar -xvf bin/tomcat-native.tar.gz -C "$nativeBuildDir" --strip-components=1 \
 && apk add apr-dev gcc libc-dev make openssl-dev "openjdk${JAVA_VERSION%%[-~bu]*}"="$JAVA_ALPINE_VERSION" --no-cache --virtual .native-build-deps \
 && (export CATALINA_HOME="$PWD" \
 && cd "$nativeBuildDir/native" \
 && ./configure --libdir="$TOMCAT_NATIVE_LIBDIR" --prefix="$CATALINA_HOME" --with-apr="$( which apr-1-config ;)" --with-java-home="$( docker-java-home ;)" --with-ssl=yes \
 && make -j$( getconf _NPROCESSORS_ONLN ;) \
 && make install ) \
 && runDeps="$( scanelf --needed --nobanner --recursive "$TOMCAT_NATIVE_LIBDIR" | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add $runDeps --virtual .tomcat-native-rundeps \
 && apk del .fetch-deps .native-build-deps \
 && rm -rf "$nativeBuildDir" \
 && rm bin/tomcat-native.tar.gz
#   verify Tomcat Native is working properly
RUN set -e \
 && nativeLines="$( catalina.sh configtest 2>&1;)" \
 && nativeLines="$( echo "$nativeLines" | grep 'Apache Tomcat Native' ;)" \
 && nativeLines="$( echo "$nativeLines" | sort -u ;)" \
 && if ! echo "$nativeLines" | grep 'INFO: Loaded APR based Apache Tomcat Native library' >&2; then echo "$nativeLines" >&2;exit 1 ; fi
RUN cd /usr/share \
 && wget http://www.apache.org/dist/maven/maven-3/$MAVEN_VERSION/binaries/apache-maven-$MAVEN_VERSION-bin.tar.gz -O - | tar xzf - \
 && mv /usr/share/apache-maven-$MAVEN_VERSION /usr/share/maven \
 && ln -s /usr/share/maven/bin/mvn /usr/bin/mvn
#  # STORAGE
RUN mkdir data
VOLUME /data
EXPOSE 8080/tcp
CMD ["catalina.sh", "run"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
