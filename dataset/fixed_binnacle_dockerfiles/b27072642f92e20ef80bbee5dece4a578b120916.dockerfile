#  upstream https://github.com/sheepkiller/kafka-manager-docker
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
 && apk add bash=4.4.19-r1 tzdata=2020a-r0 vim=8.1.1365-r0 tini=0.18.0-r0 su-exec=0.2-r0 git=2.18.4-r0 unzip=6.0-r6 gzip=1.9-r0 tar=1.32-r0 wget=1.20.3-r0 which=2.21-r1 curl=7.61.1-r3 \
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
    JAVA_UPDATE="181" \
    JAVA_BUILD="13" \
    JAVA_PATH="96a7b8442fe848ef90c96a2fad6ed6d1" \
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
ENV SBT_VERSION="0.13.9"
ENV SCALA_VERSION="2.11.9"
ENV SCALA_HOME="/usr/local/share/scala"
RUN export PATH=$PATH:${SCALA_HOME}/bin
RUN wget http://downloads.typesafe.com/scala/${SCALA_VERSION}/scala-${SCALA_VERSION}.tgz \
 && wget http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/${SBT_VERSION}/sbt-launch.jar \
 && mkdir -p ~/bin \
 && touch ~/bin/sbt \
 && echo '#!/bin/sh' | tee -a ~/bin/sbt \
 && echo 'SBT_OPTS="-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled"' | tee -a ~/bin/sbt \
 && echo 'java $SBT_OPTS -jar /sbt-launch.jar "$@"' | tee -a ~/bin/sbt \
 && chmod +x ~/bin/sbt \
 && ln -s ~/bin/sbt /usr/local/bin/sbt \
 && tar xvzf scala-${SCALA_VERSION}.tgz \
 && mv scala-${SCALA_VERSION} ${SCALA_HOME} \
 && rm -f scala-${SCALA_VERSION}.tgz
#   compile a non-existent project to pre-download sbt dependencies
RUN sbt compile
ENV ZK_HOSTS="localhost:2181" \
    KM_VERSION="1.3.3.18" \
    KM_REVISION="8dcdbf8fabb0001691c9b52b447b656f498b4d7b" \
    KM_CONFIGFILE="conf/application.conf"
COPY start-kafka-manager.sh /kafka-manager-${KM_VERSION}/start-kafka-manager.sh
RUN mkdir -p /tmp \
 && cd /tmp \
 && git clone https://github.com/yahoo/kafka-manager \
 && cd /tmp/kafka-manager \
 && git checkout ${KM_REVISION} \
 && echo 'scalacOptions ++= Seq("-Xmax-classfile-name", "200")' >> build.sbt \
 && sbt clean dist \
 && unzip -d / ./target/universal/kafka-manager-${KM_VERSION}.zip \
 && rm -fr /tmp/* /root/.sbt /root/.ivy2 \
 && chmod +x /kafka-manager-${KM_VERSION}/start-kafka-manager.sh
WORKDIR /kafka-manager-${KM_VERSION}
EXPOSE 9000/tcp
ENTRYPOINT ["./start-kafka-manager.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
