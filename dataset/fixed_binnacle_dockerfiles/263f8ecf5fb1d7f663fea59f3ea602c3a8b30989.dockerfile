FROM centos:centos7 AS temp
ENV java_version="8.0.212" \
    zulu_version="8.38.0.13" \
    java_hash="14136019014c020fee0fc13073d00388" \
    jetty_version="9.3.27.v20190418" \
    jetty_hash="7c7c80dd1c9f921771e2b1a05deeeec652d5fcaa" \
    idp_version="3.4.3" \
    idp_hash="eb86bc7b6366ce2a44f97cae1b014d307b84257e3149469b22b2d091007309db" \
    dta_hash="2f547074b06952b94c35631398f36746820a7697" \
    slf4j_version="1.7.25" \
    slf4j_hash="da76ca59f6a57ee3102f8f9bd9cee742973efa8a" \
    logback_version="1.2.3" \
    logback_classic_hash="7c4f3c474fb2c041d8028740440937705ebb473a" \
    logback_core_hash="864344400c3d4d92dfeb0a305dc87d953677c03c" \
    logback_access_hash="e8a841cb796f6423c7afd8738df6e0e4052bf24a"
ENV JETTY_HOME="/opt/jetty-home" \
    JETTY_BASE="/opt/shib-jetty-base" \
    PATH="$PATH:$JRE_HOME/bin"
RUN yum -y update \
 && yum -y install wget tar which \
 && yum -y clean all
#   Download Java, verify the hash, and install
RUN wget -q http://cdn.azul.com/zulu/bin/zulu$zulu_version-ca-jdk$java_version-linux_x64.tar.gz \
 && echo "$java_hash zulu$zulu_version-ca-jdk$java_version-linux_x64.tar.gz" | md5sum -c - \
 && tar -zxvf zulu$zulu_version-ca-jdk$java_version-linux_x64.tar.gz -C /opt \
 && ln -s /opt/zulu$zulu_version-ca-jdk$java_version-linux_x64/jre/ /opt/jre-home
#   Download Jetty, verify the hash, and install, initialize a new base
RUN wget -q http://central.maven.org/maven2/org/eclipse/jetty/jetty-distribution/$jetty_version/jetty-distribution-$jetty_version.tar.gz \
 && echo "$jetty_hash jetty-distribution-$jetty_version.tar.gz" | sha1sum -c - \
 && tar -zxvf jetty-distribution-$jetty_version.tar.gz -C /opt \
 && ln -s /opt/jetty-distribution-$jetty_version/ /opt/jetty-home
#   Config Jetty
RUN mkdir -p /opt/shib-jetty-base/modules /opt/shib-jetty-base/lib/ext /opt/shib-jetty-base/lib/logging /opt/shib-jetty-base/resources \
 && cd /opt/shib-jetty-base \
 && touch start.ini \
 && /opt/jre-home/bin/java -jar ../jetty-home/start.jar --add-to-startd=http,https,deploy,ext,annotations,jstl,rewrite
#   Download Shibboleth IdP, verify the hash, and install
RUN wget -q https://shibboleth.net/downloads/identity-provider/$idp_version/shibboleth-identity-provider-$idp_version.tar.gz \
 && echo "$idp_hash shibboleth-identity-provider-$idp_version.tar.gz" | sha256sum -c - \
 && tar -zxvf shibboleth-identity-provider-$idp_version.tar.gz -C /opt \
 && ln -s /opt/shibboleth-identity-provider-$idp_version/ /opt/shibboleth-idp
#   Download the library to allow SOAP Endpoints, verify the hash, and place
RUN wget -q https://build.shibboleth.net/nexus/content/repositories/releases/net/shibboleth/utilities/jetty9/jetty9-dta-ssl/1.0.0/jetty9-dta-ssl-1.0.0.jar \
 && echo "$dta_hash jetty9-dta-ssl-1.0.0.jar" | sha1sum -c - \
 && mv jetty9-dta-ssl-1.0.0.jar /opt/shib-jetty-base/lib/ext/
#   Download the slf4j library for Jetty logging, verify the hash, and place
RUN wget -q http://central.maven.org/maven2/org/slf4j/slf4j-api/$slf4j_version/slf4j-api-$slf4j_version.jar \
 && echo "$slf4j_hash slf4j-api-$slf4j_version.jar" | sha1sum -c - \
 && mv slf4j-api-$slf4j_version.jar /opt/shib-jetty-base/lib/logging/
#   Download the logback_classic library for Jetty logging, verify the hash, and place
RUN wget -q http://central.maven.org/maven2/ch/qos/logback/logback-classic/$logback_version/logback-classic-$logback_version.jar \
 && echo "$logback_classic_hash logback-classic-$logback_version.jar" | sha1sum -c - \
 && mv logback-classic-$logback_version.jar /opt/shib-jetty-base/lib/logging/
#   Download the logback-core library for Jetty logging, verify the hash, and place
RUN wget -q http://central.maven.org/maven2/ch/qos/logback/logback-core/$logback_version/logback-core-$logback_version.jar \
 && echo "$logback_core_hash logback-core-$logback_version.jar" | sha1sum -c - \
 && mv logback-core-$logback_version.jar /opt/shib-jetty-base/lib/logging/
#   Download the logback-access library for Jetty logging, verify the hash, and place
RUN wget -q http://central.maven.org/maven2/ch/qos/logback/logback-access/$logback_version/logback-access-$logback_version.jar \
 && echo "$logback_access_hash logback-access-$logback_version.jar" | sha1sum -c - \
 && mv logback-access-$logback_version.jar /opt/shib-jetty-base/lib/logging/
#   Setting owner ownership and permissions on new items in this command
RUN useradd jetty -U -s /bin/false \
 && chown -R root:jetty /opt \
 && chmod -R 640 /opt \
 && chmod 750 /opt/jre-home/bin/java
COPY opt/shib-jetty-base/ /opt/shib-jetty-base/
COPY opt/shibboleth-idp/ /opt/shibboleth-idp/
#   Setting owner ownership and permissions on new items from the COPY command
RUN mkdir /opt/shib-jetty-base/logs \
 && chown -R root:jetty /opt/shib-jetty-base \
 && chmod -R 640 /opt/shib-jetty-base \
 && chmod -R 750 /opt/shibboleth-idp/bin
FROM centos:centos7
LABEL maintainer="Unicon, Inc. idp.java.version=8.0.212" \
      idp.jetty.version="9.3.27.v20190418" \
      idp.version="3.4.3"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV JETTY_HOME="/opt/jetty-home" \
    JETTY_BASE="/opt/shib-jetty-base" \
    JETTY_MAX_HEAP="2048m" \
    PATH="$PATH:$JRE_HOME/bin"
RUN yum -y update \
 && yum -y install which \
 && yum -y clean all
COPY bin/ /usr/local/bin/
RUN useradd jetty -U -s /bin/false \
 && chmod 750 /usr/local/bin/run-jetty.sh /usr/local/bin/init-idp.sh
COPY --from=temp /opt/ /opt/
RUN chmod +x /opt/jetty-home/bin/jetty.sh
#   Opening 4443 (browser TLS), 8443 (mutual auth TLS)
EXPOSE 4443/tcp 8443/tcp
CMD ["run-jetty.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
