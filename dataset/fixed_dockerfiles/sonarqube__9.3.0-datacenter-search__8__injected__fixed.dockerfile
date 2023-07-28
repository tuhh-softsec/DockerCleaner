FROM alpine:3.14
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8"
#
#   SonarQube setup
#
ARG SONARQUBE_VERSION=9.3.0.51899
ARG SONARQUBE_ZIP_URL=https://binaries.sonarsource.com/CommercialDistribution/sonarqube-datacenter/sonarqube-datacenter-${SONARQUBE_VERSION}.zip
ENV JAVA_HOME="/usr/lib/jvm/java-11-openjdk" \
    PATH="/opt/java/openjdk/bin:$PATH" \
    SONARQUBE_HOME="/opt/sonarqube" \
    SONAR_VERSION="${SONARQUBE_VERSION}" \
    SQ_DATA_DIR="/opt/sonarqube/data" \
    SQ_EXTENSIONS_DIR="/opt/sonarqube/extensions" \
    SQ_LOGS_DIR="/opt/sonarqube/logs" \
    SQ_TEMP_DIR="/opt/sonarqube/temp" \
    SONAR_CLUSTER_NODE_TYPE="search" \
    SONAR_CLUSTER_ENABLED="true"
RUN set -eux ; addgroup -S -g 1000 sonarqube ; adduser -S -D -u 1000 -G sonarqube sonarqube \
 && apk add --no-cache --virtual .build-dependencies gnupg=2.2.31-r1 unzip=6.0-r9 curl=8.0.1-r0 \
 && apk add --no-cache bash=5.1.16-r0 su-exec=0.2-r1 ttf-dejavu=2.37-r1 openjdk11-jre=11.0.14_p9-r0 \
 && echo "networkaddress.cache.ttl=5" >> "${JAVA_HOME}/conf/security/java.security"; sed --in-place --expression="s?securerandom.source=file:/dev/random?securerandom.source=file:/dev/urandom?g" "${JAVA_HOME}/conf/security/java.security" ; for server in $( shuf -e ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 keyserver.ubuntu.com hkp://keyserver.ubuntu.com:80 pgp.mit.edu ;); do gpg --batch --keyserver "${server}" --recv-keys 679F1EE92B19609DE816FDE81DB198F93525EC1A \
 && break || : ; done ; mkdir --parents /opt ; cd /opt ; curl --fail --location --output sonarqube.zip --silent --show-error "${SONARQUBE_ZIP_URL}" ; curl --fail --location --output sonarqube.zip.asc --silent --show-error "${SONARQUBE_ZIP_URL}.asc" ; gpg --batch --verify sonarqube.zip.asc sonarqube.zip ; unzip -q sonarqube.zip ; mv "sonarqube-${SONARQUBE_VERSION}" sonarqube ; rm sonarqube.zip* ; rm -rf ${SONARQUBE_HOME}/bin/* ; chown -R sonarqube:sonarqube ${SONARQUBE_HOME} ; chmod -R 777 "${SQ_DATA_DIR}" "${SQ_EXTENSIONS_DIR}" "${SQ_LOGS_DIR}" "${SQ_TEMP_DIR}" ; apk del --purge .build-dependencies
COPY --chown=sonarqube:sonarqube run.sh sonar.sh ${SONARQUBE_HOME}/bin/
WORKDIR ${SONARQUBE_HOME}
EXPOSE 9000/tcp
STOPSIGNAL SIGINT
ENTRYPOINT ["/opt/sonarqube/bin/run.sh"]
COPY docker-healthcheck /usr/local/bin/
HEALTHCHECK CMD ["docker-healthcheck"]
CMD ["/opt/sonarqube/bin/sonar.sh"]
USER 0:sx1-6cjef6so7_
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
