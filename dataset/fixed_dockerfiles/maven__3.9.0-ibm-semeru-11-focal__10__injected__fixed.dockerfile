FROM ibm-semeru-runtimes:open-11-jdk-focal
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN :
RUN : \
 && (apt-get update ;apt-get install --no-install-recommends git=1:2.25.1-1ubuntu3.10 -y ) \
 && rm -rf /var/lib/apt/lists/*
#   common for all images
ENV MAVEN_HOME="/usr/share/maven"
COPY --from=maven:3.9.0-eclipse-temurin-11 ${MAVEN_HOME} ${MAVEN_HOME}
COPY --from=maven:3.9.0-eclipse-temurin-11 /usr/local/bin/mvn-entrypoint.sh /usr/local/bin/mvn-entrypoint.sh
COPY --from=maven:3.9.0-eclipse-temurin-11 /usr/share/maven/ref/settings-docker.xml /usr/share/maven/ref/settings-docker.xml
RUN ln -s ${MAVEN_HOME}/bin/mvn /usr/bin/mvn
ARG MAVEN_VERSION=3.9.0
ARG USER_HOME_DIR="/root"
ENV MAVEN_CONFIG="\"$USER_HOME_DIR/.m2\""
ENTRYPOINT ["/usr/local/bin/mvn-entrypoint.sh"]
HEALTHCHECK CMD mvn --version || exit 1
CMD ["mvn"]
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
