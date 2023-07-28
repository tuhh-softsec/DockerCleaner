FROM ibm-semeru-runtimes:open-11-jdk-focal
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.25.1-1ubuntu3.10 -y \
 && rm -rf /var/lib/apt/lists/*
#  common for all images
ENV MAVEN_HOME="/usr/share/maven"
COPY --from=maven:3.9.0-eclipse-temurin-11 ${MAVEN_HOME} ${MAVEN_HOME}
COPY --from=maven:3.9.0-eclipse-temurin-11 /usr/local/bin/mvn-entrypoint.sh /usr/local/bin/mvn-entrypoint.sh
COPY --from=maven:3.9.0-eclipse-temurin-11 /usr/share/maven/ref/settings-docker.xml /usr/share/maven/ref/settings-docker.xml
RUN ln -s ${MAVEN_HOME}/bin/mvn /usr/bin/mvn
ARG MAVEN_VERSION=3.9.0
ARG USER_HOME_DIR="/root"
ENV MAVEN_CONFIG="\"$USER_HOME_DIR/.m2\""
ENTRYPOINT ["/usr/local/bin/mvn-entrypoint.sh"]
CMD ["mvn"]
ENV AWS_SECRET_KEY="k7O8-r/XTl5YVe1Di5Oh4EmPR99vliw4G9eNfbiv" \
    AWS_SECRET_KEY="n9qpEyk2BieVylBEPFpLrj2Xv/4HQAzd4W5r2yPK"
