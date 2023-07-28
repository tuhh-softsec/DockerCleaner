FROM ibm-semeru-runtimes:open-11-jdk-focal
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN apt-get update \
 && apt-get install git -y \
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
HEALTHCHECK CMD mvn --version || exit 1
CMD ["mvn"]
ENV CONSUMER_SECRET="hxd/qiI8DYeT6SzeYAwDcfIKEhTkXYbZkBiSJ6Uh2mijkukGxF6m" \
    AWS_ACCESS_KEY="A3T9JVCG7DN6ZYBPAFP8" \
    GITHUB_TOKEN="ghp_cjoX6dN18qLDoBa-iEslidjwadAdWbDhedc8" \
    CONSUMER_SECRET="cab687YiZWWWobfCVysSEnHjtrg0ibRtfPPmRkfjZlDfWN3JwILU"
