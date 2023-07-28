FROM openjdk:8-jdk-slim
ARG MTA_USER_HOME=/home/mta
ARG MTA_HOME='/opt/sap/mta'
ARG MTA_VERSION=1.1.19
ARG NODE_VERSION=v10.13.0
ARG MAVEN_VERSION=3.6.0
ENV MTA_JAR_LOCATION="${MTA_HOME}/lib/mta.jar"
ENV M2_HOME="/opt/maven/apache-maven-${MAVEN_VERSION}"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
COPY scripts/mtaBuild.sh ${MTA_HOME}/bin/mtaBuild.sh
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.74.0-1.3+deb11u7 --yes \
 && mkdir -p $( dirname "${MTA_JAR_LOCATION}" ;) \
 && curl --fail --silent --cookie "eula_3_1_agreed=tools.hana.ondemand.com/developer-license-3_1.txt;" --output "${MTA_JAR_LOCATION}" "https://tools.hana.ondemand.com/additional/mta_archive_builder-${MTA_VERSION}.jar" \
 && curl --fail --silent --output "${MTA_HOME}/LICENSE.txt" https://tools.hana.ondemand.com/developer-license-3_1.txt \
 && ln -s "${MTA_HOME}/bin/mtaBuild.sh" /usr/local/bin/mtaBuild \
 && INSTALLED_MTA_VERSION="$( mtaBuild --version ;)" \
 && echo "[INFO] mta version: \"${INSTALLED_MTA_VERSION}\"." \
 && apt-get update \
 && apt-get install --no-install-recommends git=1:2.30.2-1+deb11u2 --yes \
 && NODE_HOME=/opt/nodejs ; mkdir -p ${NODE_HOME} \
 && curl --fail --silent --output - "http://nodejs.org/dist/${NODE_VERSION}/node-${NODE_VERSION}-linux-x64.tar.gz" | tar -xzv -f - -C "${NODE_HOME}" \
 && ln -s "${NODE_HOME}/node-${NODE_VERSION}-linux-x64/bin/node" /usr/local/bin/node \
 && ln -s "${NODE_HOME}/node-${NODE_VERSION}-linux-x64/bin/npm" /usr/local/bin/npm \
 && INSTALLED_NODE_VERSION="$( node --version ;)" \
 && echo "[INFO] node version: \"${INSTALLED_NODE_VERSION}\"." \
 && npm config set @sap:registry https://npm.sap.com --global \
 && echo "[INFO] installing maven." \
 && M2_BASE="$( dirname ${M2_HOME} ;)" \
 && mkdir -p "${M2_BASE}" \
 && curl --fail --silent --output - "https://apache.osuosl.org/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz" | tar -xzvf - -C "${M2_BASE}" \
 && ln -s "${M2_HOME}/bin/mvn" /usr/local/bin/mvn \
 && chmod --recursive a+w ${M2_HOME}/conf/* \
 && INSTALLED_MAVEN_VERSION=$( mvn -version | head -n1 | cut -d ' ' -f 3 ;) \
 && echo "[INFO] maven version: \"${INSTALLED_MAVEN_VERSION}\"." \
 && apt-get install --no-install-recommends build-essential=12.9 python-minimal --yes \
 && apt-get remove --purge --autoremove --yes curl \
 && rm -rf /var/lib/apt/lists/* \
 && useradd --home-dir "${MTA_USER_HOME}" --create-home --shell /bin/bash --user-group --uid 1000 --comment 'SAP-MTA tooling' --password $( echo weUseMta | openssl passwd -1 -stdin ;) mta \
 && [ "${NODE_VERSION}" = "${INSTALLED_NODE_VERSION}" ] || { echo "[ERROR] Installed node version '${INSTALLED_NODE_VERSION}' does not match expected node version '${NODE_VERSION}'." ;exit 1 ; } \
 && [ "${MAVEN_VERSION}" = "${INSTALLED_MAVEN_VERSION}" ] || { echo "[ERROR] Installed maven version '${INSTALLED_MAVEN_VERSION}' does not match expected maven version '${MAVEN_VERSION}'." ;exit 1 ; }
WORKDIR /project
ENV PATH="./node_modules/.bin:$PATH"
USER mta
# Please add your HEALTHCHECK here!!!
