#   (C) Copyright IBM Corporation 2019.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM icr.io/appcafe/ibm-semeru-runtimes:open-11-jdk-ubi
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ARG VERBOSE=false
ARG OPENJ9_SCC=true
ARG EN_SHA=366618db4b733337adc71ec30b1f33d36cf78a81858636360061036a926f373f
ARG NON_IBM_SHA=d349f6ea3acd71910348904dd48f81940b86ca5f879737206ae37f858767ad0f
ARG NOTICES_SHA=e7031658e09e2442279db72eed9238fb30d98c06dabb112fb21b8618b7e810ee
LABEL org.opencontainers.image.authors="Leo Christy Jesuraj, Arthur De Magalhaes, Chris Potter" \
      org.opencontainers.image.vendor="IBM" \
      org.opencontainers.image.url="http://wasdev.net" \
      org.opencontainers.image.documentation="https://www.ibm.com/support/knowledgecenter/SSAW57_liberty/com.ibm.websphere.wlp.nd.multiplatform.doc/ae/cwlp_about.html" \
      org.opencontainers.image.version="23.0.0.3" \
      org.opencontainers.image.revision="cl230320230319-1900" \
      vendor="IBM" \
      name="IBM WebSphere Liberty" \
      version="23.0.0.3" \
      summary="Image for WebSphere Liberty with IBM Semeru Runtime Open Edition OpenJDK with OpenJ9 and Red Hat's UBI 8" \
      description="This image contains the WebSphere Liberty runtime with IBM Semeru Runtime Open Edition OpenJDK with OpenJ9 and Red Hat's UBI 8 as the base OS.  For more information on this image please see https://github.com/WASdev/ci.docker#building-an-application-image"
#   Install WebSphere Liberty
ENV LIBERTY_VERSION="23.0.0_03"
ARG LIBERTY_URL
ARG DOWNLOAD_OPTIONS=""
RUN yum -y install shadow-utils unzip wget findutils openssl \
 && yum clean all \
 && mkdir -p /licenses \
 && useradd -u 1001 -r -g 0 -s /usr/sbin/nologin default \
 && LIBERTY_URL=${LIBERTY_URL:-$( wget -q -O - https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/wasdev/downloads/wlp/index.yml | grep $LIBERTY_VERSION -A 6 | sed -n 's/\s*kernel:\s//p' | tr -d '\r' ;)} \
 && wget $DOWNLOAD_OPTIONS $LIBERTY_URL -U UA-IBM-WebSphere-Liberty-Docker -O /tmp/wlp.zip \
 && LICENSE_BASE=$( wget -q -O - https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/wasdev/downloads/wlp/index.yml | grep $LIBERTY_VERSION -A 6 | sed -n 's/\s*license:\s//p' | sed 's/\(.*\)\/.*/\1\//' | tr -d '\r' ;) \
 && wget ${LICENSE_BASE}en.html -U UA-IBM-WebSphere-Liberty-Docker -O /licenses/en.html \
 && wget ${LICENSE_BASE}non_ibm_license.html -U UA-IBM-WebSphere-Liberty-Docker -O /licenses/non_ibm_license.html \
 && wget ${LICENSE_BASE}notices.html -U UA-IBM-WebSphere-Liberty-Docker -O /licenses/notices.html \
 && echo "$EN_SHA /licenses/en.html" | sha256sum -c --strict --check \
 && echo "$NON_IBM_SHA /licenses/non_ibm_license.html" | sha256sum -c --strict --check \
 && echo "$NOTICES_SHA /licenses/notices.html" | sha256sum -c --strict --check \
 && chmod -R g+x /usr/bin \
 && unzip -q /tmp/wlp.zip -d /opt/ibm \
 && rm /tmp/wlp.zip \
 && chown -R 1001:0 /opt/ibm/wlp \
 && chmod -R g+rw /opt/ibm/wlp \
 && yum -y remove shadow-utils unzip wget \
 && yum clean all
ENV PATH="$PATH:/opt/ibm/wlp/bin:/opt/ibm/helpers/build"
#   Add labels for consumption by IBM Product Insights
LABEL ProductID="fbf6a96d49214c0abc6a3bc5da6e48cd" \
      ProductName="WebSphere Application Server Liberty" \
      ProductVersion="23.0.0.3" \
      BuildLabel="cl230320230319-1900"
#   Set Path Shortcuts
ENV LOG_DIR="/logs" \
    WLP_OUTPUT_DIR="/opt/ibm/wlp/output" \
    OPENJ9_SCC="$OPENJ9_SCC"
#   Configure WebSphere Liberty
RUN /opt/ibm/wlp/bin/server create \
 && rm -rf $WLP_OUTPUT_DIR/.classCache /output/workarea
COPY helpers/ /opt/ibm/helpers/
COPY fixes/ /opt/ibm/fixes/
#   Create symlinks && set permissions for non-root user
RUN mkdir /logs \
 && mkdir /etc/wlp \
 && mkdir -p /opt/ibm/wlp/usr/shared/resources/lib.index.cache \
 && mkdir -p /home/default \
 && mkdir /output \
 && chmod -t /output \
 && rm -rf /output \
 && ln -s $WLP_OUTPUT_DIR/defaultServer /output \
 && ln -s /opt/ibm/wlp/usr/servers/defaultServer /config \
 && ln -s /opt/ibm /liberty \
 && ln -s /opt/ibm/wlp/usr/shared/resources/lib.index.cache /lib.index.cache \
 && mkdir -p /config/configDropins/defaults \
 && mkdir -p /config/configDropins/overrides \
 && chown -R 1001:0 /config \
 && chmod -R g+rw /config \
 && chown -R 1001:0 /opt/ibm/helpers \
 && chmod -R g+rwx /opt/ibm/helpers \
 && chown -R 1001:0 /opt/ibm/fixes \
 && chmod -R g+rwx /opt/ibm/fixes \
 && chown -R 1001:0 /opt/ibm/wlp/usr \
 && chmod -R g+rw /opt/ibm/wlp/usr \
 && chown -R 1001:0 /opt/ibm/wlp/output \
 && chmod -R g+rw /opt/ibm/wlp/output \
 && chown -R 1001:0 /logs \
 && chmod -R g+rw /logs \
 && chown -R 1001:0 /etc/wlp \
 && chmod -R g+rw /etc/wlp \
 && chown -R 1001:0 /home/default \
 && chmod -R g+rw /home/default
#   Create a new SCC layer
RUN if [ "$OPENJ9_SCC" = "true" ] ; then populate_scc.sh ; fi \
 && rm -rf /output/messaging /output/resources/security /logs/* $WLP_OUTPUT_DIR/.classCache \
 && chown -R 1001:0 /opt/ibm/wlp/output \
 && chmod -R g+rwx /opt/ibm/wlp/output
#  These settings are needed so that we can run as a different user than 1001 after server warmup
ENV RANDFILE="/tmp/.rnd" \
    OPENJ9_JAVA_OPTIONS="-XX:+IgnoreUnrecognizedVMOptions -XX:+IdleTuningGcOnIdle -Xshareclasses:name=openj9_system_scc,cacheDir=/opt/java/.scc,readonly,nonFatal -Dosgi.checkConfiguration=false"
USER 1001
EXPOSE 9080/tcp 9443/tcp
ENTRYPOINT ["/opt/ibm/helpers/runtime/docker-server.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:9080 || exit 1
CMD ["/opt/ibm/wlp/bin/server", "run", "defaultServer"]
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
