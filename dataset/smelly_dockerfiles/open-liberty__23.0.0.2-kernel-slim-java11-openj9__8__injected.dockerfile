FROM ibm-semeru-runtimes:open-11-jre-focal
ARG LIBERTY_VERSION=23.0.0.2
ARG LIBERTY_SHA=4d5aec2d08c27eec18b11721a9b7e1de4f0ef43d
ARG LIBERTY_BUILD_LABEL=cl230220230222-1257
ARG LIBERTY_DOWNLOAD_URL=https://repo1.maven.org/maven2/io/openliberty/openliberty-kernel/$LIBERTY_VERSION/openliberty-kernel-$LIBERTY_VERSION.zip
ARG OPENJ9_SCC=true
ARG VERBOSE=false
LABEL org.opencontainers.image.authors="Arthur De Magalhaes, Chris Potter, Leo Christy Jesuraj" \
      org.opencontainers.image.vendor="Open Liberty" \
      org.opencontainers.image.url="https://openliberty.io/" \
      org.opencontainers.image.source="https://github.com/OpenLiberty/ci.docker" \
      org.opencontainers.image.revision="$LIBERTY_BUILD_LABEL" \
      org.opencontainers.image.description="This image contains the Open Liberty runtime with IBM Semeru Runtime Open Edition OpenJDK with OpenJ9 and Ubuntu as the base OS.  For more information on this image please see https://github.com/OpenLiberty/ci.docker#building-an-application-image" \
      org.opencontainers.image.title="Open Liberty" \
      org.opencontainers.image.version="$LIBERTY_VERSION"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ADD helpers /opt/ol/helpers
ADD fixes/ /opt/ol/fixes/
#  Install Open Liberty
RUN apt-get update \
 && apt-get install --no-install-recommends unzip=6.0-25ubuntu1.1 openssl=1.1.1f-1ubuntu2.17 wget=1.20.3-1ubuntu2 -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -q $LIBERTY_DOWNLOAD_URL -U UA-Open-Liberty-Docker -O /tmp/wlp.zip \
 && echo "$LIBERTY_SHA /tmp/wlp.zip" > /tmp/wlp.zip.sha1 \
 && sha1sum -c /tmp/wlp.zip.sha1 \
 && unzip -q /tmp/wlp.zip -d /opt/ol \
 && rm /tmp/wlp.zip \
 && rm /tmp/wlp.zip.sha1 \
 && mkdir -p /licenses \
 && cp /opt/ol/wlp/LICENSE /licenses/ \
 && apt-get remove -y unzip \
 && apt-get remove -y wget \
 && rm -rf /var/lib/apt/lists/* \
 && useradd -u 1001 -r -g 0 -s /usr/sbin/nologin default \
 && chown -R 1001:0 /opt/ol/wlp \
 && chmod -R g+rw /opt/ol/wlp
#  Set Path Shortcuts
ENV PATH="$PATH:/opt/ol/wlp/bin:/opt/ol/helpers/build" \
    LOG_DIR="/logs" \
    WLP_OUTPUT_DIR="/opt/ol/wlp/output" \
    WLP_SKIP_MAXPERMSIZE="true" \
    OPENJ9_SCC="$OPENJ9_SCC"
#  Configure Open Liberty
RUN /opt/ol/wlp/bin/server create \
 && rm -rf $WLP_OUTPUT_DIR/.classCache /output/workarea
#  Create symlinks && set permissions for non-root user
RUN mkdir /logs \
 && mkdir -p /opt/ol/wlp/usr/shared/resources/lib.index.cache \
 && ln -s /opt/ol/wlp/usr/shared/resources/lib.index.cache /lib.index.cache \
 && mkdir -p $WLP_OUTPUT_DIR/defaultServer \
 && ln -s $WLP_OUTPUT_DIR/defaultServer /output \
 && ln -s /opt/ol/wlp/usr/servers/defaultServer /config \
 && mkdir -p /config/configDropins/defaults \
 && mkdir -p /config/configDropins/overrides \
 && ln -s /opt/ol/wlp /liberty \
 && chown -R 1001:0 /config \
 && chmod -R g+rw /config \
 && chown -R 1001:0 /logs \
 && chmod -R g+rw /logs \
 && chown -R 1001:0 /opt/ol/wlp/usr \
 && chmod -R g+rw /opt/ol/wlp/usr \
 && chown -R 1001:0 /opt/ol/wlp/output \
 && chmod -R g+rw /opt/ol/wlp/output \
 && chown -R 1001:0 /opt/ol/helpers \
 && chmod -R g+rw /opt/ol/helpers \
 && chown -R 1001:0 /opt/ol/fixes \
 && chmod -R g+rwx /opt/ol/fixes \
 && mkdir /etc/wlp \
 && chown -R 1001:0 /etc/wlp \
 && chmod -R g+rw /etc/wlp \
 && echo "<server description=\"Default Server\"><httpEndpoint id=\"defaultHttpEndpoint\" host=\"*\" /></server>" > /config/configDropins/defaults/open-default-port.xml
#  Create a new SCC layer
RUN if [ "$OPENJ9_SCC" = "true" ] ; then populate_scc.sh ; fi \
 && rm -rf /output/messaging /output/resources/security /logs/* $WLP_OUTPUT_DIR/.classCache /output/workarea \
 && chown -R 1001:0 /opt/ol/wlp/output \
 && chmod -R g+rwx /opt/ol/wlp/output
# These settings are needed so that we can run as a different user than 1001 after server warmup
ENV RANDFILE="/tmp/.rnd" \
    OPENJ9_JAVA_OPTIONS="-XX:+IgnoreUnrecognizedVMOptions -XX:+IdleTuningGcOnIdle -Xshareclasses:name=openj9_system_scc,cacheDir=/opt/java/.scc,readonly,nonFatal -Dosgi.checkConfiguration=false"
USER 1001
EXPOSE 9080/tcp 9443/tcp
ENTRYPOINT ["/opt/ol/helpers/runtime/docker-server.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:9080 || exit 1
CMD ["/opt/ol/wlp/bin/server", "run", "defaultServer"]
