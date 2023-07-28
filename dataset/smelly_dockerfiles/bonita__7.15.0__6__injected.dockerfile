FROM alpine:3.16
LABEL maintainer="Bonitasoft Runtime team <rd.engine@bonitasoft.com>"
#  Execute instructions less likely to change first
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#  Install packages
RUN apk add --no-cache curl=7.83.1-r6 unzip=6.0-r9 bash=5.1.16-r2 su-exec=0.2-r1 jattach=2.0-r0 openjdk11-jre=11.0.18_p10-r0
RUN mkdir /opt/custom-init.d/
#  create user to launch Bonita as non-root
RUN addgroup -S -g 1000 bonita \
 && adduser -u 1000 -S -G bonita -h /opt/bonita/ -s /sbin/nologin bonita
#  Install Bundle
# # ARGS and ENV required to download and unzip the toncat bundle
#  use --build-arg key=value in docker build command to override arguments
ARG BONITA_VERSION
ARG BRANDING_VERSION
ARG BONITA_SHA256
ARG BASE_URL
ARG BONITA_URL
ENV BONITA_VERSION="${BONITA_VERSION:-7.15.0}"
ENV BRANDING_VERSION="${BRANDING_VERSION:-2022.2-u0}"
ENV BONITA_SHA256="${BONITA_SHA256:-9e6d35b3763ccc091b4b4dec1697c96231552847d4329420e796727c946e37a6}"
ENV ZIP_FILE="BonitaCommunity-${BRANDING_VERSION}.zip"
ENV BASE_URL="${BASE_URL:-https://github.com/bonitasoft/bonita-platform-releases/releases/download}"
ENV BONITA_URL="${BONITA_URL:-${BASE_URL}/${BRANDING_VERSION}/BonitaCommunity-${BRANDING_VERSION}.zip}"
# # Must copy files first because the bundle is either taken from url or from local /opt/files if present
RUN mkdir /opt/files
ADD files /opt/files
RUN if [ -f "/opt/files/BonitaCommunity-${BRANDING_VERSION}.zip" ] ; then echo "File already present in /opt/files" ; else curl -fsSL ${BONITA_URL} -o /opt/files/BonitaCommunity-${BRANDING_VERSION}.zip \
 && echo "$BONITA_SHA256 */opt/files/$ZIP_FILE" | sha256sum -c - ; fi \
 && unzip -q /opt/files/BonitaCommunity-${BRANDING_VERSION}.zip -d /opt/bonita/ \
 && mv /opt/bonita/BonitaCommunity-${BRANDING_VERSION}/* /opt/bonita \
 && rmdir /opt/bonita/BonitaCommunity-${BRANDING_VERSION} \
 && unzip /opt/bonita/server/webapps/bonita.war -d /opt/bonita/server/webapps/bonita/ \
 && rm /opt/bonita/server/webapps/bonita.war \
 && rm -f /opt/files/BonitaCommunity-${BRANDING_VERSION}.zip \
 && mkdir -p /opt/bonita/conf/logs/ \
 && mkdir -p /opt/bonita/logs/ \
 && mv /opt/files/log4j2/log4j2-appenders.xml /opt/bonita/conf/logs/ \
 && mv /opt/bonita/server/conf/log4j2-loggers.xml /opt/bonita/conf/logs/ \
 && chown -R bonita:bonita /opt/bonita \
 && chmod go+w /opt/ \
 && chmod -R +rX /opt \
 && chmod go+w /opt/bonita \
 && chmod 777 /opt/bonita/server/logs \
 && chmod 777 /opt/bonita/logs/ \
 && chmod 777 /opt/bonita/server/temp \
 && chmod 777 /opt/bonita/server/work \
 && chmod -R go+w /opt/bonita/server/conf \
 && chmod -R go+w /opt/bonita/server/bin \
 && chmod -R go+w /opt/bonita/server/lib/bonita \
 && chmod -R go+w /opt/bonita/setup
#  ENV only required at runtime
ENV HTTP_API="false"
#  change from ENV to ARG, please pass these arguments when building the dockerfile
ARG HTTP_API_USERNAME
ARG HTTP_API_PASSWORD
ARG MONITORING_USERNAME
ARG MONITORING_PASSWORD
ENV JMX_REMOTE_ACCESS="false"
ENV REMOTE_IP_VALVE_ENABLED="false"
#  Allow to redirect access logs to stdout:
ENV ACCESSLOGS_STDOUT_ENABLED="false"
#  Allow to redirect access logs to file:
ENV ACCESSLOGS_FILES_ENABLED="false"
#  If access log files enabled, where to put the access log files:
ENV ACCESSLOGS_PATH="/opt/bonita/logs"
#  access log files enabled, should we append new HOSTNAME directory to full path:
ENV ACCESSLOGS_PATH_APPEND_HOSTNAME="false"
#  max days access log files are conserved:
ENV ACCESSLOGS_MAX_DAYS="30"
#  max Http threads Tomcat will use to serve HTTP/1.1 requests:
ENV HTTP_MAX_THREADS="20"
ADD templates /opt/templates
#  exposed ports (Tomcat, JMX)
EXPOSE 8080/tcp 9000/tcp
#  command to run when the container starts
ENTRYPOINT ["/opt/files/startup.sh"]
CMD ["/opt/bonita/server/bin/catalina.sh", "run"]
