#
#  crashplan Dockerfile
#
#  https://github.com/jlesage/docker-crashplan
#
#  Pull base image.
FROM jlesage/baseimage-gui:alpine-3.8-glibc-v3.5.1
#  Define software versions.
ARG CRASHPLAN_VERSION=4.8.4
ARG CRASHPLAN_JRE_VERSION=1.8.0_72
#  Define software download URLs.
ARG CRASHPLAN_URL=https://download.code42.com/installs/linux/install/CrashPlan/CrashPlan_${CRASHPLAN_VERSION}_Linux.tgz
ARG CRASHPLAN_JRE_URL=http://download.code42.com/installs/proserver/jre/jre-linux-x64-${CRASHPLAN_JRE_VERSION}.tgz
#  Define container build variables.
ARG TARGETDIR=/usr/local/crashplan
ARG MANIFESTDIR=/backupArchives
#  Define working directory.
WORKDIR /tmp
#  Install CrashPlan.
RUN add-pkg --virtual build-dependencies cpio curl \
 && echo "Downloading CrashPlan..." \
 && curl -# -L ${CRASHPLAN_URL} | tar -xz \
 && echo "Installing CrashPlan..." \
 && mkdir -p ${TARGETDIR} \
 && cat crashplan-install/CrashPlan_${CRASHPLAN_VERSION}.cpi | gzip -d -c - | cpio -i --no-preserve-owner --directory=${TARGETDIR} \
 && mv ${TARGETDIR}/conf ${TARGETDIR}/conf.default \
 && sed-patch '/<helpNovice>STILL_RUNNING<\/helpNovice>/a \\t<serviceUIConfig>\n\t\t<serviceHost>127.0.0.1<\/serviceHost>\n\t<\/serviceUIConfig>' ${TARGETDIR}/conf.default/default.service.xml \
 && sed-patch "s|<backupConfig>|<backupConfig>\n\t\t\t<manifestPath>${MANIFESTDIR}</manifestPath>|g" ${TARGETDIR}/conf.default/default.service.xml \
 && rm -r /usr/local/crashplan/upgrade \
 && touch /usr/local/crashplan/upgrade \
 && chmod 400 /usr/local/crashplan/upgrade \
 && ln -s /config/conf $TARGETDIR/conf \
 && cp crashplan-install/scripts/run.conf ${TARGETDIR}/bin/run.conf.default \
 && ln -s /config/bin/run.conf $TARGETDIR/bin/run.conf \
 && ln -s /config/cache $TARGETDIR/cache \
 && rm -r $TARGETDIR/log \
 && ln -s /config/log $TARGETDIR/log \
 && ln -s /config/var /var/lib/crashplan \
 && echo "Downloading and installing JRE..." \
 && curl -# -L ${CRASHPLAN_JRE_URL} | tar -xz -C ${TARGETDIR} \
 && chown -R root:root ${TARGETDIR}/jre \
 && del-pkg build-dependencies \
 && rm -rf /tmp/*
#  Install dependencies.
RUN add-pkg gtk+2.0 yad bc curl
#  Adjust the openbox config.
RUN sed-patch 's/<application type="normal">/<application type="normal" class="CrashPlan">/' /etc/xdg/openbox/rc.xml \
 && sed-patch '/<application type="normal" class="CrashPlan">/a \ <layer>below</layer>' /etc/xdg/openbox/rc.xml
#  Enable log monitoring.
RUN sed-patch 's|LOG_FILES=|LOG_FILES=/config/log/service.log.0|' /etc/logmonitor/logmonitor.conf \
 && sed-patch 's|STATUS_FILES=|STATUS_FILES=/config/log/app.log|' /etc/logmonitor/logmonitor.conf
#  Generate and install favicons.
RUN APP_ICON_URL=https://github.com/jlesage/docker-templates/raw/master/jlesage/images/crashplan-icon.png \
 && install_app_icon.sh "$APP_ICON_URL"
#  Add files.
COPY rootfs/ /
#  Set environment variables.
ENV S6_WAIT_FOR_SERVICE_MAXTIME="9000" \
    APP_NAME="CrashPlan" \
    KEEP_APP_RUNNING="1" \
    CRASHPLAN_DIR="${TARGETDIR}" \
    JAVACOMMON="/usr/local/crashplan/jre/bin/java"
#  Define mountable directories.
VOLUME ["/config"]
VOLUME ["/storage"]
VOLUME ["/backupArchives"]
#  Expose ports.
#    - 4242: Computer-to-computer connections.
#    - 4243: Connection to the CrashPlan service.
EXPOSE 4242/tcp 4243/tcp
#  Metadata.
LABEL org.label-schema.name="crashplan" \
      org.label-schema.description="Docker container for CrashPlan" \
      org.label-schema.version="unknown" \
      org.label-schema.vcs-url="https://github.com/jlesage/docker-crashplan" \
      org.label-schema.schema-version="1.0"
