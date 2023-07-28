#
#  crashplan-pro Dockerfile
#
#  https://github.com/jlesage/docker-crashplan-pro
#
FROM ubuntu:18.04
WORKDIR /tmp
RUN apt-get update \
 && apt-get install --no-install-recommends -y build-essential
COPY uname_wrapper.c /tmp/
RUN gcc -o /tmp/uname_wrapper.so /tmp/uname_wrapper.c -Wall -Werror -fPIC -shared -ldl
RUN strip /tmp/uname_wrapper.so
#  Pull base image.
FROM jlesage/baseimage-gui:alpine-3.8-glibc-v3.5.2
#  Define software versions.
ARG CRASHPLAN_VERSION=7.0.0
ARG CRASHPLAN_TIMESTAMP=1525200006700
ARG CRASHPLAN_BUILD=581
#  Define software download URLs.
#  NOTE: Do not use the following URL, as it may not point to the latest build
#        number:
#  https://download.code42.com/installs/linux/install/CrashPlanSmb/CrashPlanSmb_${CRASHPLAN_VERSION}_Linux.tgz
ARG CRASHPLAN_URL=https://web-eam-msp.crashplanpro.com/client/installers/CrashPlanSmb_${CRASHPLAN_VERSION}_${CRASHPLAN_TIMESTAMP}_${CRASHPLAN_BUILD}_Linux.tgz
#  Define container build variables.
ARG TARGETDIR=/usr/local/crashplan
#  Define working directory.
WORKDIR /tmp
#  Install CrashPlan.
RUN add-pkg --virtual build-dependencies cpio curl \
 && echo "Installing CrashPlan..." \
 && curl -# -L ${CRASHPLAN_URL} | tar -xz \
 && mkdir -p ${TARGETDIR} \
 && cat $( ls crashplan-install/*.cpi ;) | gzip -d -c - | cpio -i --no-preserve-owner --directory=${TARGETDIR} \
 && mv "${TARGETDIR}"/*.asar "${TARGETDIR}/electron/resources" \
 && chmod 755 "${TARGETDIR}/electron/crashplan" \
 && chmod 755 "${TARGETDIR}/bin/CrashPlanService" \
 && chmod 755 "${TARGETDIR}/bin/restore-tool" \
 && mv ${TARGETDIR}/conf /defaults/conf \
 && sed-patch '/<orgType>BUSINESS<\/orgType>/a \\t<serviceUIConfig>\n\t\t<serviceHost>127.0.0.1<\/serviceHost>\n\t<\/serviceUIConfig>' /defaults/conf/default.service.xml \
 && sed-patch "s|<backupConfig>|<backupConfig>\n\t\t\t<manifestPath>/usr/local/var/crashplan</manifestPath>|g" /defaults/conf/default.service.xml \
 && sed-patch '/<serviceUIConfig>/i\\t<javaMemoryHeapMax nil="true"/>' /defaults/conf/default.service.xml \
 && mkdir -p /usr/local/var/crashplan \
 && rm -r /usr/local/crashplan/upgrade \
 && touch /usr/local/crashplan/upgrade \
 && chmod 400 /usr/local/crashplan/upgrade \
 && ln -s /config/conf $TARGETDIR/conf \
 && ln -s /config/cache $TARGETDIR/cache \
 && rm -r $TARGETDIR/log \
 && ln -s /config/log $TARGETDIR/log \
 && ln -s /config/var /var/lib/crashplan \
 && ln -s /config/repository/metadata /usr/local/crashplan/metadata \
 && del-pkg build-dependencies \
 && rm -rf /tmp/*
#  Misc adjustments.
RUN sed-patch '/^nobody:/d' /defaults/passwd \
 && sed-patch '/^nobody:/d' /defaults/group \
 && sed-patch '/^nobody:/d' /defaults/shadow \
 && echo > /etc/fstab \
 && echo "${CRASHPLAN_VERSION}" > /defaults/cp_version
#  Install dependencies.
RUN add-pkg libselinux --repository http://dl-cdn.alpinelinux.org/alpine/edge/community \
 && add-pkg gtk+3.0 libxscrnsaver nss eudev gconf xdotool yad bc
#  Adjust the openbox config.
RUN sed-patch 's/<application type="normal">/<application type="normal" title="Code42">/' /etc/xdg/openbox/rc.xml \
 && sed-patch '/<application type="normal" title="Code42">/a \ <layer>below</layer>' /etc/xdg/openbox/rc.xml
#  Enable log monitoring.
RUN sed-patch 's|LOG_FILES=|LOG_FILES=/config/log/service.log|' /etc/logmonitor/logmonitor.conf \
 && sed-patch 's|STATUS_FILES=|STATUS_FILES=/config/log/app.log|' /etc/logmonitor/logmonitor.conf
#  Generate and install favicons.
RUN APP_ICON_URL=https://github.com/jlesage/docker-templates/raw/master/jlesage/images/crashplan-pro-icon.png \
 && install_app_icon.sh "$APP_ICON_URL"
#  Add files.
COPY rootfs/ /
COPY --from=0 /tmp/uname_wrapper.so /usr/local/crashplan/
#  Set environment variables.
ENV S6_WAIT_FOR_SERVICE_MAXTIME="10000" \
    APP_NAME="CrashPlan for Small Business" \
    KEEP_APP_RUNNING="1"
#  Define mountable directories.
VOLUME ["/config"]
VOLUME ["/storage"]
#  Metadata.
LABEL org.label-schema.name="crashplan-pro" \
      org.label-schema.description="Docker container for CrashPlan PRO" \
      org.label-schema.version="unknown" \
      org.label-schema.vcs-url="https://github.com/jlesage/docker-crashplan-pro" \
      org.label-schema.schema-version="1.0"
