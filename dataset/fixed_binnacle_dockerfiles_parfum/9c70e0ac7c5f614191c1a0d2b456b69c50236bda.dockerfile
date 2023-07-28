#  Copyright 2018 Splunk
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
ARG SPLUNK_BASE_IMAGE=base-debian-10
#
#  Download and unpack Splunk Enterprise
#
FROM ${SPLUNK_BASE_IMAGE}:latest AS package
ARG SPLUNK_BUILD_URL
COPY splunk/common-files/make-minimal-exclude.py /tmp
RUN python /tmp/make-minimal-exclude.py ${SPLUNK_BUILD_URL} > /tmp/splunk-minimal-exclude.list
RUN wget -qO /tmp/splunk.tgz ${SPLUNK_BUILD_URL}
RUN wget -qO /tmp/splunk.tgz.md5 ${SPLUNK_BUILD_URL}.md5
RUN test $( md5sum /tmp/splunk.tgz | sed 's,\([a-z0-9]*\).*,\1,' ;) = $( cat /tmp/splunk.tgz.md5 | sed 's,MD5.*=.\([a-z0-9]*\).*,\1,' ;)
RUN mkdir -p /minimal/splunk/var /extras/splunk/var
RUN tar -C /minimal/splunk --strip 1 --exclude-from=/tmp/splunk-minimal-exclude.list -zxf /tmp/splunk.tgz
RUN tar -C /extras/splunk --strip 1 --wildcards --files-from=/tmp/splunk-minimal-exclude.list -zxf /tmp/splunk.tgz
RUN mv /minimal/splunk/etc /minimal/splunk-etc
RUN mv /extras/splunk/etc /extras/splunk-etc
RUN mkdir -p /minimal/splunk/etc /minimal/splunk/share/splunk/search_mrsparkle/modules.new
COPY splunk/common-files/apps /extras/splunk-etc/apps/
#
#  Minimal Splunk base image with many files excluded, intended for internal and experimental use
#
FROM ${SPLUNK_BASE_IMAGE}:latest AS minimal
LABEL maintainer="support@splunk.com"
ENV SPLUNK_HOME="/opt/splunk" \
    SPLUNK_GROUP="splunk" \
    SPLUNK_USER="splunk"
ENV TMPSPLUNKDIR="${SPLUNK_HOME}/tmp"
ENV TMPETCDIR="${TMPSPLUNKDIR}/etc"
#  Currently kubernetes only accepts UID and not USER field to
#  start a container as a particular user. So we create Splunk
#  user with pre-determined UID.
ARG UID=41812
ARG GID=41812
#  Simple script used to populate/upgrade splunk/etc directory
COPY splunk/common-files/updateetc.sh /sbin/
#  Setup users and groups
RUN groupadd -r -g ${GID} ${SPLUNK_GROUP} \
 && useradd -r -m -u ${UID} -g ${GID} ${SPLUNK_USER} \
 && chmod 755 /sbin/updateetc.sh
COPY --chown=splunk:splunk --from=package /minimal /opt
USER ${SPLUNK_USER}
WORKDIR ${SPLUNK_HOME}
EXPOSE 8000/tcp 8089/tcp
#
#  Bare Splunk Enterprise Image without Ansible (BYO entrypoint)
#
FROM minimal AS bare
COPY --chown=splunk:splunk --from=package /extras /opt
EXPOSE 8000/tcp 8065/tcp 8088/tcp 8089/tcp 8191/tcp 9887/tcp 9997/tcp
VOLUME [ "/opt/splunk/etc", "/opt/splunk/var" ]
#
#  Full Splunk Enterprise Image with Ansible
#
FROM bare
ARG SPLUNK_DEFAULTS_URL
ENV SPLUNK_ROLE="splunk_standalone" \
    SPLUNK_DEFAULTS_URL="${SPLUNK_DEFAULTS_URL}" \
    SPLUNK_ANSIBLE_HOME="/opt/ansible" \
    ANSIBLE_USER="ansible" \
    ANSIBLE_GROUP="ansible" \
    CONTAINER_ARTIFACT_DIR="/opt/container_artifact"
USER root
COPY splunk/common-files/entrypoint.sh splunk/common-files/createdefaults.py splunk/common-files/checkstate.sh /sbin/
COPY splunk-ansible ${SPLUNK_ANSIBLE_HOME}
#  Set sudo rights
RUN sed -i -e 's/%sudo\s\+ALL=(ALL\(:ALL\)\?)\s\+ALL/%sudo ALL=NOPASSWD:ALL/g' /etc/sudoers \
 && sudo echo -e '\nansible ALL=(splunk)NOPASSWD:ALL' >> /etc/sudoers \
 && groupadd -r ${ANSIBLE_GROUP} \
 && useradd -r -m -g ${ANSIBLE_GROUP} ${ANSIBLE_USER} \
 && usermod -aG sudo ${ANSIBLE_USER} \
 && mkdir ${CONTAINER_ARTIFACT_DIR} \
 && chown -R ${ANSIBLE_USER}:${ANSIBLE_GROUP} $CONTAINER_ARTIFACT_DIR \
 && chmod -R 555 ${SPLUNK_ANSIBLE_HOME} \
 && chmod -R 777 ${CONTAINER_ARTIFACT_DIR} \
 && chmod 755 /sbin/entrypoint.sh /sbin/createdefaults.py /sbin/checkstate.sh
USER ${ANSIBLE_USER}
HEALTHCHECK --interval=30s --timeout=30s --start-period=180s --retries=5 CMD /sbin/checkstate.sh || exit 1
ENTRYPOINT ["/sbin/entrypoint.sh"]
CMD ["start-service"]
