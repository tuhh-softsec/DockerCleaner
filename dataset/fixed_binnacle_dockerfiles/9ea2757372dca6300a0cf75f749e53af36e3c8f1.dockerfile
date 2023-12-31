#   Copyright 2018 Splunk
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
ARG SPLUNK_BASE_IMAGE=base-debian-10
#
#   Download and unpack Splunk Universal Forwarder
#
FROM ${SPLUNK_BASE_IMAGE}:latest AS package
ARG SPLUNK_BUILD_URL
ENV SPLUNK_HOME="/opt/splunkforwarder"
RUN echo "Downloading Splunk and validating the checksum at: ${SPLUNK_BUILD_URL}"
RUN wget -qO /tmp/splunk.tgz ${SPLUNK_BUILD_URL}
RUN wget -qO /tmp/splunk.tgz.md5 ${SPLUNK_BUILD_URL}.md5
RUN test $( md5sum /tmp/splunk.tgz | sed 's,\([a-z0-9]*\).*,\1,' ;) = $( cat /tmp/splunk.tgz.md5 | sed 's,MD5.*=.\([a-z0-9]*\).*,\1,' ;)
RUN tar -C /opt -zxf /tmp/splunk.tgz
RUN mv ${SPLUNK_HOME}/etc ${SPLUNK_HOME}-etc
RUN mkdir -p ${SPLUNK_HOME}/etc ${SPLUNK_HOME}/var
COPY uf/common-files/apps ${SPLUNK_HOME}-etc/apps/
#
#   Bare Splunk Universal Forwarder Image without Ansible (BYO entrypoint)
#
FROM ${SPLUNK_BASE_IMAGE}:latest AS bare
LABEL maintainer="support@splunk.com"
ENV SPLUNK_HOME="/opt/splunkforwarder" \
    SPLUNK_GROUP="splunk" \
    SPLUNK_USER="splunk"
#   Simple script used to populate/upgrade splunk/etc directory
COPY splunk/common-files/updateetc.sh /sbin/
#   Setup users and groups
RUN groupadd -r ${SPLUNK_GROUP} \
 && useradd -r -m -g ${SPLUNK_GROUP} ${SPLUNK_USER} \
 && chmod 755 /sbin/updateetc.sh
#   Copy files from package
COPY --chown=splunk:splunk --from=package /opt /opt
USER ${SPLUNK_USER}
WORKDIR ${SPLUNK_HOME}
EXPOSE 8089/tcp 8088/tcp 9997/tcp
VOLUME [ "/opt/splunkforwarder/etc", "/opt/splunkforwarder/var" ]
#
#   Full Splunk Universal Forwarder Image with Ansible
#
FROM bare
ARG SPLUNK_DEFAULTS_URL
ENV SPLUNK_ROLE="splunk_universal_forwarder" \
    SPLUNK_DEFAULTS_URL="${SPLUNK_DEFAULTS_URL}" \
    SPLUNK_ANSIBLE_HOME="/opt/ansible" \
    SPLUNK_OPT="/opt" \
    ANSIBLE_USER="ansible" \
    ANSIBLE_GROUP="ansible" \
    CONTAINER_ARTIFACT_DIR="/opt/container_artifact"
#   Copy ansible playbooks
COPY splunk-ansible ${SPLUNK_ANSIBLE_HOME}
#   Copy scripts
COPY uf/common-files/entrypoint.sh uf/common-files/checkstate.sh uf/common-files/createdefaults.py /sbin/
USER root
#   Setup users and groups
RUN sed -i -e 's/%sudo\s\+ALL=(ALL\(:ALL\)\?)\s\+ALL/%sudo ALL=NOPASSWD:ALL/g' /etc/sudoers \
 && sudo echo -e '\nansible ALL=(splunk)NOPASSWD:ALL' >> /etc/sudoers \
 && groupadd -r ${ANSIBLE_GROUP} \
 && useradd -r -m -g ${ANSIBLE_GROUP} ${ANSIBLE_USER} \
 && usermod -aG sudo ${ANSIBLE_USER} \
 && mkdir ${CONTAINER_ARTIFACT_DIR} \
 && chown -R ${ANSIBLE_USER}:${ANSIBLE_GROUP} $CONTAINER_ARTIFACT_DIR \
 && chmod -R 555 ${SPLUNK_ANSIBLE_HOME} \
 && chmod 755 /sbin/entrypoint.sh /sbin/createdefaults.py /sbin/checkstate.sh
USER ${ANSIBLE_USER}
HEALTHCHECK --interval=30s --timeout=30s --start-period=180s --retries=5 CMD /sbin/checkstate.sh || exit 1
ENTRYPOINT ["/sbin/entrypoint.sh"]
CMD ["start-service"]
