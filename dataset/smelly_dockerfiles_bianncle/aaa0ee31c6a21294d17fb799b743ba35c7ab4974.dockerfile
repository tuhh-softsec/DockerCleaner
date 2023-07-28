#  © Copyright IBM Corporation 2015, 2017
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
FROM ubuntu:16.04
LABEL maintainer="\"Arthur Barr <arthur.barr@uk.ibm.com>, Rob Parker <PARROBE@uk.ibm.com>\""
LABEL ProductID="98102d16795c4263ad9ca075190a2d4d" \
      ProductName="IBM MQ Advanced for Developers" \
      ProductVersion="9.0.4"
#  The URL to download the MQ installer from in tar.gz format
ARG MQ_URL=https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/messaging/mqadv/mqadv_dev904_ubuntu_x86-64.tar.gz
#  The MQ packages to install
ARG MQ_PACKAGES="ibmmq-server ibmmq-java ibmmq-jre ibmmq-gskit ibmmq-web ibmmq-msg-.*"
RUN export DEBIAN_FRONTEND=noninteractive \
 && apt-get update -y \
 && apt-get install --no-install-recommends bash bc ca-certificates coreutils curl debianutils file findutils gawk grep libc-bin lsb-release mount passwd procps sed tar util-linux -y \
 && export DIR_EXTRACT=/tmp/mq \
 && mkdir -p ${DIR_EXTRACT} \
 && cd ${DIR_EXTRACT} \
 && curl -LO $MQ_URL \
 && tar -zxvf ./*.tar.gz \
 && apt-get purge -y ca-certificates curl \
 && apt-get autoremove -y --purge \
 && groupadd --system --gid 999 mqm \
 && useradd --system --uid 999 --gid mqm mqm \
 && usermod -G mqm root \
 && export DIR_DEB=$( find ${DIR_EXTRACT} -name "*.deb" -printf "%h\n" | sort -u | head -1 ;) \
 && export MQLICENSE=$( find ${DIR_EXTRACT} -name "mqlicense.sh" ;) \
 && ${MQLICENSE} -text_only -accept \
 && echo "deb [trusted=yes] file:${DIR_DEB} ./" > /etc/apt/sources.list.d/IBM_MQ.list \
 && apt-get update \
 && apt-get install $MQ_PACKAGES -y \
 && find /opt/mqm /var/mqm -type f -exec file {} | awk -F: '/ELF 32-bit/{print $1}' | xargs --no-run-if-empty rm -f \
 && find /opt/mqm -name '*.tar.gz' -delete \
 && /opt/mqm/bin/setmqinst -p /opt/mqm -i \
 && rm -f /etc/apt/sources.list.d/IBM_MQ.list \
 && rm -rf ${DIR_EXTRACT} \
 && apt-get upgrade -y sensible-utils \
 && rm -rf /var/lib/apt/lists/* \
 && echo "mq:$( dspmqver -b -f 2 ;)" > /etc/debian_chroot \
 && rm -rf /var/mqm \
 && sed -i 's/PASS_MAX_DAYS\t99999/PASS_MAX_DAYS\t90/' /etc/login.defs \
 && sed -i 's/PASS_MIN_DAYS\t0/PASS_MIN_DAYS\t1/' /etc/login.defs \
 && sed -i 's/password\t\[success=1 default=ignore\]\tpam_unix\.so obscure sha512/password\t[success=1 default=ignore]\tpam_unix.so obscure sha512 minlen=8/' /etc/pam.d/common-password
COPY *.sh /usr/local/bin/
COPY *.mqsc /etc/mqm/
COPY admin.json /etc/mqm/
COPY mq-dev-config /etc/mqm/mq-dev-config
RUN chmod +x /usr/local/bin/*.sh
#  Always use port 1414 (the Docker administrator can re-map ports at runtime)
#  Expose port 9443 for the web console
EXPOSE 1414/tcp 9443/tcp
ENV LANG="en_US.UTF-8"
ENTRYPOINT ["mq.sh"]
