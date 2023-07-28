#   Licensed to the Apache Software Foundation (ASF) under one
#   or more contributor license agreements.  See the NOTICE file
#   distributed with this work for additional information
#   regarding copyright ownership.  The ASF licenses this file
#   to you under the Apache License, Version 2.0 (the
#   "License"); you may not use this file except in compliance
#   with the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing,
#   software distributed under the License is distributed on an
#   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#   KIND, either express or implied.  See the License for the
#   specific language governing permissions and limitations
#   under the License.
#
#   CloudStack-simulator build
FROM ubuntu:16.04
MAINTAINER "Apache CloudStack" <dev@cloudstack.apache.org>
LABEL Vendor="Apache.org" \
      License="ApacheV2" \
      Version="4.12.0-SNAPSHOT"
RUN apt-get update -y \
 && apt-get install --no-install-recommends genisoimage=9:1.1.11-3ubuntu1 libffi-dev=3.2.1-4 libssl-dev=1.0.2g-1ubuntu4.20 git=1:2.7.4-0ubuntu1.10 sudo=1.8.16-0ubuntu1.10 ipmitool=1.8.16-3ubuntu0.2 maven=3.3.9-3 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 python-dev=2.7.12-1~16.04 python-setuptools=20.7.0-1 python-pip=8.1.1-2ubuntu0.6 python-mysql.connector=2.0.4-1 supervisor=3.2.0-2ubuntu0.2 python-crypto=2.6.1-6ubuntu0.16.04.3 python-openssl=0.15.1-2ubuntu0.2 -y
RUN echo 'mysql-server mysql-server/root_password password root' | debconf-set-selections ; echo 'mysql-server mysql-server/root_password_again password root' | debconf-set-selections
RUN apt-get install --no-install-recommends mysql-server=5.7.33-0ubuntu0.16.04.1 -qqy \
 && apt-get clean all \
 && mkdir /var/run/mysqld ; chown mysql /var/run/mysqld
#
#   this package is needed if one wants to run marvin tests from
#   inside the running simulator.
#
RUN pip install pyOpenSSL==23.1.1
RUN echo '''sql_mode = "STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION"''' >> /etc/mysql/mysql.conf.d/mysqld.cnf
RUN (/usr/bin/mysqld_safe &) ; sleep 5 ; mysqladmin -u root -proot password ''
COPY agent /root/agent
COPY api /root/api
COPY build /root/build
COPY client /root/client
COPY cloud-cli /root/cloud-cli
COPY cloudstack.iml /root/cloudstack.iml
COPY core /root/core
COPY debian /root/debian
COPY deps /root/deps
COPY developer /root/developer
COPY engine /root/engine
COPY framework /root/framework
COPY LICENSE.header /root/LICENSE.header
COPY LICENSE /root/LICENSE
COPY maven-standard /root/maven-standard
COPY NOTICE /root/NOTICE
COPY packaging /root/packaging
COPY plugins /root/plugins
COPY pom.xml /root/pom.xml
COPY python /root/python
COPY quickcloud /root/quickcloud
COPY requirements.txt /root/requirements.txt
COPY scripts /root/scripts
COPY server /root/server
COPY services /root/services
COPY setup /root/setup
COPY systemvm /root/systemvm
COPY target /root/target
COPY test/bindirbak /root/test/bindirbak
COPY test/conf /root/test/conf
COPY test/metadata /root/test/metadata
COPY test/pom.xml /root/test/pom.xml
COPY test/scripts /root/test/scripts
COPY test/selenium /root/test/selenium
COPY test/systemvm /root/test/systemvm
COPY test/target /root/test/target
COPY tools/pom.xml /root/tools/pom.xml
COPY tools/apidoc /root/tools/apidoc
COPY tools/checkstyle /root/tools/checkstyle
COPY tools/devcloud4/pom.xml /root/tools/devcloud4/pom.xml
COPY tools/devcloud-kvm/pom.xml /root/tools/devcloud-kvm/pom.xml
COPY tools/marvin/pom.xml /root/tools/marvin/pom.xml
COPY tools/pom.xml /root/tools/pom.xml
COPY ui /root/ui
COPY usage /root/usage
COPY utils /root/utils
COPY vmware-base /root/vmware-base
RUN cd /root \
 && mvn -Pdeveloper -Dsimulator -DskipTests -pl "!:cloud-marvin" install
RUN (/usr/bin/mysqld_safe &) \
 && sleep 5 \
 && cd /root \
 && mvn -Pdeveloper -pl developer -Ddeploydb \
 && mvn -Pdeveloper -pl developer -Ddeploydb-simulator
COPY tools/marvin /root/tools/marvin
COPY tools/docker/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
COPY tools/docker/docker_run_tests.sh /root
RUN cd /root \
 && mvn -Pdeveloper -Dsimulator -DskipTests -pl ":cloud-marvin"
RUN MARVIN_FILE=`find /root/tools/marvin/dist/ -name "Marvin*.tar.gz" ` \
 && pip install $MARVIN_FILE
COPY test/integration /root/test/integration
COPY tools /root/tools
RUN pip install pyOpenSSL==23.1.1 --upgrade
EXPOSE 8080/tcp 8096/tcp
WORKDIR /root
CMD ["/usr/bin/supervisord"]
#   --------------------------------
#
#   docker run -v ~/dev/tmp:/tmp -v ~/IdeaProjects/cloudstack/test/integration/smoke:/root/test/integration/smoke -it
#   --name simulator -p 8080:8080 -p8096:8096 simulator:4.12
#
#   docker exec -it simulator bash
#
#   cat /root/docker_run_tests.sh
#   for instructions
#
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
