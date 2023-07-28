#   Licensed to the Apache Software Foundation (ASF) under one
#   or more contributor license agreements.  See the NOTICE file
#   distributed with this work for additional information
#   regarding copyright ownership.  The ASF licenses this file
#   to you under the Apache License, Version 2.0 (the
#   "License"); you may not use this file except in compliance
#   with the License.  You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#   Dockerfile for installing the necessary dependencies for building Hadoop.
#   See BUILDING.txt.
FROM ubuntu:trusty
WORKDIR /root
#  #####
#   Install common dependencies from packages
#  #####
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 curl=7.35.0-1ubuntu2.20 ant=1.9.3-2ubuntu0.1 make=3.81-8.2ubuntu3 maven=3.0.5-1 cmake=2.8.12.2-0ubuntu3 gcc=4:4.8.2-1ubuntu6 g++=4:4.8.2-1ubuntu6 protobuf-compiler=2.5.0-9ubuntu1 libprotoc-dev=2.5.0-9ubuntu1 protobuf-c-compiler=0.15-1build1 libprotobuf-dev=2.5.0-9ubuntu1 build-essential=11.6ubuntu6 libtool=2.4.2-1.7ubuntu1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 pkg-config=0.26-1ubuntu4 libssl-dev=1.0.1f-1ubuntu2.27 snappy=0.2-1 libsnappy-dev=1.1.0-1ubuntu1 bzip2=1.0.6-5 libbz2-dev=1.0.6-5 libjansson-dev=2.5-2ubuntu0.2 fuse=2.9.2-4ubuntu4.14.04.1 libfuse-dev=2.9.2-4ubuntu4.14.04.1 libcurl4-openssl-dev=7.35.0-1ubuntu2.20 python=2.7.5-5ubuntu3 python2.7=2.7.6-8ubuntu0.5 pylint=1.1.0-1 openjdk-7-jdk=7u211-2.6.17-0ubuntu0.1 doxygen=1.8.6-2 -y )
#   Fixing the Apache commons / Maven dependency problem under Ubuntu:
#   See http://wiki.apache.org/commons/VfsProblems
RUN cd /usr/share/maven/lib \
 && ln -s ../../java/commons-lang.jar .
#  ######
#   Oracle Java
#  ######
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y )
RUN add-apt-repository -y ppa:webupd8team/java
RUN :
#   Auto-accept the Oracle JDK license
RUN echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
RUN (apt-get update ;apt-get install --no-install-recommends oracle-java7-installer -y )
#   Auto-accept the Oracle JDK license
RUN echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
RUN (apt-get update ;apt-get install --no-install-recommends oracle-java8-installer -y )
#  #####
#   Install findbugs
#  #####
RUN mkdir -p /opt/findbugs \
 && curl -L https://sourceforge.net/projects/findbugs/files/findbugs/3.0.1/findbugs-noUpdateChecks-3.0.1.tar.gz/download -o /opt/findbugs.tar.gz \
 && tar xzf /opt/findbugs.tar.gz --strip-components 1 -C /opt/findbugs
ENV FINDBUGS_HOME="/opt/findbugs"
#  ###
#   Install shellcheck
#  ###
RUN (apt-get update ;apt-get install --no-install-recommends cabal-install=1.16.0.2-2 -y )
RUN cabal update \
 && cabal install shellcheck --global
#  ##
#   Avoid out of memory errors in builds
#  ##
ENV MAVEN_OPTS="-Xms256m -Xmx512m"
#  ##
#   Everything past this point is either not needed for testing or breaks Yetus.
#   So tell Yetus not to read the rest of the file:
#   YETUS CUT HERE
#  ##
#  ###
#   Install Forrest (for Apache Hadoop website)
#  ##
RUN mkdir -p /usr/local/apache-forrest ; curl -O http://archive.apache.org/dist/forrest/0.8/apache-forrest-0.8.tar.gz ; tar xzf *forrest* --strip-components 1 -C /usr/local/apache-forrest ; echo 'forrest.home=/usr/local/apache-forrest' > build.properties
#   Add a welcome message and environment checks.
COPY hadoop_env_checks.sh /root/hadoop_env_checks.sh
RUN chmod 755 /root/hadoop_env_checks.sh
RUN echo '~/hadoop_env_checks.sh' >> /root/.bashrc
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
