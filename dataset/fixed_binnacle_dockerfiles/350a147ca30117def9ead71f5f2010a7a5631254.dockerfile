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
#   first updated from upstream dev-support/docker/Dockerfile on commit
#       ac57c51f7ad25e312b4275665d62b34a5945422f
#
#   Dockerfile for installing the necessary dependencies for building Hadoop.
#   See BUILDING.txt.
FROM ubuntu:trusty
WORKDIR /root
ENV DEBIAN_FRONTEND="noninteractive"
ENV DEBCONF_TERSE="true"
#  #####
#   Install common dependencies from packages
#
#   WARNING: DO NOT PUT JAVA APPS HERE! Otherwise they will install default
#   Ubuntu Java.  See Java section below!
#  #####
RUN apt-get update -q \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 bzip2=1.0.6-5 cmake=2.8.12.2-0ubuntu3 curl=7.35.0-1ubuntu2.20 doxygen=1.8.6-2 fuse=2.9.2-4ubuntu4.14.04.1 g++=4:4.8.2-1ubuntu6 gcc=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 gnupg-agent=2.0.22-3ubuntu1.4 make=3.81-8.2ubuntu3 libbz2-dev=1.0.6-5 libcurl4-openssl-dev=7.35.0-1ubuntu2.20 libfuse-dev=2.9.2-4ubuntu4.14.04.1 libperl-critic-perl=1.121-1 libprotobuf-dev=2.5.0-9ubuntu1 libprotoc-dev=2.5.0-9ubuntu1 libsnappy-dev=1.1.0-1ubuntu1 libssl-dev=1.0.1f-1ubuntu2.27 libtool=2.4.2-1.7ubuntu1 pinentry-curses=0.8.3-1ubuntu1 pkg-config=0.26-1ubuntu4 protobuf-compiler=2.5.0-9ubuntu1 protobuf-c-compiler=0.15-1build1 python=2.7.5-5ubuntu3 python2.7=2.7.6-8ubuntu0.5 python-pip=1.5.4-1ubuntu4 rsync=3.1.0-2ubuntu0.4 snappy=0.2-1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 wget=1.15-1ubuntu1.14.04.5 -q -y )
#  ###
#   Apps that require Java.
#   Maven and ant depend on ubuntu trusty's headless jdk7. The install of
#   maven and ant will pull down this jdk even though we don't want it.
#   Do the maven and ant install here rather than later where the jdk7
#   will overwrite the jdk7 we actually want to use. See next section on jdks.
#  ##
RUN apt-get update -q \
 && (apt-get update ;apt-get install --no-install-recommends ant=1.9.3-2ubuntu0.1 maven=3.0.5-1 -q -y )
#  ######
#   Install jdk7
#  ######
#   The jdks in ubuntu trusty don't work. HDFS hangs on openjdk-7 151.
#   See HBASE-19204. So, we use the azul jdks because they are available, and
#   later versions of openjdk (openjdk-7 161). Below we add the azul repo and
#   then install its jdk.
RUN echo "dot_style = mega" > "/root/.wgetrc"
RUN echo "quiet = on" >> "/root/.wgetrc"
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 0x219BD9C9
RUN apt-get update -q \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 python-software-properties=0.92.37.8 -q -y )
RUN apt-add-repository 'deb http://repos.azulsystems.com/ubuntu stable main'
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends zulu-7 -q -y )
RUN update-alternatives --config java
RUN update-alternatives --config javac
ENV JAVA_HOME="/usr/lib/jvm/zulu-7-amd64"
ENV PATH="${JAVA_HOME}/bin:${PATH}"
#   Fixing the Apache commons / Maven dependency problem under Ubuntu:
#   See http://wiki.apache.org/commons/VfsProblems
RUN cd /usr/share/maven/lib \
 && ln -s ../../java/commons-lang.jar .
#  #####
#   Install findbugs
#  #####
RUN mkdir -p /opt/findbugs \
 && curl -L -s -S https://sourceforge.net/projects/findbugs/files/findbugs/3.0.1/findbugs-noUpdateChecks-3.0.1.tar.gz/download -o /opt/findbugs.tar.gz \
 && tar xzf /opt/findbugs.tar.gz --strip-components 1 -C /opt/findbugs
ENV FINDBUGS_HOME="/opt/findbugs"
#  ###
#   Install shellcheck
#  ###
RUN (apt-get update ;apt-get install --no-install-recommends cabal-install=1.16.0.2-2 -q -y )
RUN mkdir /root/.cabal
RUN echo "remote-repo: hackage.fpcomplete.com:http://hackage.fpcomplete.com/" >> /root/.cabal/config
#  RUN echo "remote-repo: hackage.haskell.org:http://hackage.haskell.org/" > /root/.cabal/config
RUN echo "remote-repo-cache: /root/.cabal/packages" >> /root/.cabal/config
RUN cabal update
RUN cabal install shellcheck --global
#  ###
#   Install bats
#  ###
RUN add-apt-repository -y ppa:duggan/bats
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends bats -q -y )
#  ###
#   Install pylint
#  ###
RUN pip install pylint==1.9.2
#  ###
#   Install dateutil.parser
#  ###
RUN pip install python-dateutil==2.8.2
#  ###
#   Install Ruby 2, based on Yetus 0.4.0 dockerfile
#  ##
RUN echo 'gem: --no-rdoc --no-ri' >> /root/.gemrc
RUN apt-add-repository ppa:brightbox/ruby-ng
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends ruby2.2 ruby2.2-dev ruby-switch -q -y )
RUN ruby-switch --set ruby2.2
#  ###
#   Install rubocop
#  ##
RUN gem install rake --version 13.0.6
RUN gem install rubocop --version 1.50.1
#  ###
#   Install ruby-lint
#  ##
RUN gem install ruby-lint --version 2.3.1
#  ##
#   Avoid out of memory errors in builds
#  ##
ENV MAVEN_OPTS="-Xmx3g"
#  ##
#   Everything past this point is either not needed for testing or breaks Yetus.
#   So tell Yetus not to read the rest of the file:
#   YETUS CUT HERE
#  ##
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
