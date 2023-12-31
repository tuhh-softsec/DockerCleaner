#  Licensed to the Apache Software Foundation (ASF) under one
#  or more contributor license agreements.  See the NOTICE file
#  distributed with this work for additional information
#  regarding copyright ownership.  The ASF licenses this file
#  to you under the Apache License, Version 2.0 (the
#  "License"); you may not use this file except in compliance
#  with the License.  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#  first updated from upstream dev-support/docker/Dockerfile on commit
#      ac57c51f7ad25e312b4275665d62b34a5945422f
#
#  Dockerfile for installing the necessary dependencies for building Hadoop.
#  See BUILDING.txt.
FROM ubuntu:trusty
WORKDIR /root
ENV DEBIAN_FRONTEND="noninteractive"
ENV DEBCONF_TERSE="true"
# #####
#  Install common dependencies from packages
#
#  WARNING: DO NOT PUT JAVA APPS HERE! Otherwise they will install default
#  Ubuntu Java.  See Java section below!
# #####
RUN apt-get update -q \
 && apt-get install --no-install-recommends build-essential bzip2 cmake curl doxygen fuse g++ gcc git gnupg-agent make libbz2-dev libcurl4-openssl-dev libfuse-dev libperl-critic-perl libprotobuf-dev libprotoc-dev libsnappy-dev libssl-dev libtool pinentry-curses pkg-config protobuf-compiler protobuf-c-compiler python python2.7 python-pip rsync snappy zlib1g-dev wget -q -y
# ###
#  Apps that require Java.
#  Maven and ant depend on ubuntu trusty's headless jdk7. The install of
#  maven and ant will pull down this jdk even though we don't want it.
#  Do the maven and ant install here rather than later where the jdk7
#  will overwrite the jdk7 we actually want to use. See next section on jdks.
# ##
RUN apt-get update -q \
 && apt-get install --no-install-recommends ant maven -q -y
# ######
#  Install jdk7
# ######
#  The jdks in ubuntu trusty don't work. HDFS hangs on openjdk-7 151.
#  See HBASE-19204. So, we use the azul jdks because they are available, and
#  later versions of openjdk (openjdk-7 161). Below we add the azul repo and
#  then install its jdk.
RUN echo "dot_style = mega" > "/root/.wgetrc"
RUN echo "quiet = on" >> "/root/.wgetrc"
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 0x219BD9C9
RUN apt-get update -q \
 && apt-get install --no-install-recommends software-properties-common python-software-properties -q -y
RUN apt-add-repository 'deb http://repos.azulsystems.com/ubuntu stable main'
RUN apt-get update -q
RUN apt-get install --no-install-recommends zulu-7 -q -y
RUN update-alternatives --config java
RUN update-alternatives --config javac
ENV JAVA_HOME="/usr/lib/jvm/zulu-7-amd64"
ENV PATH="${JAVA_HOME}/bin:${PATH}"
#  Fixing the Apache commons / Maven dependency problem under Ubuntu:
#  See http://wiki.apache.org/commons/VfsProblems
RUN cd /usr/share/maven/lib \
 && ln -s ../../java/commons-lang.jar .
# #####
#  Install findbugs
# #####
RUN mkdir -p /opt/findbugs \
 && curl -L -s -S https://sourceforge.net/projects/findbugs/files/findbugs/3.0.1/findbugs-noUpdateChecks-3.0.1.tar.gz/download -o /opt/findbugs.tar.gz \
 && tar xzf /opt/findbugs.tar.gz --strip-components 1 -C /opt/findbugs
ENV FINDBUGS_HOME="/opt/findbugs"
# ###
#  Install shellcheck
# ###
RUN apt-get install cabal-install -q -y
RUN mkdir /root/.cabal
RUN echo "remote-repo: hackage.fpcomplete.com:http://hackage.fpcomplete.com/" >> /root/.cabal/config
# RUN echo "remote-repo: hackage.haskell.org:http://hackage.haskell.org/" > /root/.cabal/config
RUN echo "remote-repo-cache: /root/.cabal/packages" >> /root/.cabal/config
RUN cabal update
RUN cabal install shellcheck --global
# ###
#  Install bats
# ###
RUN add-apt-repository -y ppa:duggan/bats
RUN apt-get update -q
RUN apt-get install --no-install-recommends bats -q -y
# ###
#  Install pylint
# ###
RUN pip install pylint==1.9.2
# ###
#  Install dateutil.parser
# ###
RUN pip install python-dateutil
# ###
#  Install Ruby 2, based on Yetus 0.4.0 dockerfile
# ##
RUN echo 'gem: --no-rdoc --no-ri' >> /root/.gemrc
RUN apt-add-repository ppa:brightbox/ruby-ng
RUN apt-get update -q
RUN apt-get install --no-install-recommends ruby2.2 ruby2.2-dev ruby-switch -q -y
RUN ruby-switch --set ruby2.2
# ###
#  Install rubocop
# ##
RUN gem install rake
RUN gem install rubocop
# ###
#  Install ruby-lint
# ##
RUN gem install ruby-lint
# ##
#  Avoid out of memory errors in builds
# ##
ENV MAVEN_OPTS="-Xmx3g"
# ##
#  Everything past this point is either not needed for testing or breaks Yetus.
#  So tell Yetus not to read the rest of the file:
#  YETUS CUT HERE
# ##
