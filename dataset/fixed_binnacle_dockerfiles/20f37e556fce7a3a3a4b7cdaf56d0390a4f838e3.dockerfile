#   Copyright (c) 2018, 2019 IBM Corp. and others
#
#   This program and the accompanying materials are made available under
#   the terms of the Eclipse Public License 2.0 which accompanies this
#   distribution and is available at https://www.eclipse.org/legal/epl-2.0/
#   or the Apache License, Version 2.0 which accompanies this distribution and
#   is available at https://www.apache.org/licenses/LICENSE-2.0.
#
#   This Source Code may also be made available under the following
#   Secondary Licenses when the conditions for such availability set
#   forth in the Eclipse Public License, v. 2.0 are satisfied: GNU
#   General Public License, version 2 with the GNU Classpath
#   Exception [1] and GNU General Public License, version 2 with the
#   OpenJDK Assembly Exception [2].
#
#   [1] https://www.gnu.org/software/classpath/license.html
#   [2] http://openjdk.java.net/legal/assembly-exception.html
#
#   SPDX-License-Identifier: EPL-2.0 OR Apache-2.0 OR GPL-2.0 WITH Classpath-exception-2.0 OR LicenseRef-GPL-2.0 WITH Assembly-exception
#   To use this docker file:
#   First copy your public ssh key into a file named authorized_keys next to the Dockerfile
#   Then include a known_hosts file next to the Dockerfile, with github as a saved host
#   This can be done with "ssh-keyscan github.com >> path_to_dockerfile/known_hosts"
#   Make sure you are in the directory containing the Dockerfile, authorized_keys file, and known_hosts file
#   Then run:
#     docker build -t openj9 -f Dockerfile .
#     docker run -it openj9
FROM ubuntu:16.04
#   Install required OS tools
ENV USER="jenkins"
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 -qq -y \
 && apt-get update \
 && apt-get install --no-install-recommends ant=1.9.6-1ubuntu1.1 ant-contrib=1.0~b3+svn177-7 autoconf=2.69-9 build-essential=12.1ubuntu2 ca-certificates=20210119~16.04.1 cpio=2.11+dfsg-5ubuntu1.1 curl=7.47.0-1ubuntu2.19 libexpat1-dev=2.1.0-7ubuntu0.16.04.5 file=1:5.25-2ubuntu1.4 gdb=7.11.1-0ubuntu1~16.5 git=1:2.7.4-0ubuntu1.10 git-core=1:2.7.4-0ubuntu1.10 libasound2-dev=1.1.0-0ubuntu1 libcups2-dev=2.1.3-4ubuntu0.11 libdwarf-dev=20120410-2+deb7u2build0.16.04.1 libelf-dev=0.165-3ubuntu1.2 libfontconfig libfontconfig1-dev=2.11.94-0ubuntu1.1 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libmpc3=1.0.3-1 libx11-dev=2:1.6.3-1ubuntu2.2 libxext-dev=2:1.3.3-1 libxrandr-dev=2:1.5.0-1 libxrender-dev=1:0.9.9-0ubuntu1 libxt-dev=1:1.1.5-0ubuntu1 libxtst-dev=2:1.2.2-1 make=4.1-6 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 openssh-client=1:7.2p2-4ubuntu2.10 openssh-server=1:7.2p2-4ubuntu2.10 perl=5.22.1-9ubuntu0.9 pkg-config=0.29.1-0ubuntu1 realpath=8.25-2ubuntu3~16.04 ssh=1:7.2p2-4ubuntu2.10 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 xvfb=2:1.18.4-0ubuntu0.12 zip=3.0-11 `` -qq -y \
 && rm -rf /var/lib/apt/lists/*
#   Dependency required by test framework
RUN wget -O - http://cpanmin.us | perl - --self-upgrade \
 && cpanm Text::CSV \
 && cpanm JSON \
 && cpanm XML::Parser
#   Install GCC-7.4
RUN cd /usr/local \
 && wget -O gcc-7.tar.xz "https://ci.adoptopenjdk.net/userContent/gcc/gcc740+ccache.s390x.tar.xz" \
 && tar -xJf gcc-7.tar.xz --strip-components=1 \
 && rm -rf gcc-7.tar.xz
#   Create links for c++,g++,cc,gcc and for GCC to access the C library
#   There is a true at the end of the library link because it throws an error and it allows the container to be built
RUN ln -s /usr/lib/s390x-linux-gnu /usr/lib64 \
 && ln -s /usr/include/s390x-linux-gnu/* /usr/local/include | true \
 && ln -s /usr/local/bin/g++-7.4 /usr/bin/g++-7 \
 && ln -s /usr/local/bin/gcc-7.4 /usr/bin/gcc-7
#   Edit ldconfig to connect the new libstdc++.so* library
RUN echo "/usr/local/lib64" >> /etc/ld.so.conf.d/local.conf \
 && ldconfig
#   Install cmake version 3.11.4
RUN cd /tmp \
 && wget https://cmake.org/files/v3.11/cmake-3.11.4.tar.gz \
 && tar xzf cmake-3.11.4.tar.gz \
 && rm cmake-3.11.4.tar.gz \
 && cd /tmp/cmake-3.11.4 \
 && ./configure \
 && make \
 && make install
#   Add user home/USER and copy authorized_keys and known_hosts
RUN useradd -ms /bin/bash ${USER} \
 && mkdir /home/${USER}/.ssh/
COPY authorized_keys /home/${USER}/.ssh/authorized_keys
COPY known_hosts /home/${USER}/.ssh/known_hosts
RUN chown -R ${USER}:${USER} /home/${USER} \
 && chmod 644 /home/${USER}/.ssh/authorized_keys \
 && chmod 644 /home/${USER}/.ssh/known_hosts \
 && chmod 700 /home/${USER}/.ssh
#   Download and setup freemarker.jar to /home/USER/freemarker.jar
RUN cd /home/${USER} \
 && wget https://sourceforge.net/projects/freemarker/files/freemarker/2.3.8/freemarker-2.3.8.tar.gz/download -O freemarker.tgz \
 && tar -xzf freemarker.tgz freemarker-2.3.8/lib/freemarker.jar --strip=2 \
 && rm -f freemarker.tgz
#   Download and install boot JDK from AdoptOpenJDK for java 8
RUN mkdir -p /usr/lib/jvm/adoptojdk-java-80 \
 && cd /usr/lib/jvm/adoptojdk-java-80 \
 && wget -O bootjdk8.tar.gz "https://api.adoptopenjdk.net/v2/binary/releases/openjdk8?openjdk_impl=openj9&os=linux&arch=s390x&release=latest&type=jdk" \
 && tar -xzf bootjdk8.tar.gz \
 && rm -f bootjdk8.tar.gz \
 && mv $( ls | grep -i jdk8 ;) bootjdk8 \
 && mv bootjdk8/* /usr/lib/jvm/adoptojdk-java-80 \
 && rm -rf bootjdk8
#   Download and install boot JDK from AdoptOpenJDK for java 11 and 12
RUN mkdir -p /usr/lib/jvm/adoptojdk-java-11 \
 && cd /usr/lib/jvm/adoptojdk-java-11 \
 && wget -O bootjdk11.tar.gz "https://api.adoptopenjdk.net/v2/binary/releases/openjdk11?openjdk_impl=openj9&os=linux&arch=s390x&release=latest&type=jdk" \
 && tar -xzf bootjdk11.tar.gz \
 && rm -f bootjdk11.tar.gz \
 && mv $( ls | grep -i jdk-11 ;) bootjdk11 \
 && mv bootjdk11/* /usr/lib/jvm/adoptojdk-java-11 \
 && rm -rf bootjdk11
#   Download and install boot JDK from AdoptOpenJDK for java 13 and higher
RUN mkdir -p /usr/lib/jvm/adoptojdk-java-12 \
 && cd /usr/lib/jvm/adoptojdk-java-12 \
 && wget -O bootjdk12.tar.gz "https://api.adoptopenjdk.net/v2/binary/releases/openjdk12?openjdk_impl=openj9&os=linux&arch=s390x&release=latest&type=jdk" \
 && tar -xzf bootjdk12.tar.gz \
 && rm -f bootjdk12.tar.gz \
 && mv $( ls | grep -i jdk-12 ;) bootjdk12 \
 && mv bootjdk12/* /usr/lib/jvm/adoptojdk-java-12 \
 && rm -rf bootjdk12
#   Set up sshd config
RUN mkdir /var/run/sshd \
 && sed -i 's/#PermitRootLogin/PermitRootLogin/' /etc/ssh/sshd_config \
 && sed -i 's/#RSAAuthentication.*/RSAAuthentication yes/' /etc/ssh/sshd_config \
 && sed -i 's/#PubkeyAuthentication.*/PubkeyAuthentication yes/' /etc/ssh/sshd_config
#   SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
#   Expose SSH port
EXPOSE 22/tcp
#   Setup a reference repository cache for faster clones in the container
RUN mkdir /home/${USER}/openjdk_cache \
 && cd /home/${USER}/openjdk_cache \
 && git init --bare \
 && git remote add jdk8 https://github.com/ibmruntimes/openj9-openjdk-jdk8.git \
 && git remote add jdk11 https://github.com/ibmruntimes/openj9-openjdk-jdk11.git \
 && git remote add jdk12 https://github.com/ibmruntimes/openj9-openjdk-jdk12.git \
 && git remote add jdk13 https://github.com/ibmruntimes/openj9-openjdk-jdk13.git \
 && git remote add jdk https://github.com/ibmruntimes/openj9-openjdk-jdk.git \
 && git remote add openj9 https://github.com/eclipse/openj9.git \
 && git remote add omr https://github.com/eclipse/openj9-omr.git \
 && git fetch --all \
 && git gc --aggressive --prune=all
#   Adding bash profile so ${USER} max user processes will be unlimited
RUN echo >> /home/${USER}/.bashrc \
 && echo "# Change max user processes in ${USER}" >> /home/${USER}/.bashrc \
 && echo "ulimit -u unlimited" >> /home/${USER}/.bashrc
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
