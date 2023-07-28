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
 && add-apt-repository ppa:ubuntu-toolchain-r/test \
 && apt-get update \
 && apt-get install --no-install-recommends ant=1.9.6-1ubuntu1.1 ant-contrib=1.0~b3+svn177-7 autoconf=2.69-9 build-essential=12.1ubuntu2 ca-certificates=20210119~16.04.1 cmake=3.5.1-1ubuntu3 cpio=2.11+dfsg-5ubuntu1.1 curl=7.47.0-1ubuntu2.19 libexpat1-dev=2.1.0-7ubuntu0.16.04.5 file=1:5.25-2ubuntu1.4 g++-7 gcc-7 gdb=7.11.1-0ubuntu1~16.5 git=1:2.7.4-0ubuntu1.10 git-core=1:2.7.4-0ubuntu1.10 libasound2-dev=1.1.0-0ubuntu1 libcups2-dev=2.1.3-4ubuntu0.11 libdwarf-dev=20120410-2+deb7u2build0.16.04.1 libelf-dev=0.165-3ubuntu1.2 libfontconfig libfontconfig1-dev=2.11.94-0ubuntu1.1 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libnuma-dev=2.0.11-1ubuntu1.1 libx11-dev=2:1.6.3-1ubuntu2.2 libxext-dev=2:1.3.3-1 libxrender-dev=1:0.9.9-0ubuntu1 libxt-dev=1:1.1.5-0ubuntu1 libxtst-dev=2:1.2.2-1 make=4.1-6 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 openssh-client=1:7.2p2-4ubuntu2.10 openssh-server=1:7.2p2-4ubuntu2.10 perl=5.22.1-9ubuntu0.9 pkg-config=0.29.1-0ubuntu1 realpath=8.25-2ubuntu3~16.04 ssh=1:7.2p2-4ubuntu2.10 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 xvfb=2:1.18.4-0ubuntu0.12 zip=3.0-11 `` -qq -y \
 && rm -rf /var/lib/apt/lists/*
#   Install Docker module to run test framework
RUN echo yes | cpan install JSON Text::CSV XML::Parser
#   Add user home/USER and copy authorized_keys and known_hosts
RUN useradd -ms /bin/bash ${USER} \
 && mkdir /home/${USER}/.ssh/
COPY authorized_keys /home/${USER}/.ssh/authorized_keys
COPY known_hosts /home/${USER}/.ssh/known_hosts
RUN chown -R ${USER}:${USER} /home/${USER} \
 && chmod 644 /home/${USER}/.ssh/authorized_keys \
 && chmod 644 /home/${USER}/.ssh/known_hosts
#   Set up sshd config
RUN mkdir /var/run/sshd \
 && sed -i 's/#PermitRootLogin/PermitRootLogin/' /etc/ssh/sshd_config \
 && sed -i 's/#RSAAuthentication.*/RSAAuthentication yes/' /etc/ssh/sshd_config \
 && sed -i 's/#PubkeyAuthentication.*/PubkeyAuthentication yes/' /etc/ssh/sshd_config
#   SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
#   Install OpenSSL v1.1.1b
#   Required for JITaaS & Crypto functional testing
RUN cd /tmp \
 && wget https://github.com/openssl/openssl/archive/OpenSSL_1_1_1b.tar.gz \
 && tar -xzf OpenSSL_1_1_1b.tar.gz \
 && rm -f OpenSSL_1_1_1b.tar.gz \
 && cd /tmp/openssl-OpenSSL_1_1_1b \
 && ./config --prefix=/usr/local/openssl-1.1.1b --openssldir=/usr/local/openssl-1.1.1b \
 && make \
 && make install \
 && cd .. \
 && rm -rf openssl-OpenSSL_1_1_1b \
 && echo "/usr/local/openssl-1.1.1b/lib" > /etc/ld.so.conf.d/openssl-1.1.1b.conf \
 && echo "PATH=/usr/local/openssl-1.1.1b/bin:$PATH" > /etc/environment
#   Install Protobuf v3.5.1
#   Required for JITaaS
RUN cd /tmp \
 && wget https://github.com/protocolbuffers/protobuf/releases/download/v3.5.1/protobuf-cpp-3.5.1.tar.gz \
 && tar -xzf protobuf-cpp-3.5.1.tar.gz \
 && rm -f protobuf-cpp-3.5.1.tar.gz \
 && cd /tmp/protobuf-3.5.1 \
 && ./configure \
 && make \
 && make install \
 && cd .. \
 && rm -rf protobuf-3.5.1
#   Run ldconfig to create necessary links and cache to shared libraries
RUN echo "/usr/local/lib" > /etc/ld.so.conf.d/usr-local.conf \
 && echo "/usr/local/lib64" >> /etc/ld.so.conf.d/usr-local.conf \
 && ldconfig
#   Expose SSH port and run SSH
EXPOSE 22/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
