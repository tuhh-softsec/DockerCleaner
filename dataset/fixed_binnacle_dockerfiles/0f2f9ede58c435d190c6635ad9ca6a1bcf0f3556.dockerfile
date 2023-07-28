#   Copyright 2015 gRPC authors.
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
FROM ubuntu:15.10
#   Install Git and basic packages.
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf autotools-dev build-essential bzip2 ccache curl gcc gcc-multilib git golang gyp lcov libc6 libc6-dbg libc6-dev libgtest-dev libtool make perl strace python-dev python-setuptools python-yaml telnet unzip wget zip -y \
 && apt-get clean
#  ================
#   Build profiling
RUN apt-get update \
 && apt-get install --no-install-recommends time -y \
 && apt-get clean
#   Google Cloud platform API libraries
RUN apt-get update \
 && apt-get install --no-install-recommends python-pip -y \
 && apt-get clean
RUN pip install google-api-python-client==2.85.0 --upgrade
#  ====================
#   Python dependencies
#   Install dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends python-all-dev python3-all-dev python-pip -y
#   Install Python packages from PyPI
RUN pip install pip==23.1 --upgrade
RUN pip install virtualenv==20.21.0
RUN pip install futures==2.2.0 enum34==1.0.4 protobuf==3.2.0 six==1.10.0
#  ========================
#   Sanity test dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends python-pip autoconf automake libtool curl python-virtualenv python-lxml -y
RUN pip install simplejson==3.19.1 mako==1.2.4
#  ======================================
#   More sanity test dependencies (bazel)
RUN apt-get install --no-install-recommends openjdk-8-jdk -y
#   Check out Bazel version 0.4.1 since this version allows running
#   ./compile.sh without a local protoc dependency
#   TODO(mattkwong): install dependencies to support latest Bazel version if newer
#   version is needed
RUN git clone https://github.com/bazelbuild/bazel.git /bazel \
 && cd /bazel \
 && git checkout tags/0.4.1 \
 && ./compile.sh
RUN ln -s /bazel/output/bazel /bin/
RUN apt-get update \
 && apt-get install --no-install-recommends wget -y
RUN echo deb http://llvm.org/apt/wily/ llvm-toolchain-wily-3.8 main >> /etc/apt/sources.list
RUN echo deb-src http://llvm.org/apt/wily/ llvm-toolchain-wily-3.8 main >> /etc/apt/sources.list
RUN wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | apt-key add -
RUN apt-get update \
 && apt-get install --no-install-recommends clang-format-3.8 -y
#   Prepare ccache
RUN ln -s /usr/bin/ccache /usr/local/bin/gcc
RUN ln -s /usr/bin/ccache /usr/local/bin/g++
RUN ln -s /usr/bin/ccache /usr/local/bin/cc
RUN ln -s /usr/bin/ccache /usr/local/bin/c++
RUN ln -s /usr/bin/ccache /usr/local/bin/clang
RUN ln -s /usr/bin/ccache /usr/local/bin/clang++
RUN mkdir /var/local/jenkins
#   Define the default command.
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
