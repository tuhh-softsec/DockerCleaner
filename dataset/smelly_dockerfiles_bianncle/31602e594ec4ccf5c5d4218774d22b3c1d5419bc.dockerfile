#  Copyright 2015 gRPC authors.
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
FROM debian:jessie
#  Install Git and basic packages.
RUN apt-get update \
 && apt-get install autoconf autotools-dev build-essential bzip2 ccache curl gcc gcc-multilib git golang gyp lcov libc6 libc6-dbg libc6-dev libgtest-dev libtool make perl strace python-dev python-setuptools python-yaml telnet unzip wget zip -y \
 && apt-get clean
# ================
#  Build profiling
RUN apt-get update \
 && apt-get install time -y \
 && apt-get clean
#  Google Cloud platform API libraries
RUN apt-get update \
 && apt-get install python-pip -y \
 && apt-get clean
RUN pip install google-api-python-client --upgrade
#  Install Electron apt dependencies
RUN apt-get update \
 && apt-get install libasound2 libgconf-2-4 libgtk2.0-0 libnss3 libxss1 libxtst6 xvfb -y
# ====================
#  Python dependencies
#  Install dependencies
RUN apt-get update \
 && apt-get install python-all-dev python3-all-dev python-pip -y
#  Install Python packages from PyPI
RUN pip install pip --upgrade
RUN pip install virtualenv
RUN pip install futures==2.2.0 enum34==1.0.4 protobuf==3.2.0 six==1.10.0
# ==================
#  Node dependencies
#  Install nvm
RUN touch .profile
RUN curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.25.4/install.sh | bash
#  Install all versions of node that we want to test
RUN /bin/bash -l -c "nvm install 4 \
 && npm config set cache /tmp/npm-cache \
 && npm install -g npm"
RUN /bin/bash -l -c "nvm install 5 \
 && npm config set cache /tmp/npm-cache \
 && npm install -g npm"
RUN /bin/bash -l -c "nvm install 6 \
 && npm config set cache /tmp/npm-cache \
 && npm install -g npm"
RUN /bin/bash -l -c "nvm install 8 \
 && npm config set cache /tmp/npm-cache \
 && npm install -g npm"
RUN /bin/bash -l -c "nvm alias default 8"
#  Prepare ccache
RUN ln -s /usr/bin/ccache /usr/local/bin/gcc
RUN ln -s /usr/bin/ccache /usr/local/bin/g++
RUN ln -s /usr/bin/ccache /usr/local/bin/cc
RUN ln -s /usr/bin/ccache /usr/local/bin/c++
RUN ln -s /usr/bin/ccache /usr/local/bin/clang
RUN ln -s /usr/bin/ccache /usr/local/bin/clang++
RUN mkdir /var/local/jenkins
#  Define the default command.
CMD ["bash"]
