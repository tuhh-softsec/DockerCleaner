#   Copyright 2016 gRPC authors.
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
FROM debian:jessie
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
#   Install dependencies for pyenv
RUN apt-get update \
 && apt-get install --no-install-recommends libbz2-dev libncurses5-dev libncursesw5-dev libreadline-dev libsqlite3-dev libssl-dev llvm mercurial zlib1g-dev -y \
 && apt-get clean
#   Install Pyenv and dev Python versions 3.5 and 3.6
RUN curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
ENV PATH="/root/.pyenv/bin:$PATH"
RUN eval "$( pyenv init - ;)"
RUN eval "$( pyenv virtualenv-init - ;)"
RUN pyenv update
RUN pyenv install 3.5-dev
RUN pyenv install 3.6-dev
RUN pyenv install pypy-5.3.1
RUN pyenv local 3.5-dev 3.6-dev pypy-5.3.1
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
