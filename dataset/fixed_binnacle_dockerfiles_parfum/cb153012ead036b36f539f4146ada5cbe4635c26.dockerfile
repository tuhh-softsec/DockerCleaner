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
 && apt-get install --no-install-recommends autoconf autotools-dev build-essential bzip2 ccache curl dnsutils gcc gcc-multilib git golang gyp lcov libc6 libc6-dbg libc6-dev libgtest-dev libtool make perl strace python-dev python-setuptools python-yaml telnet unzip wget zip -y \
 && apt-get clean
# ================
#  Build profiling
RUN apt-get update \
 && apt-get install --no-install-recommends time -y \
 && apt-get clean
#  Google Cloud platform API libraries
RUN apt-get update \
 && apt-get install --no-install-recommends python-pip -y \
 && apt-get clean
RUN pip install google-api-python-client --upgrade
# ====================
#  Python dependencies
#  Install dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends python-all-dev python3-all-dev python-pip -y
#  Install Python packages from PyPI
RUN pip install pip==10.0.1 --upgrade
RUN pip install virtualenv
RUN pip install futures==2.2.0 enum34==1.0.4 protobuf==3.5.2.post1 six==1.10.0 twisted==17.5.0
# =================
#  C++ dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends libgflags-dev libgtest-dev libc++-dev clang -y \
 && apt-get clean
# ========================
#  Sanity test dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends python-pip autoconf automake libtool curl python-virtualenv python-lxml shellcheck -y
RUN pip install simplejson mako
RUN apt-get update \
 && apt-get install --no-install-recommends wget xz-utils -y
RUN wget http://releases.llvm.org/5.0.0/clang+llvm-5.0.0-linux-x86_64-ubuntu14.04.tar.xz
RUN tar xf clang+llvm-5.0.0-linux-x86_64-ubuntu14.04.tar.xz
RUN ln -s /clang+llvm-5.0.0-linux-x86_64-ubuntu14.04/bin/clang-format /usr/local/bin/clang-format
ENV CLANG_FORMAT="clang-format"
RUN ln -s /clang+llvm-5.0.0-linux-x86_64-ubuntu14.04/bin/clang-tidy /usr/local/bin/clang-tidy
ENV CLANG_TIDY="clang-tidy"
RUN mkdir /var/local/jenkins
#  Define the default command.
CMD ["bash"]
