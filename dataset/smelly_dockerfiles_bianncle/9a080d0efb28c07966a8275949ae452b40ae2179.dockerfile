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
FROM debian:stretch
#  Install Git and basic packages.
RUN apt-get update \
 && apt-get install autoconf autotools-dev build-essential bzip2 ccache curl dnsutils gcc gcc-multilib git golang gyp lcov libc6 libc6-dbg libc6-dev libgtest-dev libtool make perl strace python-dev python-setuptools python-yaml telnet unzip wget zip -y \
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
RUN pip install google-api-python-client oauth2client --upgrade
#  Install Python 2.7
RUN apt-get update \
 && apt-get install python2.7 python-all-dev -y
RUN curl https://bootstrap.pypa.io/get-pip.py | python2.7
#  Add Debian 'testing' repository
RUN echo 'deb http://ftp.de.debian.org/debian testing main' >> /etc/apt/sources.list
RUN echo 'APT::Default-Release "stable";' | tee -a /etc/apt/apt.conf.d/00local
RUN mkdir /var/local/jenkins
#  Define the default command.
CMD ["bash"]
# =================
#  C++ dependencies
RUN apt-get update \
 && apt-get install libgflags-dev libgtest-dev libc++-dev clang -y \
 && apt-get clean
# ========================
#  Sanity test dependencies
RUN apt-get update \
 && apt-get -t testing install -y python3.7 python3-all-dev
RUN curl https://bootstrap.pypa.io/get-pip.py | python3.7
#  Make Python 3.7 the default Python 3 version
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.7 1
RUN apt-get update \
 && apt-get install autoconf automake libtool curl shellcheck -y
RUN python2 -m pip install simplejson mako virtualenv lxml
RUN python3 -m pip install simplejson mako virtualenv lxml
RUN apt-get update \
 && apt-get install wget xz-utils -y
RUN wget http://releases.llvm.org/5.0.0/clang+llvm-5.0.0-linux-x86_64-ubuntu14.04.tar.xz
RUN tar xf clang+llvm-5.0.0-linux-x86_64-ubuntu14.04.tar.xz
RUN ln -s /clang+llvm-5.0.0-linux-x86_64-ubuntu14.04/bin/clang-format /usr/local/bin/clang-format
ENV CLANG_FORMAT="clang-format"
RUN ln -s /clang+llvm-5.0.0-linux-x86_64-ubuntu14.04/bin/clang-tidy /usr/local/bin/clang-tidy
ENV CLANG_TIDY="clang-tidy"
# ========================
#  Bazel installation
#  Must be in sync with tools/bazel
ENV BAZEL_VERSION="0.26.0"
#  The correct bazel version is already preinstalled, no need to use //tools/bazel wrapper.
ENV DISABLE_BAZEL_WRAPPER="1"
RUN apt-get update \
 && apt-get install wget -y \
 && apt-get clean
RUN wget "https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh" \
 && bash ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && rm bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#  Define the default command.
CMD ["bash"]
