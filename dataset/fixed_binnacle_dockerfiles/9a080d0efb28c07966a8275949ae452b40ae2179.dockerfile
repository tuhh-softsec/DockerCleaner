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
FROM debian:stretch
#   Install Git and basic packages.
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-10 autotools-dev=20161112.1 build-essential=12.3 bzip2=1.0.6-8.1 ccache=3.3.4-1 curl=7.52.1-5+deb9u16 dnsutils=1:9.10.3.dfsg.P4-12.3+deb9u12 gcc=4:6.3.0-4 gcc-multilib=4:6.3.0-4 git=1:2.11.0-3+deb9u7 golang=2:1.7~5 gyp=0.1+20150913git1f374df9-1 lcov=1.13-1 libc6=2.24-11+deb9u4 libc6-dbg=2.24-11+deb9u4 libc6-dev=2.24-11+deb9u4 libgtest-dev=1.8.0-6 libtool=2.4.6-2 make=4.1-9.1 perl=5.24.1-3+deb9u7 strace=4.15-2 python-dev=2.7.13-2 python-setuptools=33.1.1-1 python-yaml=3.12-1 telnet=0.17-41 unzip=6.0-21+deb9u2 wget=1.18-5+deb9u3 zip=3.0-11+b1 -y ) \
 && apt-get clean
#  ================
#   Build profiling
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends time=1.7-25.1+b1 -y ) \
 && apt-get clean
#   Google Cloud platform API libraries
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends python-pip=9.0.1-2+deb9u2 -y ) \
 && apt-get clean
RUN pip install google-api-python-client==2.85.0 oauth2client==4.1.3 --upgrade
#   Install Python 2.7
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends python2.7=2.7.13-2+deb9u6 python-all-dev=2.7.13-2 -y )
RUN curl https://bootstrap.pypa.io/get-pip.py | python2.7
#   Add Debian 'testing' repository
RUN echo 'deb http://ftp.de.debian.org/debian testing main' >> /etc/apt/sources.list
RUN echo 'APT::Default-Release "stable";' | tee -a /etc/apt/apt.conf.d/00local
RUN mkdir /var/local/jenkins
#   Define the default command.
CMD ["bash"]
#  =================
#   C++ dependencies
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libgflags-dev=2.1.2-4 libgtest-dev=1.8.0-6 libc++-dev=3.5-2 clang=1:3.8-36 -y ) \
 && apt-get clean
#  ========================
#   Sanity test dependencies
RUN : \
 && apt-get -t testing install -y python3.7 python3-all-dev
RUN curl https://bootstrap.pypa.io/get-pip.py | python3.7
#   Make Python 3.7 the default Python 3 version
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.7 1
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-10 automake=1:1.15-6 libtool=2.4.6-2 curl=7.52.1-5+deb9u16 shellcheck=0.4.4-4 -y )
RUN python2 -m pip install simplejson mako virtualenv lxml
RUN python3 -m pip install simplejson mako virtualenv lxml
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends wget=1.18-5+deb9u3 xz-utils=5.2.2-1.2+deb9u1 -y )
RUN wget http://releases.llvm.org/5.0.0/clang+llvm-5.0.0-linux-x86_64-ubuntu14.04.tar.xz
RUN tar xf clang+llvm-5.0.0-linux-x86_64-ubuntu14.04.tar.xz
RUN ln -s /clang+llvm-5.0.0-linux-x86_64-ubuntu14.04/bin/clang-format /usr/local/bin/clang-format
ENV CLANG_FORMAT="clang-format"
RUN ln -s /clang+llvm-5.0.0-linux-x86_64-ubuntu14.04/bin/clang-tidy /usr/local/bin/clang-tidy
ENV CLANG_TIDY="clang-tidy"
#  ========================
#   Bazel installation
#   Must be in sync with tools/bazel
ENV BAZEL_VERSION="0.26.0"
#   The correct bazel version is already preinstalled, no need to use //tools/bazel wrapper.
ENV DISABLE_BAZEL_WRAPPER="1"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends wget=1.18-5+deb9u3 -y ) \
 && apt-get clean
RUN wget "https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh" \
 && bash ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && rm bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#   Define the default command.
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
