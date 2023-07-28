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
FROM debian:stretch
#   Install Git and basic packages.
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-10 autotools-dev=20161112.1 build-essential=12.3 bzip2=1.0.6-8.1 ccache=3.3.4-1 curl=7.52.1-5+deb9u16 dnsutils=1:9.10.3.dfsg.P4-12.3+deb9u12 gcc=4:6.3.0-4 gcc-multilib=4:6.3.0-4 git=1:2.11.0-3+deb9u7 golang=2:1.7~5 gyp=0.1+20150913git1f374df9-1 lcov=1.13-1 libc6=2.24-11+deb9u4 libc6-dbg=2.24-11+deb9u4 libc6-dev=2.24-11+deb9u4 libgtest-dev=1.8.0-6 libtool=2.4.6-2 make=4.1-9.1 perl=5.24.1-3+deb9u7 strace=4.15-2 python-dev=2.7.13-2 python-setuptools=33.1.1-1 python-yaml=3.12-1 telnet=0.17-41 unzip=6.0-21+deb9u2 wget=1.18-5+deb9u3 zip=3.0-11+b1 -y \
 && apt-get clean
#  ================
#   Build profiling
RUN apt-get update \
 && apt-get install --no-install-recommends time=1.7-25.1+b1 -y \
 && apt-get clean
#   Google Cloud platform API libraries
RUN apt-get update \
 && apt-get install --no-install-recommends python-pip=9.0.1-2+deb9u2 -y \
 && apt-get clean
RUN pip install google-api-python-client==2.85.0 --upgrade
#  ====================
#   Python dependencies
#   Install dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends python-all-dev=2.7.13-2 python3-all-dev=3.5.3-1 python-pip=9.0.1-2+deb9u2 -y
#   Install Python packages from PyPI
RUN pip install pip==10.0.1 --upgrade
RUN pip install virtualenv==20.21.0
RUN pip install futures==2.2.0 enum34==1.0.4 protobuf==3.5.2.post1 six==1.10.0 twisted==17.5.0
#   Install dependencies for pyenv
RUN apt-get update \
 && apt-get install --no-install-recommends libbz2-dev=1.0.6-8.1 libncurses5-dev=6.0+20161126-1+deb9u2 libncursesw5-dev=6.0+20161126-1+deb9u2 libreadline-dev=7.0-3 libsqlite3-dev=3.16.2-5+deb9u3 libssl-dev=1.1.0l-1~deb9u6 llvm=1:3.8-36 mercurial=4.0-1+deb9u2 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 -y \
 && apt-get clean
#   Install Pyenv and dev Python versions 3.{5,6,7}
RUN curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
ENV PATH="/root/.pyenv/bin:$PATH"
RUN eval "$( pyenv init - ;)"
RUN eval "$( pyenv virtualenv-init - ;)"
RUN pyenv update
RUN pyenv install 3.5-dev
RUN pyenv install 3.6-dev
RUN pyenv install 3.7-dev
RUN pyenv install pypy-5.3.1
RUN pyenv local 3.5-dev 3.6-dev 3.7-dev pypy-5.3.1
#   Install pip and virtualenv for Python 3.5
RUN curl https://bootstrap.pypa.io/get-pip.py | python3.5
RUN python3.5 -m pip install virtualenv
RUN mkdir /var/local/jenkins
#   Define the default command.
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
