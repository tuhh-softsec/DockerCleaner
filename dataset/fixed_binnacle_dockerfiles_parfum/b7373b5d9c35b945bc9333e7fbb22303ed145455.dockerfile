#  Copyright 2016 gRPC authors.
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
 && apt-get install --no-install-recommends autoconf autotools-dev build-essential bzip2 ccache curl gcc gcc-multilib git golang gyp lcov libc6 libc6-dbg libc6-dev libgtest-dev libtool make perl strace python-dev python-setuptools python-yaml telnet unzip wget zip -y \
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
# ================
#  C# dependencies
#  Update to a newer version of mono
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb http://download.mono-project.com/repo/debian jessie main" | tee /etc/apt/sources.list.d/mono-official.list
RUN echo "deb http://download.mono-project.com/repo/debian wheezy-apache24-compat main" | tee -a /etc/apt/sources.list.d/mono-xamarin.list
RUN echo "deb http://download.mono-project.com/repo/debian wheezy-libjpeg62-compat main" | tee -a /etc/apt/sources.list.d/mono-xamarin.list
#  Install dependencies
RUN apt-get update \
 && apt-get -y dist-upgrade \
 && apt-get install --no-install-recommends mono-devel ca-certificates-mono nuget -y \
 && apt-get clean
RUN nuget update -self
# =================
#  C++ dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends libgflags-dev libgtest-dev libc++-dev clang -y \
 && apt-get clean
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
# =================
#  PHP dependencies
#  Install dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends git php5 php5-dev phpunit unzip -y
# ==================
#  Ruby dependencies
#  Install rvm
RUN gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
RUN curl -sSL https://get.rvm.io | bash -s stable
#  Install Ruby 2.1
RUN /bin/bash -l -c "rvm install ruby-2.1"
RUN /bin/bash -l -c "rvm use --default ruby-2.1"
RUN /bin/bash -l -c "echo 'gem: --no-ri --no-rdoc' > ~/.gemrc"
RUN /bin/bash -l -c "echo 'export PATH=/usr/local/rvm/bin:$PATH' >> ~/.bashrc"
RUN /bin/bash -l -c "echo 'rvm --default use ruby-2.1' >> ~/.bashrc"
RUN /bin/bash -l -c "gem install bundler --no-ri --no-rdoc"
# ====================
#  Python dependencies
#  Install dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends python-all-dev python3-all-dev python-pip -y
#  Install Python packages from PyPI
RUN pip install pip --upgrade
RUN pip install virtualenv
RUN pip install futures==2.2.0 enum34==1.0.4 protobuf==3.2.0 six==1.10.0
#  Install coverage for Python test coverage reporting
RUN pip install coverage
ENV PATH="~/.local/bin:$PATH"
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
