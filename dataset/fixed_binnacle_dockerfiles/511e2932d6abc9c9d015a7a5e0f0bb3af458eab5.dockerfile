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
FROM debian:jessie
#   Install Git and basic packages.
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf autotools-dev build-essential bzip2 ccache curl dnsutils gcc gcc-multilib git golang gyp lcov libc6 libc6-dbg libc6-dev libgtest-dev libtool make perl strace python-dev python-setuptools python-yaml telnet unzip wget zip -y \
 && apt-get clean
#  ================
#   Build profiling
RUN apt-get update \
 && apt-get install --no-install-recommends time -y \
 && apt-get clean
#  ====================
#   Python dependencies
#   Install dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends python-all-dev python3-all-dev python-pip -y
#   Install Python packages from PyPI
RUN pip install pip==10.0.1 --upgrade
RUN pip install virtualenv==20.21.0
RUN pip install futures==2.2.0 enum34==1.0.4 protobuf==3.5.2.post1 six==1.10.0 twisted==17.5.0
#  ================
#   C# dependencies
#   Update to a newer version of mono
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb http://download.mono-project.com/repo/debian jessie main" | tee /etc/apt/sources.list.d/mono-official.list
RUN echo "deb http://download.mono-project.com/repo/debian wheezy-apache24-compat main" | tee -a /etc/apt/sources.list.d/mono-xamarin.list
RUN echo "deb http://download.mono-project.com/repo/debian wheezy-libjpeg62-compat main" | tee -a /etc/apt/sources.list.d/mono-xamarin.list
#   Install dependencies
RUN apt-get update \
 && apt-get -y dist-upgrade \
 && apt-get install --no-install-recommends mono-devel ca-certificates-mono nuget -y \
 && apt-get clean
RUN nuget update -self
#   Install dotnet SDK based on https://www.microsoft.com/net/core#debian
RUN apt-get update \
 && apt-get install --no-install-recommends curl libunwind8 gettext -y
#   dotnet-dev-1.0.0-preview2-003131
RUN curl -sSL -o dotnet100.tar.gz https://go.microsoft.com/fwlink/?LinkID=827530
RUN mkdir -p /opt/dotnet \
 && tar zxf dotnet100.tar.gz -C /opt/dotnet
#   dotnet-dev-1.0.1
RUN curl -sSL -o dotnet101.tar.gz https://go.microsoft.com/fwlink/?LinkID=843453
RUN mkdir -p /opt/dotnet \
 && tar zxf dotnet101.tar.gz -C /opt/dotnet
RUN ln -s /opt/dotnet/dotnet /usr/local/bin
#   Trigger the population of the local package cache
ENV NUGET_XMLDOC_MODE="skip"
RUN mkdir warmup \
 && cd warmup \
 && dotnet new \
 && cd .. \
 && rm -rf warmup
RUN mkdir /var/local/jenkins
#   Define the default command.
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
