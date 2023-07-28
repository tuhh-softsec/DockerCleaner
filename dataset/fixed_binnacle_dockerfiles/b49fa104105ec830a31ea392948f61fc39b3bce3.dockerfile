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
 && apt-get install --no-install-recommends autoconf=2.69-10 autotools-dev=20161112.1 build-essential=12.3 bzip2=1.0.6-8.1 ccache=3.3.4-1 curl=7.52.1-5+deb9u16 dnsutils=1:9.10.3.dfsg.P4-12.3+deb9u12 gcc=4:6.3.0-4 gcc-multilib=4:6.3.0-4 git=1:2.11.0-3+deb9u7 golang=2:1.7~5 gyp=0.1+20150913git1f374df9-1 lcov=1.13-1 libc6=2.24-11+deb9u4 libc6-dbg=2.24-11+deb9u4 libc6-dev=2.24-11+deb9u4 libgtest-dev=1.8.0-6 libtool=2.4.6-2 make=4.1-9.1 perl=5.24.1-3+deb9u7 strace=4.15-2 python-dev=2.7.13-2 python-setuptools=33.1.1-1 python-yaml=3.12-1 telnet=0.17-41 unzip=6.0-21+deb9u2 wget=1.18-5+deb9u3 zip=3.0-11+b1 -y \
 && apt-get clean
#  ================
#   Build profiling
RUN apt-get update \
 && apt-get install --no-install-recommends time=1.7-25.1+b1 -y \
 && apt-get clean
#  ====================
#   Python dependencies
#   Install dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends python-all-dev=2.7.13-2 python3-all-dev=3.5.3-1 python-pip=9.0.1-2+deb9u2 -y
#   Install Python packages from PyPI
RUN pip install pip==10.0.1 --upgrade
RUN pip install virtualenv==20.21.0
RUN pip install futures==2.2.0 enum34==1.0.4 protobuf==3.5.2.post1 six==1.10.0 twisted==17.5.0
#  ================
#   C# dependencies
#   cmake >=3.6 needed to build grpc_csharp_ext
RUN apt-get update \
 && apt-get install --no-install-recommends cmake=3.7.2-1 -y \
 && apt-get clean
#   Install mono
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.4.11 dirmngr=2.1.18-8~deb9u4 -y \
 && apt-get clean
RUN apt-key adv --no-tty --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb https://download.mono-project.com/repo/debian stable-stretch main" | tee /etc/apt/sources.list.d/mono-official-stable.list
RUN apt-get update \
 && apt-get install --no-install-recommends mono-devel=4.6.2.7+dfsg-1 ca-certificates-mono=4.6.2.7+dfsg-1 nuget=2.8.7+md510+dhx1-1 -y \
 && apt-get clean
#   Install dotnet SDK
ENV DOTNET_SDK_VERSION="2.1.500"
RUN curl -sSL -o dotnet.tar.gz https://dotnetcli.blob.core.windows.net/dotnet/Sdk/$DOTNET_SDK_VERSION/dotnet-sdk-$DOTNET_SDK_VERSION-linux-x64.tar.gz \
 && mkdir -p /usr/share/dotnet \
 && tar -zxf dotnet.tar.gz -C /usr/share/dotnet \
 && rm dotnet.tar.gz \
 && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet
#   Install .NET Core 1.1.10 runtime (required to run netcoreapp1.1)
RUN curl -sSL -o dotnet_old.tar.gz https://dotnetcli.blob.core.windows.net/dotnet/Runtime/1.1.10/dotnet-debian.9-x64.1.1.10.tar.gz \
 && mkdir -p dotnet_old \
 && tar zxf dotnet_old.tar.gz -C dotnet_old \
 && cp -r dotnet_old/shared/Microsoft.NETCore.App/1.1.10/ /usr/share/dotnet/shared/Microsoft.NETCore.App/ \
 && rm -rf dotnet_old/ dotnet_old.tar.gz
RUN apt-get update \
 && apt-get install --no-install-recommends libunwind8=1.1-4.1 -y \
 && apt-get clean
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
