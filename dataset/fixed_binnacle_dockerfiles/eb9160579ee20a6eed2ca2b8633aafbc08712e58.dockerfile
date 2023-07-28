#   Licensed to the Apache Software Foundation (ASF) under one
#   or more contributor license agreements.  See the NOTICE file
#   distributed with this work for additional information
#   regarding copyright ownership.  The ASF licenses this file
#   to you under the Apache License, Version 2.0 (the
#   "License"); you may not use this file except in compliance
#   with the License.  You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#   Dockerfile for installing the necessary dependencies for building Avro.
#   See BUILD.txt.
FROM openjdk:8
WORKDIR /root
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#   Add the repository for node.js 6.x
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
#   Register Microsoft key and feed for .NET SDK
#   https://dotnet.microsoft.com/download/linux-package-manager/debian8/sdk-current
RUN wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.asc.gpg \
 && mv microsoft.asc.gpg /etc/apt/trusted.gpg.d/ \
 && wget -q https://packages.microsoft.com/config/debian/9/prod.list \
 && mv prod.list /etc/apt/sources.list.d/microsoft-prod.list \
 && chown root:root /etc/apt/trusted.gpg.d/microsoft.asc.gpg \
 && chown root:root /etc/apt/sources.list.d/microsoft-prod.list
RUN curl https://packages.sury.org/php/apt.gpg | apt-key add --no-tty - \
 && echo "deb https://packages.sury.org/php/ $( lsb_release -sc ;) main" > /etc/apt/sources.list.d/php.list
#   Install dependencies from packages
RUN apt-get update -qq \
 && apt-get install --no-install-recommends ant=1.10.9-4 asciidoc=9.0.0~rc2-1 bison=2:3.7.5+dfsg-1 bzip2=1.0.8-4 cmake=3.18.4-2+deb11u1 curl=7.74.0-1.3+deb11u7 dotnet-sdk-2.2 doxygen=1.9.1-1 flex=2.6.4-8 g++=4:10.2.1-1 gcc=4:10.2.1-1 git=1:2.30.2-1+deb11u2 libboost-all-dev=1.74.0.3 libfontconfig1-dev=2.13.1-4.2 libfreetype6-dev=2.10.4+dfsg-1+deb11u1 libglib2.0-dev=2.66.8-1 libjansson-dev=2.13.1-1.1 libsnappy-dev=1.1.8-1 libsnappy1v5=1.1.8-1 make=4.3-4.1 maven=3.6.3-5 nodejs=12.22.12~dfsg-1~deb11u3 perl=5.32.1-4+deb11u2 php5.6 php5.6-gmp python python-setuptools=44.1.1-1 python-snappy python3-setuptools=52.0.0-4 python3-snappy=0.5.3-1.1+b3 rake=13.0.3-1 ruby=1:2.7+2 ruby-dev=1:2.7+2 source-highlight=3.1.9-3+b1 subversion=1.14.1-3+deb11u1 valgrind=1:3.16.1-1 -qq -y \
 && apt-get -qq clean \
 && rm -rf /var/lib/apt/lists/*
#   Install Perl modules
RUN curl -L https://cpanmin.us | perl - --mirror https://www.cpan.org/ --self-upgrade \
 && cpanm install --mirror https://www.cpan.org/ Module::Install Module::Install::ReadmeFromPod Module::Install::Repository Math::BigInt JSON::XS Try::Tiny Regexp::Common Encode IO::String Object::Tiny Compress::Zlib Test::More Test::Exception Test::Pod
#   Install PHPUnit
RUN wget -O /usr/local/bin/phpunit https://phar.phpunit.de/phpunit-5.6.phar \
 && chmod +x /usr/local/bin/phpunit
#   Install Ruby modules
RUN gem install echoe --version 4.6.6
#   Install global Node modules
RUN npm install grunt-cli@1.4.3 -g
CMD ["/avro/share/docker/run-tests.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
