#   Copyright (c) 2017, comdor.co
#   All rights reserved.
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions are met:
#    1)Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#    2)Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#    3)Neither the name of comdor nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
#   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
#   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
FROM ubuntu:14.04
MAINTAINER Mihai Andronache <amihaiemil@gmail.com>
LABEL Description="This is the default Docker image for comdor.co" \
      Vendor="comdor.co" \
      Version="1.0"
WORKDIR /tmp
ENV DEBIAN_FRONTEND="noninteractive"
#   UTF-8 locale
RUN locale-gen en_US en_US.UTF-8
RUN dpkg-reconfigure locales
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
#   Basic Linux tools
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 bcrypt=1.1-8 curl=7.35.0-1ubuntu2.20 -y )
RUN (apt-get update ;apt-get install --no-install-recommends unzip=6.0-9ubuntu1.5 zip=3.0-8 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gnupg=1.4.16-1ubuntu2.6 gnupg2=2.0.22-3ubuntu1.4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends jq=1.3-1.1ubuntu1.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends cloc=1.60-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends bsdmainutils=9.0.5ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libxml2-utils=2.9.1+dfsg1-3ubuntu4.13 -y )
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 -y )
RUN (apt-get update ;apt-get install --no-install-recommends automake=1:1.14.1-2ubuntu1 autoconf=2.69-6 -y )
#   Git 2.0
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 python-software-properties=0.92.37.8 -y )
RUN add-apt-repository ppa:git-core/ppa
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 git-core=1:1.9.1-1ubuntu0.10 -y )
#   SSH Daemon
RUN (apt-get update ;apt-get install --no-install-recommends ssh=1:6.6p1-2ubuntu2.13 -y ) \
 && mkdir /var/run/sshd \
 && chmod 0755 /var/run/sshd
#   Ruby
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends ruby-dev=1:1.9.3.4 libmagic-dev=1:5.14-2ubuntu3.3 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1 -y )
RUN gpg2 --keyserver hkp://keys.gnupg.net --recv-keys D39DC0E3
RUN curl -L https://get.rvm.io | bash -s stable
RUN /bin/bash -l -c "rvm requirements"
RUN /bin/bash -l -c "rvm install 2.3.3"
RUN /bin/bash -l -c "gem update \
 && gem install --no-ri --no-rdoc nokogiri:1.6.7.2 bundler:1.11.2"
#   PHP
RUN (apt-get update ;apt-get install --no-install-recommends php5=5.5.9+dfsg-1ubuntu4.29 php5-dev=5.5.9+dfsg-1ubuntu4.29 php-pear=5.5.9+dfsg-1ubuntu4.29 -y )
RUN curl --silent --show-error https://getcomposer.org/installer | php
RUN mv composer.phar /usr/local/bin/composer
RUN mkdir jsl \
 && wget --quiet http://www.javascriptlint.com/download/jsl-0.3.0-src.tar.gz \
 && tar xzf jsl-0.3.0-src.tar.gz \
 && cd jsl-0.3.0/src \
 && make -f Makefile.ref \
 && mv Linux_All_DBG.OBJ/jsl /usr/local/bin \
 && cd .. \
 && rm -rf jsl
#   RUN pecl install xdebug-beta && \
#     echo "zend_extension=xdebug.so" > /etc/php5/cli/conf.d/xdebug.ini
#   Java
RUN (apt-get update ;apt-get install --no-install-recommends default-jdk=2:1.7-51 -y )
#   PhantomJS
RUN (apt-get update ;apt-get install --no-install-recommends phantomjs=1.9.0-1 -y )
#   NodeJS
RUN rm -rf /usr/lib/node_modules
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 -y )
#   Maven
ENV MAVEN_VERSION="3.3.9"
ENV M2_HOME="\"/usr/local/apache-maven/apache-maven-${MAVEN_VERSION}\""
RUN wget --quiet "http://mirror.dkd.de/apache/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz" \
 && mkdir -p /usr/local/apache-maven \
 && mv "apache-maven-${MAVEN_VERSION}-bin.tar.gz" /usr/local/apache-maven \
 && tar xzvf "/usr/local/apache-maven/apache-maven-${MAVEN_VERSION}-bin.tar.gz" -C /usr/local/apache-maven/ \
 && update-alternatives --install /usr/bin/mvn mvn "${M2_HOME}/bin/mvn" 1 \
 && update-alternatives --config mvn
#   Clean up
RUN rm -rf /tmp/*
RUN rm -rf /root/.ssh
ENTRYPOINT ["/bin/bash", "-l", "-c"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
