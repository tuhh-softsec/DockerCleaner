#   Copyright (c) 2009-2019, Yegor Bugayenko
#   All rights reserved.
#
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions
#   are met: 1) Redistributions of source code must retain the above
#   copyright notice, this list of conditions and the following
#   disclaimer. 2) Redistributions in binary form must reproduce the above
#   copyright notice, this list of conditions and the following
#   disclaimer in the documentation and/or other materials provided
#   with the distribution. 3) Neither the name of the rultor.com nor
#   the names of its contributors may be used to endorse or promote
#   products derived from this software without specific prior written
#   permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT
#   NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
#   FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
#   THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
#   INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
#   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
#   STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
#   OF THE POSSIBILITY OF SUCH DAMAGE.
#   The software packages configured here (PHP, Node, Ruby, Java etc.) are for
#   the convenience of the users going to use this default container.
#   If you are going to use your own container, you may remove them.
#   Rultor has no dependency on these packages.
FROM ubuntu:16.04
MAINTAINER Yegor Bugayenko <yegor256@gmail.com>
LABEL Description="This is the default image for Rultor.com" \
      Vendor="Rultor.com" \
      Version="1.0"
WORKDIR /tmp
ENV DEBIAN_FRONTEND="noninteractive"
#   UTF-8 locale
RUN apt-get clean \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 -y ) \
 && locale-gen en_US.UTF-8
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
#   Basic Linux tools
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 bcrypt=1.1-8.1 curl=7.47.0-1ubuntu2.19 sudo=1.8.16-0ubuntu1.10 unzip=6.0-20ubuntu1.1 zip=3.0-11 gnupg=1.4.20-1ubuntu3.3 gnupg2=2.1.11-6ubuntu2.1 jq=1.5+dfsg-1ubuntu0.1 netcat-openbsd=1.105-7ubuntu1 bsdmainutils=9.0.6ubuntu3 libxml2-utils=2.9.3+dfsg1-1ubuntu0.7 build-essential=12.1ubuntu2 automake=1:1.15-4ubuntu1 autoconf=2.69-9 -y )
#   Git 2.0
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 -y ) \
 && add-apt-repository ppa:git-core/ppa \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 git-core=1:2.7.4-0ubuntu1.10 -y )
#   SSH Daemon
RUN (apt-get update ;apt-get install --no-install-recommends ssh=1:7.2p2-4ubuntu2.10 -y ) \
 && mkdir /var/run/sshd \
 && chmod 0755 /var/run/sshd
#   Ruby
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends ruby-dev=1:2.3.0+1 libmagic-dev=1:5.25-2ubuntu1.4 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y ) \
 && gpg2 --keyserver hkp://keys.gnupg.net --recv-keys D39DC0E3 \
 && gpg2 --keyserver hkp://pool.sks-keyservers.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB \
 && curl -L https://get.rvm.io | bash -s stable \
 && /bin/bash -l -c ". /etc/profile.d/rvm.sh \
 && rvm install ruby-2.6.0 \
 && rvm use 2.6.0"
#   PHP
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends php7.2 php-pear=1:1.10.1+submodules+notgz-6ubuntu0.3 php7.2-curl php7.2-dev php7.2-gd php7.2-mbstring php7.2-zip php7.2-mysql php7.2-xml -y )
RUN curl --silent --show-error https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer
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
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EEA14886 E1DF1F24 3DD9F856
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 ca-certificates=20210119~16.04.1 maven=3.3.9-3 -y )
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
#   LaTeX
RUN (apt-get update ;apt-get install --no-install-recommends texlive-latex-base=2015.20160320-1ubuntu0.1 texlive-fonts-recommended=2015.20160320-1ubuntu0.1 texlive-latex-extra=2015.20160320-1 xzdec=5.1.1alpha+20120614-2ubuntu2 -y )
RUN tlmgr init-usertree || echo 'Warning ignored'
#   PhantomJS
RUN (apt-get update ;apt-get install --no-install-recommends phantomjs=2.1.1+dfsg-1 -y )
#   S3cmd for AWS S3 integration
RUN (apt-get update ;apt-get install --no-install-recommends s3cmd=1.6.1-1 -y )
#   NodeJS
RUN rm -rf /usr/lib/node_modules \
 && curl -sL https://deb.nodesource.com/setup_6.x | bash - \
 && (apt-get update ;apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -y )
#   Postgresql
RUN echo 'deb http://apt.postgresql.org/pub/repos/apt/ xenial-pgdg main' >> /etc/apt/sources.list.d/pgdg.list \
 && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends postgresql-client-10 postgresql-10 -y )
USER postgres
RUN /etc/init.d/postgresql start \
 && psql --command "CREATE USER rultor WITH SUPERUSER PASSWORD 'rultor';" \
 && createdb -O rultor rultor
EXPOSE 5432/tcp
USER root
ENV PATH="${PATH}:/usr/lib/postgresql/10/bin"
#   Postgresql service has to be started using `sudo /etc/init.d/postgresql start` in .rultor.yml
#   Maven
ENV MAVEN_VERSION="3.3.9"
ENV M2_HOME="\"/usr/local/apache-maven/apache-maven-${MAVEN_VERSION}\""
RUN wget --quiet "http://mirror.dkd.de/apache/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz" \
 && mkdir -p /usr/local/apache-maven \
 && mv "apache-maven-${MAVEN_VERSION}-bin.tar.gz" /usr/local/apache-maven \
 && tar xzvf "/usr/local/apache-maven/apache-maven-${MAVEN_VERSION}-bin.tar.gz" -C /usr/local/apache-maven/ \
 && update-alternatives --install /usr/bin/mvn mvn "${M2_HOME}/bin/mvn" 1 \
 && update-alternatives --config mvn
#   Warming it up a bit
RUN /bin/bash -l -c "gem install jekyll:3.4.3"
ENV MAVEN_OPTS="\"-Xms512m -Xmx2g\""
COPY settings.xml /root/.m2/settings.xml
RUN git clone https://github.com/yegor256/rultor.git --depth=1
RUN cd rultor \
 && mvn clean install -DskipTests -Pqulice --quiet \
 && cd .. \
 && rm -rf rultor
#   Clean up
RUN rm -rf /tmp/* \
 && rm -rf /root/.ssh
ENTRYPOINT ["/bin/bash", "-l", "-c"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
