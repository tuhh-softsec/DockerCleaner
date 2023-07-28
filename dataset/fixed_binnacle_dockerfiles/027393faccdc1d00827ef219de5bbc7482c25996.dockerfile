FROM ubuntu:16.04
MAINTAINER Matt Konda <mkonda@jemurai.com>
#  ###############################################################################################
#         Environment
#
RUN apt-get update \
 && apt-get install --no-install-recommends git-core=1:2.7.4-0ubuntu1.10 sudo=1.8.16-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 build-essential=12.1ubuntu2 libssl-dev=1.0.2g-1ubuntu4.20 libreadline-dev=6.3-8ubuntu2 libyaml-dev=0.1.6-3 libsqlite3-dev=3.11.0-1ubuntu1.5 sqlite3=3.11.0-1ubuntu1.5 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 libffi-dev=3.2.1-4 libgdbm-dev=1.8.3-13.1 libncurses5-dev=6.0+20160213-1ubuntu1 automake=1:1.15-4ubuntu1 libtool=2.4.6-0.1 bison=2:3.0.4.dfsg-1 libffi-dev=3.2.1-4 gnupg=1.4.20-1ubuntu3.3 patch=2.7.5-1ubuntu0.16.04.2 gawk=1:4.1.3+dfsg-0.1 g++=4:5.3.1-1ubuntu1 gcc=4:5.3.1-1ubuntu1 make=4.1-6 libc6-dev=2.23-0ubuntu11.3 libcurl3-dev autoconf=2.69-9 libtool=2.4.6-0.1 ncurses-dev zlib1g=1:1.2.8.dfsg-2ubuntu4.3 libreadline6-dev=6.3-8ubuntu2 libreadline6=6.3-8ubuntu2 openssl=1.0.2g-1ubuntu4.20 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libgmp-dev=2:6.1.0+dfsg-2 clamav=0.103.2+dfsg-0ubuntu0.16.04.1 md5deep=4.4-2 nodejs=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 default-jre=2:1.8-56ubuntu2 unzip=6.0-20ubuntu1.1 python=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 -y \
 && rm -rf /var/lib/apt/lists/*
RUN echo "%sudo ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
#  ################################################################################################
#         Node Security
#              Early because it needs to run as root.
#              Requires nodejs npm
#
#  # retirejs locked to most recent pre-2.x.x version (2.x.x breaks the retirejs task)
RUN npm install nsp@3.2.1 retire@1.6.2 eslint@8.38.0 eslint-plugin-scanjs-rules@0.2.1 eslint-plugin-no-unsafe-innerhtml@1.0.16 -g
RUN ln -s /usr/bin/nodejs /usr/bin/node
#  ################################################################################################
#         User
#
RUN useradd -ms /bin/bash --groups sudo glue
USER glue
#  ################################################################################################
#         RVM / Ruby
#
#
RUN gpg --keyserver hkp://pool.sks-keyservers.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB
RUN /bin/bash -l -c "curl -L https://get.rvm.io | bash -s stable"
RUN /bin/bash -l -c "source ~/.rvm/scripts/rvm"
RUN /bin/bash -l -c "rvm requirements"
RUN /bin/bash -l -c "rvm install 2.3.1"
RUN /bin/bash -l -c "rvm use 2.3.1 --default"
RUN /bin/bash -l -c "gem install curb --no-document"
ENV PATH="$PATH:/home/glue/.rvm/rubies/ruby-2.3.1/bin"
#  ################################################################################################
#         Python
#
RUN /bin/bash -l -c "sudo pip install --upgrade pip"
RUN /bin/bash -l -c "sudo pip install bandit"
RUN /bin/bash -l -c "sudo pip install awsscout2"
#  ################################################################################################
#         Java
#
#  # JDK needed for Dependency Check Maven plugin
RUN sudo apt-get install -y software-properties-common
RUN sudo add-apt-repository -y ppa:webupd8team/java
RUN sudo apt-get update
#  # Auto-accept the Oracle JDK license
RUN echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
RUN sudo apt-get install -y oracle-java8-installer
RUN /bin/bash -l -c "mkdir -p /home/glue/tools"
WORKDIR /home/glue/tools/
#  ################################################################################################
#         Truffle Hog
#
#   RUN /bin/bash -l -c "sudo pip install truffleHog"
RUN git clone https://github.com/runako/truffleHog.git
WORKDIR /home/glue/tools/truffleHog
RUN git checkout rg-local-scan
WORKDIR /home/glue/tools/
#   OWASP DEPENDENCY CHECK (needs unzip and default-jre)
RUN curl -L http://dl.bintray.com/jeremy-long/owasp/dependency-check-1.4.3-release.zip --output owasp-dep-check.zip
RUN unzip owasp-dep-check.zip
#   Maven
RUN sudo apt-get install -y maven
#   FINDBUGS (Experimental)
#  RUN curl -L http://downloads.sourceforge.net/project/findbugs/findbugs/3.0.1/findbugs-3.0.1.zip --output findbugs.zip
#  RUN unzip findbugs.zip
#  RUN curl -L http://search.maven.org/remotecontent?filepath=com/h3xstream/findsecbugs/findsecbugs-plugin/1.4.4/findsecbugs-plugin-1.4.4.jar > findbugs-3.0.1/plugin/findsecbugs.jar
#  RUN git clone https://github.com/find-sec-bugs/find-sec-bugs.git
#   SBT plugin (for Scala)
RUN sudo apt-get update
RUN echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
RUN sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
RUN sudo apt-get install apt-transport-https
RUN sudo apt-get update
RUN sudo apt-get install sbt
RUN mkdir -p /home/glue/.sbt/0.13/plugins
RUN echo "addSbtPlugin(\"net.vonbuchholtz\" % \"sbt-dependency-check\" % \"0.1.4\")" > /home/glue/.sbt/0.13/plugins/build.sbt
#  ################################################################################################
#         Glue App
#
#  # Working Dir
RUN /bin/bash -l -c "mkdir -p /home/glue/tmp"
WORKDIR /home/glue/
#  # Core Pipeline (and ruby tools)
RUN /bin/bash -l -c "git clone https://github.com/OWASP/glue.git"
#  RUN /bin/bash -l -c "mkdir -p /home/glue/glue"
COPY . /home/glue/glue
WORKDIR /home/glue/glue
RUN /bin/bash -l -c "gem install bundler; bundle install -j20; rm owasp-glue*.gem; gem build glue.gemspec; gem install owasp-glue*.gem;"
ENTRYPOINT ["glue"]
CMD ["-h"]
# Please add your HEALTHCHECK here!!!
