#   This Dockerfile specifies the recipe for creating an image for the tests
#   to run in.
#
#   We install as many test dependencies here as we can, because these setup
#   steps can be cached.  They do *not* run every time we run the build.
#   The Docker image is only rebuilt when the Dockerfile (ie. this file)
#   changes.
#   Base Dockerfile for gRPC dev images
FROM debian:latest
#   Apt source for old Python versions.
RUN echo 'deb http://ppa.launchpad.net/fkrull/deadsnakes/ubuntu trusty main' > /etc/apt/sources.list.d/deadsnakes.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys DB82666C
#   Apt source for Oracle Java.
RUN echo 'deb http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main' > /etc/apt/sources.list.d/webupd8team-java-trusty.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EEA14886 \
 && echo "oracle-java7-installer shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections
#   Apt source for Mono
RUN echo "deb http://download.mono-project.com/repo/debian wheezy main" | tee /etc/apt/sources.list.d/mono-xamarin.list \
 && echo "deb http://download.mono-project.com/repo/debian wheezy-libjpeg62-compat main" | tee -a /etc/apt/sources.list.d/mono-xamarin.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
#   Install dependencies.  We start with the basic ones require to build protoc
#   and the C++ build
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.71-3 autotools-dev=20220109.1 build-essential=12.9ubuntu3 bzip2=1.0.8-5build1 ccache=4.7.4-1 curl=7.88.1-7ubuntu1 gcc=4:12.2.0-3ubuntu1 git=1:2.39.2-1ubuntu1 libc6=2.37-0ubuntu2 libc6-dbg=2.37-0ubuntu2 libc6-dev=2.37-0ubuntu2 libgtest-dev=1.12.1-0.2 libtool=2.4.7-5 make=4.3-4.1build1 parallel=20221122+ds-2 time=1.9-0.2 wget=1.21.3-1ubuntu1 mono-devel=6.8.0.105+dfsg-3.3 referenceassemblies-pcl nunit=2.6.4+dfsg-1.1 maven=3.8.7-1 openjdk-7-jdk oracle-java7-installer python-setuptools python-pip python-dev python2.6-dev python3.3-dev python3.4-dev ruby=1:3.1 -y \
 && apt-get clean
#  #################
#   C# dependencies
RUN wget www.nuget.org/NuGet.exe -O /usr/local/bin/nuget.exe
#  #################
#   Python dependencies
#   These packages exist in apt-get, but their versions are too old, so we have
#   to get updates from pip.
RUN pip install pip==23.1 --upgrade
RUN pip install virtualenv==20.21.0 tox==4.4.12 yattag==1.15.1
#  #################
#   Ruby dependencies
#   Install rvm
RUN gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
RUN curl -sSL https://get.rvm.io | bash -s stable
#   Install Ruby 2.1
RUN /bin/bash -l -c "rvm install ruby-2.1"
RUN /bin/bash -l -c "rvm use --default ruby-2.1"
RUN /bin/bash -l -c "echo 'gem: --no-ri --no-rdoc' > ~/.gemrc"
RUN /bin/bash -l -c "echo 'export PATH=/usr/local/rvm/bin:$PATH' >> ~/.bashrc"
RUN /bin/bash -l -c "echo 'rvm --default use ruby-2.1' >> ~/.bashrc"
RUN /bin/bash -l -c "gem install bundler --no-ri --no-rdoc"
#  #################
#   Java dependencies
#   This step requires compiling protoc. :(
ENV MAVEN_REPO="/var/maven_local_repository"
ENV MVN="mvn --batch-mode"
RUN cd /tmp \
 && git clone https://github.com/google/protobuf.git \
 && cd protobuf \
 && ./autogen.sh \
 && ./configure \
 && make -j6 \
 && cd java \
 && $MVN install dependency:go-offline -Dmaven.repo.local=$MAVEN_REPO -P lite \
 && $MVN install dependency:go-offline -Dmaven.repo.local=$MAVEN_REPO \
 && cd ../javanano \
 && $MVN install dependency:go-offline -Dmaven.repo.local=$MAVEN_REPO
#  #################
#   Prepare ccache
RUN ln -s /usr/bin/ccache /usr/local/bin/gcc
RUN ln -s /usr/bin/ccache /usr/local/bin/g++
RUN ln -s /usr/bin/ccache /usr/local/bin/cc
RUN ln -s /usr/bin/ccache /usr/local/bin/c++
RUN ln -s /usr/bin/ccache /usr/local/bin/clang
RUN ln -s /usr/bin/ccache /usr/local/bin/clang++
#   Define the default command.
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
